use std::{collections::HashMap, rc::Rc, cell::RefCell};

use num_traits::Pow;

use crate::{value::{Value, Complex, func::{Func, CIterator}, TypeData, CxprStruct}, expr::{Stmt, Expr}, token::{TokenType, Token, OpType}, RuntimeError, Position, env::{EnvRef, Environment}};

thread_local!(static PIPE_NAME: Option<Rc<str>> = Some(Rc::from("<pipeline>")));
thread_local!(static FORLOOP_NAME: Option<Rc<str>> = Some(Rc::from("<for loop>")));
fn exit_pipe(pos: &Position) -> impl FnOnce(RuntimeError) -> RuntimeError + '_ {
    |e: RuntimeError| e.exit_fn(PIPE_NAME.with(|x| x.clone()), pos.clone())
}


#[derive(Debug)]
pub enum Unwind {
    Continue{pos: Position}, 
    Break{pos: Position}, 
    Return{pos: Position, value: Value}, 
    Error(RuntimeError)
}

impl Unwind {
    pub fn as_error(self) -> RuntimeError {
        match self {
            Self::Error(e) => e,
            Self::Continue { pos } => RuntimeError::new("continue statement outside of loop", pos),
            Self::Break { pos } => RuntimeError::new("break statement outside of loop", pos),
            Self::Return { pos, .. } => RuntimeError::new("return statement outside of function", pos),
        }
    }
}

impl From<RuntimeError> for Unwind {
    fn from(e: RuntimeError) -> Self {
        Self::Error(e)
    }
}

//
// Statements
//

fn unwrap_ident_token(tok: &Token) -> &Rc<str> {
    if let Token { ty: TokenType::Ident(s),.. } = tok {
        s
    } else {
        unreachable!("precondition failed")
    }
}

pub fn eval_stmt(stmt: &Stmt, env: EnvRef) -> Result<(), Unwind> {
    match stmt {
        Stmt::Expr{ expr } 
            => drop(eval_expr(expr, env)?),
        Stmt::Let { lhs, rhs: None } 
            => env.borrow_mut().declare(unwrap_ident_token(lhs).clone(), Value::Nil),
        Stmt::Let { lhs, rhs: Some(rhs) } => {
            let r = eval_expr(rhs, env.clone())?;
            env.borrow_mut().declare(unwrap_ident_token(lhs).clone(), r)
        },
        Stmt::Block { stmts } => {
            let block_env = Environment::extend(env).wrap();
            for stmt in stmts {
                eval_stmt(stmt, block_env.clone())?;
            }
        },
        Stmt::If { if_clauses, else_clause } => {
            for ic in if_clauses {
                let cond = eval_expr(&ic.0, env.clone())?;
                if cond.truthy() {
                    return eval_stmt(&ic.1, env)
                }
            }
            if let Some(ec) = else_clause {
                return eval_stmt(ec, env)
            }
        },
        Stmt::For { var, expr, stmt, iter_pos } => {
            let name = unwrap_ident_token(var);
            let iter = eval_expr(expr, env.clone())?;
            env.borrow_mut().declare(name.clone(), Value::Nil);
            let iterator = iter.iter();
            if let Err(e) = iterator {
                return Err(RuntimeError::new(e, iter_pos.clone()).into())
            }
            if let Ok(i) = iterator {
                for v in i {
                    let v = v.map_err(|e| e.exit_fn(FORLOOP_NAME.with(|x| x.clone()), iter_pos.clone()))?;
                    let env = env.clone();
                    env.borrow_mut().set(name.clone(), v).expect("unreachable");
                    match eval_stmt(stmt, env) {
                        Ok(_) => (),
                        Err(Unwind::Break{..}) => break,
                        Err(Unwind::Continue{..}) => continue,
                        Err(e) => return Err(e)
                    }
                }
            } 
        },
        Stmt::While { expr, stmt } => {
            loop {
                let cond = eval_expr(expr, env.clone())?;
                if !cond.truthy() {
                    break
                }
                match eval_stmt(stmt, env.clone()) {
                    Ok(_) => (),
                    Err(Unwind::Break{..}) => break,
                    Err(Unwind::Continue{..}) => continue,
                    Err(e) => return Err(e)
                }
            }
        },
        Stmt::Fn { name, args, body } => {
            let name = name.ty.clone().as_ident().unwrap();
            let func = Func::Func { 
                name: Some(name.clone()),
                args: args.iter().map(|a| a.ty.clone().as_ident().unwrap()).collect(),
                env: env.clone(),
                func: Box::new(body.as_ref().clone())
            };
            env.borrow_mut().declare(name, Value::Func(func));
        },
        Stmt::Break { pos } => return Err(Unwind::Break { pos: pos.clone() }),
        Stmt::Continue { pos } => return Err(Unwind::Continue { pos: pos.clone() }),
        Stmt::Return { pos, expr } => {
            let value = eval_expr(expr, env)?;
            return Err(Unwind::Return { pos: pos.clone(), value })
        },
        Stmt::StructDef { name, ty } => env.borrow_mut().declare(name.ty.clone().as_ident().unwrap(), Value::Type(ty.clone()))
    }
    Ok(())
}

//
// Expressions
//

pub fn eval_expr(expr: &Expr, env: EnvRef) -> Result<Value, RuntimeError> {
    match expr {
        Expr::Literal { value } => Ok(eval_literal(value)),
        Expr::Ident { value } => eval_ident(value, env),
        Expr::Binary { lhs, rhs, op } => match op.ty.get_op_type() {
            Some(OpType::Additive) 
            | Some(OpType::Multiplicative) 
            | Some(OpType::Exponential)
            | Some(OpType::Comparison) => {
                let l = eval_expr(lhs, env.clone())?;
                let r = eval_expr(rhs, env)?;
                eval_standard_binary(l, r, &op.ty, &op.pos)
            },
            Some(OpType::Assignment) 
                => eval_assignment(lhs, rhs, op, env),
            Some(OpType::LogicalAnd) | Some(OpType::LogicalOr)
                => eval_boolean(lhs, rhs, op, env),
            Some(OpType::Pipeline) => {
                let l = eval_expr(lhs, env.clone())?;
                let r = eval_expr(rhs, env)?;
                let f = r.as_func().map_err(|e| RuntimeError::new(e, op.pos.clone()))?;
                eval_pipeline(l, f, op).map_err(exit_pipe(&op.pos))
            }
            o => todo!("{:?}", o) // TODO other operations
        },
        Expr::Ternary { arg1, arg2, arg3, op } => eval_ternary(arg1, arg2, arg3, op, env),
        Expr::Unary { arg, op } => eval_unary(arg, op, env),
        Expr::Range { start, end, step, incl } 
            => eval_range(start, 
                end.as_ref().map(|x| x.as_ref()), 
                step.as_ref().map(|x| x.as_ref()), 
                *incl, env),
        Expr::BoxedInfix { func } => Ok(Value::Func(func.clone())),
        Expr::FieldAccess { target, name, .. } => {
            let target = eval_expr(target, env)?;
            match target {
                Value::Struct(s) => {
                    if let Some(v) = s.data.clone().borrow().get(name.as_ref()) {
                        Ok(v.clone())
                    } else {
                        Err(format!("Struct {} has no field '{}'", Value::Struct(s).repr(), name).into())
                    }
                },
                _ => Err(format!("{} is not a struct", target.repr()).into())
            }
        },
        Expr::List { items } => {
            let mut list = Vec::with_capacity(items.len());
            for item in items {
                list.push(eval_expr(item, env.clone())?);
            }
            Ok(Value::from(list))
        },
        Expr::Map { items } => {
            let mut map = HashMap::with_capacity(items.len());
            for (k, v) in items {
                let key = eval_expr(k, env.clone())?;
                let value = eval_expr(v, env.clone())?;
                map.insert(key, value);
            }
            Ok(Value::from(map))
        },
        Expr::FuncCall { func, args, pos } => {
            let lhs = eval_expr(func, env.clone())?;
            let mut arg_values = Vec::with_capacity(args.len());
            for arg in args {
                let result = eval_expr(arg, env.clone())?;
                arg_values.push(result);
            }
            let func = lhs.as_func()
                .map_err(|e| RuntimeError::new(e, pos.clone()))?;
            func.call(arg_values).map_err(|e| e.exit_fn(func.name(), pos.clone()))
        },
        Expr::Index { lhs, index, pos } => {
            let l = eval_expr(lhs, env.clone())?;
            let idx = eval_expr(index, env)?;
            l.index(&idx).map_err(|e| RuntimeError::new(e, pos.clone()))
        },
        Expr::Fn { args, body } => {
            let func = Func::Func { 
                name: None,
                args: args.iter().map(|a| a.ty.clone().as_ident().unwrap()).collect(),
                env,
                func: Box::new(body.as_ref().clone())
            };
            Ok(Value::Func(func))
        },
        Expr::StructInit { ty, args, .. } => {
            let ty_val = eval_expr(ty, env.clone())?;
            let ty = match ty_val {
                Value::Type(ty) => ty,
                _ => return Err(format!("'{}' is not a type", ty_val.repr()).into())
            };
            match ty.typedata {
                TypeData::None => Err(format!("'{}' is not a struct type", Value::Type(ty).repr()).into()),
                TypeData::StructFields(ref fields) => if fields.len() == args.len() {
                    let mut data = HashMap::new();
                    for (k, v) in fields.iter().zip(args.iter()) {
                        data.insert(k.to_owned(), eval_expr(v, env.clone())?);
                    }
                    let result = CxprStruct {
                        ty: ty.clone(),
                        data: Rc::new(RefCell::new(data))
                    };
                    Ok(Value::Struct(result))
                } else {
                    Err(format!("Wrong number of fields for type '{}', expected {}, got {}", ty.name, fields.len(), args.len()).into())
                }
            }
        }
    }
}

pub fn eval_literal(token: &Token) -> Value {
    match &token.ty {
        TokenType::Nil => Value::Nil,
        TokenType::True => Value::Bool(true),
        TokenType::False => Value::Bool(false),
        TokenType::Int(n) => Value::Int(*n),
        TokenType::Float(f) => Value::Float(*f),
        TokenType::ImFloat(f) => Value::Complex(Complex::new(0.0, *f)),
        TokenType::String(s) => Value::String(s.clone()),
        TokenType::Char(c) => Value::Char(*c),
        _ => todo!()
    }
}

pub fn eval_ident(token: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    if let Token { ty: TokenType::Ident(name), ..} = token {
        env.borrow_mut()
            .get(name)
            .ok_or_else(|| RuntimeError::new(format!("Variable {} not defined in scope", name), token.pos.clone()))
    } else { 
        unreachable!() 
    }
}

fn compound_assignment_inner(l: &Value, r: &Value, op: &Token) -> Result<Value, RuntimeError> {
    match op.ty {
        TokenType::PlusEqual => l + r,
        TokenType::MinusEqual => l - r,
        TokenType::StarEqual => l * r,
        TokenType::SlashEqual => l / r,
        TokenType::PercentEqual => l % r,
        TokenType::CaretEqual => l.pow(r),
        TokenType::DoubleSlashEqual => l.fracdiv(r),
        _ => todo!() // TODO more operations
    }.map_err(|e| RuntimeError::new(e, op.pos.clone()))
}

pub fn eval_assignment(lhs: &Expr, rhs: &Expr, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    match lhs {
        Expr::Ident{value, ..} => {
            let name = value.ty.clone().as_ident().unwrap();
            if op.ty == TokenType::Equal {
                // plain assignment
                let r = eval_expr(rhs, env.clone())?;
                env.borrow_mut()
                    .set(name, r.clone())
                    .map_err(|_| RuntimeError::new("Variable not declared before assignment", op.pos.clone()))?;
                Ok(r)
            } else {
                // compound assignment
                let prev_value = env.borrow_mut()
                    .get(&name)
                    .ok_or_else(|| RuntimeError::new("Variable not defined in scope", op.pos.clone()))?;
                let r = eval_expr(rhs, env.clone())?;

                let result = compound_assignment_inner(&prev_value, &r, op)?;

                env.borrow_mut().set(name, result).expect("unreachable");
                Ok(Value::Nil)
            }
        },
        Expr::Index { lhs, index, pos } => {
            let l = eval_expr(lhs, env.clone())?;
            let idx = eval_expr(index, env.clone())?;
            if op.ty == TokenType::Equal {
                let r = eval_expr(rhs, env)?;
                l.assign_index(&idx, r.clone()).map_err(|e| RuntimeError::new(e, pos.clone()))?;
                Ok(r)
            } else {
                let prev_value = l.index(&idx).map_err(|e| RuntimeError::new(e, pos.clone()))?;
                let r = eval_expr(rhs, env)?;
                let result = compound_assignment_inner(&prev_value, &r, op)?;
                l.assign_index(&idx, result).map_err(|e| RuntimeError::new(e, pos.clone()))?;
                Ok(Value::Nil)
            }
        },
        Expr::FieldAccess { target, name, pos } => {
            let target = eval_expr(target, env.clone())?;
            if let Value::Struct(s) = target { 
                if s.data.borrow().contains_key(name.as_ref()) {
                    if op.ty == TokenType::Equal {
                        let r = eval_expr(rhs, env)?;
                        s.data.borrow_mut().insert(name.to_string(), r.clone());
                        Ok(r)
                    } else {
                        let result = {
                            let prev_value = &s.data.borrow()[name.as_ref()];
                            let r = eval_expr(rhs, env)?;
                            compound_assignment_inner(prev_value, &r, op)?
                        };
                        s.data.borrow_mut().insert(name.to_string(), result);
                        Ok(Value::Nil)
                    }
                } else {
                    Err(RuntimeError::new(format!("Struct {} does not have field {}", Value::Struct(s).repr(), name), pos.clone()))
                }
            } else {
                Err(RuntimeError::new(format!("'{}' is not a struct", target.repr()), pos.clone()))
            }
        }
        _ => unreachable!()
    }
}

pub fn eval_standard_binary(l: Value, r: Value, opty: &TokenType, pos: &Position) -> Result<Value, RuntimeError> {
    let mk_err = || format!("Cannot compare {:?} with {:?}", l, r);
    match opty {
        TokenType::Plus => &l + &r,
        TokenType::Minus => &l - &r,
        TokenType::Star => &l * &r,
        TokenType::Slash => &l / &r,
        TokenType::Percent => &l % &r,
        TokenType::Caret => l.pow(&r),
        TokenType::DoubleSlash => l.fracdiv(&r),
        TokenType::DoubleEqual => Ok(Value::Bool(l == r)),
        TokenType::BangEqual => Ok(Value::Bool(l != r)),
        TokenType::Greater => l.partial_cmp(&r)
            .ok_or_else(mk_err)
            .map(|o| Value::from(o.is_gt())),
        TokenType::Less => l.partial_cmp(&r)
            .ok_or_else(mk_err)
            .map(|o| Value::from(o.is_lt())),
        TokenType::GreaterEqual => l.partial_cmp(&r)
            .ok_or_else(mk_err)
            .map(|o| Value::from(o.is_ge())),
        TokenType::LessEqual => l.partial_cmp(&r)
            .ok_or_else(mk_err)
            .map(|o| Value::from(o.is_le())),
        TokenType::Spaceship => l.partial_cmp(&r)
            .ok_or_else(mk_err)
            .map(|o| Value::from(o as i8)),
        _ => unreachable!()
    }.map_err(|e| RuntimeError::new(e, pos.clone()))
}

pub fn eval_boolean(lhs: &Expr, rhs: &Expr, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    let l = eval_expr(lhs, env.clone())?;
    match op.ty {
        TokenType::DoubleAmper => if l.truthy() {
            eval_expr(rhs, env)
        } else { 
            Ok(l)
        },
        TokenType::DoublePipe => if !l.truthy() {
            eval_expr(rhs, env)
        } else { 
            Ok(l)
        },
        _ => unreachable!()
    }
}

fn mk_pipecolon_inner(f: Func, it: CIterator) -> Func {
    let it = RefCell::new(it);
    Func::BuiltinClosure{
        arg_count: 0,
        func: Rc::new(move |_| {
            if let Some(next) = it.borrow_mut().next() {
                f.call(vec![next?])
            } else {
                Ok(Value::Nil)
            }
        })
    }
}

fn mk_pipequestion_inner(f: Func, it: CIterator) -> Func {
    let it = RefCell::new(it);
    Func::BuiltinClosure {
        arg_count: 0,
        func: Rc::new(move |_| {
            loop {
                let next = it.borrow_mut().next();
                if let Some(next) = next {
                    let next = next?;
                    let success = f.call(vec![next.clone()])?.truthy();
                    if success {
                        return Ok(next)
                    }
                } else {
                    return Ok(Value::Nil)
                }
            }
        })
    }
}

#[allow(clippy::needless_collect)] // collect is necesary to allow for rev() call
fn eval_pipeline(l: Value, r: &Func, op: &Token) -> Result<Value, RuntimeError> {
    match op.ty {
        TokenType::PipePoint => r.call(vec![l]),
        TokenType::PipeColon => Ok(Value::Func(mk_pipecolon_inner(r.clone(), l.iter()?))),
        TokenType::PipeQuestion => Ok(Value::Func(mk_pipequestion_inner(r.clone(), l.iter()?))),
        TokenType::PipeDoubleSlash => {
            let mut result = Value::Nil;
            let mut first_iter = true;
            for v in l.iter().map_err(|e| RuntimeError::new(e, op.pos.clone()))? {
                let v = v.map_err(exit_pipe(&op.pos))?;
                if first_iter {
                    result = v;
                    first_iter = false;
                } else {
                    result = r.call(vec![result, v]).map_err(exit_pipe(&op.pos))?;
                }
            }
            Ok(result)
        },
        TokenType::PipeDoubleBackslash => {
            let mut result = Value::Nil;
            let mut first_iter = true;
            let lst = l.iter().map_err(|e| RuntimeError::new(e, op.pos.clone()))?.collect::<Vec<Result<Value, RuntimeError>>>();
            for v in lst.into_iter().rev() {
                let v = v.map_err(exit_pipe(&op.pos))?;
                if first_iter {
                    result = v;
                    first_iter = false;
                } else {
                    result = r.call(vec![v, result]).map_err(exit_pipe(&op.pos))?;
                }
            }
            Ok(result)
        },
        _ => todo!()
    }

}

#[allow(clippy::needless_collect)] // collect is necesary to allow for rev() call
pub fn eval_ternary(arg1: &Expr, arg2: &Expr, arg3: &Expr, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    match op.ty {
        TokenType::PipeSlash => {
            let iter = eval_expr(arg1, env.clone())?;
            let mut result = eval_expr(arg2, env.clone())?;
            let func = eval_expr(arg3, env)?;
            let func = func.as_func()
                .map_err(|e| RuntimeError::new(e, op.pos.clone()))?;
            for v in iter.iter().map_err(|e| RuntimeError::new(e, op.pos.clone()))? {
                let v = v.map_err(exit_pipe(&op.pos))?;
                result = func.call(vec![result, v]).map_err(exit_pipe(&op.pos))?;
            }
            Ok(result)
        },
        TokenType::PipeBackslash => {
            let iter = eval_expr(arg1, env.clone())?;
            let mut result = eval_expr(arg2, env.clone())?;
            let func = eval_expr(arg3, env)?;
            let func = func.as_func()
                .map_err(|e| RuntimeError::new(e, op.pos.clone()))?;
            let lst = iter.iter().map_err(|e| RuntimeError::new(e, op.pos.clone()))?.collect::<Vec<Result<Value, RuntimeError>>>();
            for v in lst.into_iter().rev() {
                let v = v.map_err(exit_pipe(&op.pos))?;
                result = func.call(vec![v, result]).map_err(exit_pipe(&op.pos))?;
            }
            Ok(result)
        },
        _ => unreachable!()
    }
}

pub fn eval_unary(arg: &Expr, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    let a = eval_expr(arg, env)?;
    match op.ty {
        TokenType::Minus => -a,
        TokenType::Bang => Ok(Value::Bool(!a.truthy())),
        _ => todo!(),
    }.map_err(|e| RuntimeError::new(e, op.pos.clone()))
}

fn mk_numeric_range(start: Value, stop: Option<Value>, step: Value, incl: bool) -> Func {
    let counter = RefCell::new(start);
    Func::BuiltinClosure {
        arg_count: 0,
        func: Rc::new(move |_| {
            let v = counter.borrow().clone();
            if let Some(st) = &stop {
                if v > *st || (!incl && v == *st) {
                    return Ok(Value::Nil)
                }
            }
            *counter.borrow_mut() = (&v + &step)?;
            Ok(v)
        })
    }
}

fn mk_char_range(start: char, stop: Option<char>, step: i64) -> Func {
    const UNICODE_ERR_MSG: &str = "Char range exceeded range of valid Unicode codepoints";
    let counter = RefCell::new(start);
    Func::BuiltinClosure {
        arg_count: 0,
        func: Rc::new(move |_| {
            let v = *counter.borrow();
            if let Some(st) = stop {
                if v > st {
                    return Ok(Value::Nil)
                }
            }
            let next_i64 = (v as u32) as i64 + step;
            let next_u32 = u32::try_from(next_i64)
                .map_err(|_| UNICODE_ERR_MSG)?;
            *counter.borrow_mut() = char::from_u32(next_u32)
                .ok_or(UNICODE_ERR_MSG)?;
            Ok(Value::Char(v))
        })
    }
}

pub fn eval_range(start: &Expr, end: Option<&Expr>, step: Option<&Expr>, incl: bool, env: EnvRef) -> Result<Value, RuntimeError> {
    let start = eval_expr(start, env.clone())?;
    let end = end.map(|e| eval_expr(e, env.clone())).transpose()?;
    let step = step.map(|e| eval_expr(e, env)).transpose()?.unwrap_or(Value::Int(1));
    match (start, end, step, incl) {
        (
            n1 @ (Value::Int(_) | Value::Rational(_)),
            n2 @ (None | Some(Value::Int(_)) | Some(Value::Rational(_))),
            n3 @ (Value::Int(_) | Value::Rational(_)),
            incl
        ) => Ok(Value::Func(mk_numeric_range(n1, n2, n3, incl))),
        (Value::Char(c1), Some(Value::Char(c2)), Value::Int(n), true) 
            => Ok(Value::Func(mk_char_range(c1, Some(c2), n))),
        (Value::Char(c1), None, Value::Int(n), false) 
            => Ok(Value::Func(mk_char_range(c1, None, n))),
        _ => Err("Invalid operands for range".into())
    }
}
