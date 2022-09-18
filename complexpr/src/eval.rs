use std::{collections::HashMap, rc::Rc, cell::RefCell};

use num_traits::Pow;

use crate::{value::{Value, Complex, Func, CIterator}, expr::{Stmt, Expr}, token::{TokenType, Token, OpType}, RuntimeError, Position, env::{EnvRef, Environment}};

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
        }
    }
    Ok(())
}

pub fn eval_expr(expr: &Expr, env: EnvRef) -> Result<Value, RuntimeError> {
    match expr {
        Expr::Literal { value } => Ok(eval_literal(value)),
        Expr::Ident { value } => eval_ident(value, env),
        Expr::Binary { lhs, rhs, op } => match op.ty.get_op_type() {
            Some(OpType::Assignment) 
                => eval_assignment(lhs, rhs, op, env),
            Some(OpType::Additive) | Some(OpType::Multiplicative) | Some(OpType::Exponential)
                => eval_arith(lhs, rhs, op, env),
            Some(OpType::LogicalAnd) | Some(OpType::LogicalOr)
                => eval_boolean(lhs, rhs, op, env),
            Some(OpType::Comparison)
                => eval_comp(lhs, rhs, op, env),
            Some(OpType::Pipeline)
                => eval_pipeline(lhs, rhs, op, env),
            o => todo!("{:?}", o) // TODO other operations
        },
        Expr::Ternary { arg1, arg2, arg3, op } => eval_ternary(arg1, arg2, arg3, op, env),
        Expr::Unary { arg, op } => eval_unary(arg, op, env),
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

fn compound_assignment_inner(l: Value, r: Value, op: &Token) -> Result<Value, RuntimeError> {
    match op.ty {
        TokenType::PlusEqual => &l + &r,
        TokenType::MinusEqual => &l - &r,
        TokenType::StarEqual => &l * &r,
        TokenType::SlashEqual => &l / &r,
        TokenType::PercentEqual => &l % &r,
        TokenType::CaretEqual => l.pow(&r),
        TokenType::DoubleSlashEqual => l.fracdiv(&r),
        _ => todo!() // TODO more operations
    }.map_err(|e| RuntimeError::new(e, op.pos.clone()))
}

pub fn eval_assignment(lhs: &Expr, rhs: &Expr, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    // lhs must be an identifier (checked in parser)
    if let Expr::Ident{value: Token{ty: TokenType::Ident(name),..}} = lhs {
        if op.ty == TokenType::Equal {
            // plain assignment
            let r = eval_expr(rhs, env.clone())?;
            env.borrow_mut()
                .set(name.clone(), r.clone())
                .map_err(|_| RuntimeError::new("Variable not declared before assignment", op.pos.clone()))?;
            Ok(r)
        } else {
            // compound assignment
            let prev_value = env.borrow_mut()
                .get(name)
                .ok_or_else(|| RuntimeError::new("Variable not defined in scope", op.pos.clone()))?;
            let r = eval_expr(rhs, env.clone())?;

            let result = compound_assignment_inner(prev_value, r, op)?;

            env.borrow_mut()
                .set(name.clone(), result.clone()).expect("unreachable");
            Ok(result)
        }
    } else if let Expr::Index { lhs, index, pos } = lhs {
        let l = eval_expr(lhs, env.clone())?;
        let idx = eval_expr(index, env.clone())?;
        if op.ty == TokenType::Equal {
            let r = eval_expr(rhs, env)?;
            l.assign_index(&idx, r.clone()).map_err(|e| RuntimeError::new(e, pos.clone()))?;
            Ok(r)
        } else {
            let prev_value = l.index(&idx).map_err(|e| RuntimeError::new(e, pos.clone()))?;
            let r = eval_expr(rhs, env)?;
            let result = compound_assignment_inner(prev_value, r, op)?;
            l.assign_index(&idx, result.clone()).map_err(|e| RuntimeError::new(e, pos.clone()))?;
            Ok(result)
        }
    } else {
        unreachable!()
    }
}

pub fn eval_arith(lhs: &Expr, rhs: &Expr, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    let l = eval_expr(lhs, env.clone())?;
    let r = eval_expr(rhs, env)?;
    match op.ty {
        TokenType::Plus => &l + &r,
        TokenType::Minus => &l - &r,
        TokenType::Star => &l * &r,
        TokenType::Slash => &l / &r,
        TokenType::Percent => &l % &r,
        TokenType::Caret => l.pow(&r),
        TokenType::DoubleSlash => l.fracdiv(&r),
        _ => todo!()
    }.map_err(|e| RuntimeError::new(e, op.pos.clone()))
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

pub fn eval_comp(lhs: &Expr, rhs: &Expr, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    let l = eval_expr(lhs, env.clone())?;
    let r = eval_expr(rhs, env)?;
    let mk_err = || RuntimeError::new(
        format!("Cannot compare {:?} with {:?}", l, r),
        op.pos.clone()
    );
    match op.ty {
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
    }
}

fn pipecolon_inner(_: Vec<Value>, data: Rc<RefCell<Vec<Value>>>, iter_data: Rc<RefCell<Vec<CIterator>>>) -> Result<Value, RuntimeError> {
    let f = &data.borrow()[0];
    if let Some(next) = iter_data.borrow_mut()[0].next() {
        let func = f.as_func()?;
        func.call(vec![next?])
    } else {
        Ok(Value::Nil)
    }
}

fn pipequestion_inner(_: Vec<Value>, data: Rc<RefCell<Vec<Value>>>, iter_data: Rc<RefCell<Vec<CIterator>>>) -> Result<Value, RuntimeError> {
    let f = &data.borrow()[0];
    loop {
        let next = iter_data.borrow_mut()[0].next();
        if let Some(next) = next {
            let next = next?;
            let func = f.as_func()?;
            let success = func.call(vec![next.clone()])?.truthy();
            if success {
                return Ok(next)
            }
        } else {
            return Ok(Value::Nil)
        }
    }
}


pub fn eval_pipeline(lhs: &Expr, rhs: &Expr, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    let l = eval_expr(lhs, env.clone())?;
    let r = eval_expr(rhs, env)?;
    let f = r.as_func().map_err(|e| RuntimeError::new(e, op.pos.clone()))?;
    eval_pipeline_inner(l, f, op).map_err(exit_pipe(&op.pos))
}

#[allow(clippy::needless_collect)] // collect is necesary to allow for rev() call
fn eval_pipeline_inner(l: Value, r: &Func, op: &Token) -> Result<Value, RuntimeError> {
    match op.ty {
        TokenType::PipePoint => r.call(vec![l]),
        TokenType::PipeColon => {
            Ok(Value::Func(Func::BuiltinClosure {
                arg_count: 0,
                data: Rc::new(RefCell::new(vec![Value::Func(r.clone())])),
                iter_data: Rc::new(RefCell::new(vec![l.iter()?])),
                func: pipecolon_inner,
            }))
        },
        TokenType::PipeQuestion => {
            Ok(Value::Func(Func::BuiltinClosure {
                arg_count: 0,
                data: Rc::new(RefCell::new(vec![Value::Func(r.clone())])),
                iter_data: Rc::new(RefCell::new(vec![l.iter()?])),
                func: pipequestion_inner,
            }))
        },
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
