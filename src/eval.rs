use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::{value::{Value, Complex}, expr::{Stmt, Expr}, token::{TokenType, Token, OpType}, RuntimeError};

#[derive(Debug)]
pub struct Environment {
    parent: Option<EnvRef>,
    map: HashMap<Rc<str>, Value>,
}

pub type EnvRef = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> Self {
        Self { parent: None, map: HashMap::new() }
    }

    pub fn wrap(self) -> EnvRef {
        Rc::new(RefCell::new(self))
    }

    pub fn extend(parent: EnvRef) -> Self {
        Self { parent: Some(parent), map: HashMap::new() }
    }

    pub fn get(&self, name: &str) -> Result<Value,()> {
        match self.map.get(name) {
            Some(v) => Ok(v.clone()),
            None => match self.parent {
                Some(ref p) => p.borrow().get(name),
                None => Err(())
            }
        }
    }

    pub fn declare(&mut self, name: Rc<str>, value: Value) {
        self.map.insert(name, value);
    }

    pub fn set(&mut self, name: Rc<str>, value: Value) -> Result<(),()> {
        match self.map.contains_key(&name) {
            true => { self.map.insert(name, value); Ok(()) },
            false => match self.parent {
                Some(ref mut p) => p.borrow_mut().set(name, value),
                None => Err(())
            }
        }
    }
}

fn unwrap_ident_token<'a>(tok: &'a Token) -> &'a Rc<str> {
    if let Token { ty: TokenType::Ident(s),.. } = tok {
        s
    } else {
        unreachable!("precondition failed")
    }
}

pub fn eval_stmt(stmt: &Stmt, env: EnvRef) -> Result<(), RuntimeError> {
    match stmt {
        Stmt::Expr{ expr } 
            => drop(eval_expr(expr, env)),
        Stmt::Let { lhs, rhs: None } 
            => env.borrow_mut().declare(unwrap_ident_token(lhs).clone(), Value::Nil),
        Stmt::Let { lhs, rhs: Some(rhs) } => {
            let r = eval_expr(rhs, env.clone())?;
            env.borrow_mut().declare(unwrap_ident_token(lhs).clone(), r)
        },
        Stmt::Block { stmts } => {
            let block_env = Environment::extend(env).wrap();
            for stmt in stmts { 
                eval_stmt(stmt, block_env.clone())? 
            }
        },
        Stmt::If { if_clauses, else_clause } => {
            for ic in if_clauses {
                let cond = eval_expr(&ic.0, env.clone())?;
                if cond.truthy() {
                    return eval_stmt(&ic.1, env.clone())
                }
            }
            if let Some(ec) = else_clause {
                return eval_stmt(&ec, env)
            }
        },
        Stmt::For { var, expr, stmt } => {
            let name = unwrap_ident_token(var);
            let iter = eval_expr(expr, env.clone())?;
            env.borrow_mut().declare(name.clone(), Value::Nil);
            if let Ok(i) = iter.iter() {
                for v in i {
                    let env = env.clone();
                    env.borrow_mut().set(name.clone(), v).expect("unreachable");
                    eval_stmt(&stmt, env)?;
                }
            } else {
                return Err(RuntimeError { message: "Cannot iterate this type".into(), pos: var.pos.clone() })
            };
        },
        Stmt::While { expr, stmt } => {
            loop {
                let cond = eval_expr(expr, env.clone())?;
                if !cond.truthy() {
                    break
                }
                eval_stmt(&stmt, env.clone())?;
            }
        },
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
            Some(OpType::Additive) | Some(OpType::Multiplicative) 
                => eval_binary(lhs, rhs, op, env),
            Some(OpType::Comparison)
                => eval_comp(lhs, rhs, op, env),
            o => todo!("{:?}", o) // TODO other operations
        },
        Expr::Unary { arg, op } => eval_unary(arg, op, env),
        Expr::List { items } => {
            let mut list = Vec::with_capacity(items.len());
            for item in items {
                list.push(eval_expr(item, env.clone())?);
            }
            Ok(Value::from(list))
        },
        Expr::FuncCall { func, args, pos } => {
            let func = eval_expr(func, env.clone())?;
            let mut arg_values = Vec::with_capacity(args.len());
            for arg in args {
                let result = eval_expr(arg, env.clone())?;
                arg_values.push(result);
            }
            func.call(arg_values, pos)
        },
        Expr::Index { lhs, index, pos } => {
            let l = eval_expr(lhs, env.clone())?;
            let idx = eval_expr(index, env)?;
            l.index(&idx).map_err(|e| RuntimeError { message: e, pos: pos.clone() })
        },
        e => todo!("{:?}", e) // TODO other expression types
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
            .map_err(|_| RuntimeError { message: "Variable not defined in scope".into(), pos: token.pos.clone() })
    } else { 
        unreachable!() 
    }
}

pub fn eval_assignment(lhs: &Box<Expr>, rhs: &Box<Expr>, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    // lhs must be an identifier (checked in parser)
    if let Expr::Ident{value: Token{ty: TokenType::Ident(name),..}} = &**lhs {
        if op.ty == TokenType::Equal {
            // plain assignment
            let r = eval_expr(rhs, env.clone())?;
            env.borrow_mut()
                .set(name.clone(), r)
                .map_err(|_| RuntimeError { message: "Variable not declared before assignment".into(), pos: op.pos.clone() })?;
            Ok(Value::Nil)
        } else {
            // compound assignment
            let prev_value = env.borrow_mut()
                .get(name)
                .map_err(|_| RuntimeError { message: "Variable not defined in scope".into(), pos: op.pos.clone()})?;
            let r = eval_expr(rhs, env.clone())?;

            let result = match op.ty {
                TokenType::PlusEqual => &prev_value + &r,
                TokenType::MinusEqual => &prev_value - &r,
                TokenType::StarEqual => &prev_value * &r,
                TokenType::SlashEqual => &prev_value / &r,
                TokenType::PercentEqual => &prev_value % &r,
                _ => todo!() // TODO more operations
            }.map_err(|e| RuntimeError { message: e, pos: op.pos.clone() })?;

            env.borrow_mut()
                .set(name.clone(), result.clone()).expect("unreachable");
            Ok(result)
        }
    } else if let Expr::Index { lhs, index, pos } = &**lhs {
        let l = eval_expr(lhs, env.clone())?;
        let idx = eval_expr(index, env.clone())?;
        if op.ty == TokenType::Equal {
            let r = eval_expr(rhs, env)?;
            l.assign_index(&idx, r.clone()).map_err(|e| RuntimeError { message: e, pos: pos.clone() })?;
            Ok(r)
        } else {
            let prev_value = l.index(&idx).map_err(|e| RuntimeError { message: e, pos: pos.clone() })?;
            let r = eval_expr(rhs, env)?;
            let result = match op.ty {
                TokenType::PlusEqual => &prev_value + &r,
                TokenType::MinusEqual => &prev_value - &r,
                TokenType::StarEqual => &prev_value * &r,
                TokenType::SlashEqual => &prev_value / &r,
                TokenType::PercentEqual => &prev_value % &r,
                _ => todo!() // TODO more operations
            }.map_err(|e| RuntimeError { message: e, pos: op.pos.clone() })?;
            l.assign_index(&idx, result.clone()).map_err(|e| RuntimeError { message: e, pos: pos.clone() })?;
            Ok(result)
        }
    } else {
        unreachable!()
    }
}

pub fn eval_binary(lhs: &Box<Expr>, rhs: &Box<Expr>, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    let l = eval_expr(lhs, env.clone())?;
    let r = eval_expr(rhs, env)?;
    match op.ty {
        TokenType::Plus => &l + &r,
        TokenType::Minus => &l - &r,
        TokenType::Star => &l * &r,
        TokenType::Slash => &l / &r,
        TokenType::Percent => &l % &r,
        _ => todo!() // TODO other operations
    }.map_err(|e| RuntimeError { message: e, pos: op.pos.clone() })
}

pub fn eval_comp(lhs: &Box<Expr>, rhs: &Box<Expr>, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    let l = eval_expr(lhs, env.clone())?;
    let r = eval_expr(rhs, env)?;
    let mk_err = || RuntimeError {
        message: format!("Cannot compare {:?} with {:?}", l, r),
        pos: op.pos.clone()
    };
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

pub fn eval_unary(arg: &Box<Expr>, op: &Token, env: EnvRef) -> Result<Value, RuntimeError> {
    let a = eval_expr(arg, env)?;
    match op.ty {
        TokenType::Minus => -a,
        _ => todo!(),
    }.map_err(|e| RuntimeError { message: e, pos: op.pos.clone() })
}