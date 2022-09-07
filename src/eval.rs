use std::{collections::HashMap, rc::Rc, cell::RefCell};

use num_complex::Complex64;

use crate::{value::Value, expr::{Stmt, Expr}, token::{TokenType, Token, OpType}, RuntimeError};


#[derive(Debug)]
pub struct Environment {
    parent: Option<EnvRef>,
    map: HashMap<Rc<str>, Value>
}

type EnvRef = Rc<RefCell<Environment>>;

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
        let x = match self.map.get(name) {
            Some(v) => Ok(v.clone()),
            None => match self.parent {
                Some(ref p) => p.borrow().get(name),
                None => Err(())
            }
        };
        println!("get {}: {:?}", name, x);
        x
    }

    pub fn declare(&mut self, name: Rc<str>, value: Value) {
        println!("declare {}: {:?}", name, value);
        self.map.insert(name, value);
    }

    pub fn set(&mut self, name: Rc<str>, value: Value) -> Result<(),()> {
        println!("set {}: {:?}", name, value);
        match self.map.contains_key(&name) {
            true => { self.map.insert(name, value); Ok(()) },
            false => match self.parent {
                Some(ref mut p) => p.borrow_mut().set(name, value),
                None => Err(())
            }
        }
    }
}

pub fn eval_stmt(stmt: &Stmt, env: EnvRef) -> Result<(), RuntimeError> {
    match stmt {
        Stmt::Expr{ expr } 
            => drop(eval_expr(expr, env)),
        Stmt::Let { lhs: Token{ty: TokenType::Ident(s),..}, rhs: None } 
            => env.borrow_mut().declare(s.clone(), Value::Nil),
        Stmt::Let { lhs: Token{ty: TokenType::Ident(s),..}, rhs: Some(rhs) } => {
            let r = eval_expr(rhs, env.clone())?;
            env.borrow_mut().declare(s.clone(), r)
        },
        Stmt::Block { stmts } => {
            let block_env = Environment::extend(env).wrap();
            for stmt in stmts { 
                eval_stmt(stmt, block_env.clone())? 
            }
        }
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
        }
        _ => unreachable!()
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
            o => todo!("{:?}", o) // TODO other operations
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
        TokenType::ImFloat(f) => Value::Complex(Complex64::new(0.0, *f)),
        TokenType::String(s) => Value::String(s.clone()),
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
                _ => todo!() // TODO more operations
            }.map_err(|e| RuntimeError { message: e, pos: op.pos.clone() })?;

            env.borrow_mut()
                .set(name.clone(), result.clone()).expect("unreachable");
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
        _ => todo!() // TODO other operations
    }.map_err(|e| RuntimeError { message: e, pos: op.pos.clone() })
}