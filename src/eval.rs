use std::{collections::HashMap, rc::Rc, cell::RefCell};

use num_complex::Complex64;

use crate::{value::Value, expr::{Stmt, Expr}, token::{TokenType, Token, OpType}};

#[derive(Debug)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    map: HashMap<Rc<str>, Value>
}

impl Environment {
    pub fn new() -> Self {
        Self { parent: None, map: HashMap::new() }
    }

    pub fn extend(parent: Rc<RefCell<Self>>) -> Self {
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

pub fn eval_stmt(stmt: &Stmt, env: Rc<RefCell<Environment>>) -> Result<(), String> {
    match stmt {
        Stmt::Expr{ expr } 
            => drop(eval_expr(expr, env)),
        Stmt::Let { lhs: Token{ty: TokenType::Ident(s),..}, rhs: None } 
            => env.borrow_mut().declare(s.clone(), Value::Nil),
        Stmt::Let { lhs: Token{ty: TokenType::Ident(s),..}, rhs: Some(rhs) } => {
            let r = eval_expr(rhs, env.clone())?;
            env.borrow_mut().declare(s.clone(), r)
        },
        Stmt::If { conditions, bodies, else_clause } 
            => todo!(), // TODO if statements
        _ => unreachable!()
    }
    Ok(())
}

pub fn eval_expr(expr: &Expr, env: Rc<RefCell<Environment>>) -> Result<Value, String> {
    match expr {
        Expr::Literal { value } => match &value.ty {
            TokenType::Nil => Ok(Value::Nil),
            TokenType::True => Ok(Value::Bool(true)),
            TokenType::False => Ok(Value::Bool(false)),
            TokenType::Int(n) => Ok(Value::Int(*n)),
            TokenType::Float(f) => Ok(Value::Float(*f)),
            TokenType::ImFloat(f) => Ok(Value::Complex(Complex64::new(0.0, *f))),
            TokenType::String(s) => Ok(Value::String(s.clone())),
            _ => todo!()
        },
        Expr::Ident { value } => if let Token { ty: TokenType::Ident(name), ..} = value {
            env.borrow_mut().get(name).map_err(|_| "Variable not defined in scope".into())
        } else { unreachable!() },
        Expr::Binary { lhs, rhs, op } => match op.ty.get_op_type() {
            Some(OpType::Assignment) => {
                let r = eval_expr(rhs, env.clone())?;
                if let Expr::Ident{value: Token{ty: TokenType::Ident(name),..}} = &**lhs {
                    if op.ty == TokenType::Equal {
                        env.borrow_mut().set(name.clone(), r).map_err(|_| "Variable not declared before assignment")?;
                        Ok(Value::Nil)
                    } else {
                        todo!() // TODO +=, -=, etc
                    }
                } else {
                    unreachable!()
                }
            },
            Some(OpType::Additive) | Some(OpType::Multiplicative) => {
                let l = eval_expr(lhs, env.clone())?;
                let r = eval_expr(rhs, env)?;
                match op.ty {
                    TokenType::Plus => &l + &r,
                    TokenType::Minus => &l - &r,
                    TokenType::Star => &l * &r,
                    TokenType::Slash => &l / &r,
                    _ => todo!() // TODO other operations
                }
            },
            o => todo!("{:?}", o) // TODO other operations
        },
        e => todo!("{:?}", e) // TODO other expression types
    }
}