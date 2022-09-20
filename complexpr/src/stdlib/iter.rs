use std::{rc::Rc, cell::RefCell};

use crate::{value::{Value, func::Func}, RuntimeError, env::Environment, declare_fn};

pub fn load(env: &mut Environment) {
    declare_fn!(env, take, 2);
    declare_fn!(env, skip, 2);
    declare_fn!(env, forall, 2);
    declare_fn!(env, exists, 2);
}

fn fn_take(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let idx = RefCell::new(0);
    let limit = match args[0] {
        Value::Int(n) => n,
        _ => return Err("Argument to take must be an integer".into()),
    };
    let it = RefCell::new(args[1].iter()?);
    Ok(Value::Func(Func::BuiltinClosure { 
        arg_count: 0,
        func: Rc::new(move |_| {
            if *idx.borrow() >= limit {
                Ok(Value::Nil)
            } else {
                *idx.borrow_mut() += 1;
                match it.borrow_mut().next() {
                    None => Ok(Value::Nil),
                    Some(x) => x
                }
            }
        })
    }))
}

fn fn_skip(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let n = match args[0] {
        Value::Int(n) if n <= 0 => return Err(RuntimeError::new_no_pos("First argument to skip must be nonnegative")),
        Value::Int(n) => n,
        _ => return Err(RuntimeError::new_no_pos("First argument to skip must be an integer"))
    };
    let it = RefCell::new(args[1].iter()?);
    let idx = RefCell::new(0);
    Ok(Value::Func(Func::BuiltinClosure {
        arg_count: 0,
        func: Rc::new(move |_| {
            while *idx.borrow() < n {
                it.borrow_mut().next();
                *idx.borrow_mut() += 1;
            }
            match it.borrow_mut().next() {
                None => Ok(Value::Nil),
                Some(x) => x
            }
        })
    }))
}

fn fn_forall(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let func = &args[0].as_func()?;
    for item in args[1].iter()? {
        let item = item?;
        if !func.call(vec![item])?.truthy() {
            return Ok(Value::Bool(false))
        }
    }
    Ok(Value::Bool(true))
}

fn fn_exists(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let func = &args[0].as_func()?;
    for item in args[1].iter()? {
        let item = item?;
        if func.call(vec![item])?.truthy() {
            return Ok(Value::Bool(true))
        }
    }
    Ok(Value::Bool(false))
}
