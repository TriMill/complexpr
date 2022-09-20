use std::{rc::Rc, cell::RefCell};

use crate::{value::{Value, func::{Func, CIterator}}, RuntimeError, env::Environment, declare_fn};

pub fn load(env: &mut Environment) {
    declare_fn!(env, take, 2);
    declare_fn!(env, skip, 2);
    declare_fn!(env, forall, 2);
    declare_fn!(env, exists, 2);
}


fn take_inner(_: Vec<Value>, data: Rc<RefCell<Vec<Value>>>, iter_data: Rc<RefCell<Vec<CIterator>>>) -> Result<Value, RuntimeError> {
    // 0: current index
    // 1: target index
    let mut d = data.borrow_mut();
    if d[0] >= d[1] {
        Ok(Value::Nil)
    } else {
        d[0] = (&d[0] + &Value::Int(1))?;
        match iter_data.borrow_mut()[0].next() {
            None => Ok(Value::Nil),
            Some(x) => x
        }
    }
}

fn fn_take(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Func(Func::BuiltinClosure { 
        arg_count: 0,
        data: Rc::new(RefCell::new(vec![Value::Int(0), args[0].clone()])),
        iter_data: Rc::new(RefCell::new(vec![args[1].iter()?])),
        func: take_inner
    }))
}

fn skip_inner(_: Vec<Value>, data: Rc<RefCell<Vec<Value>>>, iter_data: Rc<RefCell<Vec<CIterator>>>) -> Result<Value, RuntimeError> {
    let mut d = if let Value::Int(d) = data.borrow()[0] { d } else {
        unreachable!() // checked by fn_skip()
    };
    while d > 0 {
        iter_data.borrow_mut()[0].next();
        d -= 1;
    }
    data.borrow_mut()[0] = Value::Int(d);
    match iter_data.borrow_mut()[0].next() {
        None => Ok(Value::Nil),
        Some(x) => x
    }
}

fn fn_skip(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let n = match args[0] {
        Value::Int(n) if n <= 0 => return Err(RuntimeError::new_no_pos("First argument to skip must be nonnegative")),
        Value::Int(n) => n,
        _ => return Err(RuntimeError::new_no_pos("First argument to skip must be an integer"))
    };
    let it = args[1].iter()?;
    Ok(Value::Func(Func::BuiltinClosure {
        arg_count: 0,
        data: Rc::new(RefCell::new(vec![Value::Int(n)])),
        iter_data: Rc::new(RefCell::new(vec![it])),
        func: skip_inner
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
