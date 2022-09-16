use std::{rc::Rc, io::Write, cmp::Ordering, time::{SystemTime, UNIX_EPOCH}, cell::RefCell};

use num_traits::ToPrimitive;

use crate::{value::{Value, Func, CIterator}, eval::Environment, RuntimeError};

pub fn load(env: &mut Environment) {
    let mut name: Rc<str>;
    name = Rc::from("str");
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_str, arg_count: 1, name }));
    name = Rc::from("repr");
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_repr, arg_count: 1, name }));
    name = Rc::from("print");
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_print, arg_count: 1, name }));
    name = Rc::from("println"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_println, arg_count: 1, name }));
    name = Rc::from("input"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_input, arg_count: 0, name }));
    name = Rc::from("ord"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_ord, arg_count: 1, name }));
    name = Rc::from("chr"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_chr, arg_count: 1, name }));
    name = Rc::from("range"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_range, arg_count: 2, name }));
    name = Rc::from("has"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_has, arg_count: 2, name }));
    name = Rc::from("len"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_len, arg_count: 1, name }));
    name = Rc::from("re"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_re, arg_count: 1, name }));
    name = Rc::from("im"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_im, arg_count: 1, name }));
    name = Rc::from("time"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_time, arg_count: 0, name }));
    name = Rc::from("list"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_list, arg_count: 1, name }));
}

fn fn_str(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::String(args[0].to_string()))
}

fn fn_repr(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::String(args[0].repr()))
}

fn fn_print(args: Vec<Value>) -> Result<Value, RuntimeError> {
    print!("{}", args[0].to_string());
    std::io::stdout().flush().map_err(|e| e.to_string())?;
    Ok(Value::Nil)
}

fn fn_println(args: Vec<Value>) -> Result<Value, RuntimeError> {
    println!("{}", args[0].to_string());
    Ok(Value::Nil)
}

fn fn_input(_: Vec<Value>) -> Result<Value, RuntimeError> {
    let mut buffer = String::new();
    let stdin = std::io::stdin();
    stdin.read_line(&mut buffer).map_err(|e| e.to_string())?;
    if buffer.ends_with('\n') {
        buffer.pop();
    }
    Ok(Value::from(buffer))
}

fn fn_ord(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if let Value::Char(c) = args[0] {
        Ok(Value::from(c as u32))
    } else {
        Err("Argument to ord must be a char".into())
    }
}

fn fn_chr(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if let Value::Int(i) = args[0] {
        if i >= 0 && i < (u32::MAX as i64) {
            if let Some(c) = char::from_u32(i as u32) {
                return Ok(Value::from(c))
            }
        }
        Err("Out of range".into())
    } else {
        Err("Argument to chr must be an integer".into())
    }
}

fn range_inner(_: Vec<Value>, data: Rc<RefCell<Vec<Value>>>, _: Rc<RefCell<Vec<CIterator>>>) -> Result<Value, RuntimeError> {
    const ZERO: Value = Value::Int(0);
    let mut d = data.borrow_mut();
    if d[2] >= ZERO && d[0] >= d[1] {
        Ok(Value::Nil)
    } else if d[2] <= ZERO && d[0] <= d[1] {
        Ok(Value::Nil)
    } else {
        let res = d[0].clone();
        d[0] = (&d[0] + &d[2])?;
        Ok(res)
    }
}

fn fn_range(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let start = &args[0];
    let end = &args[1];
    let delta = match (start, end) {
        (Value::Int(a), Value::Int(b)) => match a.cmp(b) {
            Ordering::Equal => 0,
            Ordering::Less => 1,
            Ordering::Greater => -1,
        },
        _ => return Err("Both arguments to range must be integers".into())
    };
    return Ok(Value::Func(Func::BuiltinClosure { 
        arg_count: 0,
        data: Rc::new(RefCell::new(vec![start.clone(), end.clone(), Value::Int(delta)])),
        iter_data: Rc::new(RefCell::new(vec![])),
        func: range_inner
    }))
}

fn fn_len(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Int(args[0].len().map_err(|e| RuntimeError::new_incomplete(e))? as i64))
}

fn fn_has(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Map(m) => Ok(Value::from(m.borrow().contains_key(&args[1]))),
        v => Err(format!("Argument to has must be a map, got {:?} ", v).into())
    }
}

fn fn_re(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Int(x) => Ok(Value::Float(*x as f64)),
        Value::Float(x) => Ok(Value::Float(*x)),
        Value::Rational(x) => Ok(Value::Float(x.to_f64().unwrap())),
        Value::Complex(x) => Ok(Value::Float(x.re)),
        x => Err(format!("Cannot get real part of {:?}", x).into())
    }
}

fn fn_im(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Int(_) | Value::Float(_) | Value::Rational(_) 
            => Ok(Value::Float(0.0)),
        Value::Complex(x) => Ok(Value::Float(x.im)),
        x => Err(format!("Cannot get real part of {:?}", x).into())
    }
}

fn fn_time(_: Vec<Value>) -> Result<Value, RuntimeError> {
    let time = SystemTime::now().duration_since(UNIX_EPOCH).map_err(|e| e.to_string())?;
    Ok(Value::from(time.as_secs_f64()))
}

fn fn_list(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let a = args[0].iter()?;
    let mut res = Vec::new();
    for v in a { res.push(v?); }
    Ok(Value::from(res))
}
