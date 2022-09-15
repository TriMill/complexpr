use std::{rc::Rc, io::Write, cmp::Ordering, time::{SystemTime, UNIX_EPOCH}};

use num_traits::ToPrimitive;

use crate::{value::{Value, Func}, eval::Environment};

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
}

fn fn_str(args: Vec<Value>) -> Result<Value, String> {
    Ok(Value::String(args[0].to_string()))
}

fn fn_repr(args: Vec<Value>) -> Result<Value, String> {
    Ok(Value::String(args[0].repr()))
}

fn fn_print(args: Vec<Value>) -> Result<Value, String> {
    print!("{}", args[0].to_string());
    std::io::stdout().flush().map_err(|e| e.to_string())?;
    Ok(Value::Nil)
}

fn fn_println(args: Vec<Value>) -> Result<Value, String> {
    println!("{}", args[0].to_string());
    Ok(Value::Nil)
}

fn fn_input(_: Vec<Value>) -> Result<Value, String> {
    let mut buffer = String::new();
    let stdin = std::io::stdin();
    stdin.read_line(&mut buffer).map_err(|e| e.to_string())?;
    if buffer.ends_with('\n') {
        buffer.pop();
    }
    Ok(Value::from(buffer))
}

fn fn_ord(args: Vec<Value>) -> Result<Value, String> {
    if let Value::Char(c) = args[0] {
        Ok(Value::from(c as u32))
    } else {
        Err("Argument to ord must be a char".into())
    }
}

fn fn_chr(args: Vec<Value>) -> Result<Value, String> {
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

fn fn_range(args: Vec<Value>) -> Result<Value, String> {
    let min = &args[0];
    let max = &args[1];
    match (min, max) {
        (Value::Int(a), Value::Int(b)) => match a.cmp(b) {
            Ordering::Equal => Ok(Value::from(vec![])),
            Ordering::Less =>
                Ok(Value::from((*a..*b).map(Value::Int).collect::<Vec<Value>>())),
            Ordering::Greater =>
                Ok(Value::from(((*b+1)..(*a+1)).rev().map(Value::Int).collect::<Vec<Value>>()))
        },
        _ => Err("Both arguments to range must be integers".into())
    }
}

fn fn_len(args: Vec<Value>) -> Result<Value, String> {
    match &args[0] {
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        Value::List(l) => Ok(Value::Int(l.borrow().len() as i64)),
        Value::Map(m) => Ok(Value::Int(m.borrow().len() as i64)),
        v => Err(format!("{:?} has no length", v))
    }
}

fn fn_has(args: Vec<Value>) -> Result<Value, String> {
    match &args[0] {
        Value::Map(m) => Ok(Value::from(m.borrow().contains_key(&args[1]))),
        v => Err(format!("Argument to has must be a map, got {:?} ", v))
    }
}

fn fn_re(args: Vec<Value>) -> Result<Value, String> {
    match &args[0] {
        Value::Int(x) => Ok(Value::Float(*x as f64)),
        Value::Float(x) => Ok(Value::Float(*x)),
        Value::Rational(x) => Ok(Value::Float(x.to_f64().unwrap())),
        Value::Complex(x) => Ok(Value::Float(x.re)),
        x => Err(format!("Cannot get real part of {:?}", x))
    }
}

fn fn_im(args: Vec<Value>) -> Result<Value, String> {
    match &args[0] {
        Value::Int(_) | Value::Float(_) | Value::Rational(_) 
            => Ok(Value::Float(0.0)),
        Value::Complex(x) => Ok(Value::Float(x.im)),
        x => Err(format!("Cannot get real part of {:?}", x))
    }
}

fn fn_time(_: Vec<Value>) -> Result<Value, String> {
    let time = SystemTime::now().duration_since(UNIX_EPOCH).map_err(|e| e.to_string())?;
    Ok(Value::from(time.as_secs_f64()))
}
