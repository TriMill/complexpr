pub mod math;
pub mod iter;

use std::{rc::Rc, io::Write, cmp::Ordering, time::{SystemTime, UNIX_EPOCH}, cell::RefCell};

use crate::{value::{Value, func::{Func, CIterator}}, RuntimeError, env::Environment};

#[macro_export]
macro_rules! declare_fn {
    ($env:ident, $name:ident, $arg_count:literal) => {paste::paste!{{
        let s: Rc<str> = Rc::from(stringify!($name));
        $env.declare(s.clone(), Value::Func(Func::Builtin { func: [<fn_ $name>], arg_count: $arg_count, name: s }));
    }}};
    ($env:ident, $name:literal, $rust_name:ident, $arg_count:literal) => {{
        let s: Rc<str> = Rc::from($name);
        $env.declare(s.clone(), Value::Func(Func::Builtin { func: $rust_name, arg_count: $arg_count, name: s }));
    }};
}

pub fn load(env: &mut Environment) {
    declare_fn!(env, "type", fn_type, 1);
    declare_fn!(env, type_eq, 1);
    declare_fn!(env, str, 1);
    declare_fn!(env, repr, 1);
    declare_fn!(env, print, 1);
    declare_fn!(env, println, 1); 
    declare_fn!(env, input, 0); 
    declare_fn!(env, ord, 1); 
    declare_fn!(env, chr, 1); 
    declare_fn!(env, range, 2); 
    declare_fn!(env, has, 2); 
    declare_fn!(env, len, 1); 
    declare_fn!(env, time, 0); 
    declare_fn!(env, list, 1); 
}

fn fn_type(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Type(args[0].get_type()))
}

fn fn_type_eq(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Bool(if args[0].get_type() != args[1].get_type() {
        false
    } else {
        args[0] == args[1]
    }))
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
    if d[2] >= ZERO && d[0] >= d[1]
    || d[2] <= ZERO && d[0] <= d[1] {
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
    Ok(Value::Func(Func::BuiltinClosure { 
        arg_count: 0,
        data: Rc::new(RefCell::new(vec![start.clone(), end.clone(), Value::Int(delta)])),
        iter_data: Rc::new(RefCell::new(vec![])),
        func: range_inner
    }))
}

fn fn_len(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Int(args[0].len().map_err(RuntimeError::new_no_pos)? as i64))
}

fn fn_has(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Map(m) => Ok(Value::from(m.borrow().contains_key(&args[1]))),
        v => Err(format!("Argument to has must be a map, got {:?} ", v).into())
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
