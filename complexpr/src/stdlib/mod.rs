pub mod io;
pub mod iter;
pub mod math;

use std::{rc::Rc, cmp::Ordering, time::{SystemTime, UNIX_EPOCH}, cell::RefCell};

use crate::{value::{Value, func::Func}, RuntimeError, env::Environment};

#[macro_export]
macro_rules! declare_fn {
    ($env:ident, $name:ident, $arg_count:literal) => {::paste::paste!{{
        let s: ::std::rc::Rc<str> = ::std::rc::Rc::from(stringify!($name));
        $env.declare(s.clone(), $crate::value::Value::Func($crate::value::func::Func::Builtin { func: [<fn_ $name>], arg_count: $arg_count, name: s }));
    }}};
    ($env:ident, $name:literal, $rust_name:ident, $arg_count:literal) => {{
        let s: ::std::rc::Rc<str> = ::std::rc::Rc::from($name);
        $env.declare(s.clone(), $crate::value::Value::Func($crate::value::func::Func::Builtin { func: $rust_name, arg_count: $arg_count, name: s }));
    }};
}

pub fn load(env: &mut Environment) {
    declare_fn!(env, "type", fn_type, 1);
    declare_fn!(env, type_eq, 1);
    declare_fn!(env, str, 1);
    declare_fn!(env, repr, 1);
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

fn mk_range_inner(start: i64, end: i64, delta: i64) -> Func {
    let counter = RefCell::new(start);
    Func::BuiltinClosure {
        arg_count: 0,
        func: Rc::new(move |_| {
            let c_value = *counter.borrow();
            if delta >= 0 && c_value >= end
            || delta <= 0 && c_value <= end {
                Ok(Value::Nil)
            } else {
                let res = *counter.borrow();
                *counter.borrow_mut() += delta;
                Ok(Value::Int(res))
            }
        })
    }
}

fn fn_range(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let (start, end, delta) = match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => (a, b, 
            match a.cmp(&b) {
                Ordering::Equal => 0,
                Ordering::Less => 1,
                Ordering::Greater => -1,
            }
        ),
        _ => return Err("Both arguments to range must be integers".into())
    };
    Ok(Value::Func(mk_range_inner(*start, *end, delta)))
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
