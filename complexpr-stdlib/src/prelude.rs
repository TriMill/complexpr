use std::{time::{SystemTime, UNIX_EPOCH}, rc::Rc, cell::RefCell};

use complexpr::{value::{Value, func::Func, self}, RuntimeError, env::Environment};
use try_partialord::TrySort;

use crate::declare_fn;

pub fn load(env: &mut Environment) {
    for ty in value::generate_builtin_types() {
        env.declare(ty.name.clone(), Value::Type(ty));
    }
    declare_fn!(env, "type", fn_type, 1);
    declare_fn!(env, type_id, 1);
    declare_fn!(env, type_eq, 2);
    declare_fn!(env, copy, 1);
    declare_fn!(env, str, 1);
    declare_fn!(env, repr, 1);
    declare_fn!(env, ord, 1); 
    declare_fn!(env, chr, 1); 
    declare_fn!(env, to_radix, 2); 
    declare_fn!(env, bin, 1); 
    declare_fn!(env, hex, 1); 
    declare_fn!(env, sex, 1); 
    declare_fn!(env, from_radix, 2); 
    declare_fn!(env, has, 2); 
    declare_fn!(env, len, 1); 
    declare_fn!(env, args, 0); 
    declare_fn!(env, time, 0); 
    declare_fn!(env, assert, 1); 
    declare_fn!(env, list, 1);
    declare_fn!(env, push, 2);
    declare_fn!(env, pop, 1);
    declare_fn!(env, append, 2);
    declare_fn!(env, insert, 3);
    declare_fn!(env, remove, 2);
    declare_fn!(env, rev, 1);
    declare_fn!(env, sort, 1);
    declare_fn!(env, sort_by, 2);
    declare_fn!(env, sort_by_key, 2);
    
}

fn fn_type(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Type(args[0].get_type()))
}

fn fn_type_id(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if let Value::Type(ty) = &args[0] {
        Ok(Value::Int(ty.id as i64))
    } else {
        Err("Argument to id must be a type".into())
    }
}

fn fn_type_eq(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Bool(if args[0].get_type() != args[1].get_type() {
        false
    } else {
        args[0] == args[1]
    }))
}

fn fn_copy(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(match &args[0] {
        Value::List(l) => Value::from(l.borrow().clone()),
        Value::Map(m) => Value::from(m.borrow().clone()),
        // Value::Func(f) => Value::Func(f.make_copy()) // TODO copy functions
        Value::Struct(_) => todo!(),
        a => a.clone(),
    })
}

fn fn_str(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::from(args[0].to_string()))
}

fn fn_repr(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::from(args[0].repr()))
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

fn to_radix(r: i64, mut n: i64) -> String {
    let mut result = String::new();
    let mut idx = 0;
    if n == 0 {
        result.push('0');
        return result
    } else if n < 0 {
        n = -n;
        idx = 1;
        result.push('-');
    }
    while n != 0 {
        let c = std::char::from_digit((n % r) as u32, r as u32).unwrap();
        result.insert(idx, c);
        n /= r;
    }
    result
}

fn fn_to_radix(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Int(r), Value::Int(n)) 
            if *r >= 2 && *r <= 36 => Ok(Value::from(to_radix(*r, *n))),
        (Value::Int(_), Value::Int(_)) => Err(format!("Radix must be an integer in the range [2, 36]").into()),
        (Value::Int(_), v) => Err(format!("Second argument to to_radix must be an integer, got {:?}", v).into()),
        (v, _) => Err(format!("First argument to to_radix must be an integer, got {:?}", v).into()),
    }
}

fn fn_bin(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Int(n) => Ok(Value::from(to_radix(2, *n))),
        v => Err(format!("Argument to bin must be an integer, got {:?}", v).into()),
    }
}

fn fn_hex(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Int(n) => Ok(Value::from(to_radix(16, *n))),
        v => Err(format!("Argument to bin must be an integer, got {:?}", v).into()),
    }
}

fn fn_sex(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Int(n) => Ok(Value::from(to_radix(6, *n))),
        v => Err(format!("Argument to bin must be an integer, got {:?}", v).into()),
    }
}

fn fn_from_radix(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Int(r), Value::String(s)) 
        if *r >= 2 && *r <= 36 
        => match i64::from_str_radix(s, *r as u32) {
            Ok(n) => Ok(Value::from(n)),
            Err(_) => Err(format!("Failed to convert string to integer in specified radix").into()),
        },
        (Value::Int(_), Value::String(_)) => Err(format!("Radix must be an integer in the range [2, 36]").into()),
        (Value::Int(_), v) => Err(format!("Second argument to from_radix must be a string, got {:?}", v).into()),
        (v, _) => Err(format!("First argument to from_radix must be an integer, got {:?}", v).into()),
    }
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

fn fn_args(_: Vec<Value>) -> Result<Value, RuntimeError> {
    let mut args = std::env::args();
    args.next();
    let args = RefCell::new(args);
    Ok(Value::Func(Func::BuiltinClosure {
        arg_count: 0,
        func: Rc::new(move |_| {
            match args.borrow_mut().next() {
                Some(s) => Ok(Value::from(s)),
                None => Ok(Value::Nil)
            }
        })
    }))
}

fn fn_time(_: Vec<Value>) -> Result<Value, RuntimeError> {
    let time = SystemTime::now().duration_since(UNIX_EPOCH).map_err(|e| e.to_string())?;
    Ok(Value::from(time.as_secs_f64()))
}

fn fn_assert(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args[0].truthy() {
        Ok(Value::Nil)
    } else {
        Err("Assertion failed".into())
    }
}

fn fn_list(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let a = args[0].iter()?;
    let mut res = Vec::new();
    for v in a { res.push(v?); }
    Ok(Value::from(res))
}

fn fn_push(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if let Value::List(l) = &args[0] {
        l.as_ref().borrow_mut().push(args[1].clone());
        Ok(Value::Nil)
    } else{
        Err("First argument to push must be a list".into())
    }
}

fn fn_pop(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if let Value::List(l) = &args[0] {
        l.as_ref().borrow_mut().pop().ok_or_else(|| "Pop on empty list".into())
    } else{
        Err("First argument to pop must be a list".into())
    }
}

fn fn_append(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::List(a), Value::List(b)) => {
            let mut newvals = b.borrow().clone();
            a.borrow_mut().append(&mut newvals);
            Ok(Value::Nil)
        },
        _ => Err("Both arguments to append must be lists".into())
    }
}

fn fn_insert(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::List(a), Value::Int(i)) => {
            if *i < 0 {
                return Err(format!("List index {} cannot be negative", i).into())
            }
            if *i > a.borrow().len() as i64 {
                return Err(format!("List index {} not valid for list of length {}", i, a.borrow().len()).into())
            }
            a.borrow_mut().insert(*i as usize, args[2].clone());
            Ok(Value::Nil)
        },
        (Value::Map(a), b) => {
            Ok(a.borrow_mut().insert(b.clone(), args[3].clone()).unwrap_or(Value::Nil))
        },
        (Value::List(_), _) => Err("Second argument to insert must be an integer when the first is a list".into()),
        _ => Err("First argument to insert must be a list or map".into()),
    }
}

fn fn_remove(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::List(a), Value::Int(i)) => {
            if *i < 0 {
                return Err(format!("List index {} cannot be negative", i).into())
            }
            if *i >= a.borrow().len() as i64 {
                return Err(format!("List index {} not valid for list of length {}", i, a.borrow().len()).into())
            }
            Ok(a.borrow_mut().remove(*i as usize))
        },
        (Value::Map(a), b) => {
            Ok(a.borrow_mut().remove(b).unwrap_or(Value::Nil))
        },
        (Value::List(_), _) => Err("Second argument to remove must be an integer when the first is a list".into()),
        _ => Err("First argument to remove must be a list or map".into()),
    }
}

fn fn_rev(mut args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::List(l) => {
            l.borrow_mut().reverse();
            Ok(args.swap_remove(0))
        },
        Value::String(s) => {
            let s: String = s.chars().rev().collect();
            Ok(Value::from(s))
        },
        _ => Err(format!("Expected list or string, got {}", args[0]).into())
    }
}

fn fn_sort(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::List(l) => match l.borrow_mut().try_sort() {
            Ok(_) => Ok(args[0].clone()),
            Err(_) => Err("List contained incomparable items".into())
        },
        _ => Err(format!("Expected list, got {}", args[0]).into())
    }
}

fn fn_sort_by(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let func = match &args[1] {
        Value::Func(f) => f,
        x => return Err(format!("Expected function as second argument, got {}", x).into())
    };
    let ord = move |a: &Value, b: &Value| {
        match func.call(vec![a.clone(), b.clone()]) {
            Ok(Value::Int(n)) => Some(n < 0),
            _ => None
        }
    };
    match &args[0] {
        Value::List(l) => match l.borrow_mut().try_sort_by(ord) {
            Ok(_) => Ok(args[0].clone()),
            Err(_) => Err("Error occured in sort comparison function".into())
        },
        _ => Err(format!("Expected list as first argument, got {}", args[0]).into())
    }
}

fn fn_sort_by_key(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let func = match &args[1] {
        Value::Func(f) => f,
        x => return Err(format!("Expected function as second argument, got {}", x).into())
    };
    let ord = move |a: &Value| {
        func.call(vec![a.clone()]).ok()
    };
    match &args[0] {
        Value::List(l) => match l.borrow_mut().try_sort_by_key(ord) {
            Ok(_) => Ok(args[0].clone()),
            Err(_) => Err("Error occured in sort key function".into())
        },
        _ => Err(format!("Expected list as first argument, got {}", args[0]).into())
    }
}
