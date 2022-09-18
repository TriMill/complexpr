use std::{rc::Rc, io::Write, cmp::Ordering, time::{SystemTime, UNIX_EPOCH}, cell::RefCell};

use num_traits::ToPrimitive;

use crate::{value::{Value, Func, CIterator}, RuntimeError, env::Environment};

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
    name = Rc::from("take"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_take, arg_count: 2, name }));
    name = Rc::from("skip"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_skip, arg_count: 2, name }));
    name = Rc::from("forall"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_forall, arg_count: 2, name }));
    name = Rc::from("exists"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_exists, arg_count: 2, name }));
    name = Rc::from("min"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_min, arg_count: 2, name }));
    name = Rc::from("max"); 
    env.declare(name.clone(), Value::Func(Func::Builtin { func: fn_max, arg_count: 2, name }));
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

fn fn_min(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0].partial_cmp(&args[1]) {
        None => Err("Arguments to min must be comparable".into()),
        Some(Ordering::Greater) => Ok(args[1].clone()),
        _ => Ok(args[0].clone())
    }
}

fn fn_max(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0].partial_cmp(&args[1]) {
        None => Err("Arguments to max must be comparable".into()),
        Some(Ordering::Less) => Ok(args[1].clone()),
        _ => Ok(args[0].clone())
    }
}
