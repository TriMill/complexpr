use std::{rc::Rc, io::Write};

use crate::{value::{Value, BuiltinFn}, eval::Environment};

pub fn load(env: &mut Environment) {
    let mut name: Rc<str>;
    name = Rc::from("str");
    env.declare(name.clone(), Value::BuiltinFn(BuiltinFn { func: fn_str, arg_count: 1, name }));
    name = Rc::from("repr");
    env.declare(name.clone(), Value::BuiltinFn(BuiltinFn { func: fn_repr, arg_count: 1, name }));
    name = Rc::from("print");
    env.declare(name.clone(), Value::BuiltinFn(BuiltinFn { func: fn_print, arg_count: 1, name }));
    name = Rc::from("println"); 
    env.declare(name.clone(), Value::BuiltinFn(BuiltinFn { func: fn_println, arg_count: 1, name }));
    name = Rc::from("input"); 
    env.declare(name.clone(), Value::BuiltinFn(BuiltinFn { func: fn_input, arg_count: 0, name }));
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
    Ok(Value::from(buffer))
}