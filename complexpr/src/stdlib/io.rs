use std::io::Write;

use crate::{env::Environment, declare_fn, value::Value, RuntimeError};

pub fn load(env: &mut Environment) {
    declare_fn!(env, print, 1);
    declare_fn!(env, println, 1);
    declare_fn!(env, input, 0);
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
