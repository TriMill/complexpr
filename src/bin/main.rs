use std::{rc::Rc, cell::RefCell, fs::{File, self}, io::{BufReader, Read}};

use complexpr::{eval::Environment, interpreter::interpret, value::Value};
use rustyline::{self, error::ReadlineError};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 2 {
        let fname = &args[1];
        let src = fs::read_to_string(fname)?;
        interpret(&src, None)?;
    } else {
        repl()?;
    }
    Ok(())
}

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::Editor::<()>::new()?;
    let env = Rc::new(RefCell::new(Environment::new()));
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let result = interpret(&line, Some(env.clone()));
                match result {
                    Ok(Value::Nil) => (),
                    Ok(value) => println!("{:?}", value),
                    Err(e) => println!("Error: {}", e)
                }
            }
            Err(ReadlineError::Eof) => break,
            Err(_) => (),
        }
    }
    Ok(())
}