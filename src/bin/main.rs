use std::{rc::Rc, cell::RefCell, fs};

use complexpr::{eval::Environment, interpreter::interpret, value::Value, stdlib};
use rustyline::{self, error::ReadlineError};

const C_RESET: &str = "\x1b[0m";
const C_BLUE: &str = "\x1b[94m";
const PROMPT: &str = "\x1b[94m>> \x1b[0m";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 2 {
        let fname = &args[1];
        let src = fs::read_to_string(fname)?;
        let res = interpret(&src, Some(fname.into()), None, false);
        if let Err(e) = res {
            println!("{}", e);
        }
    } else {
        repl()?;
    }
    Ok(())
}

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    println!("Press {}Ctrl+D{} to exit.", C_BLUE, C_RESET);
    let mut rl = rustyline::Editor::<()>::new()?;
    let env = Rc::new(RefCell::new(Environment::new()));
    stdlib::load(&mut env.borrow_mut());
    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                let result = interpret(&line, None, Some(env.clone()), true);
                match result {
                    Ok(Value::Nil) => (),
                    Ok(value) => println!("{}", value.repr()),
                    Err(e) => println!("{}", e)
                }
            }
            Err(ReadlineError::Eof) => break,
            Err(_) => (),
        }
    }
    Ok(())
}
