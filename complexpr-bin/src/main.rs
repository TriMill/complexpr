use std::{fs, panic::{self, PanicInfo}};

use backtrace::Backtrace;
use complexpr::{env::Environment, interpreter::interpret, value::Value, stdlib};
use rustyline::{self, error::ReadlineError, Config, CompletionType, EditMode, hint::HistoryHinter, validate::MatchingBracketValidator, Editor};

mod helper;
use helper::CxprHelper;

const C_RESET: &str = "\x1b[0m";
const C_BLUE: &str = "\x1b[94m";
const C_RED: &str = "\x1b[91m";
const PROMPT: &str = "\x1b[94m>> \x1b[0m";

fn panic_hook(info: &PanicInfo) {
    eprintln!("{:?}", Backtrace::new());
    eprintln!("!!! Internal interpreter error occured !!!");
    if let Some(s) = std::thread::current().name() {
        eprintln!("Thread: {}", s);
    }
    if let Some(loc) = info.location() {
        eprintln!("Location: {}:{}:{}", loc.file(), loc.line(), loc.column())
    }
    if let Some(s) = info.payload().downcast_ref::<&str>() {
        eprintln!("Message: {}", s);
    } else if let Some(s) = info.payload().downcast_ref::<String>() {
        eprintln!("Message: {}", s);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    panic::set_hook(Box::new(panic_hook));
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 2 {
        let fname = &args[1];
        let src = fs::read_to_string(fname)?;
        let res = interpret(&src, Some(fname.into()), None, false);
        if let Err(e) = res {
            eprintln!("Error: {}", e);
        }
    } else {
        repl()?;
    }
    Ok(())
}

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();
    let env = Environment::new().wrap();
    let h = CxprHelper {
        hinter: HistoryHinter {},
        colored_prompt: PROMPT.to_owned(),
        validator: MatchingBracketValidator::new(),
        env: env.clone(),
    };
    let mut rl = Editor::with_config(config)?;
    rl.set_helper(Some(h));
    println!("Press {}Ctrl+D{} to exit.", C_BLUE, C_RESET);
    stdlib::load(&mut env.borrow_mut());
    stdlib::iter::load(&mut env.borrow_mut());
    stdlib::math::load(&mut env.borrow_mut());
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let result = interpret(&line, None, Some(env.clone()), true);
                match result {
                    Ok(value) => {
                        if value != Value::Nil {
                            println!("{}", value.repr());
                        }
                        env.borrow_mut().declare("_".into(), value);
                    }
                    Err(e) => eprintln!("{}Error: {}{}", C_RED, C_RESET, e)
                }
            }
            Err(ReadlineError::Eof) => break,
            Err(_) => (),
        }
    }
    Ok(())
}
