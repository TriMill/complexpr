use std::{fs, panic::{self, PanicInfo}, process::ExitCode};

use backtrace::Backtrace;
use complexpr::{interpreter::interpret, env::Environment};

mod helper;
mod repl;

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

fn create_env() -> Environment {
    let mut env = Environment::new();

    complexpr_stdlib::prelude::load(&mut env);
    complexpr_stdlib::io::load(&mut env);
    complexpr_stdlib::iter::load(&mut env);
    complexpr_stdlib::math::load(&mut env);

    env
}

fn main() -> ExitCode {
    panic::set_hook(Box::new(panic_hook));
    let args: Vec<String> = std::env::args().collect();
    if args.len() >= 2 {
        let fname = &args[1];
        let src = match fs::read_to_string(fname) {
            Ok(src) => src,
            Err(e) => {
                eprintln!("Error reading file: {}", e);
                return ExitCode::from(2);
            }
        };

        let env = create_env().wrap();

        let res = interpret(&src, Some(fname.into()), Some(env), false);
        if let Err(e) = res {
            eprintln!("Error: {}", e);
            return ExitCode::from(1);
        }
    } else {
        #[cfg(feature = "repl")]
        {
            if let Err(e) = repl::repl() {
                eprintln!("Error: {}", e);
                return ExitCode::from(4)
            }
        }
        #[cfg(not(feature = "repl"))]
        {
            eprintln!("Expected a file to execute. To use complexpr in repl mode, enable the 'repl' feature (enabled by default).");
            return ExitCode::from(3)
        }
    }
    ExitCode::SUCCESS
}
