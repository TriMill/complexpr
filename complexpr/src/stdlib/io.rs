use std::{io::{Write, Read}, fs::{OpenOptions, File}, rc::Rc, cell::RefCell, fmt};

use crate::{env::Environment, declare_fn, value::{Value, Native, TypeData}, RuntimeError};

pub fn load(env: &mut Environment) {
    declare_fn!(env, print, 1);
    declare_fn!(env, println, 1);
    declare_fn!(env, input, 0);
    declare_fn!(env, open, 1);
    declare_fn!(env, close, 1);
    declare_fn!(env, read, 1);
}

fn fn_print(args: Vec<Value>) -> Result<Value, RuntimeError> {
    print!("{}", args[0]);
    std::io::stdout().flush().map_err(|e| e.to_string())?;
    Ok(Value::Nil)
}

fn fn_println(args: Vec<Value>) -> Result<Value, RuntimeError> {
    println!("{}", args[0]);
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

struct FileBox {
    f: Option<File>
}

lazy_static::lazy_static! {
    static ref FILE_TYPE_ID: usize = crate::value::generate_type_id();
}
thread_local!(static FILE_TYPE_NAME: Rc<str> = Rc::from("File"));

impl Native for FileBox {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn get_type(&self) -> crate::value::Type {
        crate::value::Type { name: FILE_TYPE_NAME.with(Rc::clone), id: *FILE_TYPE_ID, typedata: TypeData::None }
        
    }
}

impl fmt::Debug for FileBox {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for FileBox {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<file {:?}>", self.f)
    }
}

fn fn_open(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let fname = match &args[0] {
        Value::String(s) => s,
        _ => return Err(format!("Expected filename, got {}", args[0]).into())
    };
    let f = match OpenOptions::new().read(true).open(fname.as_ref()) {
        Ok(f) => f,
        Err(e) => return Err(format!("Could not open file '{}': {}", fname, e).into())
    };
    Ok(Value::Native(Rc::new(RefCell::new(FileBox { f: Some(f) }))))
}

fn fn_close(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Native(s) => {
            let mut bs = s.borrow_mut();
            let f: &mut FileBox = match bs.as_any_mut().downcast_mut() {
                Some(f) => f,
                None => return Err(format!("Expected a file, got {}", args[0]).into())
            };
            drop(f.f.take());
            Ok(Value::Nil)
        },
        _ => return Err(format!("Expected a file, got {}", args[0]).into())
    }
}

fn fn_read(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Native(s) => {
            let mut bs = s.borrow_mut();
            let f: &mut FileBox = match bs.as_any_mut().downcast_mut() {
                Some(f) => f,
                None => return Err(format!("Expected a file, got {}", args[0]).into())
            };
            if let Some(file) = &mut f.f {
                let mut buf = String::new();
                match file.read_to_string(&mut buf) {
                    Ok(_) => return Ok(Value::from(buf)),
                    Err(e) => return Err(format!("Error reading file {}: {}", f, e).into())
                }
            } else {
                Err("Attempt to read file that has been closed".into())
            }
        },
        _ => return Err(format!("Expected a file, got {}", args[0]).into())
    }
}
