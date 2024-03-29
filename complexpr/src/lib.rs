use std::{rc::Rc, error::Error, fmt};

pub mod token;
pub mod expr;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod eval;
pub mod interpreter;
pub mod env;

#[derive(Clone, Debug)]
pub struct Position {
    pub pos: usize,
    pub line: usize,
    pub col: usize,
    pub file: Option<Rc<str>>
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", 
            self.file.as_ref().map(|x| x.as_ref()).unwrap_or("<unknown>"),
            self.line,
            self.col
        )
    }
}


#[derive(Debug)]
pub struct ParserError {
    pub message: String,
    pub pos: Position
}

#[derive(Debug)]
pub struct Stackframe {
    pub pos: Option<Position>,
    pub fn_name: Option<Rc<str>>,
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub stacktrace: Vec<Stackframe>,
    pub last_pos: Option<Position>,
}

impl RuntimeError {
    pub fn new<S>(message: S, pos: Position) -> Self
    where S: Into<String> {
        Self { 
            message: message.into(), 
            stacktrace: vec![],
            last_pos: Some(pos)
        }
    }

    pub fn new_no_pos<S>(message: S) -> Self
    where S: Into<String> {
        Self { 
            message: message.into(), 
            stacktrace: vec![],
            last_pos: None
        }
    }

    pub fn exit_fn(mut self, fn_name: Option<Rc<str>>, pos: Position) -> Self {
        self.stacktrace.push(Stackframe { pos: self.last_pos, fn_name });
        self.last_pos = Some(pos);
        self
    }

    pub fn finish(mut self, ctx_name: Option<Rc<str>>) -> Self {
        self.stacktrace.push(Stackframe { pos: self.last_pos, fn_name: ctx_name });
        self.last_pos = None;
        self
    }
}

impl From<String> for RuntimeError {
    fn from(s: String) -> Self {
        Self::new_no_pos(s)
    }
}

impl From<&str> for RuntimeError {
    fn from(s: &str) -> Self {
        Self::new_no_pos(s)
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n    In {} at {},{}", 
            self.message, 
            self.pos.file.as_ref().map(|o| o.as_ref()).unwrap_or("<unknown>"), 
            self.pos.line, 
            self.pos.col
        )
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;
        for frame in &self.stacktrace {
            let fn_name = frame.fn_name.as_ref().map(|o| o.as_ref()).unwrap_or("<anonymous fn>");
            match &frame.pos {
                Some(pos) => write!(f, "\n    In {} at {}", fn_name, pos)?,
                None => write!(f, "\n    In {} at <unknown>", fn_name)?,
            }
        }
        Ok(())
    }
}

impl Error for ParserError {}
impl Error for RuntimeError {}
