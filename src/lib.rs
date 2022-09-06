use std::{rc::Rc, error::Error, fmt};

pub mod token;
pub mod expr;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod eval;
pub mod interpreter;

#[derive(Clone, Debug)]
pub struct Position {
    pub pos: usize,
    pub line: usize,
    pub col: usize,
    pub file: Option<Rc<str>>
}


#[derive(Debug)]
pub struct ParserError {
    pub message: String,
    pub pos: Position
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub pos: Position
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {}\n    In {} at {},{}", 
            self.message, 
            self.pos.file.as_ref().map(|o| o.as_ref()).unwrap_or("<unknown>"), 
            self.pos.line, 
            self.pos.col
        )
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {}\n    In {} at {},{}", 
            self.message, 
            self.pos.file.as_ref().map(|o| o.as_ref()).unwrap_or("<unknown>"), 
            self.pos.line, 
            self.pos.col
        )
    }
}

impl Error for ParserError {}
impl Error for RuntimeError {}