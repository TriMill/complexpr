#![cfg(test)]

use std::{cell::RefCell, rc::Rc};

use complexpr::{lexer::Lexer, parser::Parser, eval::{Environment, eval_stmt}};

#[test]
pub fn test() {
    let mut lexer = Lexer::new("let a = 1 + 1; let b = a + 1;", None);
    lexer.lex().unwrap();
    let mut parser = Parser::new(lexer.into_tokens());
    let ast = parser.parse().unwrap();
    let env = Rc::new(RefCell::new(Environment::new()));
    for stmt in ast {
        eval_stmt(&stmt, env.clone()).unwrap();
    }
    println!("{:?}", env);
    todo!("end of tests")
}