use std::{cell::RefCell, rc::Rc};

use crate::{value::Value, lexer::Lexer, parser::Parser, eval::{Environment, eval_stmt, eval_expr}, expr::Stmt};

pub fn interpret(src: &str, env: Option<Rc<RefCell<Environment>>>) -> Result<Value, Box<dyn std::error::Error>> {
    let mut lexer = Lexer::new(src, None);
    lexer.lex()?;
    let mut parser = Parser::new(lexer.into_tokens());
    let ast = parser.parse()?;
    let environ;
    if let Some(env) = env {
        environ = env;
    } else {
        environ = Rc::new(RefCell::new(Environment::new()))
    }
    let mut result = Value::Nil;
    for stmt in ast {
        if let Stmt::Expr{expr} = stmt {
            result = eval_expr(&expr, environ.clone())?;
        } else {
            eval_stmt(&stmt, environ.clone())?;
            result = Value::Nil;
        }
    }
    Ok(result)
}