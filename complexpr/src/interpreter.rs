use std::{cell::RefCell, rc::Rc};

use crate::{value::Value, lexer::Lexer, parser::Parser, eval::{eval_stmt, eval_expr}, expr::Stmt, env::{EnvRef, Environment}};

pub fn interpret(src: &str, fname: Option<String>, env: Option<EnvRef>, repl: bool) -> Result<Value, Box<dyn std::error::Error>> {
    let ctx_name = if repl { "<interactive input>" } else { fname.as_ref().map(|s| s.as_ref()).unwrap_or("<unknown>") };
    let mut lexer = Lexer::new(src, fname.clone());
    lexer.lex()?;
    let tokens = lexer.into_tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    let environ;
    if let Some(env) = env {
        environ = env;
    } else {
        environ = Rc::new(RefCell::new(Environment::new()));
    }

    let mut result = Value::Nil;
    for stmt in ast {
        if let Stmt::Expr{expr} = stmt {
            result = eval_expr(&expr, environ.clone()).map_err(|e| e.finish(Some(Rc::from(ctx_name))))?;
        } else {
            eval_stmt(&stmt, environ.clone()).map_err(|e| e.as_error().finish(Some(Rc::from(ctx_name))))?;
            result = Value::Nil;
        }
    }
    Ok(result)
}
