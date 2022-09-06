use crate::{token::{Token, TokenType, OpType}, ParserError, expr::{Stmt, Expr}};

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, idx: 0 }
    }

    fn at_end(&self) -> bool {
        self.idx >= self.tokens.len()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.idx]
    }

    fn next(&mut self) -> Token {
        let t = self.tokens[self.idx].clone();
        self.idx += 1;
        t
    }

    fn mk_error<S>(&self, msg: S) -> ParserError where S: Into<String> {
        let token = if self.at_end() {
            self.tokens.last().unwrap()
        } else {
            self.peek()
        };
        ParserError { pos: token.pos.clone(), message: msg.into() }
    }

    fn err_on_eof(&self) -> Result<(), ParserError> {
        if self.at_end() {
            Err(self.mk_error("Unexpected EOF"))
        } else {
            Ok(())
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = vec![];
        while !self.at_end() {
            stmts.push(self.statement()?);
        }
        Ok(stmts)
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        let next = self.peek();

        match next.ty {
            // let statement
            TokenType::Let => {
                self.next();
                let expr = self.assignment()?;

                // must be followed by an assignment expression
                if let Expr::Binary{lhs, rhs, op: Token{ty: TokenType::Equal,..}} = expr {
                    if let Expr::Ident{value: tok} = *lhs {
                        if self.at_end() {
                            return Ok(Stmt::Let{lhs: tok, rhs: Some(*rhs)})
                        }
                        let next = self.next();
                        return match next.ty {
                            TokenType::Semicolon => Ok(Stmt::Let{lhs: tok, rhs: Some(*rhs)}),
                            _ => Err(self.mk_error("Missing semicolon after 'let' statement".to_owned()))
                        };
                    } else {
                        Err(self.mk_error("Invalid expression after 'let'".to_owned()))
                    }
                } else if let Expr::Ident{value: tok} = expr {
                    if self.at_end() {
                        return Ok(Stmt::Let{lhs: tok, rhs: None})
                    }
                    let next = self.next();
                    return match next.ty {
                        TokenType::Semicolon => Ok(Stmt::Let{lhs: tok, rhs: None}),
                        _ => Err(self.mk_error("Missing semicolon after 'let' statement".to_owned()))
                    };
                } else {
                    Err(self.mk_error("Invalid expression after 'let'".to_owned()))
                }
            }

            // if statement
            TokenType::If => {
                self.next();
                return self.ifstmt()
            }

            // fallback to an expression terminated with a semicolon
            _ => {
                let expr = self.assignment()?;
                if self.at_end() {
                    return Ok(Stmt::Expr{expr})
                }

                let next = self.next();

                return match next.ty {
                    TokenType::Semicolon => Ok(Stmt::Expr{expr}),
                    _ => Err(self.mk_error("Missing semicolon after statement"))
                };
            }
        }
    }

    // Generic method for left-associative operators
    fn expr(&mut self, op_type: OpType, next_level: fn(&mut Parser) -> Result<Expr, ParserError>) -> Result<Expr, ParserError> {
        let mut expr = next_level(self)?;
        while !self.at_end() && self.peek().ty.get_op_type() == Some(op_type) {
            let op = self.next();
            let right = next_level(self)?;
            expr = Expr::Binary { lhs: Box::new(expr), rhs: Box::new(right), op };
        }
        Ok(expr)
    }

    fn commalist(&mut self, terminator: TokenType, parse_item: fn(&mut Parser) -> Result<Expr, ParserError>) -> Result<Vec<Expr>, ParserError> {
        let mut items = vec![];
        while self.peek().ty != terminator {
            let expr = parse_item(self)?;
            items.push(expr);
            if self.peek().ty == TokenType::Comma {
                self.next();
            } else if self.peek().ty == terminator {
                break;
            } else {
                return Err(self.mk_error(format!("Expected Comma or {:?} after list", terminator)))
            }
        }
        self.next();
        Ok(items)
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let mut stack= vec![];
        let mut expr = self.pipeline()?;
        while !self.at_end() && self.peek().ty.get_op_type() == Some(OpType::Assignment) {
            let op = self.next();
            stack.push((expr, op));
            expr = self.pipeline()?;
        }
        while let Some(item) = stack.pop() {
            if !item.0.is_lvalue() {
                return Err(self.mk_error("Invalid LValue for assignment operation"))
            }
            expr = Expr::Binary{ lhs: Box::new(item.0), rhs: Box::new(expr), op: item.1 };
        }
        Ok(expr)
    }

    fn pipeline(&mut self) -> Result<Expr, ParserError> {
        self.expr(OpType::Pipeline, Self::additive)
    }

    fn additive(&mut self) -> Result<Expr, ParserError> {
        self.expr(OpType::Additive, Self::multiplicative)
    }

    fn multiplicative(&mut self) -> Result<Expr, ParserError> {
        self.expr(OpType::Multiplicative, Self::exponential)
    }

    // Right associative, so cannot use self.expr(..)
    fn exponential(&mut self) -> Result<Expr, ParserError> {
        let mut stack= vec![];
        let mut expr = self.unary()?;
        while !self.at_end() && self.peek().ty == TokenType::Caret {
            let op = self.next();
            stack.push((expr, op));
            expr = self.unary()?;
        }
        while let Some(item) = stack.pop() {
            expr = Expr::Binary{ lhs: Box::new(item.0), rhs: Box::new(expr), op: item.1 };
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        self.err_on_eof()?;
        if matches!(self.peek().ty, TokenType::Bang | TokenType::Minus) {
            let op = self.next();
            Ok(Expr::Unary { arg: Box::new(self.fncall()?), op })
        } else {
            self.fncall()
        }
    }

    fn fncall(&mut self) -> Result<Expr, ParserError> {
        let expr = self.expr_base()?;
        if !self.at_end() && self.peek().ty == TokenType::LParen {
            self.next();
            let args = self.commalist(TokenType::RParen, Self::assignment)?;
            Ok(Expr::FuncCall { func: Box::new(expr), args })
        } else {
            Ok(expr)
        }
    }

    fn expr_base(&mut self) -> Result<Expr, ParserError> {
        self.err_on_eof()?;
        let next = self.next();
        if matches!(next.ty, 
            TokenType::True | TokenType::False | TokenType::Nil 
            | TokenType::Int(_) | TokenType::Float(_) | TokenType::ImFloat(_)
            | TokenType::String(_) 
        ) {
            Ok(Expr::Literal { value: next })
        } else if let TokenType::Ident(..) = next.ty {
            Ok(Expr::Ident { value: next })
        } else if next.ty == TokenType::LParen {
            let expr = self.assignment()?;
            if self.at_end() || TokenType::RParen != self.next().ty {
                Err(self.mk_error("Left parenthesis never closed"))
            } else {
                Ok(expr)
            }
        } else {
            Err(self.mk_error(format!("Unexpected token: {:?}", next.ty)))
        }
    }

    fn ifstmt(&mut self) -> Result<Stmt, ParserError> {
        todo!()
    }
}