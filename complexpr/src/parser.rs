use std::rc::Rc;

use crate::{token::{Token, TokenType, OpType}, ParserError, expr::{Stmt, Expr}, value::{self, func::Func}, eval::eval_standard_binary};

pub struct Parser {
    tokens: Vec<Token>,
    repl: bool,
    idx: usize
}

impl Parser {
    pub fn new(tokens: Vec<Token>, repl: bool) -> Self {
        Self { tokens, repl, idx: 0 }
    }
    
    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = vec![];
        while !self.at_end() {
            stmts.push(self.statement(!self.repl)?);
        }
        Ok(stmts)
    }

    ////////////////////////
    //                    //
    //  Helper functions  //
    //                    //
    ////////////////////////

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

    fn expect(&mut self, tokty: TokenType) -> (bool, Token) {
        let next = self.next();
        (tokty == next.ty, next)
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

    fn ident(&mut self) -> Result<Token, ParserError> {
        let next = self.next();
        match next.ty {
            TokenType::Ident(_) => Ok(next),
            _ => Err(ParserError { message: "Expected identifier".into(), pos: next.pos })
        }
    }

    fn commalist<T>(&mut self, terminator: TokenType, parse_item: fn(&mut Parser) -> Result<T, ParserError>) -> Result<Vec<T>, ParserError> {
        let mut items = vec![];
        while !self.at_end() && self.peek().ty != terminator {
            let expr = parse_item(self)?;
            items.push(expr);
            self.err_on_eof()?;
            if self.peek().ty == TokenType::Comma {
                self.next();
            } else if self.peek().ty == terminator {
                break;
            } else {
                return Err(self.mk_error(format!("Expected Comma or {:?} after list", terminator)))
            }
        }
        self.err_on_eof()?;
        self.next();
        Ok(items)
    }

    //////////////////
    //              //
    //  Statements  //
    //              //
    //////////////////

    fn statement(&mut self, req_semicolon: bool) -> Result<Stmt, ParserError> {
        let next_ty = &self.peek().ty;

        match next_ty {
            TokenType::Let => {
                // let statement
                self.next();
                self.letstmt(req_semicolon)
            },
            TokenType::LBrace => {
                // block
                self.next();
                self.block()
            },
            TokenType::If => {
                // if statement
                self.next();
                self.ifstmt()
            },
            TokenType::For => {
                // for loop
                self.next();
                self.forstmt()
            },
            TokenType::While => {
                // while loop
                self.next();
                self.whilestmt()
            },
            TokenType::Break => {
                let tok = self.next();
                self.terminate_stmt(Stmt::Break{ pos: tok.pos }, req_semicolon)
            },
            TokenType::Continue => {
                let tok = self.next();
                self.terminate_stmt(Stmt::Continue{ pos: tok.pos }, req_semicolon)
            },
            TokenType::Return => {
                let tok = self.next();
                let expr = self.assignment()?;
                self.terminate_stmt(Stmt::Return{ pos: tok.pos, expr }, req_semicolon)
            },
            TokenType::Fn => {
                self.next();
                self.fndef()
            },
            TokenType::Struct => {
                self.next();
                self.structstmt()
            },
            _ => {
                // fallback to an expression terminated with a semicolon
                let expr = self.assignment()?;
                self.terminate_stmt(Stmt::Expr{ expr }, req_semicolon)
            }
        }
    }

    fn terminate_stmt(&mut self, stmt: Stmt, req_semicolon: bool) -> Result<Stmt, ParserError> {
        if self.at_end() {
            if req_semicolon {
                self.err_on_eof()?;
            } else{
                return Ok(stmt)
            }
        }

        match self.expect(TokenType::Semicolon).0 {
            true => Ok(stmt),
            false if !req_semicolon => Ok(stmt),
            false => Err(self.mk_error("Missing semicolon after statement"))
        }

    }

    fn letstmt(&mut self, req_semicolon: bool) -> Result<Stmt, ParserError> {
        let expr = self.assignment()?;
        // must be followed by an assignment expression
        if let Expr::Binary{lhs, rhs, op: Token{ty: TokenType::Equal,..}} = expr {
            if let Expr::Ident{value: tok} = *lhs {
                self.terminate_stmt(Stmt::Let{lhs: tok, rhs: Some(*rhs)}, req_semicolon)
            } else {
                Err(self.mk_error("Invalid expression after 'let'".to_owned()))
            }
        } else if let Expr::Ident{value: tok} = expr {
            self.terminate_stmt(Stmt::Let{lhs: tok, rhs: None}, req_semicolon)
        } else {
            Err(self.mk_error("Invalid expression after 'let'".to_owned()))
        }
    }

    fn ifstmt(&mut self) -> Result<Stmt, ParserError> {
        let mut if_clauses = vec![];
        let mut ec = false;
        loop {
            let condition = self.assignment()?;
            let body = self.statement(true)?;
            if_clauses.push((condition, body));
            match self.peek().ty {
                TokenType::Elif => { self.next(); continue },
                TokenType::Else => { self.next(); ec = true; break },
                _ => break
            }
        }
        let else_clause = if ec {
            Some(Box::new(self.statement(true)?))
        } else {
            None
        };
        Ok(Stmt::If{ 
            if_clauses, else_clause
        })
    }

    fn forstmt(&mut self) -> Result<Stmt, ParserError> {
        self.err_on_eof()?;
        let var = self.next();
        if let TokenType::Ident(_) = &var.ty {
            self.err_on_eof()?;
            let colon = self.next();
            if colon.ty != TokenType::Colon {
                return Err(self.mk_error("Expected colon"))
            }
            self.err_on_eof()?;
            let expr = self.assignment()?;
            self.err_on_eof()?;
            let stmt = self.statement(true)?;
            Ok(Stmt::For{ var, expr, stmt: Box::new(stmt), iter_pos: colon.pos })
        } else {
            Err(self.mk_error("Expected identifier after for"))
        }
    }

    fn whilestmt(&mut self) -> Result<Stmt, ParserError> {
        self.err_on_eof()?;
        let expr = self.assignment()?;
        self.err_on_eof()?;
        let stmt = self.statement(true)?;
        Ok(Stmt::While{ expr, stmt: Box::new(stmt) })
    }

    fn fndef(&mut self) -> Result<Stmt, ParserError> {
        self.err_on_eof()?;
        let name = self.next();
        let name = if let TokenType::Ident(_) = name.ty {
            name
        } else {
            return Err(ParserError { message: "Expected identifer in function declaration".into(), pos: name.pos })
        };
        self.err_on_eof()?;
        if !self.expect(TokenType::LParen).0 {
            return Err(self.mk_error("Expected left parenthesis to start arguments list"))
        }
        let args = self.commalist(TokenType::RParen, Self::ident)?;
        self.err_on_eof()?;
        if self.peek().ty == TokenType::LParen {
            self.next();
            self.err_on_eof()?;
            let body = self.assignment()?;
            self.err_on_eof()?;
            if !self.expect(TokenType::RParen).0 {
                return Err(self.mk_error("Expected right parenthesis to close function body"))
            }
            Ok(Stmt::Fn { name, args, body: Box::new(Stmt::Expr {expr: body }) })
        } else if self.peek().ty == TokenType::LBrace {
            self.next();
            self.err_on_eof()?;
            let body = self.block()?;
            Ok(Stmt::Fn { name, args, body: Box::new(body) })
        } else {
            Err(self.mk_error("Expected '(' or '{' after function arguments list to begin body"))
        }
    }

    fn block(&mut self) -> Result<Stmt, ParserError> {
        let mut stmts = vec![];
        while !self.at_end() && self.peek().ty != TokenType::RBrace {
            stmts.push(self.statement(true)?)
        }
        self.err_on_eof()?;
        self.next();
        Ok(Stmt::Block{ stmts })
    }

    fn structstmt(&mut self) -> Result<Stmt, ParserError> {
        self.err_on_eof()?;
        let tok_name = self.ident()?;
        let name = tok_name.ty.clone().as_ident().unwrap();
        self.err_on_eof()?;
        if !self.expect(TokenType::LBrace).0 {
            return Err(self.mk_error("Expected left brace in struct definition"))
        }
        self.err_on_eof()?;
        let items = self.commalist(TokenType::RBrace, Self::ident)?;
        let ty = value::generate_struct_type(name, items.iter().map(|x| x.ty.clone().as_ident().unwrap().to_string()).collect());
        Ok(Stmt::StructDef { name: tok_name, ty })
    }

    ///////////////////
    //               //
    //  Expressions  //
    //               //
    ///////////////////
     
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
        let mut expr = self.logical_or()?;
        while !self.at_end() && self.peek().ty.get_op_type() == Some(OpType::Pipeline) {
            let op = self.next();
            let right = self.logical_or()?;
            if op.ty == TokenType::PipeSlash || op.ty == TokenType::PipeBackslash {
                self.err_on_eof()?;
                if !self.expect(TokenType::Comma).0 {
                    return Err(self.mk_error("Expected comma after first argument"))
                }
                let right2 = self.logical_or()?;
                expr = Expr::Ternary { arg1: Box::new(expr), arg2: Box::new(right), arg3: Box::new(right2), op }
            } else {
                expr = Expr::Binary { lhs: Box::new(expr), rhs: Box::new(right), op };
            }
        }
        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, ParserError> {
        self.expr(OpType::LogicalOr, Self::logical_and)
    }

    fn logical_and(&mut self) -> Result<Expr, ParserError> {
        self.expr(OpType::LogicalAnd, Self::comparison)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        self.expr(OpType::Comparison, Self::additive)
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
        let mut expr = self.range()?;
        while !self.at_end() && self.peek().ty == TokenType::Caret {
            let op = self.next();
            stack.push((expr, op));
            expr = self.range()?;
        }
        while let Some(item) = stack.pop() {
            expr = Expr::Binary{ lhs: Box::new(item.0), rhs: Box::new(expr), op: item.1 };
        }
        Ok(expr)
    }

    fn range(&mut self) -> Result<Expr, ParserError> {
        let start = self.unary()?;
        if !self.at_end() && self.peek().ty == TokenType::DoubleDot {
            self.next();
            // consume = if inclusive
            let incl = if !self.at_end() && self.peek().ty == TokenType::Equal {
                self.next();
                true
            } else { 
                false 
            };
            // consume end number or * for endless
            let end = if !incl && !self.at_end() && self.peek().ty == TokenType::Star {
                self.next();
                None
            } else {
                Some(self.unary()?)
            };
            // consume :step if it exists
            let step = if !self.at_end() && self.peek().ty == TokenType::Colon {
                self.next();
                Some(self.unary()?)
            } else {
                None
            };
            Ok(Expr::Range { 
                start: Box::new(start), 
                end: end.map(|x| Box::new(x)), 
                step: step.map(|x| Box::new(x)), 
                incl 
            })
        } else {
            Ok(start)
        }
    }

    // unary: !x, -x
    fn unary(&mut self) -> Result<Expr, ParserError> {
        self.err_on_eof()?;
        if matches!(self.peek().ty, TokenType::Bang | TokenType::Minus) {
            let op = self.next();
            Ok(Expr::Unary { arg: Box::new(self.fieldaccess()?), op })
        } else {
            self.fieldaccess()
        }
    }

    // dot notation for field access
    fn fieldaccess(&mut self) -> Result<Expr, ParserError> {
        let mut target = self.suffix()?;
        while !self.at_end() && self.peek().ty == TokenType::Dot {
            let pos = self.next().pos;
            self.err_on_eof()?;
            let name = self.ident()?.ty.as_ident().unwrap();
            target = Expr::FieldAccess { target: Box::new(target), name, pos }
        } 
        Ok(target)
    }

    // function calls, array access, struct initializaiton
    fn suffix(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.expr_base()?;
        while !self.at_end() {
            match self.peek().ty {
                TokenType::LParen => expr = self.fncall_inner(expr)?,
                TokenType::LBrack => expr = self.arrindex_inner(expr)?,
                TokenType::LBrace => expr = self.structinit_inner(expr)?,
                _ => return Ok(expr)
            }
        }
        Ok(expr)
    }

    // function call: a(b)
    fn fncall_inner(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        let lparen = self.next();
        let args = self.commalist(TokenType::RParen, Self::assignment)?;
        Ok(Expr::FuncCall { func: Box::new(expr), args, pos: lparen.pos })
    }

    // array index: a[b]
    fn arrindex_inner(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        let lbrack = self.next();
        let index = self.assignment()?;
        self.err_on_eof()?;
        if !self.expect(TokenType::RBrack).0 {
            return Err(self.mk_error("Expected RBrack after collection index"))
        }
        Ok(Expr::Index { lhs: Box::new(expr), index: Box::new(index), pos: lbrack.pos })
    }

    // struct initialization: A { b }
    fn structinit_inner(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        let lbrace = self.next();
        let args = self.commalist(TokenType::RBrace, Self::assignment)?;
        Ok(Expr::StructInit { ty: Box::new(expr), args, pos: lbrace.pos })
    }

    // key-value pairs for maps
    fn kv_pair(&mut self) -> Result<(Expr, Expr), ParserError> {
        let key = self.assignment()?;
        self.err_on_eof()?;
        if !self.expect(TokenType::Colon).0 {
            return Err(self.mk_error("Expected colon in key-value pair"))
        }
        self.err_on_eof()?;
        let value = self.assignment()?;
        Ok((key, value))
    }

    // After infix operators, unary operators, function calls, and array indexes have been parsed
    fn expr_base(&mut self) -> Result<Expr, ParserError> {
        self.err_on_eof()?;
        let next = self.next();
        if matches!(next.ty, 
            TokenType::True | TokenType::False | TokenType::Nil 
            | TokenType::Int(_) | TokenType::Float(_) | TokenType::ImFloat(_)
            | TokenType::String(_) | TokenType::Char(_)
        ) {
            // A literal value
            Ok(Expr::Literal { value: next })
        } else if let TokenType::Ident(..) = next.ty {
            // An identifier
            Ok(Expr::Ident { value: next })
        } else if next.ty == TokenType::LParen {
            // Special case for "boxed infix operators"
            if !self.at_end() && self.peek().ty.is_infix_op() {
                let op = self.next();
                self.err_on_eof()?;
                if self.peek().ty != TokenType::RParen {
                    return Err(self.mk_error("Expected right parenthesis after enclosed operator"))
                }
                self.next();
                let func = Func::BuiltinClosure {
                    arg_count: 2,
                    func: Rc::new(move |args| {
                        eval_standard_binary(args[0].clone(), args[1].clone(), &op.ty, &op.pos)
                    })
                };
                return Ok(Expr::BoxedInfix { func })
            }
            // general case: parentheses as grouping symbols
            let expr = self.assignment()?;
            if self.at_end() || TokenType::RParen != self.next().ty {
                Err(self.mk_error("Left parenthesis never closed"))
            } else {
                Ok(expr)
            }
        } else if next.ty == TokenType::LBrack {
            // list literal
            let items = self.commalist(TokenType::RBrack, Self::assignment)?;
            Ok(Expr::List { items })
        } else if next.ty == TokenType::LBrace {
            // map literal
            let items = self.commalist(TokenType::RBrace, Self::kv_pair)?;
            Ok(Expr::Map { items })
        } else if next.ty == TokenType::Fn {
            // anonymous (lambda) function definition
            self.err_on_eof()?;
            if !self.expect(TokenType::LParen).0 {
                return Err(self.mk_error("Expected left parenthesis to start arguments list"))
            }
            let args = self.commalist(TokenType::RParen, Self::ident)?;
            self.err_on_eof()?;
            if self.peek().ty == TokenType::LParen {
                self.next();
                self.err_on_eof()?;
                let body = self.assignment()?;
                self.err_on_eof()?;
                if !self.expect(TokenType::RParen).0 {
                    return Err(self.mk_error("Expected right parenthesis to close function body"))
                }
                Ok(Expr::Fn { args, body: Box::new(Stmt::Expr {expr: body }) })
            } else if self.peek().ty == TokenType::LBrace {
                self.next();
                self.err_on_eof()?;
                let body = self.block()?;
                Ok(Expr::Fn { args, body: Box::new(body) })
            } else {
                Err(self.mk_error("Expected '(' or '{' after function arguments list to begin body"))
            }
        } else {
            Err(self.mk_error(format!("Unexpected token: {:?}", next.ty)))
        }
    }
}
