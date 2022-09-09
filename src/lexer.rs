use std::rc::Rc;

use crate::{ParserError, Position, token::{Token, TokenType}};

pub struct Lexer {
    // name of file being lexed
    filename: Option<Rc<str>>,
    line: usize,
    col: usize,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    code: Vec<char>
}

impl Lexer {
    pub fn new(code: &str, filename: Option<String>) -> Self {
        Self { filename: filename.map(|s| Rc::from(s)), line: 1, col: 1, tokens: vec![], start: 0, current: 0, code: code.chars().collect() }
    }

    pub fn into_tokens(self) -> Vec<Token> {
        self.tokens
    }

    fn next(&mut self) -> char {
        let c = self.code[self.current];
        self.advance(c == '\n');
        c
    }

    fn expect(&mut self, chars: &[char]) -> Option<char> {
        if self.at_end() { return None }
        for c in chars {
            if self.code[self.current] == *c {
                self.advance(*c == '\n');
                return Some(*c)
            }
        }
        None
    }

    fn at_end(&self) -> bool {
        self.current >= self.code.len()
    }

    fn peek(&self) -> char {
        self.code[self.current]
    }

    fn add_token<S>(&mut self, ty: TokenType, text: S) where S: Into<String> {
        self.tokens.push(Token { 
            ty, 
            text: text.into(),
            pos: Position {
                file: self.filename.clone(),
                pos: self.start, 
                line: self.line, 
                col: self.col - (self.current - self.start) 
            }
        });
    }

    fn mk_error<S>(&self, msg: S) -> ParserError where S: Into<String> {
        ParserError { 
            pos: Position { file: self.filename.clone(), pos: self.start, line: self.line, col: self.col}, 
            message: msg.into() 
        }
    }
    
    fn collect_literal(&self) -> String {
        self.code[self.start..self.current].iter().collect::<String>()       
    }

    fn advance(&mut self, newline: bool) {
        if newline {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        self.current += 1;
    }

    pub fn lex(&mut self) -> Result<(), ParserError> {
        while !self.at_end() {
            self.start = self.current;
            match self.next() {
                '+' => match self.expect(&['=']) {
                    Some('=') => self.add_token(TokenType::PlusEqual, "+="),
                    _ => self.add_token(TokenType::Plus, "+"),
                },
                '-' => match self.expect(&['=', '>']) {
                    Some('=') => self.add_token(TokenType::MinusEqual, "-="),
                    Some('>') => self.add_token(TokenType::Arrow, "->"),
                    _ => self.add_token(TokenType::Minus, "-"),
                },
                '*' => match self.expect(&['=']) {
                    Some('=') => self.add_token(TokenType::StarEqual, "*="),
                    _ => self.add_token(TokenType::Star, "*"),
                },
                '%' => match self.expect(&['=']) {
                    Some('=') => self.add_token(TokenType::PercentEqual, "%="),
                    _ => self.add_token(TokenType::Percent, "%"),
                },
                '/' => match self.expect(&['=', '/']) {
                    Some('=') => self.add_token(TokenType::SlashEqual, "/="),
                    Some('/') => match self.expect(&['=']) {
                        Some('=') => self.add_token(TokenType::DoubleSlashEqual, "//="),
                        _ => self.add_token(TokenType::DoubleSlash, "//")
                    }
                    _ => self.add_token(TokenType::Slash, "/"),
                },
                '^' => match self.expect(&['=']) {
                    Some('=') => self.add_token(TokenType::CaretEqual, "^="),
                    _ => self.add_token(TokenType::Caret, "^"),
                },
                '=' => match self.expect(&['=']) {
                    Some('=') => self.add_token(TokenType::DoubleEqual, "=="),
                    _ => self.add_token(TokenType::Equal, "=")
                },
                '!' => match self.expect(&['=']) {
                    Some('=') => self.add_token(TokenType::BangEqual, "!="),
                    _ => self.add_token(TokenType::Bang, "!")
                },
                '>' => match self.expect(&['=']) {
                    Some('=') => self.add_token(TokenType::GreaterEqual, ">="),
                    _ => self.add_token(TokenType::Greater, ">")
                },
                '<' => match self.expect(&['=']) {
                    Some('=') => match self.expect(&['>']) {
                        Some('>') => self.add_token(TokenType::Spaceship, "<=>"),
                        _ => self.add_token(TokenType::LessEqual, "<="),
                    }
                    _ => self.add_token(TokenType::Less, "<")
                },
                '&' => match self.expect(&['&']) {
                    Some('&') => self.add_token(TokenType::DoubleAmper, "&&"),
                    _ => self.add_token(TokenType::Amper, "&"),
                },
                '|' => match self.expect(&['|', ':', '?', '>', '&']) {
                    Some('|') => self.add_token(TokenType::DoublePipe, "||"),
                    Some(':') => self.add_token(TokenType::PipeColon, "|:"),
                    Some('?') => self.add_token(TokenType::PipeQuestion, "|?"),
                    Some('>') => self.add_token(TokenType::PipePoint, "|>"),
                    Some('&') => self.add_token(TokenType::PipeAmper, "|&"),
                    _ => self.add_token(TokenType::Pipe, "|"),
                },
                ',' => self.add_token(TokenType::Comma, ","),
                ';' => self.add_token(TokenType::Semicolon, ";"),
                ':' => self.add_token(TokenType::Colon, ":"),
                '(' => self.add_token(TokenType::LParen, "("),
                ')' => self.add_token(TokenType::RParen, ")"),
                '[' => self.add_token(TokenType::LBrack, "["),
                ']' => self.add_token(TokenType::RBrack, "]"),
                '{' => self.add_token(TokenType::LBrace, "{"),
                '}' => self.add_token(TokenType::RBrace, "}"),
                '#' => {
                    while !self.at_end() && self.peek() != '\n' {
                        self.advance(false);
                    }
                    self.advance(true);
                },
                '"' => self.string()?,
                '\'' => self.char()?,
                ' ' | '\t' | '\r' | '\n' => (),
                '0'..='9' => self.number()?,
                'a'..='z' | 'A'..='Z' | '_' => self.ident()?,
                c => return Err(self.mk_error(format!("Unexpected character: {}", c)))
            }
        }
        Ok(())
    }

    fn char(&mut self) -> Result<(), ParserError> {
        if self.at_end() { return Err(self.mk_error("Unexpected EOF in character literal")) }
        let mut c = self.next();
        if c == '\'' {
            return Err(self.mk_error("Empty character literal"))
        } else if c == '\\' {
            if self.at_end() { return Err(self.mk_error("Unexpected EOF in character literal")) }
            // TODO Escapes
        }
        if self.at_end() { return Err(self.mk_error("Unexpected EOF in character literal")) }
        self.expect(&['\'']).ok_or(self.mk_error("Expected ' to terminate character literal"))?;
        self.add_token(TokenType::Char(c), self.collect_literal());
        Ok(())
    }

    fn string(&mut self) -> Result<(), ParserError> {
        let mut s = String::new();
        while !self.at_end() && self.peek() != '"' {
            if self.peek() == '\\' {
                self.advance(false);
                if self.at_end() { 
                    return Err(self.mk_error("Unexpected EOF in string literal")) 
                }
                // TODO more escape codes! \xHH, \u{HH..} or maybe \uhhhh \Uhhhhhhhh
                match self.peek() {
                    '0' => s.push('\0'),
                    'n' => s.push('\n'),
                    't' => s.push('\t'),
                    'r' => s.push('\r'),
                    'e' => s.push('\x1b'),
                    '\\' => s.push('\\'),
                    '"' => s.push('"'),
                    '\n' => (),
                    c => return Err(self.mk_error(format!("Unknown escape code \\{}", c)))
                }
                self.advance(self.peek() == '\n')
            } else {
                s.push(self.peek());
                self.advance(self.peek() == '\n');
            }
        }
        if self.at_end() {
            return Err(self.mk_error("Unexpected EOF in string literal"))
        }
        self.advance(false);
        self.add_token(TokenType::String(Rc::from(s)), self.collect_literal());
        Ok(())
    }

    fn number(&mut self) -> Result<(), ParserError> {
        let mut has_dot = false;
        while !self.at_end() && (self.peek().is_numeric() || self.peek() == '.') {
            if self.peek() == '.' {
                if has_dot {
                    break;
                } else {
                    has_dot = true;
                }
            } 
            self.advance(false);
        }
        let is_imag = !self.at_end() && self.peek() == 'i';
        if is_imag { self.advance(false); }
        let literal = self.collect_literal();
        if is_imag {
            match literal[..literal.len()-1].parse::<f64>() {
                Ok(num) => self.add_token(TokenType::ImFloat(num), literal),
                Err(e) => return Err(self.mk_error(format!("Error parsing float: {}", e)))
            }
        } else if has_dot {
            match literal.parse::<f64>() {
                Ok(num) => self.add_token(TokenType::Float(num), literal),
                Err(e) => return Err(self.mk_error(format!("Error parsing float: {}", e)))
            }
        } else {
            match literal.parse::<i64>() {
                Ok(num) => self.add_token(TokenType::Int(num), literal),
                Err(e) => return Err(self.mk_error(format!("Error parsing float: {}", e)))
            }
        }
        Ok(())
    }

    fn ident(&mut self) -> Result<(), ParserError> {
        while !self.at_end() && (self.peek().is_ascii_alphanumeric() || self.peek() == '_') {
            self.advance(false);
        }
        let literal = self.collect_literal();
        let token_ty = match literal.as_ref() {
            "true" => TokenType::True,
            "false" => TokenType::False,
            "nil" => TokenType::Nil,
            "if" => TokenType::If,
            "elif" => TokenType::Elif,
            "else" => TokenType::Else,
            "while" => TokenType::While,
            "for" => TokenType::For,
            "let" => TokenType::Let,
            "return" => TokenType::Return,
            s => TokenType::Ident(Rc::from(s))
        };
        self.add_token(token_ty, literal);
        Ok(())
    }
}
