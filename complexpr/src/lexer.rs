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
        Self { filename: filename.map(Rc::from), line: 1, col: 1, tokens: vec![], start: 0, current: 0, code: code.chars().collect() }
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

    fn err_on_eof(&self, msg: &str) -> Result<(), ParserError> {
        if self.at_end() {
            Err(self.mk_error(msg))
        } else {
            Ok(())
        }
    }

    fn peek(&self) -> char {
        self.code[self.current]
    }

    fn peek_ahead(&self, n: usize) -> Option<char> {
        self.code.get(self.current + n).cloned()
    }

    fn add_token<S>(&mut self, ty: TokenType, text: S) where S: Into<String> {
        self.tokens.push(Token { 
            ty, 
            text: text.into(),
            pos: Position {
                file: self.filename.clone(),
                pos: self.start, 
                line: self.line, 
                col: if self.col < (self.current - self.start) {
                    0
                } else {
                    self.col - (self.current - self.start) 
                }
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

    fn parse_escape(&mut self, eof_msg: &str) -> Result<Option<char>, ParserError> {
        self.err_on_eof(eof_msg)?;
        Ok(Some(match self.peek() {
            '0' => '\0',
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            'e' => '\x1b',
            '\\' => '\\',
            '"' => '"',
            '\'' => '\'',
            '\n' => { return Ok(None) },
            'x' => {
                self.advance(false);
                self.err_on_eof(eof_msg)?;
                let c1 = self.peek();
                self.advance(c1 == '\n');
                self.err_on_eof(eof_msg)?;
                let c2 = self.peek();
                let code = format!("{}{}", c1, c2);
                let code = u32::from_str_radix(&code, 16).map_err(|_| self.mk_error("Invalid hex code in escape"))?;
                char::from_u32(code).unwrap()
            },
            'u' => {
                self.advance(false);
                self.err_on_eof(eof_msg)?;
                if self.peek() != '{' {
                    return Err(self.mk_error("Expected { to begin unicode escape"))
                }
                self.advance(false);
                self.err_on_eof(eof_msg)?;
                let mut esc_str = String::new();
                while self.peek().is_ascii_hexdigit() {
                    esc_str.push(self.peek());
                    self.advance(false);
                    self.err_on_eof(eof_msg)?;
                }
                if self.peek() != '}' {
                    return Err(self.mk_error("Expected } to terminate unicode escape"))
                }
                let code = u32::from_str_radix(&esc_str, 16).map_err(|_| self.mk_error("Invalid hex code in escape"))?;
                char::from_u32(code).ok_or_else(|| self.mk_error("Invalid unicode character"))?
            },
            c => return Err(self.mk_error(format!("Unknown escape code \\{}", c)))
        }))
    }

    pub fn lex(&mut self) -> Result<(), ParserError> {
        while !self.at_end() {
            self.start = self.current;
            match self.next() {
                '.' => match self.expect(&['.']) {
                    Some('.') => self.add_token(TokenType::DoubleDot, ".."),
                    _ => self.add_token(TokenType::Dot, ".")
                },
                '+' => match self.expect(&['=']) {
                    Some('=') => self.add_token(TokenType::PlusEqual, "+="),
                    _ => self.add_token(TokenType::Plus, "+"),
                },
                '-' => match self.expect(&['=']) {
                    Some('=') => self.add_token(TokenType::MinusEqual, "-="),
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
                '|' => match self.expect(&['|', ':', '?', '>', '&', '/', '\\']) {
                    Some('|') => self.add_token(TokenType::DoublePipe, "||"),
                    Some(':') => self.add_token(TokenType::PipeColon, "|:"),
                    Some('?') => self.add_token(TokenType::PipeQuestion, "|?"),
                    Some('>') => self.add_token(TokenType::PipePoint, "|>"),
                    Some('&') => self.add_token(TokenType::PipeAmper, "|&"),
                    Some('/') => match self.expect(&['/']) {
                        Some('/') => self.add_token(TokenType::PipeDoubleSlash, "|//"),
                        _ => self.add_token(TokenType::PipeSlash, "|/")
                    },
                    Some('\\') => match self.expect(&['\\']) {
                        Some('\\') => self.add_token(TokenType::PipeDoubleBackslash, "|\\\\"),
                        _ => self.add_token(TokenType::PipeBackslash, "|\\")
                    },
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
                '#' => match self.expect(&['{']) {
                    Some(_) => {
                        while !self.at_end() {
                            if self.peek() == '}' {
                                self.advance(false);
                                if self.at_end() { break }
                                if self.peek() == '#' {
                                    break
                                }
                            }
                            self.advance(false);
                        }
                        if self.at_end() {
                            return Err(self.mk_error("Unexpected EOF in block comment"))
                        }
                        self.advance(true);
                    },
                    None => {
                        while !self.at_end() && self.peek() != '\n' {
                            self.advance(false);
                        }
                        self.advance(true);
                    },
                }
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
        const EOF_MSG: &str = "Unexpected EOF in character literal";
        self.err_on_eof(EOF_MSG)?;
        let mut c = self.peek();
        if c == '\'' {
            return Err(self.mk_error("Empty character literal"))
        } else if c == '\\' {
            self.advance(self.peek() == '\n');
            if let Some(nc) = self.parse_escape(EOF_MSG)? {
                c = nc
            } else {
                return Err(self.mk_error("Character literal cannot contain escaped newline"));
            }
        }
        self.err_on_eof(EOF_MSG)?;
        self.advance(self.peek() == '\n');
        self.expect(&['\'']).ok_or_else(|| self.mk_error("Expected ' to terminate character literal"))?;
        self.add_token(TokenType::Char(c), self.collect_literal());
        Ok(())
    }

    fn string(&mut self) -> Result<(), ParserError> {
        const EOF_MSG: &str = "Unexpected EOF in string literal";
        let mut s = String::new();
        while !self.at_end() && self.peek() != '"' {
            if self.peek() == '\\' {
                self.advance(false);
                if let Some(c) = self.parse_escape(EOF_MSG)? {
                    s.push(c);
                }
                self.advance(self.peek() == '\n')
            } else {
                s.push(self.peek());
                self.advance(self.peek() == '\n');
            }
        }
        self.err_on_eof(EOF_MSG)?;
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
                    if self.peek_ahead(1) == Some('.') {
                        break;
                    }
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
            "fn" => TokenType::Fn,
            "let" => TokenType::Let,
            "struct" => TokenType::Struct,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "return" => TokenType::Return,
            s => TokenType::Ident(Rc::from(s))
        };
        self.add_token(token_ty, literal);
        Ok(())
    }
}
