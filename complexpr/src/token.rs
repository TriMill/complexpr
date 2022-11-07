use std::{fmt, rc::Rc};

use crate::Position;


#[derive(Clone)]
pub struct Token { 
    pub ty: TokenType,
    pub text: String,
    pub pos: Position
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} @ {},{}", self.ty, self.pos.line, self.pos.col)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Int(i64), Float(f64), ImFloat(f64), String(Rc<str>), Char(char),
    Ident(Rc<str>),

    Plus, Minus, Star, Slash, Percent, DoubleSlash, Caret,
    Bang, DoubleAmper, DoublePipe,
    Tilde, Amper, Pipe, 

    Dot, DoubleDot,

    Equal, PlusEqual, MinusEqual, StarEqual, SlashEqual, PercentEqual, DoubleSlashEqual, CaretEqual,
    DoubleEqual, BangEqual, Greater, GreaterEqual, Less, LessEqual, Spaceship,

    PipeColon, PipePoint, PipeQuestion, PipeAmper,
    PipeSlash, PipeDoubleSlash,
    Backslash,

    Comma, Semicolon, Colon, 

    LParen, RParen, LBrack, RBrack, LBrace, RBrace,

    True, False, Nil,
    If, Elif, Else, For, While, 
    Fn, Let, Struct,
    Break, Continue, Return
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Int(n) => write!(f, "integer literal {}", n),
            TokenType::Float(x) => write!(f, "float literal {}", x),
            TokenType::ImFloat(x) => write!(f, "imaginary float literal {}i", x),
            TokenType::String(s) => write!(f, "string literal {}", crate::value::Value::from(s.as_ref())),
            TokenType::Char(c) => write!(f, "character literal {}", crate::value::Value::from(*c)),
            TokenType::Ident(n) => write!(f, "identifier '{}'", n),
            TokenType::Plus => f.write_str("operator +"),
            TokenType::Minus => f.write_str("operator -"),
            TokenType::Star => f.write_str("operator *"),
            TokenType::Slash => f.write_str("operator /"),
            TokenType::Percent => f.write_str("operator %"),
            TokenType::DoubleSlash => f.write_str("operator //"),
            TokenType::Caret => f.write_str("operator ^"),
            TokenType::Bang => f.write_str("operator !"),
            TokenType::DoubleAmper => f.write_str("operator &&"),
            TokenType::DoublePipe => f.write_str("operator ||"),
            TokenType::Tilde => f.write_str("operator ~"),
            TokenType::Amper => f.write_str("operator &"),
            TokenType::Pipe => f.write_str("operator |"),
            TokenType::Dot => f.write_str("operator ."),
            TokenType::DoubleDot => f.write_str("operator .."),
            TokenType::Equal => f.write_str("operator ="),
            TokenType::PlusEqual => f.write_str("operator +="),
            TokenType::MinusEqual => f.write_str("operator -="),
            TokenType::StarEqual => f.write_str("operator *="),
            TokenType::SlashEqual => f.write_str("operator /="),
            TokenType::PercentEqual => f.write_str("operator %="),
            TokenType::DoubleSlashEqual => f.write_str("operator //="),
            TokenType::CaretEqual => f.write_str("operator ^="),
            TokenType::DoubleEqual => f.write_str("operator =="),
            TokenType::BangEqual => f.write_str("operator !="),
            TokenType::Greater => f.write_str("operator >"),
            TokenType::GreaterEqual => f.write_str("operator >="),
            TokenType::Less => f.write_str("operator <"),
            TokenType::LessEqual => f.write_str("operator <="),
            TokenType::Spaceship => f.write_str("operator <=>"),
            TokenType::PipeColon => f.write_str("operator |:"),
            TokenType::PipePoint => f.write_str("operator |>"),
            TokenType::PipeQuestion => f.write_str("operator |?"),
            TokenType::PipeAmper => f.write_str("operator |&"),
            TokenType::PipeSlash => f.write_str("operator |/"),
            TokenType::PipeDoubleSlash => f.write_str("operator |//"),
            TokenType::Backslash => f.write_str("backslash"),
            TokenType::Comma => f.write_str("comma"),
            TokenType::Semicolon => f.write_str("semicolon"),
            TokenType::Colon => f.write_str("colon"),
            TokenType::LParen => f.write_str("left paren"),
            TokenType::RParen => f.write_str("right paren"),
            TokenType::LBrack => f.write_str("left bracket"),
            TokenType::RBrack => f.write_str("right bracket"),
            TokenType::LBrace => f.write_str("left brace"),
            TokenType::RBrace => f.write_str("right brace"),
            TokenType::True => f.write_str("literal 'true'"),
            TokenType::False => f.write_str("literal 'false'"),
            TokenType::Nil => f.write_str("literal 'nil'"),
            TokenType::If => f.write_str("keyword 'if'"),
            TokenType::Elif => f.write_str("keyword 'elif'"),
            TokenType::Else => f.write_str("keyword 'else'"),
            TokenType::For => f.write_str("keyword 'for'"),
            TokenType::While => f.write_str("keyword 'while'"),
            TokenType::Fn => f.write_str("keyword 'fn'"),
            TokenType::Let => f.write_str("keyword 'let'"),
            TokenType::Struct => f.write_str("keyword 'struct'"),
            TokenType::Break => f.write_str("keyword 'break'"),
            TokenType::Continue => f.write_str("keyword 'continue'"),
            TokenType::Return => f.write_str("keyword 'return'"),
        }
    }
}

impl TokenType {
    pub fn get_op_type(&self) -> Option<OpType> {
        match self {
            Self::Plus | Self::Minus => Some(OpType::Additive),

            Self::Star | Self::Slash | Self::DoubleSlash 
            | Self::Percent => Some(OpType::Multiplicative),

            Self::Caret => Some(OpType::Exponential),

            Self::PipeColon | Self::PipeAmper | Self::PipePoint | Self::PipeQuestion 
            | Self::PipeSlash | Self::PipeDoubleSlash  => Some(OpType::Pipeline),

            Self::Greater | Self::GreaterEqual | Self::Less | Self::LessEqual
            | Self::DoubleEqual | Self::BangEqual | Self::Spaceship => Some(OpType::Comparison),

            Self::Equal | Self::PlusEqual | Self::MinusEqual
            | Self::StarEqual | Self::SlashEqual | Self::DoubleSlashEqual
            | Self::CaretEqual | Self::PercentEqual => Some(OpType::Assignment),

            Self::Amper => Some(OpType::BitwiseAnd),
            Self::Pipe => Some(OpType::BitwiseOr),

            Self::DoubleAmper => Some(OpType::LogicalAnd),
            Self::DoublePipe => Some(OpType::LogicalOr),

            _ => None
        }
    }

    pub fn as_ident(self) -> Option<Rc<str>> {
        match self {
            Self::Ident(s) => Some(s),
            _ => None
        }
    }

    pub fn is_infix_op(&self) -> bool {
        matches!(self.get_op_type(), Some(
            OpType::Additive
            | OpType::Multiplicative
            | OpType::Exponential
            | OpType::Comparison
            | OpType::BitwiseAnd
            | OpType::BitwiseOr
            // TODO | OpType::Pipeline
        ))
    }
}

#[derive(Clone,Copy,Debug,PartialEq,Eq)]
pub enum OpType {
    Assignment, Comparison, Pipeline, 
    Additive, Multiplicative, Exponential, 
    LogicalAnd, LogicalOr,
    BitwiseAnd, BitwiseOr,
}

impl OpType {
    pub fn is_right_associative(&self) -> bool {
        matches!(self, OpType::Exponential | OpType::Assignment)
    }
}
