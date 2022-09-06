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
    Int(i64), Float(f64), ImFloat(f64), String(Rc<str>),
    Ident(Rc<str>),

    Plus, Minus, Star, Slash, Percent, DoubleSlash, Caret,
    Bang, Amper, Pipe, DoubleAmper, DoublePipe,

    Equal, PlusEqual, MinusEqual, StarEqual, SlashEqual, PercentEqual, DoubleSlashEqual, CaretEqual,
    DoubleEqual, BangEqual, Greater, GreaterEqual, Less, LessEqual,

    Arrow, PipeColon, PipePoint, PipeQuestion, PipeAmper,

    Comma, Semicolon, 

    LParen, RParen, LBrack, RBrack, LBrace, RBrace,

    True, False, Nil, If, Elif, Else, For, While, Let, Return
}

impl TokenType {
    pub fn get_op_type(&self) -> Option<OpType> {
        match self {
            Self::Plus | Self::Minus => Some(OpType::Additive),

            Self::Star | Self::Slash | Self::DoubleSlash 
            | Self::Percent => Some(OpType::Multiplicative),

            Self::Caret => Some(OpType::Exponential),

            Self::PipeColon | Self::PipeAmper | Self::PipePoint 
            | Self::PipeQuestion => Some(OpType::Pipeline),

            Self::Equal | Self::PlusEqual | Self::MinusEqual
            | Self::StarEqual | Self::SlashEqual | Self::DoubleSlashEqual
            | Self::CaretEqual | Self::PercentEqual => Some(OpType::Assignment),

            _ => None
        }
    }
}

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum OpType {
    Assignment, Pipeline, Additive, Multiplicative, Exponential
}

impl OpType {
    pub fn is_right_associative(&self) -> bool {
        matches!(self, OpType::Exponential | OpType::Assignment)
    }
}