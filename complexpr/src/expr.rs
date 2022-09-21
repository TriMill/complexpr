use std::fmt;

use crate::{token::{Token, OpType}, Position, value::Type};

#[derive(Clone)]
pub enum Stmt {
    Expr { expr: Expr },
    Let { lhs: Token, rhs: Option<Expr> },
    Block { stmts: Vec<Stmt> },
    If { if_clauses: Vec<(Expr, Stmt)>, else_clause: Option<Box<Stmt>> },
    For { var: Token, expr: Expr, stmt: Box<Stmt>, iter_pos: Position },
    While { expr: Expr, stmt: Box<Stmt> },
    Break { pos: Position },
    Continue { pos: Position },
    Return { pos: Position, expr: Expr },
    Fn { name: Token, args: Vec<Token>, body: Box<Stmt> },
    Struct { name: Token, ty: Type, items: Vec<Token> }
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Expr { expr } => write!(f, "{:?}", expr),
            Self::Let { lhs, rhs } => write!(f, "(let {:?} = {:?})", lhs, rhs),
            Self::Block { stmts } => write!(f, "(block {:?})", stmts),
            Self::If { if_clauses, else_clause } => write!(f, "(if {:?} else {:?})", if_clauses, else_clause),
            Self::For { var, expr, stmt, .. } => write!(f, "(for {:?} : {:?} do {:?})", var, expr, stmt),
            Self::While { expr, stmt } => write!(f, "(while {:?} do {:?})", expr, stmt),
            Self::Break { .. } => write!(f, "(break)"),
            Self::Continue { .. } => write!(f, "(continue)"),
            Self::Return { expr, .. } => write!(f, "(return {:?})", expr),
            Self::Fn { name, args, body } => write!(f, "(fn {:?} {:?} {:?})", name, args, body),
            Self::Struct { name, ty, items } => write!(f, "(struct {:?} #{:?} {:?})", name, ty.id, items),
        }
    }
}

#[derive(Clone)]
pub enum Expr {
    Binary { lhs: Box<Expr>, rhs: Box<Expr>, op: Token },
    Ternary { arg1: Box<Expr>, arg2: Box<Expr>, arg3: Box<Expr>, op: Token },
    Unary { arg: Box<Expr>, op: Token },
    Range { start: Box<Expr>, end: Option<Box<Expr>>, step: Option<Box<Expr>>, incl: bool },
    Ident { value: Token },
    Literal { value: Token },
    List { items: Vec<Expr> },
    Map { items: Vec<(Expr,Expr)> },
    FuncCall { func: Box<Expr>, args: Vec<Expr>, pos: Position },
    Index { lhs: Box<Expr>, index: Box<Expr>, pos: Position },
    Fn { args: Vec<Token>, body: Box<Stmt> },
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Binary { lhs: left, rhs: right, op} => write!(f, "({:?} {:?} {:?})", op, left, right),
            Self::Ternary { arg1, arg2, arg3, op} => write!(f, "({:?} {:?} {:?} {:?})", op, arg1, arg2, arg3),
            Self::Unary { arg, op } => write!(f, "({:?} {:?})", op, arg),
            Self::Range { start, end, step, incl } => write!(f, "(range {:?}..{:?} step {:?} incl {:?})", start, end, step, incl),
            Self::Ident { value } => write!(f, "(ident {:?})", value),
            Self::Literal { value } => write!(f, "(lit {:?})", value),
            Self::List { items } => write!(f, "(list {:?})", items),
            Self::Map { items } => write!(f, "(map {:?})", items),
            Self::FuncCall { func, args, .. } => write!(f, "(call {:?} {:?})", func, args),
            Self::Index { lhs, index, .. } => write!(f, "(index {:?} {:?})", lhs, index),
            Self::Fn { args, body } => write!(f, "(fn {:?} {:?})", args, body)
        }
    }
}

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        matches!(self, Expr::Ident{..} | Expr::Index{..})
    }

    pub fn is_assignment(&self) -> bool {
        matches!(self, Expr::Binary{op, ..} if op.ty.get_op_type() == Some(OpType::Assignment))
    }
}
