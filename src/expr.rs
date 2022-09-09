use std::fmt;

use crate::{token::{Token, OpType}, Position};

pub enum Stmt {
    Expr { expr: Expr },
    Let { lhs: Token, rhs: Option<Expr> },
    Block { stmts: Vec<Stmt> },
    If { if_clauses: Vec<(Expr, Stmt)>, else_clause: Option<Box<Stmt>> },
    For { var: Token, expr: Expr, stmt: Box<Stmt> },
    While { expr: Expr, stmt: Box<Stmt> },
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Expr { expr } => write!(f, "{:?}", expr),
            Self::Let { lhs, rhs } => write!(f, "(let {:?} = {:?})", lhs, rhs),
            _ => todo!()
        }
    }
}

pub enum Expr {
    Binary { lhs: Box<Expr>, rhs: Box<Expr>, op: Token },
    Unary { arg: Box<Expr>, op: Token },
    Ident { value: Token },
    Literal { value: Token },
    List { items: Vec<Expr> },
    FuncCall { func: Box<Expr>, args: Vec<Expr>, pos: Position }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Binary { lhs: left, rhs: right, op} => write!(f, "({:?} {:?} {:?})", op, left, right),
            Self::Unary { arg, op } => write!(f, "({:?} {:?})", op, arg),
            Self::Ident { value } => write!(f, "(ident {:?})", value),
            Self::Literal { value } => write!(f, "(lit {:?})", value),
            Self::List { items } => write!(f, "(list {:?})", items),
            Self::FuncCall { func, args, .. } => write!(f, "(call {:?} {:?})", func, args),
        }
    }
}

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        matches!(self, Expr::Ident{..})
    }

    pub fn is_assignment(&self) -> bool {
        matches!(self, Expr::Binary{op, ..} if op.ty.get_op_type() == Some(OpType::Assignment))
    }
}