use std::{rc::Rc, fmt, cmp::Ordering, cell::RefCell, hash::Hash};

use crate::{RuntimeError, eval::{eval_stmt, Unwind, eval_expr}, expr::Stmt, env::{EnvRef, Environment}};

use super::Value;

pub type ClosureData = Rc<RefCell<Vec<Value>>>;
pub type ClosureIterData = Rc<RefCell<Vec<CIterator>>>;

#[derive(Clone)]
pub enum Func {
    Func {
        name: Option<Rc<str>>,
        args: Vec<Rc<str>>,
        env: EnvRef,
        func: Box<Stmt>,
    },
    Builtin {
        name: Rc<str>,
        func: fn(Vec<Value>) -> Result<Value, RuntimeError>,
        arg_count: usize,
    },
    BuiltinClosure { 
        func: fn(Vec<Value>, ClosureData, ClosureIterData) -> Result<Value, RuntimeError>,
        data: ClosureData,
        iter_data: ClosureIterData,
        arg_count: usize,
    },
    Partial {
        inner: Box<Func>,
        filled_args: Vec<Value>,
    }
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Func { name, args, .. } 
                => f.debug_struct("Func::Func")
                    .field("name", name)
                    .field("args", args)
                    .finish_non_exhaustive(),
            Self::Builtin { name, arg_count, .. } 
                => f.debug_struct("Func::Builtin") 
                    .field("name", name)
                    .field("arg_count", arg_count)
                    .finish_non_exhaustive(),
            Self::BuiltinClosure { arg_count, data, .. } 
                => f.debug_struct("Func::BuiltinClosure") 
                    .field("arg_count", arg_count)
                    .field("data", data)
                    .finish_non_exhaustive(),
            Self::Partial { inner, filled_args } 
                => f.debug_struct("Func::Partial") 
                    .field("inner", inner)
                    .field("filled_args", filled_args)
                    .finish(),
        }
    }
    
}

impl Func {
    pub fn arg_count(&self) -> usize {
        match self {
            Self::Builtin { arg_count, .. } => *arg_count,
            Self::BuiltinClosure { arg_count, .. } => *arg_count,
            Self::Func { args, .. } => args.len(),
            Self::Partial { inner, filled_args } => inner.arg_count() - filled_args.len(),
        }
    }

    pub fn name(&self) -> Option<Rc<str>> {
        match self {
            Self::Builtin { name, .. } => Some(name.clone()),
            Self::BuiltinClosure { .. } => None,
            Self::Func { name, .. } => name.clone(),
            Self::Partial { inner, .. } => inner.name()
        }
    }

    pub fn call(&self, mut arg_values: Vec<Value>) -> Result<Value, RuntimeError> {
        match arg_values.len().cmp(&self.arg_count()) {
            Ordering::Equal => match self {
                Self::Builtin { func, .. } 
                    => func(arg_values),
                Self::BuiltinClosure { func, data, iter_data, .. } 
                    => func(arg_values, data.clone(), iter_data.clone()),
                Self::Func { args, func, env, .. } => {
                    let mut env = Environment::extend(env.clone());
                    for (k, v) in args.iter().zip(arg_values.iter()) {
                        env.declare(k.clone(), v.clone());
                    }
                    match func.as_ref() {
                        Stmt::Expr { expr } => eval_expr(expr, env.wrap()),
                        stmt => match eval_stmt(stmt, env.wrap()) {
                            Ok(()) => Ok(Value::Nil),
                            Err(Unwind::Return{ value, .. }) => Ok(value),
                            Err(e) => Err(e.as_error()),
                        }

                    }
                },
                Self::Partial { inner, filled_args } => {
                    let mut filled_args = filled_args.clone();
                    filled_args.append(&mut arg_values);
                    inner.call(filled_args)
                }
            },
            Ordering::Less if arg_values.is_empty() => Err(RuntimeError::new_no_pos(
                format!("Cannot call this function with zero arguments: expected {}", self.arg_count())
            )),
            Ordering::Less => match self {
                Self::Partial { inner, filled_args } => {
                    let mut args = filled_args.clone();
                    args.append(&mut arg_values);
                    Ok(Value::Func(Func::Partial { inner: inner.clone(), filled_args: args }))
                }
                f => Ok(Value::Func(Func::Partial { inner: Box::new(f.clone()), filled_args: arg_values }))
            },
            Ordering::Greater => Err(RuntimeError::new_no_pos(
                format!("Too many arguments for function: expected {}, got {}", self.arg_count(), arg_values.len())
            ))
        }
    }
}

impl Hash for Func {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Builtin { name, arg_count, func } => {
                name.hash(state);
                arg_count.hash(state);
                func.hash(state);
            },
            Self::Func { name, args, .. } => {
                name.hash(state);
                args.hash(state);
            },
            Self::BuiltinClosure { arg_count, data, .. } => {
                arg_count.hash(state);
                data.borrow().hash(state);
            },
            Self::Partial { inner, filled_args } => {
                filled_args.hash(state);
                inner.hash(state);
            }
        }
    }
}

pub enum CIterator {
    // precondition: value must be len()able
    Indexable{ value: Value, idx: i64 },
    Func(Func)
}

impl Iterator for CIterator {
    type Item = Result<Value, RuntimeError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Indexable{ value, ref mut idx } => {
                if *idx >= value.len().unwrap() as i64 {
                    None
                } else {
                    let result = value.index(&Value::Int(*idx)).unwrap();
                    *idx += 1;
                    Some(Ok(result))
                }
            },
            Self::Func(f) => match f.call(vec![]) {
                Ok(Value::Nil) => None,
                x => Some(x)
            },
        }
    }
}
