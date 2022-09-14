use std::{rc::Rc, collections::HashMap, ops::*, fmt, cmp::Ordering, cell::RefCell};

use num_traits::{Zero, ToPrimitive};

use crate::{RuntimeError, Position, eval::{EnvRef, eval_stmt, Environment, Unwind}, expr::Stmt};

pub type Rational = num_rational::Ratio<i64>;
pub type Complex = num_complex::Complex64;

#[derive(Clone)]
pub enum Func {
    Func {
        name: Option<Rc<str>>,
        args: Vec<Rc<str>>,
        env: EnvRef,
        func: Stmt
    },
    Builtin {
        name: Rc<str>,
        func: fn(Vec<Value>) -> Result<Value, String>,
        arg_count: usize
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
        }
    }
    
}

impl Func {
    pub fn arg_count(&self) -> usize {
        match self {
            Self::Builtin { arg_count, .. } => *arg_count,
            Self::Func { args, .. } => args.len()
        }
    }

    pub fn call(&self, arg_values: Vec<Value>, pos: &Position) -> Result<Value, RuntimeError> {
        match arg_values.len().cmp(&self.arg_count()) {
            Ordering::Equal => match self {
                Self::Builtin { func, .. } 
                    => func(arg_values).map_err(|e| RuntimeError::new(e, pos.clone())),
                Self::Func { name, args, func, env } => {
                    let mut env = Environment::extend(env.clone());
                    for (k, v) in args.iter().zip(arg_values.iter()) {
                        env.declare(k.clone(), v.clone());
                    }
                    match eval_stmt(func, env.wrap()) {
                        Ok(()) => Ok(Value::Nil),
                        Err(Unwind::Return{ value, .. }) => Ok(value),
                        Err(e) => Err(e.as_error().exit_fn(name.clone(), pos.clone()))
                    }
                }
            }
            Ordering::Less => Err(RuntimeError::new(
                format!("Not enough arguments for function: expected {}, got {}", self.arg_count(), arg_values.len()), 
                pos.clone() 
            )),
            Ordering::Greater => Err(RuntimeError::new(
                format!("Too many arguments for function: expected {}, got {}", self.arg_count(), arg_values.len()), 
                pos.clone() 
            ))
        }
    }
}

pub enum EitherRteOrString {
    Rte(RuntimeError), String(String)
}

impl EitherRteOrString {
    pub fn to_rte(self, pos: &Position) -> RuntimeError {
        match self {
            Self::Rte(e) => e,
            Self::String(s) => RuntimeError::new(s, pos.clone())
        }
    }
}

impl From<String> for EitherRteOrString {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<RuntimeError> for EitherRteOrString {
    fn from(e: RuntimeError) -> Self {
        Self::Rte(e)
    }
}

impl Iterator for Func {
    type Item = Result<Value, EitherRteOrString>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Builtin { func, .. } => match (func)(vec![]) {
                Ok(Value::Nil) => None,
                r => Some(r.map_err(|e| e.into())),
            },
            Self::Func { func, env, .. } => {
                let env = Environment::extend(env.clone()).wrap();
                match eval_stmt(&func, env) {
                    Ok(_) => None,
                    Err(Unwind::Return{ value: Value::Nil, .. }) => None,
                    Err(Unwind::Return{ value, .. }) => Some(Ok(value)),
                    Err(e) => Some(Err(e.as_error().into()))
                }
            }
        }
    }
}
//
//#[derive(Clone)]
//pub struct BuiltinFunc {
//    pub name: Rc<str>,
//    pub func: fn(Vec<Value>) -> Result<Value, String>,
//    pub arg_count: usize
//}
//
//impl fmt::Debug for BuiltinFunc {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        f.debug_struct("BuiltinFn").field("name", &self.name).field("arg_count", &self.arg_count).finish()
//    }
//}
//

#[derive(Clone, Debug)]
pub struct Data {
    pub ty: usize,
    // TODO user-defined data types
}

#[derive(Clone, Debug)]
pub struct Type {
    pub name: Rc<str>,
    pub id: usize
}

#[derive(Clone, Debug)]
#[repr(u8)]
pub enum Value {
    Nil,
    Type(usize),
    Int(i64), Float(f64), Complex(Complex), Rational(Rational),
    Bool(bool), 
    Char(char),
    String(Rc<str>), 
    List(Rc<RefCell<Vec<Value>>>), Map(Rc<HashMap<Value,Value>>),
    Func(Func),
    Data(Data),
}

impl Value {
    pub fn truthy(&self) -> bool {
        use Value::*;
        match self {
            Bool(false) | Nil | Int(0) => false,
            Float(f) => *f != 0.0,
            Complex(z) => !z.is_zero(),
            Rational(r) => !r.is_zero(),
            String(s) => !s.len() == 0,
            List(l) => !l.borrow().len() == 0,
            Map(m) => !m.len() == 0,
            _ => true
        }
    }

    pub fn iter<'a>(&'a self, pos: &'a Position) -> Result<Box<dyn Iterator<Item=Result<Value, RuntimeError>> + '_>, String> {
        match self {
            Value::String(s) 
                => Ok(Box::new(s.chars()
                    .map(Value::Char).map(Ok))),
            Value::List(l) => Ok(Box::new(l.borrow().clone().into_iter().map(Ok))),
            Value::Func(f) => {
                if f.arg_count() == 0 {
                    Ok(Box::new(f.clone().map(|e| e.map_err(|e| e.to_rte(pos)))))
                } else {
                    Err("Only zero-argument functions can be used as iterators".into())
                }
            },
            v => Err(format!("{:?} is not iterable", v))
        }
    }

    pub fn call(&self, args: Vec<Value>, pos: &Position) -> Result<Value, RuntimeError> {
        if let Value::Func(f) = self {
            f.call(args, pos)
        } else {
            Err(RuntimeError::new("Cannot call", pos.clone()))           
        }
    }

    pub fn to_string(&self) -> Rc<str> {
        match self {
            Self::Nil => Rc::from("nil"),
            Self::Bool(b) => Rc::from(b.to_string()),
            Self::Int(n) => Rc::from(n.to_string()),
            Self::Float(f) => Rc::from(f.to_string()),
            Self::Rational(r) => Rc::from(r.to_string()),
            Self::Complex(z) => Rc::from(z.to_string()),
            Self::Char(c) => Rc::from(c.to_string()),
            Self::String(s) => s.clone(),
            Self::List(l) => Rc::from(format!("{:?}", l)), // TODO fix
            Self::Map(m) => Rc::from(format!("{:?}", m)), // TODO fix
            Self::Type(_) => todo!(),
            Self::Func(Func::Builtin { name, func, .. }) => Rc::from(format!("<builtin fn {} at {:?}>", name, *func as *const ())),
            Self::Func(Func::Func { name, .. }) => match name {
                Some(name) => Rc::from(format!("<fn {}>", name)),
                None => Rc::from("<anonymous fn>"),
            },
            Self::Data(_) => todo!(),
        }
    }
    
    pub fn repr(&self) -> Rc<str> {
        match self {
            Self::Nil => Rc::from("nil"),
            Self::Bool(b) => Rc::from(b.to_string()),
            Self::Int(n) => Rc::from(n.to_string()),
            Self::Float(f) => Rc::from(f.to_string()),
            Self::Rational(r) => Rc::from(r.to_string()),
            Self::Complex(z) => Rc::from(z.to_string()),
            Self::Char(c) => Rc::from(format!("'{}'", c)), // TODO escaping
            Self::String(s) => Rc::from(format!("\"{}\"", s)), // TODO escaping
            Self::List(l) => Rc::from(format!("{:?}", l.borrow())), // TODO fix
            Self::Map(m) => Rc::from(format!("{:?}", m)), // TODO fix
            Self::Type(_) => todo!(),
            Self::Func(Func::Builtin { name, func, .. }) => Rc::from(format!("<builtin fn {} at {:?}>", name, *func as *const ())),
            Self::Func(Func::Func { name, .. }) => match name {
                Some(name) => Rc::from(format!("<fn {}>", name)),
                None => Rc::from("<anonymous fn>"),
            },
            Self::Data(_) => todo!(),
        }
    }

    pub fn index(&self, idx: &Value) -> Result<Value, String> {
        match self {
            Self::String(s) => match idx {
                Value::Int(i) if *i >= 0 => s.chars().nth(*i as usize)
                    .ok_or_else(|| format!("String index {} out of bounds for length {}", i, s.chars().count()))
                    .map(Value::Char),
                Value::Int(i) => Err(format!("String index {} cannot be negative", i)),
                _ => Err(format!("Cannot index {:?} with {:?}", self, idx))
            },
            Self::List(l) => match idx {
                Value::Int(i) if *i >= 0 => l.borrow().get(*i as usize)
                    .ok_or_else(|| format!("List index {} out of bounds for length {}", i, l.borrow().len()))
                    .map(|v| v.clone()),
                Value::Int(i) => Err(format!("List index {} cannot be negative", i)),
                _ => Err(format!("Cannot index {:?} with {:?}", self, idx))
            }
            Self::Map(_) => todo!(),
            v => Err(format!("Cannot index into {:?}", v))
        }
    }

    pub fn assign_index(&self, idx: &Value, value: Value) -> Result<(), String> {
        match self {
            Self::String(_) => todo!("Can't mutate strings yet"),
            Self::List(l) => match idx {
                Value::Int(i) if *i >= 0 && (*i as usize) < l.borrow().len() => {
                    l.borrow_mut()[*i as usize] = value;
                    Ok(())
                }
                Value::Int(i) if *i >= 0 => Err(format!("List index {} out of bounds for length {}", i, l.borrow().len())),
                Value::Int(i) => Err(format!("List index {} cannot be negative", i)),
                _ => Err(format!("Cannot index {:?} with {:?}", self, idx))
            }
            Self::Map(_) => todo!(),
            v => Err(format!("Cannot index into {:?}", v))
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil)                 => true,
            (Self::Type(a), Self::Type(b))         => a == b,

            (Self::Int(a), Self::Int(b))           => a == b,
            (Self::Rational(a), Self::Int(b))      => *a == Rational::from(*b),
            (Self::Int(a), Self::Rational(b))      => Rational::from(*a) == *b,
            (Self::Rational(a), Self::Rational(b)) => a == b,
            (Self::Float(a), Self::Int(b))         => *a == *b as f64,
            (Self::Int(a), Self::Float(b))         => *a as f64 == *b,
            (Self::Float(a), Self::Rational(b))    => *a == b.to_f64().unwrap(),
            (Self::Rational(a), Self::Float(b))    => a.to_f64().unwrap() == *b,
            (Self::Float(a), Self::Float(b))       => a == b,

            (Self::Complex(a), Self::Int(b))       => *a == Complex::from(*b as f64),
            (Self::Int(a), Self::Complex(b))       => Complex::from(*a as f64) == *b,
            (Self::Complex(a), Self::Rational(b))  => *a == Complex::from(b.to_f64().unwrap()),
            (Self::Rational(a), Self::Complex(b))  => Complex::from(a.to_f64().unwrap()) == *b,
            (Self::Complex(a), Self::Float(b))     => *a == Complex::from(*b),
            (Self::Float(a), Self::Complex(b))     => Complex::from(*a) == *b,
            (Self::Complex(a), Self::Complex(b))   => a == b,

            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Char(a), Self::Char(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::List(a), Self::List(b)) => a == b,
            (Self::Map(_), Self::Map(_)) => todo!("Can't test maps for equality yet"),
            (Self::Func(f1), Self::Func(f2)) => match (f1, f2) {
                (
                    Func::Builtin { func: f1, arg_count: c1, .. },
                    Func::Builtin { func: f2, arg_count: c2, .. }
                ) => (*f1 as *const ()) == (*f2 as *const ()) && c1 == c2,
                 _ => false
            }
            (Self::Data(_), Self::Data(_)) => todo!("Can't compare data yet"),
            _ => false
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal)
        }
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => a.partial_cmp(b),
            (Self::Int(a), Self::Float(b)) => (*a as f64).partial_cmp(b),
            (Self::Float(a), Self::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Self::Float(a), Self::Float(b)) => a.partial_cmp(b),
            (Self::Int(a), Self::Rational(b)) => Rational::from(*a).partial_cmp(b),
            (Self::Rational(a), Self::Int(b)) => a.partial_cmp(&Rational::from(*b)),
            (Self::Float(a), Self::Rational(b)) => a.partial_cmp(&b.to_f64().unwrap()),
            (Self::Rational(a), Self::Float(b)) => a.to_f64().unwrap().partial_cmp(b),
            (Self::Rational(a), Self::Rational(b)) => a.partial_cmp(b),

            (Self::Char(a), Self::Char(b)) => a.partial_cmp(b),
            (Self::String(a), Self::String(b)) => a.partial_cmp(b),
            (Self::List(a), Self::List(b)) => a.partial_cmp(b),
            _ => None
        }
    }
}

macro_rules! value_from {
    ($variant:ident, $($kind:ty)*) => {
        $(
            impl From<$kind> for Value {
                fn from(x: $kind) -> Self {
                    Self::$variant(x.into())
                }
            }
        )*
    };
}

impl From<Vec<Value>> for Value {
    fn from(x: Vec<Value>) -> Self {
        Self::List(RefCell::new(x).into())
    }
}

value_from!(Int, u8 u16 u32 i8 i16 i32 i64);
value_from!(Float, f32 f64);
value_from!(Complex, Complex);
value_from!(Rational, Rational);
value_from!(Bool, bool);
value_from!(String, String Rc<str>);
value_from!(List, RefCell<Vec<Value>>);
value_from!(Char, char);
value_from!(Map, HashMap<Value,Value>);


macro_rules! impl_numeric_op {
    ($optrait:ty, $fnname:ident, { $($bonus:tt)* }) => {
        impl $optrait for &Value {
            type Output = Result<Value, String>;
            fn $fnname(self, other: Self) -> Self::Output {
                use Value::*;
                use num_traits::ToPrimitive;
                const RATIO_CAST_FAIL: &'static str = "Failed to cast Rational to Float";
                match (self, other) {
                    (Int(a),      Int(b))      => Ok(a.$fnname(b).into()),
                    (Rational(a), Int(b))      => Ok(a.$fnname(b).into()),
                    (Int(a),      Rational(b)) => Ok(self::Rational::from(*a).$fnname(b).into()),
                    (Rational(a), Rational(b)) => Ok(a.$fnname(b).into()),
                    (Float(a),    Int(b))      => Ok(a.$fnname(*b as f64).into()),
                    (Int(a),      Float(b))    => Ok((*a as f64).$fnname(b).into()),
                    (Float(a),    Rational(b)) => Ok(a.$fnname(b.to_f64().ok_or(RATIO_CAST_FAIL)?).into()),
                    (Rational(a), Float(b))    => Ok(a.to_f64().ok_or(RATIO_CAST_FAIL)?.$fnname(b).into()),
                    (Float(a),    Float(b))    => Ok(a.$fnname(b).into()),
                    (Int(a),      Complex(b))  => Ok(self::Complex::from(*a as f64).$fnname(b).into()),
                    (Complex(a),  Int(b))      => Ok(a.$fnname(self::Complex::from(*b as f64)).into()),
                    (Float(a),    Complex(b))  => Ok(self::Complex::from(a).$fnname(b).into()),
                    (Complex(a),  Float(b))    => Ok(a.$fnname(self::Complex::from(b)).into()),
                    (Rational(a), Complex(b))  => Ok(self::Complex::from(a.to_f64().ok_or(RATIO_CAST_FAIL)?).$fnname(b).into()),
                    (Complex(a),  Rational(b)) => Ok(a.$fnname(self::Complex::from(b.to_f64().ok_or(RATIO_CAST_FAIL)?)).into()),
                    (Complex(a),  Complex(b))  => Ok(a.$fnname(b).into()),
                    $($bonus)*
                    (lhs, rhs) => Err(format!("Unsupported operation '{}' between {:?} and {:?}", stringify!($fnname), lhs, rhs))
                }
            }
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, String>;
    fn neg(self) -> Self::Output {
        match self {
            Value::Int(a) => Ok(Value::Int(-a)),
            _ => Err(format!("Unsupported operation 'neg' on {:?}", self))
        }
    }
}

impl_numeric_op!(Add, add, {
    (String(a), String(b)) => Ok(((**a).to_owned() + b).into()),
    (String(a), Char(c)) => {
        let mut s = (**a).to_owned();
        s.push(*c);
        Ok(s.into())
    },
    (Char(c), String(a)) => Ok((c.to_string() + a).into()),
    (Char(c1), Char(c2)) => {
        let mut s = c1.to_string();
        s.push(*c2);
        Ok(s.into())
    }
    (List(a), List(b)) => {
        let a = (**a).clone();
        a.borrow_mut().append(&mut (**b).borrow().clone());
        Ok(a.into())
    },
});
impl_numeric_op!(Sub, sub, {});
impl_numeric_op!(Mul, mul, {
    (String(a), Int(b)) | (Int(b), String(a)) 
        => Ok(Value::from(a.chars().cycle().take(a.chars().count()*(*b as usize)).collect::<std::string::String>())),
    (List(a), Int(b)) | (Int(b), List(a))
        => Ok(Value::from(a.borrow().iter().cycle().take(a.borrow().len()*(*b as usize)).cloned().collect::<Vec<Value>>())),
});
impl_numeric_op!(Div, div, {});
impl_numeric_op!(Rem, rem, {});
