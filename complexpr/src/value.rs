use std::{rc::Rc, collections::HashMap, ops::*, fmt, cmp::Ordering, cell::RefCell, hash::Hash};

use num_traits::{Zero, ToPrimitive, Pow};

use crate::{RuntimeError, eval::{eval_stmt, Unwind, eval_expr}, expr::Stmt, env::{EnvRef, Environment}};

pub type Rational = num_rational::Ratio<i64>;
pub type Complex = num_complex::Complex64;
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

    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Builtin { name, .. } => Some(name.as_ref()),
            Self::BuiltinClosure { .. } => None,
            Self::Func { name, .. } => name.as_ref().map(|s| s.as_ref()),
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
            }
            Ordering::Less => {
                Ok(Value::Func(Func::Partial { inner: Box::new(self.clone()), filled_args: arg_values }))
            },
            Ordering::Greater => Err(RuntimeError::new_incomplete(
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
pub enum Value {
    Nil,
    Type(usize),
    Int(i64), Float(f64), Complex(Complex), Rational(Rational),
    Bool(bool), 
    Char(char),
    String(Rc<str>), 
    List(Rc<RefCell<Vec<Value>>>), 
    Map(Rc<RefCell<HashMap<Value,Value>>>),
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
            Map(m) => !m.borrow().len() == 0,
            Char(c) => *c != '\0',
            _ => true
        }
    }

    pub fn iter(&self) -> Result<CIterator, String> {
        match self {
            Value::String(_) | Value::List(_)
                => Ok(CIterator::Indexable { value: self.clone(), idx: 0 }),
            Value::Func(f) => {
                if f.arg_count() == 0 {
                    Ok(CIterator::Func(f.clone()))
                } else {
                    Err("Only zero-argument functions can be used as iterators".into())
                }
            },
            v => Err(format!("{:?} is not iterable", v))
        }
    }

    pub fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if let Value::Func(f) = self {
            f.call(args)
        } else {
            Err(RuntimeError::new_incomplete("Cannot call"))           
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
            Self::Func(Func::BuiltinClosure { func, .. }) => Rc::from(format!("<builtin anonymous fn at {:?}>", *func as *const ())),
            Self::Func(f @ Func::Partial { .. }) => match f.name() {
                Some(name) => Rc::from(format!("<partial of fn {}>", name)),
                None => Rc::from("<partial of anonymous fn>"),
            }
            Self::Func(Func::Func { name, .. }) => match name {
                Some(name) => Rc::from(format!("<fn {}>", name)),
                None => Rc::from("<anonymous fn>"),
            },
            Self::Data(_) => todo!(),
        }
    }
    
    pub fn repr(&self) -> Rc<str> {
        match self {
            Self::Float(f) => Rc::from(format!("{:?}",f)),
            Self::Rational(r) => Rc::from(r.numer().to_string() + "//" + &r.denom().to_string()),
            Self::Char(c) => Rc::from(format!("'{}'", c)), // TODO escaping
            Self::String(s) => Rc::from(format!("\"{}\"", s)), // TODO escaping
            Self::List(l) => Rc::from(format!("{:?}", l.borrow())), // TODO fix
            Self::Map(m) => Rc::from(format!("{:?}", m)), // TODO fix
            _ => self.to_string(),
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
            Self::Map(m) => m.borrow().get(idx).cloned().ok_or_else(|| format!("Map does not contain key {:?}", idx)),
            v => Err(format!("Cannot index into {:?}", v))
        }
    }

    pub fn assign_index(&self, idx: &Value, value: Value) -> Result<(), String> {
        match self {
            Self::List(l) => match idx {
                Value::Int(i) if *i >= 0 && (*i as usize) < l.borrow().len() => {
                    l.borrow_mut()[*i as usize] = value;
                    Ok(())
                }
                Value::Int(i) if *i >= 0 => Err(format!("List index {} out of bounds for length {}", i, l.borrow().len())),
                Value::Int(i) => Err(format!("List index {} cannot be negative", i)),
                _ => Err(format!("Cannot index {:?} with {:?}", self, idx))
            }
            Self::Map(m) => {
                m.borrow_mut().insert(idx.clone(), value); 
                Ok(())
            }
            v => Err(format!("Cannot assign to index in {:?}", v))
        }
    }

    pub fn len(&self) -> Result<usize, String> {
        match self {
            Value::String(s) => Ok(s.len()),
            Value::List(l) => Ok(l.borrow().len()),
            Value::Map(m) => Ok(m.borrow().len()),
            v => Err(format!("{:?} has no length", v))
        }
    }

    pub fn fracdiv(&self, other: &Value) -> Result<Value, String> {
        use Value::*;
        match (self, other) {
            (Int(_), Int(b)) if *b == 0 => Err("Integer division by zero".into()),
            (Rational(_), Int(b)) if *b == 0 => Err("Rational division by zero".into()),
            (Int(_), Rational(b)) if b.is_zero() => Err("Rational division by zero".into()),
            (Rational(_), Rational(b)) if b.is_zero() => Err("Rational division by zero".into()),
            (Int(a), Int(b)) => Ok(Value::from(crate::value::Rational::new(*a, *b))),
            (Rational(a), Int(b)) => Ok(Value::from(a/b)),
            (Int(a), Rational(b)) => Ok(Value::from(b.recip()*a)),
            (Rational(a), Rational(b)) => Ok(Value::from(a/b)),
            (x,y) => Err(format!("Unsupported operation 'fracdiv' between {:?} and {:?}", x, y))
        }
    }
}

#[allow(clippy::ptr_eq)] // provided fix does not work
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
            (Self::Map(a), Self::Map(b)) => {
                // prevent double borrow
                if a.as_ref().as_ptr() == b.as_ref().as_ptr() { return true }
                if a.borrow().len() != b.borrow().len() { return false }
                for (k, v1) in a.borrow().iter() {
                    let bb = b.borrow();
                    let v2 = bb.get(k);
                    if v2 != Some(v1) {
                        return false
                    }
                }
                true
            }
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

impl Eq for Value {}

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

fn hash_f64<H: std::hash::Hasher>(f: f64, state: &mut H) {
    if f.is_nan() {
        "NaN".hash(state)
    } else if f == 0.0 {
        "0.0".hash(state)
    } else if f.is_infinite() {
        if f > 0.0 {
            "+inf".hash(state)
        } else{
            "-inf".hash(state)
        }
    } else{
        f.to_bits().hash(state);
    }
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Nil => "nil".hash(state),
            Self::Type(_) => todo!(),
            Self::Int(i) => i.hash(state),
            Self::Float(f) => hash_f64(*f, state),
            Self::Complex(z) => { hash_f64(z.re, state); hash_f64(z.im, state); }
            Self::Rational(r) => { r.numer().hash(state); r.denom().hash(state); }
            Self::Bool(b) => b.hash(state),
            Self::Char(c) => c.hash(state),
            Self::String(s) => s.hash(state),
            Self::List(l) => l.borrow().hash(state),
            Self::Map(_) => todo!(),
            Self::Func(f) => f.hash(state),
            Self::Data(_) => todo!(),

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

impl From<HashMap<Value,Value>> for Value {
    fn from(x: HashMap<Value,Value>) -> Self {
        Self::Map(RefCell::new(x).into())
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
value_from!(Map, RefCell<HashMap<Value,Value>>);


macro_rules! impl_numeric_op {
    ($optrait:ty, $fnname:ident, { $($bonus:tt)* }) => {
        impl $optrait for &Value {
            type Output = Result<Value, String>;
            fn $fnname(self, other: Self) -> Self::Output {
                use Value::*;
                const RATIO_CAST_FAIL: &'static str = "Failed to cast Rational to Float";
                match (self, other) {
                    $($bonus)*
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
            Value::Float(a) => Ok(Value::Float(-a)),
            Value::Rational(a) => Ok(Value::Rational(-a)),
            Value::Complex(a) => Ok(Value::Complex(-a)),
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
impl_numeric_op!(Div, div, {
    (Int(_), Int(b)) if *b == 0 => Err("Integer division by zero".into()),
    (Rational(_), Int(b)) if *b == 0 => Err("Rational division by zero".into()),
    (Int(_), Rational(b)) if b.is_zero() => Err("Rational division by zero".into()),
    (Rational(_), Rational(b)) if b.is_zero() => Err("Rational division by zero".into()),
});
impl_numeric_op!(Rem, rem, {
    (Int(_), Int(b)) if *b == 0 => Err("Integer modulo by zero".into()),
    (Rational(_), Int(b)) if *b == 0 => Err("Rational modulo by zero".into()),
    (Int(_), Rational(b)) if b.is_zero() => Err("Rational modulo by zero".into()),
    (Rational(_), Rational(b)) if b.is_zero() => Err("Rational modulo by zero".into()),
});

impl Pow<&Value> for &Value {
    type Output = Result<Value, String>;

    fn pow(self, other: &Value) -> Self::Output {
        use Value::*;
        const RATIO_CAST_FAIL: &str = "Failed to convert rational to float";
        match (self, other) {
            (Int(a),      Int(b)) => match b {
                x if *x < 0 => Err(format!("Cannot raise integer {:?} to negative integer exponent {:?}", a, b)),
                x if *x > (u32::MAX as i64) => Err(format!("Integer exponent {:?} too large", x)),
                _ => Ok(Value::from(a.pow(*b as u32)))
            },
            (Rational(a), Int(b))      => match b {
                x if *x > (i32::MAX as i64) => Err(format!("Integer exponent {:?} too large", x)),
                x if *x < (i32::MIN as i64) => Err(format!("Integer exponent {:?} too small", x)),
                _ => Ok(Value::from(a.pow(*b as i32)))
            },
            (Int(_),      Rational(_)) => Err("Cannot raise integer to rational exponent".into()),
            (Rational(_), Rational(_)) => Err("Cannot raise rational to rational exponent".into()),
            (Float(a),    Int(b))      => Ok(a.pow(*b as f64).into()),
            (Int(a),      Float(b))    => Ok((*a as f64).pow(b).into()),
            (Float(a),    Rational(b)) => Ok(a.pow(b.to_f64().ok_or(RATIO_CAST_FAIL)?).into()),
            (Rational(a), Float(b))    => Ok(a.to_f64().ok_or(RATIO_CAST_FAIL)?.pow(b).into()),
            (Float(a),    Float(b))    => Ok(a.pow(b).into()),
            (Int(a),      Complex(b))  => Ok(self::Complex::from(*a as f64).pow(b).into()),
            (Complex(a),  Int(b))      => Ok(a.pow(self::Complex::from(*b as f64)).into()),
            (Float(a),    Complex(b))  => Ok(self::Complex::from(a).pow(b).into()),
            (Complex(a),  Float(b))    => Ok(a.pow(self::Complex::from(b)).into()),
            (Rational(a), Complex(b))  => Ok(self::Complex::from(a.to_f64().ok_or(RATIO_CAST_FAIL)?).pow(b).into()),
            (Complex(a),  Rational(b)) => Ok(a.pow(self::Complex::from(b.to_f64().ok_or(RATIO_CAST_FAIL)?)).into()),
            (Complex(a),  Complex(b))  => Ok(a.pow(b).into()),
            (lhs, rhs) => Err(format!("Unsupported operation 'pow' between {:?} and {:?}", lhs, rhs))
        }
    }
}
