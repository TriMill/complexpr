use std::{rc::Rc, collections::HashMap, ops::*, cmp::Ordering, cell::RefCell, hash::Hash, sync::atomic::{AtomicUsize, self}};

use num_traits::{Zero, ToPrimitive, Pow};
use strum::{EnumCount, EnumDiscriminants, EnumIter, IntoEnumIterator, AsRefStr};

use self::func::{Func, CIterator};

pub mod func;

pub type Rational = num_rational::Ratio<i64>;
pub type Complex = num_complex::Complex64;

static TYPE_COUNTER: AtomicUsize = AtomicUsize::new(Value::COUNT);

#[derive(Clone, Debug)]
pub enum TypeData {
    None,
    StructFields(Rc<Vec<String>>),
}

#[derive(Clone, Debug)]
pub struct Type {
    pub name: Rc<str>,
    pub id: usize,
    pub typedata: TypeData
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Type {}

pub fn generate_struct_type(name: Rc<str>, fields: Vec<String>) -> Type {
    Type { 
        name, 
        id: TYPE_COUNTER.fetch_add(1, atomic::Ordering::Relaxed),
        typedata: TypeData::StructFields(Rc::new(fields))
    }
}

pub fn generate_builtin_types() -> Vec<Type> {
    let mut types = vec![];
    for x in ValueDiscriminants::iter() {
        types.push(Type {
            name: Rc::from(x.as_ref()),
            id: x as usize,
            typedata: TypeData::None,
        })
    }
    types
}

fn fmt_list(list: &Vec<Value>) -> String {
    let mut result: String = "[".into();
    for v in list {
        result += &(v.repr() + ", ");
    }
    result.pop();
    result.pop();
    result + "]"
}

fn fmt_map(map: &HashMap<Value, Value>) -> String {
    let mut result: String = "{".into();
    for (k, v) in map {
        result += &(k.repr() + ": " + &v.repr() + ", ");
    }
    result.pop();
    result.pop();
    result + "}"
}

enum EscapeResult { Char(char), Str(&'static str), String(String) }

fn escape_char(c: char, is_char: bool) -> EscapeResult {
    match c {
        '\0' => EscapeResult::Str("\\0"),
        '\n' => EscapeResult::Str("\\n"),
        '\r' => EscapeResult::Str("\\r"),
        '\t' => EscapeResult::Str("\\t"),
        '\x1b' => EscapeResult::Str("\\e"),
        '\\' if is_char => EscapeResult::Str("\\\\"),
        '\'' if is_char => EscapeResult::Str("\\'"),
        '"' if !is_char => EscapeResult::Str("\\\""),
        _ if c.is_control() => {
            if c <= '\u{ff}' {
                EscapeResult::String(format!("\\x{:02x}", c as u32))
            } else {
                EscapeResult::String(format!("\\u{{{:06x}}}", c as u32))
            }
        }
        _ => EscapeResult::Char(c)
    }
}

fn repr_string(s: &str) -> String {
    let mut result: String = "\"".into();
    for c in s.chars() {
        match escape_char(c, false) {
            EscapeResult::Char(c) => result.push(c),
            EscapeResult::Str(s) => result += s,
            EscapeResult::String(s) => result += &s,
        }
    }
    result.push('"');
    result
}

fn repr_char(c: char) -> String {
    let mut result: String = "'".into();
    match escape_char(c, true) {
        EscapeResult::Char(c) => result.push(c),
        EscapeResult::Str(s) => result += s,
        EscapeResult::String(s) => result += &s
    }
    result.push('\'');
    result
}

#[derive(Clone, Debug)]
pub struct CxprStruct {
    pub ty: Type,
    pub data: Rc<RefCell<HashMap<String, Value>>>,
}

#[derive(Clone, Debug, EnumCount, EnumDiscriminants)]
#[strum_discriminants(derive(EnumIter, AsRefStr))]
#[repr(u8)]
pub enum Value {
    Nil,
    Type(Type),
    Int(i64), Float(f64), Complex(Complex), Rational(Rational),
    Bool(bool), 
    Char(char),
    String(Rc<str>), 
    List(Rc<RefCell<Vec<Value>>>), 
    Map(Rc<RefCell<HashMap<Value,Value>>>),
    Func(Func),
    Struct(CxprStruct),
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
            v => Err(format!("{} is not iterable", v.repr()))
        }
    }

    pub fn as_func(&self) -> Result<&Func, String> {
        match self {
            Value::Func(f) => Ok(f),
            v => Err(format!("{} is not a function", v.repr()))
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::Nil => "nil".into(),
            Self::Bool(b) => b.to_string(),
            Self::Int(n) => n.to_string(),
            Self::Float(f) => f.to_string(),
            Self::Rational(r) => r.to_string(),
            Self::Complex(z) => z.to_string(),
            Self::Char(c) => c.to_string(),
            Self::String(s) => s.as_ref().to_owned(),
            Self::List(l) => fmt_list(&l.borrow()),
            Self::Map(m) => fmt_map(&m.borrow()),
            Self::Type(t) => format!("<type {}>", t.name),
            Self::Func(Func::Builtin { name, func, .. }) => format!("<builtin fn {} at {:?}>", name, *func as *const ()),
            Self::Func(Func::BuiltinClosure { .. }) => format!("<builtin anonymous fn>"),
            Self::Func(f @ Func::Partial { .. }) => match f.name() {
                Some(name) => format!("<partial of fn {}>", name),
                None => "<partial of anonymous fn>".into(),
            }
            Self::Func(Func::Func { name, .. }) => match name {
                Some(name) => format!("<fn {}>", name),
                None => "<anonymous fn>".into(),
            },
            Self::Struct(CxprStruct { ty, data }) 
                => format!("{} {{ {} }}", ty.name, 
                    data.borrow().iter().map(|(k, v)| format!("{}: {}", k, v.repr()))
                        .collect::<Vec<String>>().join(", "))
        }
    }
    
    pub fn repr(&self) -> String {
        match self {
            Self::Float(f) => format!("{:?}",f),
            Self::Rational(r) => r.numer().to_string() + "//" + &r.denom().to_string(),
            Self::Char(c) => repr_char(*c),
            Self::String(s) => repr_string(s),
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
                _ => Err(format!("Cannot index {} with {}", self.repr(), idx.repr()))
            },
            Self::List(l) => match idx {
                Value::Int(i) if *i >= 0 => l.borrow().get(*i as usize)
                    .ok_or_else(|| format!("List index {} out of bounds for length {}", i, l.borrow().len()))
                    .map(|v| v.clone()),
                Value::Int(i) => Err(format!("List index {} cannot be negative", i)),
                _ => Err(format!("Cannot index {} with {}", self.repr(), idx.repr()))
            }
            Self::Map(m) => m.borrow().get(idx).cloned().ok_or_else(|| format!("Map does not contain key {}", idx.repr())),
            v => Err(format!("Cannot index into {}", v.repr()))
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
                _ => Err(format!("Cannot index {} with {}", self.repr(), idx.repr()))
            }
            Self::Map(m) => {
                m.borrow_mut().insert(idx.clone(), value); 
                Ok(())
            }
            v => Err(format!("Cannot assign to index in {}", v.repr()))
        }
    }

    pub fn len(&self) -> Result<usize, String> {
        match self {
            Value::String(s) => Ok(s.len()),
            Value::List(l) => Ok(l.borrow().len()),
            Value::Map(m) => Ok(m.borrow().len()),
            v => Err(format!("{} has no length", v.repr()))
        }
    }
    
    pub fn is_empty(&self) -> Result<bool, String> {
        Ok(self.len()? == 0)
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
            (x,y) => Err(format!("Unsupported operation 'fracdiv' between {} and {}", x.repr(), y.repr()))
        }
    }
    
    pub fn get_type(&self) -> Type {
        let discr = ValueDiscriminants::from(self);
        if let Self::Struct(_) = self {
            todo!()
        } else {
            Type {
                name: Rc::from(discr.as_ref()),
                id: discr as usize,
                typedata: TypeData::None,
            }
        }
    }
}

#[allow(clippy::ptr_eq)] // provided fix does not work
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil)                 => true,
            (Self::Type(a), Self::Type(b))         => a.id == b.id,

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
            (Self::Struct(_), Self::Struct(_)) => todo!("Can't compare data yet"),
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
    } else {
        f.to_bits().hash(state);
    }
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Nil => "nil".hash(state),
            Self::Type(t) => { "<type>".hash(state); t.id.hash(state); }
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
            Self::Struct(_) => todo!(),

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
                    (lhs, rhs) => Err(format!("Unsupported operation '{}' between {} and {}", stringify!($fnname), lhs.repr(), rhs.repr()))
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
            _ => Err(format!("Unsupported operation 'neg' on {}", self.repr()))
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
            (lhs, rhs) => Err(format!("Unsupported operation 'pow' between {} and {}", lhs.repr(), rhs.repr()))
        }
    }
}
