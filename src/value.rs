use std::{rc::Rc, collections::HashMap, ops::*, ascii::AsciiExt};

use num_traits::Zero;

type Rational = num_rational::Ratio<i64>;
type Complex = num_complex::Complex64;

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

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Int(i64), Float(f64), Complex(Complex), Rational(Rational),
    Bool(bool), 
    Char(char),
    String(Rc<str>), 
    List(Rc<Vec<Value>>), Map(Rc<HashMap<Value,Value>>),
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
            List(l) => !l.len() == 0,
            Map(m) => !m.len() == 0,
            _ => true
        }
    }

    pub fn iter(&self) -> Result<Box<dyn Iterator<Item=Value> + '_>, ()> {
        match self {
            Value::String(s) 
                => Ok(Box::new(s.chars()
                    .map(|c| Value::Char(c)))),
            Value::List(l) => Ok(Box::new(l.iter().cloned())),
            _ => Err(())
        }
    }
}


value_from!(Int, u8 u16 u32 i8 i16 i32 i64);
value_from!(Float, f32 f64);
value_from!(Complex, Complex);
value_from!(Rational, Rational);
value_from!(Bool, bool);
value_from!(String, String Rc<str>);
value_from!(Char, char);
value_from!(List, Vec<Value>);
value_from!(Map, HashMap<Value,Value>);

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
        let mut a = (**a).clone();
        a.append(&mut (**b).clone());
        Ok(a.into())
    },
});
impl_numeric_op!(Sub, sub, {});
impl_numeric_op!(Mul, mul, {});
impl_numeric_op!(Div, div, {});
impl_numeric_op!(Rem, rem, {});