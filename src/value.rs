use std::{rc::Rc, collections::HashMap, ops::*};

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
    String(Rc<str>), 
    List(Rc<Vec<Value>>), Map(Rc<HashMap<Value,Value>>),
}


value_from!(Int, u8 u16 u32 i8 i16 i32 i64);
value_from!(Float, f32 f64);
value_from!(Complex, Complex);
value_from!(Rational, Rational);
value_from!(Bool, bool);
value_from!(String, String Rc<str>);
value_from!(List, Vec<Value>);
value_from!(Map, HashMap<Value,Value>);

impl_numeric_op!(Add, add, {
    (String(a), String(b)) => Ok(((**a).to_owned() + b).into()),
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