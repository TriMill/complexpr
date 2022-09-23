use std::cmp::Ordering;

use num_traits::{ToPrimitive, Pow, Signed};

use crate::{value::{Value, Complex, Rational}, RuntimeError, env::Environment, declare_fn};

enum Floaty {
    Real(f64), Complex(Complex)
}

impl From<f64> for Floaty {
    fn from(f: f64) -> Self {
        Self::Real(f)
    }
}

impl From<Complex> for Floaty {
    fn from(z: Complex) -> Self {
        Self::Complex(z)
    }
}

pub fn load(env: &mut Environment) {
    declare_fn!(env, re, 1);
    declare_fn!(env, im, 1);
    declare_fn!(env, min, 2);
    declare_fn!(env, max, 2);
    declare_fn!(env, abs, 1);
    declare_fn!(env, floor, 1);
    declare_fn!(env, ceil, 1);
    declare_fn!(env, round, 1);
    declare_fn!(env, round_to, 2);
    declare_fn!(env, sin, 1);
    declare_fn!(env, cos, 1);
    declare_fn!(env, tan, 1);
    declare_fn!(env, asin, 1);
    declare_fn!(env, acos, 1);
    declare_fn!(env, atan, 1);
    declare_fn!(env, sinh, 1);
    declare_fn!(env, cosh, 1);
    declare_fn!(env, tanh, 1);
    declare_fn!(env, asinh, 1);
    declare_fn!(env, acosh, 1);
    declare_fn!(env, atanh, 1);
    declare_fn!(env, exp, 1);
    declare_fn!(env, "log", fn_ln, 1);
    declare_fn!(env, is_prime, 1);
    declare_fn!(env, factors, 1);
}

//
// Helper functions
//

fn try_into_floaty(v: &Value, name: &'static str) -> Result<Floaty, String> {
    match v {
        Value::Int(n) => Ok((*n as f64).into()),
        Value::Float(f) => Ok((*f).into()),
        Value::Rational(r) => Ok((r.to_f64().ok_or("Could not convert rational to float")?).into()),
        Value::Complex(z) => Ok((*z).into()),
        _ => Err(format!("Argument to {} must be numeric", name))
    }
}

//
// Misc functions
//

fn fn_re(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Int(x) => Ok(Value::Float(*x as f64)),
        Value::Float(x) => Ok(Value::Float(*x)),
        Value::Rational(x) => Ok(Value::Float(x.to_f64().unwrap())),
        Value::Complex(x) => Ok(Value::Float(x.re)),
        x => Err(format!("Cannot get real part of {:?}", x).into())
    }
}

fn fn_im(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Int(_) | Value::Float(_) | Value::Rational(_) 
            => Ok(Value::Float(0.0)),
        Value::Complex(x) => Ok(Value::Float(x.im)),
        x => Err(format!("Cannot get real part of {:?}", x).into())
    }
}

fn fn_min(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0].partial_cmp(&args[1]) {
        None => Err("Arguments to min must be comparable".into()),
        Some(Ordering::Greater) => Ok(args[1].clone()),
        _ => Ok(args[0].clone())
    }
}

fn fn_max(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0].partial_cmp(&args[1]) {
        None => Err("Arguments to max must be comparable".into()),
        Some(Ordering::Less) => Ok(args[1].clone()),
        _ => Ok(args[0].clone())
    }
}

fn fn_abs(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        Value::Rational(r) => Ok(Value::Rational(r.abs())),
        _ => Err("Argument to floor must be real".into()),
    }
}

//
// Rounding
//

fn fn_floor(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0] {
        Value::Int(n) => Ok(Value::Int(n)),
        Value::Float(f) => Ok(Value::Float(f.floor())),
        Value::Complex(c) => Ok(Value::Complex(Complex::new(c.re.floor(), c.im.floor()))),
        Value::Rational(r) => Ok(Value::Rational(r.floor())),
        _ => Err("Argument to floor must be numeric".into()),
    }
}

fn fn_ceil(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0] {
        Value::Int(n) => Ok(Value::Int(n)),
        Value::Float(f) => Ok(Value::Float(f.ceil())),
        Value::Complex(c) => Ok(Value::Complex(Complex::new(c.re.ceil(), c.im.ceil()))),
        Value::Rational(r) => Ok(Value::Rational(r.ceil())),
        _ => Err("Argument to ceil must be numeric".into()),
    }
}

fn fn_round(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0] {
        Value::Int(n) => Ok(Value::Int(n)),
        Value::Float(f) => Ok(Value::Float(f.round())),
        Value::Complex(c) => Ok(Value::Complex(Complex::new(c.re.round(), c.im.round()))),
        Value::Rational(r) => Ok(Value::Rational(r.round())),
        _ => Err("Argument to round must be numeric".into()),
    }
}

fn fn_round_to(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let places = if let Value::Int(x) = args[1] { 
        x as i32
    } else {
        return Err("Second argument to round_to must be an integer".into())
    };
    match args[0] {
        Value::Int(n) if places >= 0 => Ok(Value::Int(n)),
        Value::Int(n) if places < 0 => {
            let factor = 10i64.pow((-places) as u32);
            Ok(Value::Int(
                (n / factor) * factor
            ))
        }
        Value::Float(f) => {
            let factor = 10.0_f64.pow(places);
            Ok(Value::Float(
                (f * factor).round() / factor
            ))
        },
        Value::Complex(c) => { 
            let factor = 10.0_f64.pow(places);
            Ok(Value::Complex(Complex::new(
                (c.re * factor).round() / factor,
                (c.im * factor).round() / factor,
            )))
        },
        Value::Rational(r) => {
            let factor = Rational::from(10).pow(places);
            Ok(Value::Rational((r * factor).round() / factor))
        },
        _ => Err("First argument to round_to must be numeric".into()),
    }
}

//
// Transcendental functions
//

macro_rules! transcendental {
    ($func:ident) => {
        paste::paste! {
            fn [<fn_ $func>](args: Vec<Value>) -> Result<Value, RuntimeError> {
                let val = try_into_floaty(&args[0], stringify!($func))?;
                match val {
                    Floaty::Real(f) => Ok(Value::Float(f.$func())),
                    Floaty::Complex(f) => Ok(Value::Complex(f.$func())),
                }
            }
        }
    };
}

transcendental!{ sin }
transcendental!{ cos }
transcendental!{ tan }
transcendental!{ asin }
transcendental!{ acos }
transcendental!{ atan }
transcendental!{ sinh }
transcendental!{ cosh }
transcendental!{ tanh }
transcendental!{ asinh }
transcendental!{ acosh }
transcendental!{ atanh }
transcendental!{ exp }
transcendental!{ ln }

//
// Factorization
//

fn fn_is_prime(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let n = match &args[0] {
        Value::Int(n) => *n,
        _ => return Err("Argument to is_prime must be an integer".into())
    };
    match n {
        _ if n <= 1 => Ok(Value::Bool(false)),
        2 | 3 => Ok(Value::Bool(true)),
        _ if (n % 2 == 0) || (n % 3 == 0) => Ok(Value::Bool(false)),
        _ => {
            let mut i = 5;
            while i*i <= n {
                if n % i == 0 {
                    return Ok(Value::Bool(false));
                }
                i += 2;
                if n % i == 0 {
                    return Ok(Value::Bool(false));
                }
                i += 4;
            }
            Ok(Value::Bool(true))
        }
    }
}

fn fn_factors(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let mut n = match &args[0] {
        Value::Int(n) => n.abs(),
        _ => return Err("Argument to is_prime must be an integer".into())
    };
    if n <= 1 {
        return Ok(Value::from(vec![]));
    }
    let mut factors = vec![];
    while n % 2 == 0 {
        factors.push(Value::Int(2));
        n >>= 1;
    }
    while n % 3 == 0 {
        factors.push(Value::Int(3));
        n /= 3;
    }
    let mut i = 5;
    while n != 1 {
        while n % i == 0 {
            factors.push(Value::Int(i));
            n /= i;
        }
        i += 2;
        while n % i == 0 {
            factors.push(Value::Int(i));
            n /= i;
        }
        i += 4;
    }

    Ok(Value::from(factors))
}
