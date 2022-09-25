use std::{cmp::Ordering, rc::Rc};

use num_traits::{ToPrimitive, Pow, Signed, Zero};

use complexpr::{value::{Value, Complex, Rational}, RuntimeError, env::Environment};

use crate::declare_fn;

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
    env.declare(Rc::from("inf"), Value::from(f64::INFINITY));
    env.declare(Rc::from("nan"), Value::from(f64::NAN));
    env.declare(Rc::from("tau"), Value::from(std::f64::consts::TAU));
    env.declare(Rc::from("pi"), Value::from(std::f64::consts::PI));
    env.declare(Rc::from("e"), Value::from(std::f64::consts::E));

    declare_fn!(env, min, 2);
    declare_fn!(env, max, 2);
    declare_fn!(env, abs, 1);
    declare_fn!(env, numer, 1);
    declare_fn!(env, denom, 1);
    declare_fn!(env, rationalize, 1);

    declare_fn!(env, re, 1);
    declare_fn!(env, im, 1);
    declare_fn!(env, conj, 1);
    declare_fn!(env, arg, 1);
    declare_fn!(env, norm, 1);
    declare_fn!(env, norm_sq, 1);
    
    declare_fn!(env, floor, 1);
    declare_fn!(env, ceil, 1);
    declare_fn!(env, round, 1);
    declare_fn!(env, round_to, 2);
    declare_fn!(env, fract, 1);
    declare_fn!(env, trunc, 1);
    declare_fn!(env, signum, 1);

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
    declare_fn!(env, sqrt, 1);

    declare_fn!(env, gcd, 2);
    declare_fn!(env, lcm, 2);
    declare_fn!(env, is_prime, 1);
    declare_fn!(env, factors, 1);
}

//
// Helper functions
//

fn try_into_floaty(v: &Value, name: &str) -> Result<Floaty, String> {
    match v {
        Value::Float(f) => Ok((*f).into()),
        Value::Complex(z) => Ok((*z).into()),
        Value::Int(n) => Ok((*n as f64).into()),
        Value::Rational(r) => Ok((r.to_f64().ok_or("Could not convert rational to float")?).into()),
        _ => Err(format!("Argument to {} must be numeric", name))
    }
}

//
// Misc functions
//

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
        _ => Err("Argument to abs must be real".into()),
    }
}

//
// Rationals
//

fn fn_numer(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0] {
        Value::Rational(r) => Ok(Value::from(*r.numer())),
        Value::Int(n) => Ok(Value::from(n)),
        _ => Err("Argument to numer must be integer or rational".into()),
    }
}

fn fn_denom(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0] {
        Value::Rational(r) => Ok(Value::from(*r.denom())),
        Value::Int(_) => Ok(Value::from(1)),
        _ => Err("Argument to denom must be integer or rational".into()),
    }
}

fn fn_rationalize(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Float(f) => match Rational::approximate_float(*f) {
            Some(r) => Ok(Value::from(r)),
            None => Err("Error rationalizing float".into())
        },
        Value::Int(n) => Ok(Value::Rational(Rational::from(*n))),
        Value::Rational(r) => Ok(Value::Rational(*r)),
        x => Err(format!("Expected float, int, or rational, got {}", x).into())
    }
}

//
// Complex
//

fn fn_re(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Complex(x) => Ok(Value::Float(x.re)),
        Value::Float(x) => Ok(Value::Float(*x)),
        Value::Int(x) => Ok(Value::Float(*x as f64)),
        Value::Rational(x) => Ok(Value::Float(x.to_f64().unwrap())),
        x => Err(format!("Cannot get real part of {:?}", x).into())
    }
}

fn fn_im(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Complex(x) => Ok(Value::Float(x.im)),
        Value::Int(_) | Value::Float(_) | Value::Rational(_) 
            => Ok(Value::Float(0.0)),
        x => Err(format!("Cannot get real part of {:?}", x).into())
    }
}

fn fn_conj(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Complex(x) => Ok(Value::from(x.conj())),
        Value::Float(n) => Ok(Value::from(Complex::from(*n))),
        Value::Int(n) => Ok(Value::from(Complex::from(*n as f64))),
        Value::Rational(n) => Ok(Value::from(Complex::from(n.to_f64().unwrap()))),
        x => Err(format!("Cannot get conjugate of {:?}", x).into())
    }
}

fn fn_arg(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Complex(x) => Ok(Value::from(x.arg())),
        Value::Float(n) if *n >= 0.0 => Ok(Value::from(0.0)),
        Value::Float(_) => Ok(Value::from(std::f64::consts::PI)),
        Value::Int(n) if *n >= 0 => Ok(Value::from(0.0)),
        Value::Int(_) => Ok(Value::from(std::f64::consts::PI)),
        Value::Rational(n) if *n >= Rational::zero() => Ok(Value::from(0.0)),
        Value::Rational(_) => Ok(Value::from(std::f64::consts::PI)),
        x => Err(format!("Cannot get argument of {:?}", x).into())
    }
}

fn fn_norm(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Complex(x) => Ok(Value::from(x.norm())),
        Value::Float(n) => Ok(Value::from(n.abs())),
        Value::Int(n) => Ok(Value::from(n.abs() as f64)),
        Value::Rational(n) => Ok(Value::from(n.abs().to_f64().unwrap())),
        x => Err(format!("Cannot get norm of {:?}", x).into())
    }
}

fn fn_norm_sq(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match &args[0] {
        Value::Complex(x) => Ok(Value::from(x.norm_sqr())),
        Value::Float(n) => Ok(Value::from(n*n)),
        Value::Int(n) => Ok(Value::from((*n as f64)*(*n as f64))),
        Value::Rational(n) => { let x = n.to_f64().unwrap(); Ok(Value::from(x*x)) },
        x => Err(format!("Cannot get norm squared of {:?}", x).into())
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

fn fn_fract(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0] {
        Value::Int(_) => Ok(Value::Int(0)),
        Value::Float(f) => Ok(Value::Float(f.fract())),
        Value::Complex(c) => Ok(Value::Complex(Complex::new(c.re.fract(), c.im.fract()))),
        Value::Rational(r) => Ok(Value::Rational(r.fract())),
        _ => Err("Argument to fract must be numeric".into()),
    }
}

fn fn_trunc(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0] {
        Value::Int(n) => Ok(Value::Int(n)),
        Value::Float(f) => Ok(Value::Float(f.trunc())),
        Value::Complex(c) => Ok(Value::Complex(Complex::new(c.re.trunc(), c.im.trunc()))),
        Value::Rational(r) => Ok(Value::Rational(r.trunc())),
        _ => Err("Argument to trunc must be numeric".into()),
    }
}

fn fn_signum(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args[0] {
        Value::Int(n) => Ok(Value::Int(n.signum())),
        Value::Float(f) => Ok(Value::Float(f.signum())),
        Value::Complex(c) => Ok(Value::Complex(Complex::new(c.re.signum(), c.im.signum()))),
        Value::Rational(r) => Ok(Value::Rational(r.signum())),
        _ => Err("Argument to trunc must be numeric".into()),
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
transcendental!{ sqrt } // not technically transcendental

//
// Integers
//

fn gcd_inner(mut a: i64, mut b: i64) -> i64 {
    if a == 0 {
        return b
    } else if b == 0 {
        return a
    }

    let az = a.trailing_zeros();
    a >>= az;
    let bz = b.trailing_zeros();
    b >>= bz;
    let z = az.min(bz);

    loop {
        if a > b  {
            std::mem::swap(&mut a, &mut b);
        }
        b -= a;
        if b == 0 {
            return a << z;
        }
        b >>= b.trailing_zeros();
    }
}

fn fn_gcd(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let (a, b) = match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => (a.abs(), b.abs()),
        _ => return Err("Arguments to gcd must be integers".into())
    };
    Ok(Value::from(gcd_inner(a, b)))
}

fn fn_lcm(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let (a, b) = match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => (a.abs(), b.abs()),
        _ => return Err("Arguments to lcm must be integers".into())
    };
    let gcd = gcd_inner(a, b);
    Ok(Value::from(a/gcd * b))
}

fn fn_is_prime(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let n = match &args[0] {
        Value::Int(n) => *n,
        _ => return Err("Argument to is_prime must be integers".into())
    };
    match n {
        _ if n <= 1 => Ok(Value::Bool(false)),
        2 | 3 => Ok(Value::Bool(true)),
        _ if (n % 2 == 0) || (n % 3 == 0) => Ok(Value::Bool(false)),
        _ => {
            let mut i = 5;
            let mut d = 2;
            while i*i <= n {
                if n % i == 0 {
                    return Ok(Value::Bool(false));
                }
                i += d;
                d = 6 - d;
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
    while n & 1 == 0 {
        factors.push(Value::Int(2));
        n >>= 1;
    }
    while n % 3 == 0 {
        factors.push(Value::Int(3));
        n /= 3;
    }
    let mut i = 5;
    let mut d = 2;
    while n != 1 {
        if i*i > n {
            factors.push(Value::Int(n));
            break;
        }
        while n % i == 0 {
            factors.push(Value::Int(i));
            n /= i;
        }
        i += d;
        d = 6 - d;
    }

    Ok(Value::from(factors))
}
