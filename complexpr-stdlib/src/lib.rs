pub mod prelude;
pub mod io;
pub mod iter;
pub mod math;

#[macro_export]
macro_rules! declare_fn {
    ($env:ident, $name:ident, $arg_count:literal) => {::paste::paste!{{
        let s: ::std::rc::Rc<str> = ::std::rc::Rc::from(stringify!($name));
        $env.declare(s.clone(), ::complexpr::value::Value::Func(::complexpr::value::func::Func::Builtin { func: [<fn_ $name>], arg_count: $arg_count, name: s }));
    }}};
    ($env:ident, $name:literal, $rust_name:ident, $arg_count:literal) => {{
        let s: ::std::rc::Rc<str> = ::std::rc::Rc::from($name);
        $env.declare(s.clone(), ::complexpr::value::Value::Func(::complexpr::value::func::Func::Builtin { func: $rust_name, arg_count: $arg_count, name: s }));
    }};
}
