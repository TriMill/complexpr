# complexpr

`complexpr` is a programming language, for a sufficiently inclusive definition of the term. It can be used to accomplish many tasks that the average programmer would wish to accomplish, including:

- Printing "Hello, world!" (and over 3 other strings!)
- Computing factorials
- Finding prime numbers
- Simulating arbitrary Turing machines
- And ∞ more!

To install complexpr, simply clone this repository, `cd complexpr-bin`, and `cargo install --path .`. You can then use the `complexpr` command to start a REPL, or pass a filename as an argument to read from a file.

## Primitive types

`complexpr` features a wide variety of primitive types, suitable for all your programming needs.

| Type       | Description                                               | Example values         |
|------------|-----------------------------------------------------------|------------------------|
| `Nil`      | Represents nothing (like `null` or `None`)                | `nil`                  |
| `Int`      | 64-bit signed integer                                     | `35`, `-1003`          |
| `Float`    | 64-bit floating-point number                              | `10.3`, `0.0061`       |
| `Complex`  | Complex number, represented as a pair of 64-bit floats    | `1.5 - 2i`             |
| `Rational` | Rational number, represented as a pair of 64-bit integers | `3//5`, `-11//25`      |
| `Bool`     | Boolean type, either `true` or `false`                    | `true`, `false`        |
| `Char`     | A single Unicode character                                | `'A'`, `'\n'`          |
| `String`   | A string of Unicode characters                            | `"example string\n"`   |
| `List`     | A heterogeneous list                                      | `[1, 2.0, "3"]`        |
| `Map`      | A heterogeneous hashmap                                   | `{1: "one", "2": 2}`   |
| `Func`     | A function                                                | `max`, `fn(x,y) (x^y)` |

Complex numbers can be written as the sum of a real component and imaginary component, where the imaginary component is suffixed with an `i`. For example, `5i`, `-1.3+3i`, and `0.5-0.2i` are all valid complex numbers.

Rational numbers are defined using the rational division operator `//`, described below.

### Truthiness

The following values are considered falsy:
- The boolean `false`, of course
- `nil`
- The numbers `0`, `0.0`, `0i`, and `0//1`
- The null character `'\0'`
- Empty lists, strings, and maps

All other values are truthy.

### Escape characters

Character and string literals can use the following escape codes to represent characters:

| Escape      | Character                                |
|-------------|------------------------------------------|
| `\\`        | Backslash `\`                            |
| `\'`        | Single quote `'`                         |
| `\"`        | Double quote `"`                         |
| `\0`        | Null byte (0x00)                         |
| `\t`        | Tab (0x09)                               |
| `\n`        | Newline (0x0A)                           |
| `\r`        | Carriage return (0x0D)                   |
| `\e`        | Escape (0x1B)                            |
| `\x##`      | Arbitrary character (one byte codepoint) |
| `\u{##…}`   | Arbitrary character (any size codepoint) |

Strings can extend across multiple lines, and will include the newlines by default. To prevent the newlines from being included, escape them using a `\` at the end of the line.

## Arithmetic operators

Data is rather useless if there is no way to operate it. As such, `complexpr` also includes several operators, the most basic of these being arithmetic operators.

| Operator | Description                                    | Example usage           |
|----------|------------------------------------------------|-------------------------|
| `+`      | Addition, concatenation of strings/chars/lists | `1 + 3.0`, `"ab" + 'c'` |
| `-`      | Subtraction                                    | `3 - 2`                 |
| `*`      | Multiplication, repeating a list n times       | `5 * 7.3`, `[0] * 10`   |
| `/`      | Division                                       | `22.3 / 5.1`            |
| `%`      | Modulo (remainder after division)              | `27 % 4`                |
| `^`      | Exponentiation                                 | `5^2`, `0.2^(-3)`       |
| `//`     | Rational division (results in a rational type) | `5//3`, `-1//10`        |

All of these operators (except `//`) follow the following rule when applied to numeric operands of differing types: the type of the result is the *more generic* of the two operand types. In order of increasing genericness, `Int`, `Rational`, `Float`, `Complex`. As an example, the type of `5.0 + 3//2` is `Float`, since floats are more generic than rationals

Unlike the other operators, `//` is only valid for integer and rational arguments, and always results in a rational value.

`+` can also be used for concatenating chars and strings to each other. The result of this is always a string. `+` can also be used to concatenate lists. `*` can be used for repeating a list or string a certain number of times, for example `[0,1,2] * 3` = `[0,1,2,0,1,2,0,1,2]`.

`^` has the highest precedence and is right-associative (so, `2^3^4` = `2^(3^4)`). `*`, `/`, `%`, and `//` have the next highest precedence, followed by `+` and `-`.

The unary minus operator `-` can be used to negate a variable.

## Comparison operators

In addition to operating on data, it is also desirable to compare it to other data. `complexpr` has the standard set of comparison operators plus a fairly uncommon one.

| Operator | Description              | Example usage (all true) |
|----------|--------------------------|--------------------------|
| `==`     | Equality                 | `5 == 5.0`               |
| `!=`     | Inequality               | `3//5 != 0.2`            |
| `>`      | Greater than             | `10 > -10`               |
| `<`      | Less than                | `3 < 4.0`                |
| `>=`     | Greater than or equal to | `5//12 >= 0.01`          |
| `<=`     | Less than or equal to    | `0.03 <= 3.03`           |
| `<=>`    | Spaceship (see below)    | `(4 <=> 5) == -1`        |

Equality and inequality will always succeed, regardless of the types of the arguments. Numeric types which have equal values will compare as equal, even if the types are different.

Comparison operators can be applied to real numbers (not `Complex`), chars, strings, and lists. Characters are compared by their codepoints, and strings and lists are compared [lexicographically](https://en.wikipedia.org/wiki/Lexicographic_order).

The spaceship operator results in 0 if the arguments are equal, 1 if the left-hand side is greater, or -1 if the right-hand side is greater.

## Logical operators

`&&` and `||` are the logical AND and OR operators. `&&` will evaluate to the left-hand argument if it is falsy, otherwise it will evaluate to the right-hand argument. `||` will evaluate to the left-hand argument if it is `truthy`, otherwise it will evaluate to the right-hand one.

`!` is the unary NOT operator.

## Comments

Line comments are preceded by a hash sign (or number sign, or pound sign, or octothorpe, etc.). Block comments are surrounded with `#{ … }#`. 

```
# line comment

#{ 
    block
    comment
}#
```

## Variables, assignment, and scope

Variables are declared using the keyword `let`. For example, `let number = 12;` declares a variable named `number` with the value of `12`. The variable does not need to be assigned to right away, in which case it defaults to `nil` (for example: `let number;`).

Use a single equals sign (`=`) to reassign a previously declared variable. As with many other programming languages, you can also use compound assignment operators to modify a variable's value. These include `+=`, `-=`, `*=`, `/=`, `%=`, `^=`, and `//=`, and do exactly what you would expect. These cannot be used in `let` statements, for obvious reasons.

Assignment can also be performed to elements of a list or map or to fields of a struct.

`complexpr` uses lexical scoping, so a variable is only valid in the block it was declared in, as well as all inner blocks. When assigning to a variable, if multiple outer scopes contain a declaration for the variable, the innermost one is modified.

```
let x = 5;
{
    println(x); # prints 5
    x = 3;
    let x = 0;
    println(x); # prints 0
}
println(x); # prints 3
let y; # y == nil
y = 3;
y += 5;
```

## Control flow

`complexpr` offers three procedural-style methods of control flow: if statements, while loops, and for-each loops. If statements use the keywords `if` for the first condition, `elif` for all subsequent conditions, and `else` for the else clause. Conditions do not need to be parenthesized. The bodies of an if statement should be enclosed in braces (currently this is not required if the body is one statement long, but this may change in the future).

While loops are syntactically very similar, using the keyword `while` and also not requiring parentheses for the condition.

For-each loops use the keyword `for`, followed by the loop variable, a colon, and then the collection to iterate over (see below for information regarding iterators).

```
let value = 5;
if value > 3 {
    while value > 4 {
        value -= 0.3;
    }
} elif value == 2 {
    for n: [1,3,5,4] {
        value += n;
    }
} else {
    value -= 1;
}
```

`continue` and `break` may be used inside loops to skip to the next iteration or exit early, respectively.

## Functions

Functions can be defined either with a name or anonymously. Defining a named function is equivalent to creating an anonymous one and immediately declaring a variable and assigning the function to it.

```
# define the function "add1"
fn add1(x, y) { 
    return x + y;
}

# create an anonymous function and assign it to the variable "add2"
let add2 = fn(x, y) {
    return x + y;
};
```

Functions can then be called using the typical syntax:

```
println(add1(3, 4)); # prints 7
```

When a function is declared, it captures the environment it was declared in, including definitions that occur after the function in the source code. This is somewhat counterintuitive.

```
let x = 5;
fn test() {
    println(x);
}
test(); # prints 5
x = 7;
test(); # prints 7
```

Functions are first-class, they can be passed around just like any other value.

### Braces vs parentheses

If a function body is one or multiple statements, it must be surrounded by braces `{ }`. If the body consists of a single expression, it can be surrounded with parentheses, and the result of the expression will be returned.

```
fn add3(x, y) (x + y)
println(add3(1, 7)); # prints 8
```

### Partial applications

If a function is called with fewer than its required number of arguments, it will return a *partial function*. Calling this partial function with the remaining arguments will return the value expected had the function been called with all of its arguments at once. 

```
fn add(x, y) (x + y)
let add7 = add(7);
println(add7(3)); # prints 10
```

## Iterators

An iterator is simply a function with the following properties:
- it takes zero arguments,
- it returns `nil` once it runs out of elements, and
- once it returns `nil` once, it will continue to do so forever

The language only enforces the first point, the second and third must be enforced by the programmer. It is also often the case that an iterator will return a different value each time it is called, however this is not neccesary.

Iterators can be used in most places that lists and strings can be used, for example in a for loop:

```
# print the numbers from 0 to 100 (exclusive)
for x in 0..100 {
    println(x);
}
```

### Range syntax

Ranges are a simple built-in type of iterator for producing a range of numbers or characters. Because of floating-point rounding issues, they can only be used with integers and rationals (not floats or complex numbers). The most basic range type is written as `a..b`, where `a` is the initial value and `b` is the limit (which is exclusive, ie. the iterator will stop before reaching this value).

Adding an equals sign before the limit value will make the range inclusive - it will stop only once it exceeds the limit. Replacing the final value with an asterisk `*` will make the range infinite (this is incompatible with the inclusive syntax).

After the limit term, an optional step amount can be specified by appending a colon followed by the step value. If no step value is specified the default is the integer `1`.

For a numeric range, the three terms can be integers or rational numbers in any combination. For character literals (which must be inclusive), the start and limit must be characters while the step, if specified, must be an integer.

```
println(list(0..6)); # [0, 1, 2, 3, 4, 5]
println(list(0..=6)); # [0, 1, 2, 3, 4, 5, 6]
# println(list(0..*)); # Constructing a list from an infinite range is a bad idea
println(list(0..6:2)); # [0, 2, 4]
println(list(0..=6:2)); # [0, 2, 4, 6]
# println(list(0..*:2)); # This is also a bad idea
```

### Pipeline syntax

One of the hallmark features of `complexpr` is its pipeline syntax. Pipelines provide a convenient way to work with data, especially iterators, by allowing the programmer to write functions in the order that they are applied. Pipeline operators all have the same precedence and are applied left-to-right.

The most basic pipeline operator is `|>`. `|>` will call the function on the right side using the value on the left side as an argument.

```
# println("Hello, world!");
"Hello, world!" |> println; 
```

`|:` is similar to `map` in other languages; it constructs an iterator that takes each element from the iterator on the left and applies the function on the right to it.

```
# prints the list of squares from 0^2 to 9^2
0..10 |: fn(x) (x^2) |> list |> println;
```

(Note: the iterator must first be converted to a list before printing.)

`|?` filters the iterator on the left using the function to the right. If the function returns `false` the item is skipped; if it returns true, the item is passed through.

```
# [1, 2, 4, 5, 7, 8]
0..10 |? fn(x) (x % 3 != 0) |> list |> println;
```

`|//` and `|\\` fold the iterator on the left over the function on the right. This is commonly used to get the sum, product, minimum, or maximum of an iterator. `|//` starts folding from the beginning of the iterator, `|\\` starts from the end. In many use cases (specifically, when the function used is associative), either can be used. `|//` should be preferred in these cases, as it is more efficient (`|\\` has to collect the entire iterator into a list first).

```
# == ((((0+1)+2)+3)+4)
0..5 |// fn(x,y) (x + y) |> println;
# == (0+(1+(2+(3+4))))
0..5 |\\ fn(x,y) (x + y) |> println;
```

In some cases (particulary when the function is not commutative) it may be desirable to use an initial value instead of taking the first two values from the list. This can be accomplished with the operators `|/` and `|\`. These operators are ternary, so the right-hand side must include the initial value and the function, separated by a comma. For `|/` this initial value will be used as the first argument during the first iteration, for `|\` it will be used as the second argument during the first iteration.

```
# == (((((0+2)+3)+4)+5)+6)
2..7 |/ 0, fn(x,y) (x + y) |> println;
# == (2+(3+(4+(5+(6+0)))))
2..7 |\ 0, fn(x,y) (x + y) |> println;
```

## Structs (WIP)

`todo!();`
