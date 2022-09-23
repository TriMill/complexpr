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

Lists 

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

## Variables, assignment, and scope

Variables are declared using the keyword `let`. For example, `let number = 12;` declares a variable named `number` with the value of `12`. The variable does not need to be assigned to right away, in which case it defaults to `nil` (for example: `let number;`). `let` can only be used in statements (we'll get to the difference between statements and expressions later).

Use a single equals sign (`=`) to reassign a previously declared variable. As with many other programming languages, you can also use compound assignment operators to modify a variable's value. These include `+=`, `-=`, `*=`, `/=`, `%=`, `^=`, and `//=`, and do exactly what you would expect. These cannot be used in `let` statements, for obvious reasons.

`complexpr` uses lexical scoping, so a variable is only valid in the block it was declared in, as well as all inner blocks. When assigning to a variable, if multiple outer scopes contain a declaration for the variable, the innermost one is modified.

## Control flow

`complexpr` offers three procedural-style methods of control flow: if statements, while loops, and for-each loops. If statements use the keywords `if` for the first condition, `elif` for all subsequent conditions, and `else` for the else clause. Conditions do not need to be parenthesized. The bodies of an if statement should be enclosed in braces (currently this is not required if the body is one statement long, but this may change in the future).

While loops are syntactically very similar, using the keyword `while` and also not requiring parentheses for the condition.

For-each loops use the keyword `for`, followed by the loop variable, a colon, and then the collection to iterate over (see below for information regarding iterators).

Examples:
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

## Functions

`todo!();`
