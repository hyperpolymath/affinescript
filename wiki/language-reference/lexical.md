# Lexical Structure

This document describes the lexical structure of AffineScript - how source code is broken into tokens.

## Table of Contents

1. [Character Set](#character-set)
2. [Whitespace](#whitespace)
3. [Comments](#comments)
4. [Identifiers](#identifiers)
5. [Keywords](#keywords)
6. [Literals](#literals)
7. [Operators](#operators)
8. [Punctuation](#punctuation)
9. [Special Tokens](#special-tokens)

---

## Character Set

AffineScript source files are encoded in UTF-8. The lexer supports full Unicode for:

- Identifiers (letters from any script)
- String literals
- Comments

```affine
// Greek identifiers
let alpha = 1
let beta = 2

// Emoji in strings (but not identifiers)
let greeting = "Hello! :)"

// Unicode operators
let result = x `add` y
```

---

## Whitespace

Whitespace characters are used to separate tokens and are otherwise ignored:

| Character | Name | Unicode |
|-----------|------|---------|
| ` ` | Space | U+0020 |
| `\t` | Tab | U+0009 |
| `\n` | Newline | U+000A |
| `\r` | Carriage Return | U+000D |

```affine
// All equivalent:
let x = 1
let  x  =  1
let	x	=	1
```

---

## Comments

### Line Comments

Start with `//` and continue to end of line:

```affine
// This is a line comment
let x = 42  // Inline comment
```

### Block Comments

Enclosed in `/* */` and can be nested:

```affine
/* This is a block comment */

/*
  Multi-line
  block comment
*/

/* Nested /* comments */ are supported */
```

### Documentation Comments (Planned)

```affine
/// Documentation for the following item
/// Supports **markdown** formatting
fn documented_function() -> Unit { }

//! Module-level documentation
//! Describes the current module
```

---

## Identifiers

### Rules

- Start with a letter (any Unicode letter) or underscore
- Followed by letters, digits, or underscores
- Case-sensitive

```ebnf
identifier     = (letter | '_') (letter | digit | '_')*
letter         = <Unicode Letter category>
digit          = '0'..'9'
```

### Examples

```affine
// Valid identifiers
x
_private
camelCase
snake_case
PascalCase
SCREAMING_CASE
alpha1
_0

// Invalid
0start      // Cannot start with digit
kebab-case  // Hyphen not allowed
```

### Reserved Patterns

- `_` alone is a wildcard pattern, not a valid identifier
- Identifiers starting with `_` suppress unused warnings

```affine
let _ = unused_value()   // Wildcard, value discarded
let _temp = compute()    // Valid identifier, unused warning suppressed
```

---

## Keywords

AffineScript reserves the following keywords:

### Value Keywords
| Keyword | Description |
|---------|-------------|
| `true` | Boolean true |
| `false` | Boolean false |

### Declaration Keywords
| Keyword | Description |
|---------|-------------|
| `fn` | Function declaration |
| `let` | Variable binding |
| `type` | Type alias |
| `struct` | Struct definition |
| `enum` | Enum definition |
| `trait` | Trait declaration |
| `impl` | Implementation block |
| `effect` | Effect declaration |
| `mod` | Module declaration |

### Modifier Keywords
| Keyword | Description |
|---------|-------------|
| `pub` | Public visibility |
| `mut` | Mutable reference |
| `own` | Owned value |
| `ref` | Borrowed reference |
| `total` | Total function marker |
| `unsafe` | Unsafe block |

### Control Flow Keywords
| Keyword | Description |
|---------|-------------|
| `if` | Conditional |
| `else` | Else branch |
| `match` | Pattern matching |
| `while` | While loop |
| `for` | For loop |
| `in` | Iterator binding |
| `loop` | Infinite loop |
| `return` | Early return |
| `break` | Loop break |
| `continue` | Loop continue |

### Type Keywords
| Keyword | Description |
|---------|-------------|
| `where` | Type constraints |
| `as` | Type coercion |
| `Self` | Self type |

### Effect Keywords
| Keyword | Description |
|---------|-------------|
| `handle` | Effect handler |
| `resume` | Resume continuation |
| `perform` | Perform effect |

### Module Keywords
| Keyword | Description |
|---------|-------------|
| `use` | Import |
| `from` | Import source |

### Reserved for Future
| Keyword | Description |
|---------|-------------|
| `async` | Async functions |
| `await` | Await expressions |
| `yield` | Generator yield |
| `macro` | Macro definitions |
| `do` | Do notation |
| `forall` | Universal quantification |
| `exists` | Existential quantification |

---

## Literals

### Integer Literals

```affine
// Decimal
42
1_000_000  // Underscores for readability

// Hexadecimal
0xFF
0xDEAD_BEEF

// Binary
0b1010
0b1111_0000

// Octal
0o755
0o777
```

### Floating-Point Literals

```affine
3.14
2.0
1e10
1.5e-3
2.5E+10
```

### Character Literals

```affine
'a'
'\n'     // Newline
'\t'     // Tab
'\\'     // Backslash
'\''     // Single quote
'\0'     // Null
'\x7F'   // Hex escape (ASCII)
'\u{1F600}'  // Unicode escape
```

### String Literals

```affine
"Hello, World!"
"Line 1\nLine 2"
"Tab\there"
"Quote: \"Hello\""
"Unicode: \u{1F600}"

// Multi-line strings
"This is a
multi-line
string"
```

### Raw String Literals (Planned)

```affine
r"No \n escapes here"
r#"Can include "quotes" freely"#
r##"Even includes #"# literally"##
```

### String Interpolation (Planned)

```affine
let name = "Alice"
let greeting = "Hello, ${name}!"
let math = "1 + 1 = ${1 + 1}"
```

---

## Operators

### Arithmetic Operators
| Operator | Description |
|----------|-------------|
| `+` | Addition |
| `-` | Subtraction |
| `*` | Multiplication |
| `/` | Division |
| `%` | Modulo |

### Comparison Operators
| Operator | Description |
|----------|-------------|
| `==` | Equality |
| `!=` | Inequality |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less or equal |
| `>=` | Greater or equal |

### Logical Operators
| Operator | Description |
|----------|-------------|
| `&&` | Logical AND |
| `\|\|` | Logical OR |
| `!` | Logical NOT |

### Bitwise Operators
| Operator | Description |
|----------|-------------|
| `&` | Bitwise AND |
| `\|` | Bitwise OR |
| `^` | Bitwise XOR |
| `~` | Bitwise NOT |
| `<<` | Left shift |
| `>>` | Right shift |

### Assignment Operators
| Operator | Description |
|----------|-------------|
| `=` | Assignment |
| `+=` | Add-assign |
| `-=` | Subtract-assign |
| `*=` | Multiply-assign |
| `/=` | Divide-assign |
| `%=` | Modulo-assign |
| `&=` | AND-assign |
| `\|=` | OR-assign |
| `^=` | XOR-assign |
| `<<=` | Left shift-assign |
| `>>=` | Right shift-assign |

### Other Operators
| Operator | Description |
|----------|-------------|
| `->` | Function arrow |
| `-{E}->` | Effectful arrow |
| `=>` | Match arm / lambda |
| `..` | Range (exclusive) |
| `..=` | Range (inclusive) |
| `++` | String/list concatenation |
| `\|>` | Pipe operator |
| `?` | Error propagation |
| `@` | Pattern binding |

---

## Punctuation

| Symbol | Description |
|--------|-------------|
| `(` `)` | Grouping, tuples, function calls |
| `[` `]` | Array indexing, type parameters |
| `{` `}` | Blocks, records |
| `,` | Separator |
| `;` | Statement terminator |
| `:` | Type annotation |
| `::` | Path separator |
| `.` | Field access |
| `..` | Row variable prefix |

---

## Special Tokens

### Quantity Annotations

For linear/affine type annotations:

| Token | Meaning |
|-------|---------|
| `0` | Erased (compile-time only) |
| `1` | Linear (exactly once) |
| `omega` or `w` | Unrestricted |

```affine
fn linear_use(x: 1 Resource) -> Unit { ... }
fn erased_type[0 T]() -> Unit { ... }
```

### Row Variables

```affine
// ..name introduces a row variable
fn extend[r](rec: {..r}) -> {x: Int, ..r} {
  {x: 42, ..rec}
}
```

### Wildcards

```affine
let _ = ignored_value
let (x, _) = pair
match value {
  Some(_) -> "has value",
  None -> "empty"
}
```

---

## Operator Precedence

From highest to lowest precedence:

| Level | Operators | Associativity |
|-------|-----------|---------------|
| 1 | `.` `::` `[]` `()` | Left |
| 2 | `!` `~` `-` (unary) | Right |
| 3 | `*` `/` `%` | Left |
| 4 | `+` `-` | Left |
| 5 | `<<` `>>` | Left |
| 6 | `&` | Left |
| 7 | `^` | Left |
| 8 | `\|` | Left |
| 9 | `++` | Right |
| 10 | `==` `!=` `<` `>` `<=` `>=` | Left |
| 11 | `&&` | Left |
| 12 | `\|\|` | Left |
| 13 | `\|>` | Left |
| 14 | `..` `..=` | Non-assoc |
| 15 | `=` `+=` etc. | Right |

---

## Lexer Error Messages

Common lexer errors:

```
error[E0001]: unexpected character
  --> src/main.as:1:5
  |
1 | let @x = 5
  |     ^ unexpected '@'
  |
  = help: identifiers cannot contain '@'

error[E0002]: unterminated string literal
  --> src/main.as:1:9
  |
1 | let s = "hello
  |         ^^^^^^ string literal not closed
  |
  = help: add closing `"` at end of string

error[E0003]: invalid escape sequence
  --> src/main.as:1:13
  |
1 | let s = "hello\q"
  |               ^^ unknown escape sequence
  |
  = help: valid escapes are: \n, \t, \r, \\, \', \", \0, \xNN, \u{NNNN}
```

---

## See Also

- [Full Specification: Lexical Grammar](../../docs/spec.md#part-1-lexical-grammar)
- [Compiler: Lexer Implementation](../compiler/lexer.md)
