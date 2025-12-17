# REPL Guide

The AffineScript REPL (Read-Eval-Print Loop) provides an interactive environment for exploring the language.

## Starting the REPL

```bash
# Start interactive session
affinescript repl

# With a prelude file
affinescript repl --prelude mylib.as

# With specific options
affinescript repl --no-color --history-file ~/.as_history
```

## Basic Usage

```
Welcome to AffineScript v0.1.0
Type :help for available commands.

> 1 + 2
3 : Int

> "hello" ++ " world"
"hello world" : String

> let x = 42
x : Int = 42

> x * 2
84 : Int
```

## REPL Commands

Commands start with `:`:

| Command | Description |
|---------|-------------|
| `:help` | Show help |
| `:quit` or `:q` | Exit REPL |
| `:type <expr>` | Show type of expression |
| `:kind <type>` | Show kind of type |
| `:effect <expr>` | Show effects of expression |
| `:load <file>` | Load file |
| `:reload` | Reload last loaded file |
| `:clear` | Clear screen |
| `:reset` | Reset environment |
| `:env` | Show current bindings |
| `:history` | Show command history |

### Type Inspection

```
> :type [1, 2, 3]
[Int; 3]

> :type |x| x + 1
(Int) -> Int

> :kind Option
* -> *

> :kind Map
* -> * -> *
```

### Effect Inspection

```
> :effect println("hello")
IO

> :effect |x| x + 1
pure

> fn get_and_print() -{State[Int], IO}-> Unit {
    let x = get()
    println(show(x))
  }
> :effect get_and_print
{State[Int], IO}
```

### Loading Files

```
> :load examples/math.as
Loaded examples/math.as

> add(1, 2)
3 : Int

> :reload
Reloaded examples/math.as
```

## Multi-line Input

Use `:{` and `:}` for multi-line:

```
> :{
| fn factorial(n: Int) -> Int {
|   if n <= 1 { 1 }
|   else { n * factorial(n - 1) }
| }
| :}
factorial : (Int) -> Int

> factorial(5)
120 : Int
```

Or simply continue with open braces:

```
> fn double(x: Int) -> Int {
|   x * 2
| }
double : (Int) -> Int
```

## Defining Types

```
> struct Point { x: Float64, y: Float64 }
Point defined

> let p = Point { x: 1.0, y: 2.0 }
p : Point = Point { x: 1.0, y: 2.0 }

> p.x
1.0 : Float64

> enum Color { Red, Green, Blue }
Color defined

> Color::Red
Red : Color
```

## Traits in REPL

```
> trait Greet {
|   fn greet(self: &Self) -> String
| }
Greet defined

> impl Greet for String {
|   fn greet(self: &Self) -> String {
|     "Hello, " ++ self
|   }
| }
impl Greet for String

> "World".greet()
"Hello, World" : String
```

## Effects in REPL

```
> effect Counter {
|   fn increment() -> Int
| }
Counter defined

> fn count_twice() -{Counter}-> Int {
|   increment() + increment()
| }
count_twice : () -{Counter}-> Int

> handle count_twice() {
|   let mut n = 0
|   increment() -> { n += 1; resume(n) }
| }
3 : Int
```

## Ownership in REPL

```
> let s = String::from("hello")
s : String = "hello"

> let t = s  // Move
t : String = "hello"

> s  // Error!
error[E0500]: use of moved value: `s`
  --> repl:1:1
  |
1 | s
  | ^ value used here after move

> let r = &t  // Borrow
r : &String

> t  // Still valid
"hello" : String
```

## Configuration

Create `~/.affine/repl.toml`:

```toml
# History settings
history_file = "~/.affine/history"
history_size = 1000

# Display settings
color = true
prompt = "> "
continuation_prompt = "| "

# Type display
show_types = true
show_effects = true

# Editor
editor = "vim"
```

## Key Bindings

| Key | Action |
|-----|--------|
| `Ctrl+C` | Cancel current input |
| `Ctrl+D` | Exit (on empty line) |
| `Ctrl+L` | Clear screen |
| `Up/Down` | Navigate history |
| `Ctrl+R` | Reverse search history |
| `Tab` | Autocomplete |

## Autocomplete

```
> Str<Tab>
String  StringBuffer  Stringify

> String::<Tab>
String::from  String::new  String::with_capacity

> let s = String::from("hello")
> s.<Tab>
s.len  s.push  s.pop  s.chars  s.bytes  ...
```

## Debugging

```
> :debug on
Debug mode enabled

> let x = 1 + 2
[debug] Parsing: let x = 1 + 2
[debug] Type inference: 1 : Int, 2 : Int
[debug] Result type: Int
x : Int = 3

> :debug off
Debug mode disabled
```

## Benchmarking

```
> :time factorial(20)
2432902008176640000 : Int
Time: 0.023ms

> :bench factorial(20)
Running 1000 iterations...
Mean: 22.5μs, Std: 1.2μs
Min: 20.1μs, Max: 35.2μs
```

---

## See Also

- [CLI Reference](cli.md) - Command-line options
- [Quick Start](../tutorials/quickstart.md) - Getting started
