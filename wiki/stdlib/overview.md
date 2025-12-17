# Standard Library Overview

The AffineScript standard library provides essential types, traits, and functions.

## Library Organization

```
std/
├── prelude.as        # Auto-imported basics
├── primitives/       # Primitive types
│   ├── int.as
│   ├── float.as
│   ├── bool.as
│   ├── char.as
│   └── string.as
├── collections/      # Data structures
│   ├── vec.as
│   ├── array.as
│   ├── list.as
│   ├── map.as
│   ├── set.as
│   └── deque.as
├── core/             # Core types
│   ├── option.as
│   ├── result.as
│   ├── tuple.as
│   └── unit.as
├── memory/           # Memory management
│   ├── box.as
│   ├── rc.as
│   ├── arc.as
│   └── ptr.as
├── traits/           # Standard traits
│   ├── eq.as
│   ├── ord.as
│   ├── hash.as
│   ├── show.as
│   ├── clone.as
│   ├── default.as
│   └── iter.as
├── effects/          # Standard effects
│   ├── io.as
│   ├── exn.as
│   ├── async.as
│   ├── state.as
│   └── random.as
├── io/               # Input/Output
│   ├── read.as
│   ├── write.as
│   ├── fs.as
│   └── net.as
├── concurrency/      # Threading
│   ├── thread.as
│   ├── sync.as
│   ├── channel.as
│   └── atomic.as
├── text/             # Text processing
│   ├── fmt.as
│   ├── regex.as
│   └── unicode.as
└── utils/            # Utilities
    ├── time.as
    ├── env.as
    └── debug.as
```

## Prelude

Auto-imported types and traits:

```affine
// Types
use std::option::Option::{self, Some, None}
use std::result::Result::{self, Ok, Err}
use std::string::String
use std::vec::Vec

// Traits
use std::traits::{Eq, Ord, Hash, Show, Clone, Default}
use std::iter::{Iterator, IntoIterator}

// Functions
use std::io::{print, println, eprint, eprintln}
```

## Primitive Types

### Numeric Types

```affine
// Signed integers
Int       // Platform-dependent (32 or 64 bit)
Int8      // -128 to 127
Int16     // -32768 to 32767
Int32     // -2^31 to 2^31-1
Int64     // -2^63 to 2^63-1

// Unsigned integers
Nat       // Natural numbers (>= 0)
UInt8     // 0 to 255
UInt16    // 0 to 65535
UInt32    // 0 to 2^32-1
UInt64    // 0 to 2^64-1

// Floating point
Float32   // 32-bit IEEE 754
Float64   // 64-bit IEEE 754

// Methods
42.abs()           // 42
(-5).abs()         // 5
10.pow(3)          // 1000
16.to_string()     // "16"
Int::max_value()   // 2147483647
```

### Bool

```affine
true.and(false)    // false
true.or(false)     // true
true.not()         // false
true.then(42)      // Some(42)
false.then(42)     // None
```

### Char

```affine
'a'.is_alphabetic()  // true
'5'.is_digit()       // true
'A'.to_lowercase()   // 'a'
'z'.to_uppercase()   // 'Z'
' '.is_whitespace()  // true
```

### String

```affine
let s = "hello"

s.len()              // 5
s.is_empty()         // false
s.chars()            // Iterator over chars
s.bytes()            // Iterator over bytes
s.lines()            // Iterator over lines

s.to_uppercase()     // "HELLO"
s.to_lowercase()     // "hello"
s.trim()             // Remove whitespace
s.split(" ")         // Split by delimiter

s.contains("ell")    // true
s.starts_with("he")  // true
s.ends_with("lo")    // true

s.replace("l", "L")  // "heLLo"

// String building
let mut buf = String::new()
buf.push_str("hello")
buf.push(' ')
buf.push_str("world")
buf  // "hello world"
```

## Core Types

### Option

```affine
enum Option[T] {
  Some(T),
  None
}

let x: Option[Int] = Some(42)
let y: Option[Int] = None

x.is_some()          // true
x.is_none()          // false
x.unwrap()           // 42
x.unwrap_or(0)       // 42
y.unwrap_or(0)       // 0

x.map(|n| n * 2)     // Some(84)
y.map(|n| n * 2)     // None

x.and_then(|n| if n > 0 { Some(n) } else { None })

x.ok_or("error")     // Ok(42)
y.ok_or("error")     // Err("error")
```

### Result

```affine
enum Result[T, E] {
  Ok(T),
  Err(E)
}

let x: Result[Int, String] = Ok(42)
let y: Result[Int, String] = Err("failed")

x.is_ok()            // true
x.is_err()           // false

x.ok()               // Some(42)
x.err()              // None

x.map(|n| n * 2)     // Ok(84)
x.map_err(|e| e.len()) // Ok(42)

x.and_then(|n| Ok(n + 1))  // Ok(43)

// Error propagation with ?
fn may_fail() -> Result[Int, Error] {
  let x = try_something()?
  let y = try_another()?
  Ok(x + y)
}
```

## Collections

### Vec

```affine
let mut v: Vec[Int] = vec![]
v.push(1)
v.push(2)
v.push(3)

v.len()              // 3
v.is_empty()         // false
v[0]                 // 1
v.get(0)             // Some(&1)
v.get(10)            // None

v.pop()              // Some(3)
v.first()            // Some(&1)
v.last()             // Some(&2)

v.iter()             // Iterator
v.iter_mut()         // Mutable iterator

v.map(|x| x * 2)     // [2, 4]
v.filter(|x| x > 1)  // [2]
v.fold(0, |acc, x| acc + x)  // 3
```

### HashMap

```affine
let mut m: HashMap[String, Int] = HashMap::new()
m.insert("one", 1)
m.insert("two", 2)

m.get("one")         // Some(&1)
m.get("three")       // None

m.contains_key("one") // true
m.remove("one")      // Some(1)

m.keys()             // Iterator over keys
m.values()           // Iterator over values
m.iter()             // Iterator over (key, value)
```

### HashSet

```affine
let mut s: HashSet[Int] = HashSet::new()
s.insert(1)
s.insert(2)

s.contains(&1)       // true
s.remove(&1)         // true

let s2 = HashSet::from([2, 3, 4])
s.union(&s2)         // {2, 3, 4}
s.intersection(&s2)  // {2}
s.difference(&s2)    // {}
```

## Traits

### Eq

```affine
trait Eq {
  fn eq(self: &Self, other: &Self) -> Bool
  fn ne(self: &Self, other: &Self) -> Bool {
    !self.eq(other)
  }
}
```

### Ord

```affine
trait Ord: Eq {
  fn compare(self: &Self, other: &Self) -> Ordering
  fn lt(self: &Self, other: &Self) -> Bool
  fn le(self: &Self, other: &Self) -> Bool
  fn gt(self: &Self, other: &Self) -> Bool
  fn ge(self: &Self, other: &Self) -> Bool
  fn min(self: Self, other: Self) -> Self
  fn max(self: Self, other: Self) -> Self
}

enum Ordering { Less, Equal, Greater }
```

### Iterator

```affine
trait Iterator {
  type Item
  fn next(self: &mut Self) -> Option[Self::Item]

  // Provided methods
  fn map[B](self, f: (Self::Item) -> B) -> Map[Self, B]
  fn filter(self, p: (&Self::Item) -> Bool) -> Filter[Self]
  fn fold[B](self, init: B, f: (B, Self::Item) -> B) -> B
  fn collect[C: FromIterator](self) -> C
  // ... many more
}
```

## Effects

### IO

```affine
effect IO {
  fn print(s: &str)
  fn println(s: &str)
  fn read_line() -> String
  fn read_file(path: &str) -> Result[String, IoError]
  fn write_file(path: &str, content: &str) -> Result[(), IoError]
}
```

### State

```affine
effect State[S] {
  fn get() -> S
  fn put(s: S)
  fn modify(f: (S) -> S)
}

fn run_state[S, A](initial: S, f: () -{State[S]}-> A) -> (A, S)
```

### Error

```affine
effect Error[E] {
  fn raise(e: E) -> Never
}

fn catch[E, A](f: () -{Error[E]}-> A, handler: (E) -> A) -> A
```

## I/O

### File System

```affine
use std::fs

// Reading
let content = fs::read_to_string("file.txt")?
let bytes = fs::read("file.bin")?

// Writing
fs::write("file.txt", "content")?
fs::write("file.bin", bytes)?

// File operations
fs::exists("file.txt")
fs::remove("file.txt")?
fs::rename("old.txt", "new.txt")?
fs::copy("src.txt", "dst.txt")?

// Directories
fs::create_dir("path")?
fs::create_dir_all("path/to/dir")?
fs::remove_dir("path")?
fs::read_dir("path")?  // Iterator
```

### Networking

```affine
use std::net::{TcpListener, TcpStream}

// Server
let listener = TcpListener::bind("127.0.0.1:8080")?
for stream in listener.incoming() {
  handle_connection(stream?)
}

// Client
let mut stream = TcpStream::connect("127.0.0.1:8080")?
stream.write_all(b"Hello")?
let mut buf = [0; 1024]
stream.read(&mut buf)?
```

---

## See Also

- [Primitives](primitives.md) - Primitive type details
- [Collections](collections.md) - Collection types
- [Effects](effects.md) - Standard effects
- [I/O](io.md) - Input/output operations
