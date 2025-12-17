(** AffineScript Standard Library *)

(** Prelude - functions automatically available *)
let prelude_source = {|
// Core type aliases
type Unit = ();
type Never = !;

// Option type
enum Option[T] {
  None,
  Some(T),
}

// Result type
enum Result[T, E] {
  Ok(T),
  Err(E),
}

// Ordering for comparisons
enum Ordering {
  Less,
  Equal,
  Greater,
}

// Either type
enum Either[L, R] {
  Left(L),
  Right(R),
}

// Pair type
struct Pair[A, B] {
  fst: A,
  snd: B,
}
|}

(** Math module *)
let math_source = {|
module Std.Math;

// Constants
let PI: Float = 3.141592653589793;
let E: Float = 2.718281828459045;
let TAU: Float = 6.283185307179586;

// Absolute value
fn abs(x: Int) -> Int {
  if x < 0 { -x } else { x }
}

fn fabs(x: Float) -> Float {
  if x < 0.0 { -x } else { x }
}

// Min/max
fn min(a: Int, b: Int) -> Int {
  if a < b { a } else { b }
}

fn max(a: Int, b: Int) -> Int {
  if a > b { a } else { b }
}

fn fmin(a: Float, b: Float) -> Float {
  if a < b { a } else { b }
}

fn fmax(a: Float, b: Float) -> Float {
  if a > b { a } else { b }
}

// Clamp
fn clamp(x: Int, lo: Int, hi: Int) -> Int {
  if x < lo { lo }
  else if x > hi { hi }
  else { x }
}

// Sign
fn sign(x: Int) -> Int {
  if x < 0 { -1 }
  else if x > 0 { 1 }
  else { 0 }
}

// Power (integer)
fn pow(base: Int, exp: Int) -> Int {
  if exp == 0 { 1 }
  else if exp == 1 { base }
  else {
    let half = pow(base, exp / 2);
    if exp % 2 == 0 { half * half }
    else { half * half * base }
  }
}

// GCD using Euclidean algorithm
fn gcd(a: Int, b: Int) -> Int {
  let x = abs(a);
  let y = abs(b);
  if y == 0 { x }
  else { gcd(y, x % y) }
}

// LCM
fn lcm(a: Int, b: Int) -> Int {
  if a == 0 || b == 0 { 0 }
  else { abs(a * b) / gcd(a, b) }
}

// Factorial
fn factorial(n: Int) -> Int {
  if n <= 1 { 1 }
  else { n * factorial(n - 1) }
}

// Fibonacci
fn fib(n: Int) -> Int {
  if n <= 1 { n }
  else { fib(n - 1) + fib(n - 2) }
}

// Is prime check
fn is_prime(n: Int) -> Bool {
  if n < 2 { false }
  else if n == 2 { true }
  else if n % 2 == 0 { false }
  else {
    let mut i = 3;
    let mut result = true;
    while i * i <= n && result {
      if n % i == 0 { result = false; }
      i += 2;
    }
    result
  }
}
|}

(** String module *)
let string_source = {|
module Std.String;

// Check if string is empty
fn is_empty(s: String) -> Bool {
  len(s) == 0
}

// Repeat a string n times
fn repeat(s: String, n: Int) -> String {
  if n <= 0 { "" }
  else if n == 1 { s }
  else {
    let mut result = "";
    let mut i = 0;
    while i < n {
      result = result + s;
      i += 1;
    }
    result
  }
}

// Check if string starts with prefix
fn starts_with(s: String, prefix: String) -> Bool {
  let slen = len(s);
  let plen = len(prefix);
  if plen > slen { false }
  else {
    let mut i = 0;
    let mut result = true;
    while i < plen && result {
      if s[i] != prefix[i] { result = false; }
      i += 1;
    }
    result
  }
}

// Check if string ends with suffix
fn ends_with(s: String, suffix: String) -> Bool {
  let slen = len(s);
  let suflen = len(suffix);
  if suflen > slen { false }
  else {
    let mut i = 0;
    let offset = slen - suflen;
    let mut result = true;
    while i < suflen && result {
      if s[offset + i] != suffix[i] { result = false; }
      i += 1;
    }
    result
  }
}

// Reverse a string
fn reverse(s: String) -> String {
  let n = len(s);
  if n <= 1 { s }
  else {
    let mut result = "";
    let mut i = n - 1;
    while i >= 0 {
      result = result + str(s[i]);
      i -= 1;
    }
    result
  }
}

// Check if character is digit
fn is_digit(c: Char) -> Bool {
  c >= '0' && c <= '9'
}

// Check if character is letter
fn is_alpha(c: Char) -> Bool {
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

// Check if character is alphanumeric
fn is_alnum(c: Char) -> Bool {
  is_digit(c) || is_alpha(c)
}

// Check if character is whitespace
fn is_space(c: Char) -> Bool {
  c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

// Convert character to uppercase
fn to_upper(c: Char) -> Char {
  if c >= 'a' && c <= 'z' {
    // ASCII math: 'a' - 'A' = 32
    c - 32
  } else { c }
}

// Convert character to lowercase
fn to_lower(c: Char) -> Char {
  if c >= 'A' && c <= 'Z' {
    c + 32
  } else { c }
}
|}

(** Array/List module *)
let array_source = {|
module Std.Array;

// Check if array is empty
fn is_empty[T](arr: Array[T]) -> Bool {
  len(arr) == 0
}

// Get first element
fn first[T](arr: Array[T]) -> Option[T] {
  if len(arr) == 0 { None }
  else { Some(arr[0]) }
}

// Get last element
fn last[T](arr: Array[T]) -> Option[T] {
  let n = len(arr);
  if n == 0 { None }
  else { Some(arr[n - 1]) }
}

// Sum of integer array
fn sum(arr: Array[Int]) -> Int {
  fold(\acc, x -> acc + x, 0, arr)
}

// Product of integer array
fn product(arr: Array[Int]) -> Int {
  fold(\acc, x -> acc * x, 1, arr)
}

// Find minimum
fn min(arr: Array[Int]) -> Option[Int] {
  if len(arr) == 0 { None }
  else {
    let mut result = arr[0];
    let mut i = 1;
    while i < len(arr) {
      if arr[i] < result { result = arr[i]; }
      i += 1;
    }
    Some(result)
  }
}

// Find maximum
fn max(arr: Array[Int]) -> Option[Int] {
  if len(arr) == 0 { None }
  else {
    let mut result = arr[0];
    let mut i = 1;
    while i < len(arr) {
      if arr[i] > result { result = arr[i]; }
      i += 1;
    }
    Some(result)
  }
}

// Check if any element satisfies predicate
fn any[T](pred: fn(T) -> Bool, arr: Array[T]) -> Bool {
  let mut i = 0;
  let mut found = false;
  while i < len(arr) && !found {
    if pred(arr[i]) { found = true; }
    i += 1;
  }
  found
}

// Check if all elements satisfy predicate
fn all[T](pred: fn(T) -> Bool, arr: Array[T]) -> Bool {
  let mut i = 0;
  let mut result = true;
  while i < len(arr) && result {
    if !pred(arr[i]) { result = false; }
    i += 1;
  }
  result
}

// Find first element satisfying predicate
fn find[T](pred: fn(T) -> Bool, arr: Array[T]) -> Option[T] {
  let mut i = 0;
  while i < len(arr) {
    if pred(arr[i]) { return Some(arr[i]); }
    i += 1;
  }
  None
}

// Find index of first element satisfying predicate
fn find_index[T](pred: fn(T) -> Bool, arr: Array[T]) -> Option[Int] {
  let mut i = 0;
  while i < len(arr) {
    if pred(arr[i]) { return Some(i); }
    i += 1;
  }
  None
}

// Reverse an array (returns new array)
fn reverse[T](arr: Array[T]) -> Array[T] {
  let n = len(arr);
  if n <= 1 { arr }
  else {
    let mut result = [];
    let mut i = n - 1;
    while i >= 0 {
      result = push(result, arr[i]);
      i -= 1;
    }
    result
  }
}

// Take first n elements
fn take[T](n: Int, arr: Array[T]) -> Array[T] {
  let count = if n > len(arr) { len(arr) } else { n };
  let mut result = [];
  let mut i = 0;
  while i < count {
    result = push(result, arr[i]);
    i += 1;
  }
  result
}

// Drop first n elements
fn drop[T](n: Int, arr: Array[T]) -> Array[T] {
  let start = if n > len(arr) { len(arr) } else { n };
  let mut result = [];
  let mut i = start;
  while i < len(arr) {
    result = push(result, arr[i]);
    i += 1;
  }
  result
}

// Zip two arrays together
fn zip[A, B](a: Array[A], b: Array[B]) -> Array[(A, B)] {
  let n = if len(a) < len(b) { len(a) } else { len(b) };
  let mut result = [];
  let mut i = 0;
  while i < n {
    result = push(result, (a[i], b[i]));
    i += 1;
  }
  result
}

// Enumerate with indices
fn enumerate[T](arr: Array[T]) -> Array[(Int, T)] {
  let mut result = [];
  let mut i = 0;
  while i < len(arr) {
    result = push(result, (i, arr[i]));
    i += 1;
  }
  result
}

// Flatten nested arrays
fn flatten[T](arrs: Array[Array[T]]) -> Array[T] {
  let mut result = [];
  let mut i = 0;
  while i < len(arrs) {
    let mut j = 0;
    while j < len(arrs[i]) {
      result = push(result, arrs[i][j]);
      j += 1;
    }
    i += 1;
  }
  result
}

// Flat map
fn flat_map[T, U](f: fn(T) -> Array[U], arr: Array[T]) -> Array[U] {
  flatten(map(f, arr))
}

// Sort array (simple insertion sort for now)
fn sort(arr: Array[Int]) -> Array[Int] {
  let n = len(arr);
  if n <= 1 { arr }
  else {
    let mut result = arr;
    let mut i = 1;
    while i < n {
      let key = result[i];
      let mut j = i - 1;
      while j >= 0 && result[j] > key {
        result[j + 1] = result[j];
        j -= 1;
      }
      result[j + 1] = key;
      i += 1;
    }
    result
  }
}
|}

(** Option module *)
let option_source = {|
module Std.Option;

// Check if option is Some
fn is_some[T](opt: Option[T]) -> Bool {
  match opt {
    Some(_) => true,
    None => false,
  }
}

// Check if option is None
fn is_none[T](opt: Option[T]) -> Bool {
  match opt {
    None => true,
    Some(_) => false,
  }
}

// Unwrap with default
fn unwrap_or[T](opt: Option[T], default: T) -> T {
  match opt {
    Some(x) => x,
    None => default,
  }
}

// Unwrap or panic
fn unwrap[T](opt: Option[T]) -> T {
  match opt {
    Some(x) => x,
    None => panic("called unwrap on None"),
  }
}

// Map over option
fn map[T, U](f: fn(T) -> U, opt: Option[T]) -> Option[U] {
  match opt {
    Some(x) => Some(f(x)),
    None => None,
  }
}

// Flat map / and_then
fn and_then[T, U](f: fn(T) -> Option[U], opt: Option[T]) -> Option[U] {
  match opt {
    Some(x) => f(x),
    None => None,
  }
}

// Or else
fn or_else[T](opt: Option[T], alternative: Option[T]) -> Option[T] {
  match opt {
    Some(_) => opt,
    None => alternative,
  }
}

// Filter option
fn filter[T](pred: fn(T) -> Bool, opt: Option[T]) -> Option[T] {
  match opt {
    Some(x) => if pred(x) { Some(x) } else { None },
    None => None,
  }
}

// Convert to array
fn to_array[T](opt: Option[T]) -> Array[T] {
  match opt {
    Some(x) => [x],
    None => [],
  }
}

// Zip two options
fn zip[A, B](a: Option[A], b: Option[B]) -> Option[(A, B)] {
  match (a, b) {
    (Some(x), Some(y)) => Some((x, y)),
    _ => None,
  }
}
|}

(** Result module *)
let result_source = {|
module Std.Result;

// Check if result is Ok
fn is_ok[T, E](res: Result[T, E]) -> Bool {
  match res {
    Ok(_) => true,
    Err(_) => false,
  }
}

// Check if result is Err
fn is_err[T, E](res: Result[T, E]) -> Bool {
  match res {
    Err(_) => true,
    Ok(_) => false,
  }
}

// Unwrap Ok value or panic
fn unwrap[T, E](res: Result[T, E]) -> T {
  match res {
    Ok(x) => x,
    Err(e) => panic("called unwrap on Err"),
  }
}

// Unwrap Err value or panic
fn unwrap_err[T, E](res: Result[T, E]) -> E {
  match res {
    Err(e) => e,
    Ok(_) => panic("called unwrap_err on Ok"),
  }
}

// Unwrap with default
fn unwrap_or[T, E](res: Result[T, E], default: T) -> T {
  match res {
    Ok(x) => x,
    Err(_) => default,
  }
}

// Map over Ok value
fn map[T, U, E](f: fn(T) -> U, res: Result[T, E]) -> Result[U, E] {
  match res {
    Ok(x) => Ok(f(x)),
    Err(e) => Err(e),
  }
}

// Map over Err value
fn map_err[T, E, F](f: fn(E) -> F, res: Result[T, E]) -> Result[T, F] {
  match res {
    Ok(x) => Ok(x),
    Err(e) => Err(f(e)),
  }
}

// Flat map / and_then
fn and_then[T, U, E](f: fn(T) -> Result[U, E], res: Result[T, E]) -> Result[U, E] {
  match res {
    Ok(x) => f(x),
    Err(e) => Err(e),
  }
}

// Or else
fn or_else[T, E, F](f: fn(E) -> Result[T, F], res: Result[T, E]) -> Result[T, F] {
  match res {
    Ok(x) => Ok(x),
    Err(e) => f(e),
  }
}

// Convert to Option (discards error)
fn ok[T, E](res: Result[T, E]) -> Option[T] {
  match res {
    Ok(x) => Some(x),
    Err(_) => None,
  }
}

// Convert to Option (discards ok)
fn err[T, E](res: Result[T, E]) -> Option[E] {
  match res {
    Ok(_) => None,
    Err(e) => Some(e),
  }
}
|}

(** IO module placeholders *)
let io_source = {|
module Std.IO;

// Note: These are effect signatures, actual implementation requires runtime support

// Read a line from stdin
// effect fn read_line() -> String / IO;

// Write to stdout
// effect fn write(s: String) -> () / IO;

// Write line to stdout
// effect fn write_line(s: String) -> () / IO;

// Read entire file
// effect fn read_file(path: String) -> Result[String, IOError] / IO;

// Write to file
// effect fn write_file(path: String, content: String) -> Result[(), IOError] / IO;

// IO error type
enum IOError {
  NotFound(String),
  PermissionDenied(String),
  Other(String),
}
|}

(** Concurrency module placeholders *)
let async_source = {|
module Std.Async;

// Note: Async requires effect system and runtime support

// Future type (placeholder)
// type Future[T] = ...

// Spawn async task
// effect fn spawn[T](f: fn() -> T) -> Future[T] / Async;

// Await a future
// effect fn await[T](fut: Future[T]) -> T / Async;

// Sleep for milliseconds
// effect fn sleep(ms: Int) -> () / Async;

// Channel types (placeholder)
// struct Sender[T] { ... }
// struct Receiver[T] { ... }

// Create a channel
// fn channel[T]() -> (Sender[T], Receiver[T]);

// Send on channel
// effect fn send[T](sender: Sender[T], value: T) -> () / Async;

// Receive from channel
// effect fn recv[T](receiver: Receiver[T]) -> Option[T] / Async;
|}

(** Collect all stdlib modules *)
let modules = [
  ("Prelude", prelude_source);
  ("Std.Math", math_source);
  ("Std.String", string_source);
  ("Std.Array", array_source);
  ("Std.Option", option_source);
  ("Std.Result", result_source);
  ("Std.IO", io_source);
  ("Std.Async", async_source);
]

(** Load prelude into environment *)
let load_prelude env =
  try
    let prog = Parse_driver.parse_string ~file:"<prelude>" prelude_source in
    Eval.eval_program env prog
  with _ ->
    (* Prelude parse failed - just continue, types will be available when type checker exists *)
    ()

(** Load a specific module *)
let load_module env name =
  match List.assoc_opt name modules with
  | Some source ->
      (try
         let prog = Parse_driver.parse_string ~file:name source in
         Eval.eval_program env prog;
         Ok ()
       with
       | Parse_driver.Parse_error (msg, _) -> Error ("Parse error: " ^ msg)
       | Eval.Runtime_error (msg, _) -> Error ("Runtime error: " ^ msg))
  | None -> Error (Printf.sprintf "Unknown module: %s" name)

(** List available modules *)
let available_modules () =
  List.map fst modules
