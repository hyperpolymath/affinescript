// SPDX-License-Identifier: PMPL-1.0
// AffineScript Standard Library - Effect Declarations

// Core IO effect - file and console operations
effect io;

// State effect - mutable references
effect state;

// Exception effect - panics and error handling
effect exn;

// Built-in IO operations
extern fn print(s: String) -> Unit / io;
extern fn println(s: String) -> Unit / io;
extern fn read_line() -> String / io;
extern fn read_file(path: String) -> String / io;
extern fn write_file(path: String, content: String) -> Unit / io;

// Built-in State operations
extern fn ref<T>(x: T) -> Ref<T> / state;
extern fn get<T>(r: Ref<T>) -> T / state;
extern fn set<T>(r: Ref<T>, x: T) -> Unit / state;

// Built-in Exception operations
extern fn panic(msg: String) -> Never / exn;
extern fn error<T>(msg: String) -> T / exn;

// Pure operations (no effects)
extern fn int_to_string(n: Int) -> String;
extern fn string_to_int(s: String) -> Option<Int>;
extern fn string_length(s: String) -> Int;
extern fn string_concat(s1: String, s2: String) -> String;
