// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// AffineScript Standard Library - Input/Output
//
// Builtin functions (implemented in interpreter runtime):
//   print(args...)          -> ()                 (print to stdout)
//   println(args...)        -> ()                 (print with newline to stdout)
//   eprint(args...)         -> ()                 (print to stderr)
//   eprintln(args...)       -> ()                 (print with newline to stderr)
//   read_file(path)         -> Result<String, String>
//   write_file(path, data)  -> Result<(), String>
//   append_file(path, data) -> Result<(), String>
//   file_exists(path)       -> Bool
//   is_directory(path)      -> Bool
//   getenv(name)            -> Option<String>
//   getcwd()                -> Result<String, String>
//   read_line()             -> Result<String, String>
//   exit(code)              -> Never
//   show(value)             -> String
//   time_now()              -> Float              (CPU time in seconds)

// ============================================================================
// Console Output
// ============================================================================

// print, println, eprint, eprintln are builtins — see module header

/// Print formatted string (simple placeholder substitution)
///
/// Format placeholders:
///   {}  — insert next argument via show()
///
/// Example:
///   printf("Hello, {}! You are {} years old.", ["Alice", 30])
fn printf(format: String, args: [Any]) -> () {
  let flen = len(format);
  let arg_idx = 0;
  let i = 0;

  while i < flen {
    if i + 1 < flen && string_sub(format, i, 2) == "{}" {
      if arg_idx < len(args) {
        print(show(args[arg_idx]));
        arg_idx = arg_idx + 1;
      } else {
        print("{}");
      }
      i = i + 2;
    } else {
      print(string_sub(format, i, 1));
      i = i + 1;
    }
  }
}

/// Print formatted string with trailing newline
fn println_fmt(format: String, args: [Any]) -> () {
  printf(format, args);
  println("");
}

/// Print debug representation of any value
fn debug<T>(value: T) -> () {
  eprintln("DEBUG: " ++ show(value));
}

/// Print error with newline to stderr (convenience wrapper)
fn error(msg: String) -> () {
  eprintln("[ERROR] " ++ msg);
}

/// Print warning with newline to stderr
fn warn(msg: String) -> () {
  eprintln("[WARN] " ++ msg);
}

// ============================================================================
// File Operations (builtins)
// ============================================================================

// read_file, write_file, append_file, file_exists are builtins — see module header

/// Read file as a list of lines
fn read_lines(path: String) -> Result<[String], String> {
  match read_file(path) {
    Ok(content) => Ok(split(content, "\n")),
    Err(msg) => Err(msg)
  }
}

/// Get file size by reading and measuring content length
///
/// Note: this reads the entire file; a more efficient builtin would be
/// preferable for large files once the runtime supports stat().
fn file_size(path: String) -> Result<Int, String> {
  match read_file(path) {
    Ok(content) => Ok(len(content)),
    Err(msg) => Err(msg)
  }
}

// ============================================================================
// Directory Operations
// ============================================================================

// is_directory is a builtin — see module header

/// List directory contents (builtin — returns sorted entries, excluding . and ..)
extern fn list_dir(path: String) -> Result<[String], String>;

/// Create directory with permissions 0o755
extern fn create_dir(path: String) -> Result<(), String>;

/// Remove an empty directory
extern fn remove_dir(path: String) -> Result<(), String>;

// ============================================================================
// Path Operations
// ============================================================================

/// Join path components with the system separator (/)
fn path_join(components: [String]) -> String {
  let result = "";
  let mut first = true;
  for component in components {
    if first {
      result = component;
      first = false;
    } else {
      result = result ++ "/" ++ component;
    }
  }
  result
}

/// Extract the file extension from a path (without the leading dot)
///
/// Returns None if no extension is found.
/// Example: path_extension("file.txt") => Some("txt")
fn path_extension(path: String) -> Option<String> {
  let plen = len(path);
  let i = plen - 1;
  while i >= 0 {
    let ch = string_get(path, i);
    if ch == '.' {
      if i == plen - 1 {
        // Trailing dot, no extension
        return None;
      }
      return Some(string_sub(path, i + 1, plen - i - 1));
    }
    if ch == '/' {
      // Hit a directory separator before finding a dot
      return None;
    }
    i = i - 1;
  }
  None
}

/// Get the filename component from a path
///
/// Example: path_filename("/home/user/file.txt") => "file.txt"
fn path_filename(path: String) -> String {
  let plen = len(path);
  if plen == 0 {
    return "";
  }
  let i = plen - 1;
  while i >= 0 {
    if string_get(path, i) == '/' {
      return string_sub(path, i + 1, plen - i - 1);
    }
    i = i - 1;
  }
  // No separator found; entire path is the filename
  path
}

/// Get the directory component from a path
///
/// Example: path_dirname("/home/user/file.txt") => "/home/user"
fn path_dirname(path: String) -> String {
  let plen = len(path);
  if plen == 0 {
    return ".";
  }
  let i = plen - 1;
  while i >= 0 {
    if string_get(path, i) == '/' {
      if i == 0 {
        return "/";
      }
      return string_sub(path, 0, i);
    }
    i = i - 1;
  }
  // No separator found; directory is the current directory
  "."
}

/// Get the filename without its extension (stem)
///
/// Example: path_stem("archive.tar.gz") => "archive.tar"
fn path_stem(path: String) -> String {
  let filename = path_filename(path);
  let flen = len(filename);
  let i = flen - 1;
  while i > 0 {
    if string_get(filename, i) == '.' {
      return string_sub(filename, 0, i);
    }
    i = i - 1;
  }
  filename
}

// ============================================================================
// Process Operations
// ============================================================================

// getenv, getcwd, exit are builtins — see module header

/// Set environment variable
extern fn setenv(name: String, value: String) -> Result<(), String>;

/// Change current working directory
extern fn chdir(path: String) -> Result<(), String>;

// ============================================================================
// Input Operations
// ============================================================================

// read_line is a builtin — see module header

/// Read all input from stdin until EOF
fn read_stdin() -> Result<String, String> {
  let parts = [];
  let done = false;
  while !done {
    match read_line() {
      Ok(line) => {
        parts = parts ++ [line];
      },
      Err(_) => {
        done = true;
      }
    }
  }
  Ok(join(parts, "\n"))
}

/// Prompt user for input and return their response
fn prompt(message: String) -> Result<String, String> {
  print(message);
  read_line()
}

// ============================================================================
// Timing
// ============================================================================

// time_now is a builtin — see module header

/// Measure the wall-clock time of a function call (in seconds)
fn timed<T>(f: () -> T) -> (T, Float) {
  let start = time_now();
  let result = f();
  let elapsed = time_now() - start;
  (result, elapsed)
}
