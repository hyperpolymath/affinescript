// Ownership and borrowing in AffineScript

type File = own { fd: Int }

effect IO {
  fn println(s: String);
}

effect Exn[E] {
  fn throw(err: E) -> Never;
}

type IOError = { message: String }

fn open(path: ref String) -> Result[own File, IOError] / IO + Exn[IOError] {
  // Implementation would go here
  Ok(File { fd: 42 })
}

fn read(file: ref File) -> Result[String, IOError] / IO + Exn[IOError] {
  // Borrows file - doesn't consume it
  Ok("file contents")
}

fn close(file: own File) -> Result[(), IOError] / IO {
  // Consumes file - can't use it after this
  Ok(())
}

// Safe resource handling with RAII pattern
fn withFile[T](
  path: ref String,
  action: (ref File) -> Result[T, IOError] / IO + Exn[IOError]
) -> Result[T, IOError] / IO + Exn[IOError] {
  let file = open(path)?;
  let result = action(ref file);
  close(file)?;
  result
}

// Usage example
fn processFile(path: ref String) -> Result[String, IOError] / IO + Exn[IOError] {
  withFile(path, |file| {
    read(file)
  })
}
