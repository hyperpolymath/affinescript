// Effect handling in AffineScript

// Define a custom exception effect
effect Exn[E] {
  fn throw(err: E) -> Never;
}

// Define a state effect
effect State[S] {
  fn get() -> S;
  fn put(s: S);
  fn modify(f: S -> S);
}

// A computation that uses state
fn increment() -> Int / State[Int] {
  let n = State.get();
  State.put(n + 1);
  n
}

fn triple() -> (Int, Int, Int) / State[Int] {
  let a = increment();
  let b = increment();
  let c = increment();
  (a, b, c)
}

// Run state effect with an initial value
fn runState[S, T](init: S, comp: () -> T / State[S]) -> (T, S) / Pure {
  let mut state = init;
  handle comp() with {
    return x => (x, state),
    get() => resume(state),
    put(s) => {
      state = s;
      resume(())
    },
    modify(f) => {
      state = f(state);
      resume(())
    }
  }
}

// Error handling with effects
type ParseError = { line: Int, message: String }

fn parseInt(s: String) -> Int / Exn[ParseError] {
  // Simplified - would actually parse
  if s == "42" {
    42
  } else {
    Exn.throw(ParseError { line: 1, message: "not a number" })
  }
}

fn parseTwo(a: String, b: String) -> (Int, Int) / Exn[ParseError] {
  (parseInt(a), parseInt(b))
}

// Convert exceptions to Result
fn catchExn[E, T](comp: () -> T / Exn[E]) -> Result[T, E] / Pure {
  handle comp() with {
    return x => Ok(x),
    throw(e) => Err(e)
  }
}

// Combine multiple effects
effect Log {
  fn log(msg: String);
}

fn processWithLogging(x: Int) -> Int / State[Int] + Log {
  Log.log("Processing input");
  let current = State.get();
  let result = current + x;
  State.put(result);
  Log.log("Updated state");
  result
}

fn main() -> () / IO {
  // Run the state computation
  let (result, finalState) = runState(0, || triple());
  println("Result: " ++ result.show());
  println("Final state: " ++ finalState.show());

  // Handle exceptions
  let parsed = catchExn(|| parseInt("42"));
  match parsed {
    Ok(n) => println("Parsed: " ++ n.show()),
    Err(e) => println("Error: " ++ e.message)
  }
}
