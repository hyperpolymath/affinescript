# What Makes AffineScript Brilliant

**AffineScript is a programming language that makes impossible errors impossible.**

If you've ever wondered "Why can't the compiler just catch this bug?" — AffineScript does.

## The Problem with Current Languages

**TypeScript:** Great for shape-checking, but can't prevent:
- Memory leaks (forgot to close that file?)
- Array bounds errors (`arr[999]` crashes at runtime)
- Division by zero
- Race conditions

**Rust:** Catches all these, but the complexity is overwhelming:
- Lifetime annotations everywhere: `fn foo<'a, 'b: 'a>(x: &'a T, y: &'b U)`
- Borrow checker fights you constantly
- Steep learning curve

**Haskell/OCaml:** Powerful type systems, but:
- No automatic resource management
- Effects are awkward (`IO monad` confusion)
- Hard to reason about performance

## AffineScript's Solution: Five Brilliant Ideas

### 1. Affine Types: Ownership Without the Pain

**Rust's problem:**
```rust
fn process<'a>(file: &'a mut File) -> Result<(), Error> // Lifetime hell
```

**AffineScript:**
```affinescript
fn process(file: ref File) -> Result[(), Error]  // That's it.
```

**What it means:**
- `own File` = You own it, must consume it (use once)
- `ref File` = You borrow it, can't consume it (use many times)
- **No lifetime parameters, no `'a` soup**

**Example - Can't use after move:**
```affinescript
fn close(file: own File) -> () { /* ... */ }

let f = open("data.txt");
close(f);      // Consumes f
read(f);       // ❌ COMPILE ERROR: f was moved!
```

**Brilliance:** Memory safety + resource safety, but **simpler than Rust**.

---

### 2. Dependent Types: Prove Correctness at Compile Time

**The dream:** Array access that can't crash.

**AffineScript makes it real:**
```affinescript
type Vec[n: Nat, T]  // Vector with length n encoded in type

fn head[n: Nat, T](v: Vec[n+1, T]) -> T {
  // Can ONLY call on non-empty vectors!
  // head(empty_vec) = COMPILE ERROR
}
```

**What just happened?**
- `Vec[0, Int]` = Empty vector (type says it has 0 elements)
- `Vec[5, String]` = Vector with exactly 5 strings
- `head()` requires `Vec[n+1, T]` = At least 1 element
- **Compiler proves you never call head() on empty vectors**

**More examples:**
```affinescript
fn append[n: Nat, m: Nat, T](
  a: Vec[n, T],
  b: Vec[m, T]
) -> Vec[n+m, T]  // Result length = sum of inputs (PROVEN!)

fn take[n: Nat, k: Nat, T](
  v: Vec[n, T],
  count: k where k <= n  // Can't take more than exists!
) -> Vec[k, T]
```

**Brilliance:** Buffer overflows? Array bounds errors? **Impossible at compile time.**

---

### 3. Refinement Types: Runtime Constraints, Compile-Time Checked

**The problem:** Division by zero crashes at runtime.

**AffineScript solution:**
```affinescript
fn div(a: Int, b: {x: Int where x != 0}) -> Int {
  a / b  // Safe! Compiler ensures b != 0
}

div(10, 5);   // ✅ OK
div(10, 0);   // ❌ COMPILE ERROR: 0 doesn't satisfy {x != 0}
```

**More powerful examples:**
```affinescript
type Percentage = {x: Int where x >= 0 && x <= 100}
type NonEmpty[T] = {arr: [T] where len(arr) > 0}
type Sorted[T] = {arr: [T] where forall i, j. i < j => arr[i] <= arr[j]}

fn first[T](arr: NonEmpty[T]) -> T  // Can't call on empty arrays!
fn binarySearch[T](arr: Sorted[T], val: T) -> Option[Int]  // Only works on sorted!
```

**Brilliance:** Turn runtime errors into **compile-time proofs**.

---

### 4. Effect System: Track What Your Code Does

**The problem:** Is this function pure? Does it do I/O? Can it throw?

**In other languages:** 🤷 Read the docs and hope.

**In AffineScript:** It's **in the type signature**.

```affinescript
fn compute(x: Int) -> Int / Pure
// Guaranteed: No I/O, no exceptions, no side effects

fn readFile(path: String) -> String / IO + Exn[IOError]
// Guaranteed: Does I/O, might throw IOError

fn parseJSON(s: String) -> Result[JSON, ParseError] / Pure
// Guaranteed: Pure (no side effects), returns Result instead of throwing
```

**What you gain:**
- **Refactoring safety:** Change `/ IO` to `/ Pure`? Compiler ensures you removed all I/O.
- **Parallel execution:** Pure functions can run in any order, any thread.
- **Testability:** Pure functions are trivial to test.

**Example - Explicit effects:**
```affinescript
fn processUser(id: Int) -> User / IO + Exn[DBError] {
  let data = queryDB(id);    // / IO + Exn[DBError]
  let validated = validate(data);  // / Pure
  validated
}
```

**Brilliance:** **See what your code does** just by reading the type signature.

---

### 5. Row Polymorphism: Structural Typing Done Right

**TypeScript problem:** Interfaces are nominal, not structural (mostly).

**AffineScript solution:** Records are **truly structural**.

```affinescript
fn getName(person: {name: String, ...rest}) -> String {
  person.name
}

getName({name: "Alice", age: 30});         // ✅ Works
getName({name: "Bob", role: "Engineer"});  // ✅ Works
getName({age: 30});                        // ❌ Missing 'name'
```

**What's `...rest`?**
- "I need a `name: String` field"
- "I don't care what other fields exist"
- True **duck typing** with **compile-time safety**

**Advanced example:**
```affinescript
fn updateAge[R](person: {age: Int, ...R}, newAge: Int) -> {age: Int, ...R} {
  {...person, age: newAge}  // Preserves all other fields!
}

let alice = {name: "Alice", age: 30, city: "NYC"};
let older = updateAge(alice, 31);
// older = {name: "Alice", age: 31, city: "NYC"} ✅
```

**Brilliance:** **Flexibility of duck typing** + **safety of static types**.

---

## The Magic: All Five Together

Here's a real-world example combining everything:

```affinescript
// Affine types: file is owned, must be closed
fn withFile[T](
  path: String,
  action: (ref File) -> Result[T, IOError] / IO  // Effect tracking
) -> Result[T, IOError] / IO {
  let file = open(path)?;     // Affine: file is owned
  let result = action(ref file);  // Borrowing: action borrows file
  close(file)?;               // Consumes file (can't use after)
  result
}

// Dependent + Refinement types: safe array access
fn safeIndex[n: Nat, T](
  arr: Vec[n, T],
  idx: {i: Int where i >= 0 && i < n}  // Proven in bounds!
) -> T / Pure {
  arr[idx]  // Can NEVER crash!
}

// Row polymorphism: flexible yet safe
fn logEvent[R](
  event: {timestamp: Int, level: String, ...R}
) -> () / IO {
  println("[\(event.timestamp)] \(event.level)");
}
```

**What you get:**
- ✅ No resource leaks (affine types)
- ✅ No array bounds errors (dependent types)
- ✅ No division by zero (refinement types)
- ✅ Clear side effects (effect system)
- ✅ Flexible APIs (row polymorphism)

---

## Why Not Just Use [X]?

| Language | What It Has | What It Lacks |
|----------|-------------|---------------|
| **TypeScript** | Easy to learn, great tooling | No resource safety, weak type system |
| **Rust** | Memory safety, zero-cost abstractions | Lifetime hell, steep learning curve |
| **Haskell** | Strong types, pure functions | Awkward effects (IO monad), hard to learn |
| **OCaml** | Fast, great type inference | No automatic resource management |
| **F#** | Nice syntax, .NET integration | No affine types, no dependent types |
| **Lean/Idris** | Dependent types, theorem proving | Too academic, slow, tiny ecosystem |

**AffineScript:** Takes the best ideas from each, **makes them learnable**.

---

## The Philosophy: Make Bugs Impossible

Not "catch bugs early" — **prevent bugs from existing**.

Traditional approach:
1. Write code
2. Run tests
3. Hope you covered all edge cases
4. Deploy
5. User finds crash you didn't test
6. 😭

AffineScript approach:
1. Write code
2. Compiler proves it's correct
3. Deploy
4. Users **cannot** trigger the impossible states
5. ✅

---

## Ready to Learn?

Start with **[Lesson 1: Hello AffineScript](lessons/01-hello-affinescript.md)**

Or try it now: **[Live Playground](../../playground/test.html)**

---

## Frequently Asked Questions

**Q: Is this just academic theory?**
A: No! The interpreter works today. You can run real code in the browser playground.

**Q: Will it be slow because of all these checks?**
A: Most checks happen at **compile time** (zero runtime cost). The few runtime checks (refinements) are optional.

**Q: Can I use it for real projects?**
A: Not yet. Interpreter is 75% complete, stdlib is 85% complete. Compiler is planned. Great for learning and prototyping now.

**Q: Is the syntax stable?**
A: Yes! Parser and type-checker are 100% complete.

**Q: How hard is it to learn?**
A: **Easier than Rust, harder than TypeScript.** If you know any typed language, you can learn AffineScript.

**Q: When will it be production-ready?**
A: Aiming for compiler + complete tooling in 2026. Follow progress at [github.com/hyperpolymath/affinescript](https://github.com/hyperpolymath/affinescript).

---

**Next:** [Lesson 1: Hello AffineScript →](lessons/01-hello-affinescript.md)
