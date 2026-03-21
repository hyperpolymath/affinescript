# Lesson 1: Hello AffineScript

**Goal:** Write your first AffineScript program and understand basic syntax.

**Time:** 15 minutes

---

## Your First Program

Let's start with the traditional "Hello, World!":

```affinescript
println("Hello, AffineScript!");
```

**Try it now:** Open the [Playground](../../../playground/test.html) and run this code!

**What happened?**
- `println` is a built-in function that prints text
- Strings use double quotes: `"text here"`
- Semicolons are optional at end of expressions (but recommended in statements)

---

## Simple Arithmetic

AffineScript is great at math:

```affinescript
// Addition
10 + 20

// Multiplication and subtraction
5 * 6 - 2

// Division
100 / 4

// Order of operations works as expected
10 + 20 * 3  // Result: 70 (not 90!)

// Use parentheses to change order
(10 + 20) * 3  // Result: 90
```

**Try it:** Type each expression in the playground and see the results.

---

## Variables: Naming Your Values

Use `let` to give names to values:

```affinescript
let x = 42;
let y = 10;
let z = x + y;

println(z);  // Prints: 52
```

**Rules for variable names:**
- Start with a letter (lowercase for variables)
- Can contain letters, numbers, underscores
- Cannot be keywords (`let`, `fn`, `if`, etc.)

**Examples:**
```affinescript
let age = 25;
let user_count = 100;
let total2023 = 50000;
```

❌ **Don't do this:**
```affinescript
let 2fast = 10;      // ❌ Can't start with number
let my-var = 5;      // ❌ Hyphens not allowed (use underscore)
let let = 10;        // ❌ 'let' is a keyword
```

---

## Types: What Kind of Value?

Every value has a **type**. AffineScript knows these automatically:

```affinescript
let age = 30;           // Type: Int (integer number)
let price = 19.99;      // Type: Float (decimal number)
let name = "Alice";     // Type: String (text)
let isActive = true;    // Type: Bool (true or false)
```

You can **explicitly** write types:

```affinescript
let age: Int = 30;
let price: Float = 19.99;
let name: String = "Alice";
let isActive: Bool = true;
```

**When to write types?**
- Usually not needed (compiler infers them)
- Write them when you want to be explicit
- Required in function parameters (next lesson!)

---

## Comments: Notes for Humans

Use `//` for single-line comments:

```affinescript
// This is a comment - the computer ignores it
let x = 10;  // Comments can go at the end of lines too

// Use comments to explain WHY, not WHAT:
// Good: "Add tax because customers in CA pay 8.5%"
// Bad:  "Add 8.5 to the price"
```

---

## Expressions vs Statements

**Expression:** Something that produces a value

```affinescript
10 + 20        // Expression: produces 30
5 * 6          // Expression: produces 30
x + y          // Expression: produces sum of x and y
```

**Statement:** An action or instruction

```affinescript
let x = 10;    // Statement: creates a variable
println("Hi"); // Statement: prints text
```

**Key insight:** In AffineScript, almost everything is an expression!

```affinescript
let x = if true { 10 } else { 20 };  // 'if' is an expression!
```

More on this in later lessons.

---

## Your Turn: Exercises

Try these in the playground:

### Exercise 1: Calculate Your Age in Days
```affinescript
let age = 25;  // Change to your age
let days = age * 365;
println(days);
```

### Exercise 2: Temperature Converter
```affinescript
let celsius = 25;
let fahrenheit = celsius * 9 / 5 + 32;
println(fahrenheit);
```

### Exercise 3: Circle Area
```affinescript
let radius = 5;
let pi = 3.14159;
let area = pi * radius * radius;
println(area);
```

### Exercise 4: Shopping Cart
```affinescript
let item1 = 19.99;
let item2 = 5.50;
let item3 = 12.00;
let subtotal = item1 + item2 + item3;
let tax = subtotal * 0.08;  // 8% tax
let total = subtotal + tax;
println(total);
```

---

## Common Beginner Mistakes

### Mistake 1: Forgetting `let`
```affinescript
x = 10;  // ❌ ERROR: Variable not declared
```

**Fix:**
```affinescript
let x = 10;  // ✅ Declare with 'let'
```

### Mistake 2: Reusing variable names (for now)
```affinescript
let x = 10;
let x = 20;  // ⚠️ Shadowing (advanced topic)
```

**Better:**
```affinescript
let x = 10;
let y = 20;  // Use different names for now
```

### Mistake 3: Mixing types in arithmetic
```affinescript
let x = 10;
let y = "20";
let z = x + y;  // ❌ ERROR: Can't add Int and String
```

**Fix:**
```affinescript
let x = 10;
let y = 20;     // Both Int
let z = x + y;  // ✅ Works!
```

---

## What You Learned

✅ **How to write and run AffineScript code**
✅ **Basic arithmetic** (`+`, `-`, `*`, `/`)
✅ **Variables** with `let`
✅ **Types** (`Int`, `Float`, `String`, `Bool`)
✅ **Comments** with `//`
✅ **Expressions vs statements**

---

## Quiz Yourself

**Question 1:** What does this print?
```affinescript
let x = 5;
let y = x * 2;
println(y);
```
<details>
<summary>Answer</summary>
10
</details>

**Question 2:** What's wrong with this code?
```affinescript
age = 30;
println(age);
```
<details>
<summary>Answer</summary>
Missing `let` before `age`. Should be: `let age = 30;`
</details>

**Question 3:** What's the type of `x`?
```affinescript
let x = 3.14;
```
<details>
<summary>Answer</summary>
`Float` (decimal number)
</details>

---

## Next Steps

**Ready for more?** → [Lesson 2: Functions and Pattern Matching](02-functions-and-patterns.md)

**Want to understand why AffineScript is special?** → [What Makes It Brilliant](../WHAT-MAKES-IT-BRILLIANT.md)

**Just want to play?** → [Playground](../../../playground/test.html)

---

## Playground Example

Copy this complete example to the playground:

```affinescript
// Lesson 1: Complete Example
// Calculate total price with tax and discount

let basePrice = 100.0;
let taxRate = 0.08;      // 8%
let discountRate = 0.15; // 15% off

// Calculate discount
let discount = basePrice * discountRate;
let priceAfterDiscount = basePrice - discount;

// Calculate tax
let tax = priceAfterDiscount * taxRate;

// Final total
let total = priceAfterDiscount + tax;

// Show results
println("Base price: ");
println(basePrice);
println("After discount: ");
println(priceAfterDiscount);
println("Tax: ");
println(tax);
println("Final total: ");
println(total);
```

**Expected output:**
```
Base price: 100.0
After discount: 85.0
Tax: 6.8
Final total: 91.8
```

---

**🎉 Congratulations!** You've completed Lesson 1!

**Next:** [Lesson 2: Functions and Pattern Matching →](02-functions-and-patterns.md)
