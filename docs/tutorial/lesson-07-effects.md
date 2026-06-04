<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# Lesson 7: Algebraic Effects

## Effect Declarations

Define computational effects:
```affinescript
effect Console {
  fn print(s: String);
  fn read() -> String;
}
```

## Using Effects

Functions declare effects they perform:
```affinescript
fn greet() -> () / Console {
  print("Hello!");
}
```

## Effect Handlers

Handle effects with custom behavior:
```affinescript
handle greet() {
  print(s) => {
    // Custom printing logic
    resume ();
  }
}
```

Next: [Lesson 8: Generics](lesson-08-generics.md)
