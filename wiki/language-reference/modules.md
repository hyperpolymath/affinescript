# Module System

AffineScript's module system organizes code into logical units with controlled visibility.

## Table of Contents

1. [Module Basics](#module-basics)
2. [File Structure](#file-structure)
3. [Visibility](#visibility)
4. [Imports](#imports)
5. [Re-exports](#re-exports)
6. [Namespacing](#namespacing)
7. [Module Patterns](#module-patterns)

---

## Module Basics

### Declaring Modules

```affine
// In main.as
mod math        // Looks for math.as or math/mod.as
mod utils       // Looks for utils.as or utils/mod.as
mod internal    // Inline module definition follows

mod internal {
  fn helper() -> Int { 42 }
}
```

### Module Contents

Modules can contain:
- Functions
- Types (structs, enums, type aliases)
- Traits
- Effects
- Constants
- Nested modules

```affine
mod geometry {
  struct Point { x: Float64, y: Float64 }
  struct Circle { center: Point, radius: Float64 }

  trait Shape {
    fn area(self: &Self) -> Float64
  }

  impl Shape for Circle {
    fn area(self: &Self) -> Float64 {
      3.14159 * self.radius * self.radius
    }
  }

  fn distance(p1: &Point, p2: &Point) -> Float64 {
    let dx = p2.x - p1.x
    let dy = p2.y - p1.y
    (dx * dx + dy * dy).sqrt()
  }
}
```

---

## File Structure

### Single File Modules

```
project/
├── main.as
├── math.as       # mod math
└── utils.as      # mod utils
```

### Directory Modules

```
project/
├── main.as
└── math/
    ├── mod.as      # mod math entry point
    ├── vectors.as  # mod vectors (inside math)
    └── matrices.as # mod matrices (inside math)
```

`math/mod.as`:
```affine
// Declare submodules
pub mod vectors
pub mod matrices

// Re-export commonly used items
pub use vectors::Vec3
pub use matrices::Mat4
```

### Nested Modules

```
project/
├── main.as
└── graphics/
    ├── mod.as
    ├── renderer/
    │   ├── mod.as
    │   ├── opengl.as
    │   └── vulkan.as
    └── primitives/
        ├── mod.as
        ├── shapes.as
        └── colors.as
```

---

## Visibility

### Default Visibility

Items are private by default:

```affine
mod internal {
  fn private_helper() -> Int { 42 }  // Private to this module
}

// main.as
internal::private_helper()  // ERROR: private function
```

### Public Items

Use `pub` to make items public:

```affine
mod math {
  pub fn add(x: Int, y: Int) -> Int { x + y }

  fn internal_helper() -> Int { 0 }  // Still private
}

// main.as
math::add(1, 2)  // OK
```

### Visibility Modifiers

```affine
pub           // Public to everyone
pub(crate)    // Public within the crate
pub(super)    // Public to parent module
pub(in path)  // Public to specific module path
              // (no modifier) - Private to current module
```

Examples:

```affine
mod outer {
  pub mod inner {
    pub fn public_fn() { }           // Everyone can access
    pub(super) fn parent_only() { }  // Only outer can access
    pub(crate) fn crate_only() { }   // Only this crate
    fn private_fn() { }               // Only inner can access
  }

  fn test() {
    inner::public_fn()    // OK
    inner::parent_only()  // OK - we're the parent
    inner::private_fn()   // ERROR - private
  }
}
```

### Struct Field Visibility

```affine
pub struct Config {
  pub name: String,          // Public field
  pub(crate) internal: Int,  // Crate-visible field
  secret: String,            // Private field
}

impl Config {
  // Constructor needed for private fields
  pub fn new(name: String, secret: String) -> Config {
    Config { name, internal: 0, secret }
  }
}
```

---

## Imports

### Basic Imports

```affine
use math::add
use math::sub

fn main() {
  add(1, 2)  // No prefix needed
}
```

### Grouped Imports

```affine
use math::{add, sub, mul, div}

use std::collections::{Vec, HashMap, HashSet}
```

### Nested Imports

```affine
use std::{
  io::{Read, Write, BufReader},
  collections::{Vec, HashMap},
  fmt::Show
}
```

### Glob Imports

```affine
use math::*  // Import all public items

// Use sparingly - can cause name conflicts
```

### Aliased Imports

```affine
use std::collections::HashMap as Map
use graphics::renderer::opengl as gl

let map: Map[String, Int] = Map::new()
gl::init()
```

### Self Import

```affine
use std::collections::{self, HashMap}

// Now can use both:
collections::Vec  // Full path
HashMap           // Direct
```

---

## Re-exports

### Basic Re-export

```affine
// In lib.as
mod internal

pub use internal::PublicType
pub use internal::public_function
```

### Aliased Re-export

```affine
pub use internal::LongTypeName as Short
```

### Glob Re-export

```affine
// Re-export everything from a module
pub use prelude::*
```

### Creating a Prelude

```affine
// prelude.as
pub use crate::types::{Result, Option, Error}
pub use crate::traits::{Show, Clone, Default}
pub use crate::macros::*

// Users can import everything commonly needed:
use mylib::prelude::*
```

---

## Namespacing

### Path Resolution

```affine
// Absolute path from crate root
use crate::module::item

// Relative to current module
use self::submodule::item

// Relative to parent module
use super::sibling::item

// External crate
use external_crate::item
```

### Name Shadowing

```affine
use std::option::Option

// Local definition shadows import
enum Option[T] {
  Some(T),
  None,
  Unknown
}

// Use explicit path for std version
let x: std::option::Option[Int] = std::option::Some(42)
let y: Option[Int] = Unknown
```

### Disambiguating

```affine
use graphics::Point
use geometry::Point as GeoPoint

let screen_pt: Point = Point { x: 100, y: 200 }
let world_pt: GeoPoint = GeoPoint { x: 1.0, y: 2.0 }
```

---

## Module Patterns

### Facade Pattern

```affine
// lib.as - expose clean public API
pub mod types
pub mod traits
pub mod error

// Re-export main items at crate root
pub use types::{Config, Settings}
pub use traits::Processor
pub use error::{Error, Result}

// Keep implementation private
mod internal {
  // Implementation details
}
```

### Feature Modules

```affine
// Conditionally compiled modules
#[cfg(feature = "async")]
pub mod async_support

#[cfg(feature = "serde")]
pub mod serialization
```

### Test Modules

```affine
pub fn add(x: Int, y: Int) -> Int {
  x + y
}

#[cfg(test)]
mod tests {
  use super::*

  #[test]
  fn test_add() {
    assert_eq(add(2, 2), 4)
  }
}
```

### Internal vs External API

```affine
// Public API
pub mod api {
  pub use crate::core::PublicType
  pub use crate::core::public_fn
}

// Internal implementation
mod core {
  pub struct PublicType { ... }
  pub fn public_fn() { ... }

  // These stay internal
  struct InternalHelper { ... }
  fn internal_fn() { ... }
}
```

---

## Crate Structure

### Library Crate

```
mylib/
├── affine.toml
└── src/
    ├── lib.as      # Crate root
    ├── types.as
    ├── traits.as
    └── utils/
        ├── mod.as
        └── helpers.as
```

`lib.as`:
```affine
pub mod types
pub mod traits
pub mod utils

pub use types::MainType
pub use traits::MainTrait
```

### Binary Crate

```
myapp/
├── affine.toml
└── src/
    ├── main.as     # Binary entry point
    └── lib.as      # Optional library part
```

### Mixed Crate

```
myproject/
├── affine.toml
└── src/
    ├── main.as     # Uses lib
    ├── lib.as      # Library code
    └── modules/
```

---

## See Also

- [Package Manager](../tooling/package-manager.md) - Crate dependencies
- [Visibility](../design/language.md#visibility) - Design rationale
