# Lesson 9: Modules and Imports

## Module Declaration

```affinescript
module math.geometry;

pub fn area(radius: Float) -> Float {
  return 3.14 * radius * radius;
}
```

## Importing

```affinescript
use math.geometry;

fn main() -> Float {
  return geometry.area(5.0);
}
```

## Selective Imports

```affinescript
use math.geometry::{area, circumference};

fn main() -> Float {
  return area(5.0) + circumference(5.0);
}
```

Next: [Lesson 10: Building Real Programs](lesson-10-building.md)
