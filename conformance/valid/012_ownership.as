// Conformance test: ownership annotations

type Resource = own { handle: Int }

fn consume(r: own Resource) -> () {
  // Takes ownership, resource is consumed
}

fn borrow(r: ref Resource) -> Int {
  // Immutable borrow
  r.handle
}

fn mutate(r: mut Resource) -> () {
  // Mutable borrow
}

fn create() -> own Resource {
  Resource { handle: 42 }
}
