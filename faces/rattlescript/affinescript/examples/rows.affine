// Row polymorphism in AffineScript

// Works on any record that has a 'name' field
fn greet[..r](person: {name: String, ..r}) -> String / Pure {
  "Hello, " ++ person.name
}

// Works on any record with both 'first' and 'last' fields
fn fullName[..r](
  person: {first: String, last: String, ..r}
) -> {first: String, last: String, fullName: String, ..r} / Pure {
  {fullName: person.first ++ " " ++ person.last, ..person}
}

// Update a specific field
fn mapName[..r](
  record: {name: String, ..r},
  f: String -> String / Pure
) -> {name: String, ..r} / Pure {
  {name: f(record.name), ..record}
}

// Remove a field from a record
fn removeName[..r](
  record: own {name: String, ..r}
) -> (own String, own {..r}) / Pure {
  (record.name, record \ name)
}

fn main() -> () / Pure {
  let alice = {name: "Alice", age: 30, role: "Engineer"};
  let bob = {name: "Bob", department: "Sales"};

  // Both work despite different record shapes
  let greeting1 = greet(alice);
  let greeting2 = greet(bob);

  let person = {first: "Jane", last: "Doe", email: "jane@example.com"};
  let withFull = fullName(person);
  // withFull now has: {first, last, fullName, email}
}
