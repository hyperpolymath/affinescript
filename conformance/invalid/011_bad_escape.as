// Conformance test: invalid escape sequence (lex error)

fn test() -> () {
  let s = "bad \q escape";
}
