; SPDX-License-Identifier: MIT
; Syntax highlighting queries for AffineScript

; Keywords
[
  "fn"
  "let"
  "mut"
  "type"
  "trait"
  "impl"
  "struct"
  "enum"
  "mod"
  "use"
  "pub"
  "extern"
] @keyword

; Control flow
[
  "if"
  "else"
  "match"
  "while"
  "for"
  "in"
  "loop"
  "break"
  "continue"
  "return"
] @keyword.control

; Effects
[
  "effect"
  "handle"
  "perform"
  "resume"
] @keyword.effect

; Quantities
[
  "linear"
  "affine"
  "unrestricted"
  "erased"
] @keyword.modifier

; Storage modifiers
[
  "move"
  "ref"
  "deref"
] @keyword.storage

; Type keywords
[
  "forall"
  "exists"
  "as"
  "where"
] @keyword.type

; Literals
(integer) @number
(float) @number
(boolean) @boolean
(string) @string
(char) @character
(unit) @constant.builtin

; Escape sequences
(escape_sequence) @string.escape

; Comments
(comment) @comment

; Identifiers
(identifier) @variable
(type_identifier) @type

; Function names
(fun_decl name: (identifier) @function)
(call_expr function: (identifier) @function.call)
(extern_decl (identifier) @function)

; Parameters
(param pattern: (identifier) @variable.parameter)

; Fields
(field_expr (identifier) @property)
(record_field (identifier) @property)
(record_pattern_field (identifier) @property)

; Effect names
(effect_decl name: (type_identifier) @type.effect)
(effect_expr (type_identifier) @type.effect)

; Trait names
(trait_decl name: (type_identifier) @type.trait)

; Type parameters
(type_params (type_identifier) @type.parameter)

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "!"
  "&"
  "|"
] @operator

; Punctuation
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  ";"
  ":"
  "::"
] @punctuation.delimiter

[
  "->"
  "=>"
  "="
  ".."
] @punctuation.special

; Visibility
(visibility) @keyword.modifier

; Built-in types
((type_identifier) @type.builtin
 (#match? @type.builtin "^(Int|Float|Bool|String|Unit|Never|Char|List|Vec|Option|Result|Ref)$"))

; Constants (SCREAMING_SNAKE_CASE)
((identifier) @constant
 (#match? @constant "^[A-Z][A-Z0-9_]*$"))

; Wildcards
"_" @variable.builtin

; Special highlighting for effect operators
(arrow_type "/" @keyword.operator.effect)
(effect_expr "|" @keyword.operator.effect)
