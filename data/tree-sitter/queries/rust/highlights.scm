(macro_invocation
  macro: (identifier) @function.macro
  "!" @function.macro)

(line_comment) @comment
(block_comment) @comment

(char_literal) @character

(string_literal) @string
(raw_string_literal) @string

; Keywords
[
  "use"
  "mod"
] @keyword.import

(use_as_clause
  "as" @keyword.import)

"fn" @keyword.function

[
  "return"
  "yield"
] @keyword.return

[
  "default"
  "impl"
  "let"
  "move"
  "unsafe"
  "where"
] @keyword

[
  "enum"
  "struct"
  "union"
  "trait"
  "type"
] @keyword.type

[
  "async"
  "await"
] @keyword.coroutine

"try" @keyword.exception

[
  "ref"
  "pub"
  (mutable_specifier)
  "const"
  "static"
  "dyn"
  "extern"
] @keyword.modifier

[
  "if"
  "else"
  "match"
] @keyword.conditional

[
  "break"
  "continue"
  "in"
  "loop"
  "while"
] @keyword.repeat

"for" @keyword

; Punctuation
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

(closure_parameters
  "|" @punctuation.bracket)

(type_arguments
  [
    "<"
    ">"
  ] @punctuation.bracket)

(type_parameters
  [
    "<"
    ">"
  ] @punctuation.bracket)

(bracketed_type
  [
    "<"
    ">"
  ] @punctuation.bracket)

(for_lifetimes
  [
    "<"
    ">"
  ] @punctuation.bracket)

[
  ","
  "."
  ":"
  "::"
  ";"
  "->"
  "=>"
] @punctuation.delimiter


; Operators
[
  "!"
  "!="
  "%"
  "%="
  "&"
  "&&"
  "&="
  "*"
  "*="
  "+"
  "+="
  "-"
  "-="
  ".."
  "..="
  "..."
  "/"
  "/="
  "<"
  "<<"
  "<<="
  "<="
  "="
  "=="
  ">"
  ">="
  ">>"
  ">>="
  "?"
  "@"
  "^"
  "^="
  "|"
  "|="
  "||"
] @operator

(use_wildcard
  "*" @character.special)

(remaining_field_pattern
  ".." @character.special)

; (attribute_item
;   "#" @punctuation.special)

; (inner_attribute_item
;   [
;     "!"
;     "#"
;   ] @punctuation.special)
