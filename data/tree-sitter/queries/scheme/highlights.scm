; A highlight query can override the highlights queries before it.
; So the order is important.
; We should highlight general rules, then highlight special forms.
(number) @number
(character) @character
(boolean) @boolean
(string) @string
(symbol) @variable

[
  (comment)
  (block_comment)
] @comment

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation
