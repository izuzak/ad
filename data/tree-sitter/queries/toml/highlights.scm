(bare_key) @variable
(boolean) @boolean
(comment) @comment
(integer) @number
(float) @number

[
  (string)
  (quoted_key)
  (escape_sequence)
] @string

[
  (local_date)
  (local_date_time)
  (local_time)
  (offset_date_time)
] @character

"=" @operator

[
  "."
  ","
  "["
  "]"
  "[["
  "]]"
  "{"
  "}"
] @punctuation
