(macro_invocation
  macro: (identifier) @function.macro
  "!" @function.macro)

(line_comment) @comment
(block_comment) @comment

(char_literal) @character

(string_literal) @string
(raw_string_literal) @string
