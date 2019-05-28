type token =
  | ID of (string)
  | BV of (string * string)
  | EOF
  | LSQUARE
  | RSQUARE
  | LBRACE
  | RBRACE
  | ASARRAY
  | ARROW

val model :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string * Z3TextualParsedModel.t) list
