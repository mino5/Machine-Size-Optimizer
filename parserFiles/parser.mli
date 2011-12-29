type token =
  | INT of (int)
  | STRING of (string)
  | IF
  | DECISION
  | CASE
  | ELSEIF
  | ARM
  | EQUALS
  | AND
  | OR
  | VAR
  | WILDCARD
  | LPAR
  | RPAR

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> character
