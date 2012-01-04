type newState = WildCard | NewState of int
		and variable = Var of string		
		and cond = Equals of variable * int | And of cond list | 
                   Or of cond list
		and statement = If of cond * statement * elseIf list * statement | 
                 Decision of newState * string | 
		 Case of variable * arm list * statement
		and arm = Arm of int list * statement
		and elseIf = ElseIf of cond * statement
		and rule = Rule of int list * statement
		and character = Character of rule list

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
