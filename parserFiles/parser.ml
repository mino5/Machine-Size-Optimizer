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

open Parsing;;
# 1 "parser.mly"

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
# 32 "parser.ml"
let yytransl_const = [|
  259 (* IF *);
  260 (* DECISION *);
  261 (* CASE *);
  262 (* ELSEIF *);
  263 (* ARM *);
  264 (* EQUALS *);
  265 (* AND *);
  266 (* OR *);
  267 (* VAR *);
  268 (* WILDCARD *);
  269 (* LPAR *);
  270 (* RPAR *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\005\000\007\000\
\007\000\003\000\009\000\009\000\009\000\006\000\008\000\013\000\
\013\000\010\000\010\000\010\000\012\000\011\000\011\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\
\002\000\004\000\009\000\005\000\008\000\005\000\007\000\000\000\
\002\000\005\000\004\000\004\000\004\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\024\000\000\000\004\000\001\000\003\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\010\000\
\000\000\000\000\022\000\023\000\000\000\000\000\000\000\000\000\
\016\000\016\000\000\000\000\000\000\000\008\000\000\000\000\000\
\000\000\006\000\012\000\000\000\000\000\000\000\019\000\017\000\
\020\000\000\000\021\000\000\000\000\000\009\000\018\000\000\000\
\000\000\007\000\000\000\000\000\000\000\000\000\004\000\013\000\
\000\000\011\000\000\000\000\000\000\000\014\000\000\000\015\000"

let yydgoto = "\002\000\
\004\000\005\000\008\000\009\000\042\000\050\000\037\000\046\000\
\012\000\040\000\021\000\023\000\032\000"

let yysindex = "\021\000\
\012\255\000\000\000\000\000\000\248\254\000\000\000\000\000\000\
\003\255\000\000\004\255\014\255\013\255\002\255\016\255\000\000\
\009\255\017\255\000\000\000\000\030\255\022\255\021\255\016\255\
\000\000\000\000\023\255\024\255\033\255\000\000\036\255\253\254\
\255\254\000\000\000\000\026\255\007\255\028\255\000\000\000\000\
\000\000\010\255\000\000\037\255\017\255\000\000\000\000\039\255\
\017\255\000\000\034\255\032\255\013\255\035\255\000\000\000\000\
\017\255\000\000\001\255\038\255\017\255\000\000\040\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\249\255\000\000\000\000\000\000\000\000\
\238\255\244\255\000\000\026\000\025\000"

let yytablesize = 54
let yytable = "\027\000\
\018\000\010\000\019\000\010\000\006\000\007\000\013\000\014\000\
\015\000\017\000\039\000\017\000\041\000\020\000\061\000\011\000\
\024\000\025\000\026\000\044\000\045\000\001\000\048\000\049\000\
\003\000\017\000\052\000\016\000\022\000\011\000\054\000\028\000\
\029\000\030\000\036\000\034\000\038\000\035\000\060\000\043\000\
\057\000\047\000\063\000\051\000\053\000\056\000\055\000\059\000\
\058\000\031\000\033\000\062\000\000\000\064\000"

let yycheck = "\018\000\
\013\000\001\001\001\001\001\001\013\001\014\001\003\001\004\001\
\005\001\013\001\014\001\013\001\014\001\012\001\014\001\013\001\
\008\001\009\001\010\001\013\001\014\001\001\000\013\001\014\001\
\013\001\013\001\045\000\014\001\013\001\013\001\049\000\002\001\
\011\001\013\001\002\001\013\001\001\001\014\001\057\000\014\001\
\053\000\014\001\061\000\007\001\006\001\014\001\013\001\055\000\
\014\001\024\000\026\000\014\001\255\255\014\001"

let yynames_const = "\
  IF\000\
  DECISION\000\
  CASE\000\
  ELSEIF\000\
  ARM\000\
  EQUALS\000\
  AND\000\
  OR\000\
  VAR\000\
  WILDCARD\000\
  LPAR\000\
  RPAR\000\
  "

let yynames_block = "\
  INT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'charac) in
    Obj.repr(
# 25 "parser.mly"
                         ( _2 )
# 147 "parser.ml"
               : character))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
                         ( Character([]) )
# 153 "parser.ml"
               : 'charac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'charac) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 27 "parser.mly"
                  ( let Character(ruleList) = _1 in 
				   Character(ruleList @ [_2])	)
# 162 "parser.ml"
               : 'charac))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
                            ( [] )
# 168 "parser.ml"
               : 'ints))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ints) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 31 "parser.mly"
                            ( _1 @ [_2] )
# 176 "parser.ml"
               : 'ints))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser.mly"
                            ( [] )
# 182 "parser.ml"
               : 'elseifs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'elseifs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'elseif) in
    Obj.repr(
# 34 "parser.mly"
                     ( _1 @ [_2] )
# 190 "parser.ml"
               : 'elseifs))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                            ( [] )
# 196 "parser.ml"
               : 'arms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'arms) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arm) in
    Obj.repr(
# 37 "parser.mly"
                            ( _1 @ [_2] )
# 204 "parser.ml"
               : 'arms))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ints) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 39 "parser.mly"
                                     ( Rule(_2,_3) )
# 212 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'elseifs) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 41 "parser.mly"
                                                       ( If(_3,_4,_6,_8) )
# 222 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'new_state) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 42 "parser.mly"
                                                ( Decision(_3,_4) )
# 230 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'variable) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'arms) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 43 "parser.mly"
                                                ( Case(_3,_5,_7) )
# 239 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 45 "parser.mly"
                                    ( ElseIf(_3,_4) )
# 247 "parser.ml"
               : 'elseif))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ints) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 47 "parser.mly"
                                        ( Arm(_4,_6) )
# 255 "parser.ml"
               : 'arm))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                            ( [] )
# 261 "parser.ml"
               : 'conds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'conds) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cond) in
    Obj.repr(
# 50 "parser.mly"
                      ( _1 @ [_2] )
# 269 "parser.ml"
               : 'conds))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 52 "parser.mly"
                                      ( Equals(_3, _4) )
# 277 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'conds) in
    Obj.repr(
# 53 "parser.mly"
                               ( And(_3) )
# 284 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'conds) in
    Obj.repr(
# 54 "parser.mly"
                               ( Or(_3) )
# 291 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "parser.mly"
                                      ( Var(_3) )
# 298 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 58 "parser.mly"
                      ( NewState(_1) )
# 305 "parser.ml"
               : 'new_state))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
               ( WildCard)
# 311 "parser.ml"
               : 'new_state))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : character)
;;
# 61 "parser.mly"

# 338 "parser.ml"
