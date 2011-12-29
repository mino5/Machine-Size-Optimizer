%{
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
%}

%token <int> INT
%token <string> STRING 
%token IF DECISION CASE ELSEIF ARM EQUALS AND OR VAR WILDCARD
%token LPAR RPAR

%start input
%type <character> input

%%

input: LPAR charac RPAR  { $2 }
charac: /* empty */      { Character([]) }
	| charac rule    { let Character(ruleList) = $1 in 
				   Character(ruleList @ [$2])	}

ints:  /* empty */          { [] }
      | ints INT            { $1 @ [$2] }

elseifs: /* empty */        { [] }
	| elseifs elseif    { $1 @ [$2] }

arms:   /* empty */         { [] }
        | arms arm          { $1 @ [$2] }

rule:  LPAR ints stmt RPAR           { Rule($2,$3) }

stmt: LPAR IF cond stmt LPAR elseifs RPAR stmt RPAR    { If($3,$4,$6,$8) }
	| LPAR DECISION new_state STRING RPAR          { Decision($3,$4) }
	| LPAR CASE variable LPAR arms RPAR stmt RPAR  { Case($3,$5,$7) }
	
elseif: LPAR ELSEIF cond stmt RPAR  { ElseIf($3,$4) }

arm: LPAR ARM LPAR ints RPAR stmt RPAR  { Arm($4,$6) }

conds: /* empty */          { [] }
	|conds cond          { $1 @ [$2] }
 
cond: LPAR EQUALS variable INT RPAR   { Equals($3, $4) }
	| LPAR AND conds RPAR         { And($3) }
	| LPAR OR conds RPAR          { Or($3) }

variable: LPAR VAR STRING RPAR        { Var($3) }

new_state: INT        { NewState($1) }
	| WILDCARD    { WildCard}
  
%%
