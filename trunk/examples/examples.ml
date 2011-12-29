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
and character = Character of rule list;; 



let st1 = If(
    And(
         [Equals(Var("kaka1"),2);
	  Equals(Var("kaka2"),3)]
	),
    Decision(NewState(4),"chuj"),
    [],
    Decision(NewState(2),"dupa")
  );;

let st2 = Case(
		Var("kaka1"),
		[Arm([1;2;3],st1);
		 Arm([4;5;6],Decision(NewState(1),"a chuj"))
		],
		Decision(NewState(2),"kakaakdas")
	       );; 

let rule1 = Rule([1;2;3], st1);;
let rule2 = Rule([4], st2);;




