(* Dominik Szczepanski - modu³ liczacy rozmiar maszyny *)
open Parser
open List

module type SIZECOUNTER =
sig

	type stateMachine
	type rule
	type statement
	type cond
	type arm
					
	(* funkcja liczaca rozmiar maszyny *)
	val size : stateMachine -> int
	
	(* funkcje pomocnicze liczace rozmiar *)
	val rule_size : rule -> int
	val statement_size : statement -> int
	val cond_size : cond -> int
	
	(* inne funkcje pomocnicze *)
	val mapfoldsum : ('a -> int) -> 'a list -> int
	val spin_cost : arm list -> int
			
end;;
			
		
module SizeCounter : SIZECOUNTER with type stateMachine = Parser.character with type rule = Parser.rule with type statement = Parser.statement 
with type cond = Parser.cond with type arm = Parser.arm = 
struct

	type stateMachine = Parser.character;;
	type rule = Parser.rule;;
	type statement = Parser.statement;;
	type cond = Parser.cond;;
	type arm = Parser.arm;;
	
	let mapfoldsum func l = List.fold_left (+) 0 (List.map func l);;
	
	let list_minmax intlist = let x = List.fold_left min (List.hd intlist) intlist in 
							  let y = List.fold_left max (List.hd intlist) intlist in (x,y);;
	
	let spin_cost (armlist:arm list) : int = let llist = List.map (fun arm -> let Arm (f,st) = arm in f) armlist in
											 let llistminmax = List.map list_minmax llist in 
											 let llistminmaxFirst = List.hd llistminmax in 
											 let (minimum, maximum) = 
											 List.fold_left (fun x y -> let (xa,xb) = x in let (ya,yb) = y in ( (min xa ya), (max xb yb))) 
											 llistminmaxFirst llistminmax 
											 in maximum - minimum + 1;;
												 
												 
	
	let rec cond_size (cond:cond) : int = match cond with
								| Equals (v,k) -> 6
								| And condlist -> mapfoldsum cond_size condlist
								| Or condlist -> mapfoldsum cond_size condlist ;;
								
								
	let rec statement_size (statement:statement) : int = match statement with 
								| Decision (a, str) -> if (a = WildCard) then 3 else 4
								| If (cond, ifstatement1, elseiflist, ifstatement2) -> cond_size cond + 
								statement_size ifstatement1 + 
								statement_size ifstatement2 +
								mapfoldsum (fun x -> let ElseIf (cond,elseifst) = x in statement_size elseifst + cond_size cond) elseiflist
								| Case (var, armlist, casestatement) -> 10 + statement_size casestatement + 
								 mapfoldsum (fun x -> let Arm (l,s) = x in statement_size s) armlist + spin_cost armlist;;
								
	
	let rule_size (rule:rule) : int = let Rule (r,s) = rule in statement_size s;;
		
	let size (machine : stateMachine) : int = let Character rules = machine in match rules with 
					|  [] -> 0
					|  l -> mapfoldsum rule_size l;;

end;;


