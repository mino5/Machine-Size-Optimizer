(* Dominik Szczepanski - modu³ zamieniajacy ifa na case'a (jesli mozna i sie oplaca) *)
open Parser
open List
open SizeCounter
open Sort

module type IFTOCASE =
sig

	type stateMachine
	type rule
	type statement
	type cond
	type arm
	type variable
	
	val convert: stateMachine -> stateMachine
	val convert_rule : rule -> rule
	val convert_statement : statement -> statement
	
	val cond_variables : cond -> variable list
	val all_variables : statement -> variable list
	
	
end;;


module IfToCase : IFTOCASE with type stateMachine = Parser.character with type rule = Parser.rule with type statement = Parser.statement 
with type cond = Parser.cond with type arm = Parser.arm with type variable = Parser.variable = 
struct

	type stateMachine = Parser.character;;
	type rule = Parser.rule;;
	type statement = Parser.statement;;
	type cond = Parser.cond;;
	type arm = Parser.arm;;
	type variable = Parser.variable;;
	
	let uniq lst = 
		let unique_set = Hashtbl.create (List.length lst) in
			List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
			Hashtbl.fold (fun x () xs -> x :: xs) unique_set [];;
											
	let rec cond_variables (cond : cond) : variable list = 	
											match cond with
												| Equals (x, v) -> [x] 
												| And condlist | Or condlist -> cond_variables_aux condlist 
																 and cond_variables_aux l = match l with 
																				|	[] -> []
																				|   x::xs ->  cond_variables x @ cond_variables_aux xs ;;
												
	
	let all_variables (statement : statement) : variable list = let all_variables_aux statement = match statement with
												| Case (_, _, _) -> [] 
												| Decision (_, _) -> [] 
												| If (cond, ifstatement1, elseiflist, ifstatement2) ->
												let elseifcond = map (fun x -> let ElseIf (c, st) = x in cond_variables c) elseiflist in
												match elseifcond with
													| x::xs -> let l = x::xs in
														 cond_variables cond @ (flatten l)
													| _ -> [] 
													
												in let x = all_variables_aux statement in uniq x;;
												
												
	(* jezeli mamy warunek typu x = 1 lub x = 2 lub y = 3 to nie zamieniamy na case'a inaczej od razu zwracamy co w tym or jest *)
	let rec not_exists_other_variable_in_or variable (condlist : cond list) = match condlist with
												 | [] -> ([], true)
												 | x::xs -> match x with
														| Equals (var, v) -> if (var = variable) then 
														let (a,b) = not_exists_other_variable_in_or variable xs in
														(v :: a, b)
														else ([], false)
														| And clist -> ([], false) (* jezeli jest AND to nie dostane juz np (X = 5 AND X = 7) po wczesniejszej opt *)
														| Or clist -> let (a,b) = 
														not_exists_other_variable_in_or variable clist in 
														let (c,d) = not_exists_other_variable_in_or variable xs in
														(a @ c, b && d) ;; 
														
	(* zwraca wartosc do arma i liste pozostalych cond w AND, ktore wstawimy do IFa w ARMie *)
	let rec take_from_and variable (condlist : cond list) = match condlist with
													| [] -> ([], []) 
													| x::xs -> match x with
															| Equals (var, v) -> let (a,b) = take_from_and variable xs in 
															if (var = variable) then ([v],xs)
															else (a, x :: b) 
															| Or clist -> let (c,d) = not_exists_other_variable_in_or variable clist in (* jezeli jest OR i jeszcze nie mamy nic do arma to jezeli sie da zbieramy z niego equalsy *)
																		  if (d = true) then (c, xs) else ([],[])
															| And clist -> take_from_and variable clist ;;
															
															
					
	(* metoda bierze zmienna i warunek, zwraca liste wartosci zmiennej do arma oraz dodatkowy cond ktory pojawi sie w armie w IF *)
	let variable_othercond_if variable cond = match cond with
												| Equals (variable, v) -> ([v], And [])
												| Or condlist -> let (orlist, can_proceed) = not_exists_other_variable_in_or variable condlist in
																 if (can_proceed && (orlist <> [])) then (orlist, And []) (* or ale z samym podanym variable, jezeli nie wystepuje variable to urywamy case'a *)
																 else 
																	([], And []) (* urywamy case'a, reszta to ify lub case po czymœ innym ?*)
												| And condlist -> let (a,b) = take_from_and variable condlist in
																	if (a <> []) then (a, And b)
																  else 
																	([], And []) ;;
																	
	let rec variable_othercond_elseif variable elseiflist = match elseiflist with
													| [] -> []
													| x::xs -> let ElseIf (cond,st) = x in (variable_othercond_if variable cond, st) :: 
													variable_othercond_elseif variable xs;;

													
	(* przeglada liste elseif i zwraca dla kazdego statement, dodatkowo trzyma w akumulatorze dodatkowe warunki - byc moze utworzymy IF w ARMie 
	- o ile jest to mozliwe *)
	
	let rec is_case_possible_statements_elseif velseiflist condvalstacc = 
											match velseiflist with
												| [] -> (true, [])
												| x::xs -> let ((values, cond), st) = x in
															let (condacc, lastval, lastst) = List.hd condvalstacc in 
															if (values = []) then (false, [[], Decision (NewState 0, "CASEFAIL")])
																else 
																	if (cond = And [] && condacc = And []) then begin (* mamy ciag if x = 1 else if x = 2 itp *)
																		let newcondvalstacc = (cond, values, st) in
																		let (a,b) = is_case_possible_statements_elseif xs (newcondvalstacc :: condvalstacc) in
																			(a, ((values, st) :: b))
																	end		
																	
																	
																	else if (condacc = And [] && cond <> And []) then begin (* mamy ciag if x = 1 else if x = 2 i w pewnym momencie x = 5 AND y = 7 *)
																		let newcondvalstacc = (cond, values, st) in
																		let (a,b) = is_case_possible_statements_elseif xs ([newcondvalstacc]) in (* zapisujemy y = 7 w akumulatorze *)
																			(a, b)
																	end
																	
																	else if (condacc <> And [] && cond = And []) then (* mielismy gdzies np x = 2 AND y = 7 , teraz mamy np x = 3, w takim razie jezeli jest x = 2 to mozemy zrobic IFa w ARM *)
																				if (values = lastval) then begin (* mozemy zrobic IFa *)
																						 if ((List.length condvalstacc) = 1) then  (* sam if, bez elseif *) 
																							let ifst = If (condacc, lastst, [], st) in
																							let (a,b) = is_case_possible_statements_elseif xs [(cond, values, st)] in
																							(a, ((values, ifst) :: b)) 
																						
																				        else if ((List.length condvalstacc) > 1) then begin (* dorzucamy elseify *)
																							let elseifs = map (fun x -> let (cond,values,st) = x in ElseIf (cond,st)) condvalstacc in 
																							let (condacc2,lastval2,lastst2) = List.hd condvalstacc in 
																							let ifst = If (condacc2, lastst2, (List.tl elseifs), st) in
																							let (a,b) = is_case_possible_statements_elseif xs [(cond, values, st)] in
																							(a, ((values, ifst) :: b))
																	
																						end  
																						else (false, [])
																				end
																				else (false,[]) (* nie mozemy zrobic case *)
																	
																	else 
																	if (values = lastval) then begin
																		let newcondvalstacc = (cond, values, st) in  (* zbieramy do elseifow warunki i st *)
																		let app = rev_append condvalstacc [newcondvalstacc] in 
																		is_case_possible_statements_elseif xs app
																	end else (false, []) ;; (* mamy przypadek x = 5 AND y = 7 AND z = 8 AND v = 9, potem np x = 5 AND p = 5 a potem X = 6 *)

	(* zwraca pare (czy case mozliwy, lista par wartosci i statement *)
	let is_case_possible_statements variable elseiflist cond st1 = 
													let (values, cond2) = variable_othercond_if variable cond in
														match values with
															| [] -> (false, [])
															| l -> 
																if (List.length elseiflist > 0) then
																	let velseiflist = variable_othercond_elseif variable elseiflist in
																	let (a, b) = is_case_possible_statements_elseif velseiflist [(cond2,values,st1)] in
																	if (List.length b > 0) then 
																		let (fval, stmts) = List.hd b in
																			if (fval = values) then (a, b) else
																		(a, ((l, st1) :: b) ) 
																	else (false, []) 
																else (a, [(l, st1)]) ;;
												
												
												
	let create_case_for_var variable if_st = let If (cond, st1, elseiflist, st2) = if_st in
										     let (a, valuestatementlist) = is_case_possible_statements variable elseiflist cond st1 in
											 if (a = true) then (* case mozliwy *)
											 match valuestatementlist with
												| [] -> Decision (NewState 0, "CASE CREATE FAIL")
												| l -> let mapl = map (fun x -> let (values, st) = x in Arm (values,st)) l in																							
																Case (variable, mapl, st2)  	
																
											else Decision(NewState 1, "CASE IMPOSSIBLE");;
																	
												
														
																	
	(* sprawdza mozliwosc utworzenia case'a po wszystkich zmiennych i jesli sie uda wybiera najlepszego, porownuje z rozmiarem ifa i zwraca ifa badz case'a *)
	let create_case if_st = let If(cond, st1, elseiflist, st2) = if_st in 
							let allvars = all_variables if_st in 
							let mapvars = map (fun x -> let y = create_case_for_var x if_st in
											   match y with 
												| Case _ -> (y, SizeCounter.statement_size y)
												| _ -> (y, -1)
							) allvars in 
								let mapvarsfiltered = filter (fun x -> let (a,b) = x in (b <> -1)) mapvars in
								if (List.length mapvarsfiltered > 0) then begin
									let sorted = sort (fun x y -> let (a,b) = x in let (c,d) = y in compare b d) mapvarsfiltered in (* sortujemy po wielkosci *)
									let if_st_size = SizeCounter.statement_size if_st in
									let (best_case, size_of_best_case) = List.hd sorted in 
									if (if_st_size > size_of_best_case) then best_case 
										else if_st
								end
								else if_st ;;
							
														
													
											
	
	let rec convert_statement (statement : statement) : statement = match statement with
					| Decision (a, str) -> Decision (a, str)
					| If (cond, ifstatement1, elseiflist, ifstatement2) -> 
							let convstat1 = convert_statement ifstatement1 in
							let convstat2 = convert_statement ifstatement2 in	
							let elseiflistconv = map (fun x -> let ElseIf (c, st) = x in ElseIf (c, convert_statement st)) elseiflist in
							create_case (If (cond, convstat1, elseiflistconv, convstat2))
					| Case (var, armlist, casestatement) ->
							let convcasest = convert_statement casestatement in
							let armlistconv = map (fun x -> let Arm(l,s) = x in Arm(l, convert_statement s)) armlist in
							Case (var, armlistconv, convcasest) ;;
															
															  
												
	
	let convert_rule (rule : rule) : rule = let Rule (r,s) = rule in Rule(r, convert_statement s);;
	
	let convert (machine : stateMachine) : stateMachine = let Character rules = machine in match rules with
											| [] -> machine
											| l -> Character (map (convert_rule) l);;

end;;
