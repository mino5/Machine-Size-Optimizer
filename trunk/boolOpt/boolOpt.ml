(* Dominik Rusak - optymalizacja instrukcji warunkowych               *)

(*type newState = WildCard | NewState of int
		and variable = Var of string		
		and cond = Equals of variable * int | And of cond list | 
                   Or of cond list
		and statement = If of cond * statement * elseIf list * statement | 
                 Decision of newState * string | 
		 Case of variable * arm list * statement
		and arm = Arm of int list * statement
		and elseIf = ElseIf of cond * statement
		and rule = Rule of int list * statement
		and character = Character of rule list;;*)

open Parser



let rec allsv cond acc =  
		match cond with 
			Equals(var,value) -> let (firsts, seconds) = List.split acc in
						if List.mem var firsts then 
						   let (var1, values) = List.find (fun (var1,_) -> var1 = var) acc in   
						    if List.mem value values then acc
						    else List.map (fun (val1,values) -> if val1 = var then (val1,value::values)
									                 else (val1,values) ) acc
						else (var, [value])::acc
			|And(condList) -> List.fold_left (fun acc con -> allsv con acc) acc condList
			|Or(condList) -> List.fold_left (fun acc con -> allsv con acc) acc condList;;
let allSensibleVars cond1 = allsv cond1 [];;

let allSensibleVars2 cond1 cond2 = allsv cond2 (allsv cond1 []);; 
	
let mama = [(Var("k1"),[1]);(Var("k2"),[1]);(Var("k3"),[1]);(Var("k4"),[1])];;


let rec pushAnd condList x = match condList with
			      [] -> Printf.printf "pushingAnd 0";[]
			      |(h::t) -> Printf.printf "pushingAnd %i " x; 
 					match h with 
					    Equals(_,_) as c -> Printf.printf "pushing_equals\n";let kaka = pushAnd t (x-1) in 
										Printf.printf "coming back %i" x ;c::(kaka)
					    |Or(_) as c ->Printf.printf "pushing_or\n"; c::(pushAnd t (x-1))
					    |And(condList) ->Printf.printf "pushing_and\n"; condList@(pushAnd t (x-1))
					    |_ -> Printf.printf "pushing___\n";pushAnd t (x-1);;
let rec pushOr condList = match condList with
			      [] -> []
			      |(h::t) -> Printf.printf "pushingOr ";
					 match h with 
					    Equals(_,_) as c -> c::(pushOr t)
					    |And(_) as c -> c::(pushOr t)
					    |Or(condList) -> condList@(pushOr t);;

let rec andsOrs cond = Printf.printf "ANDSORS!!!! \n";match cond with 
			Equals(_,_) as c -> c
			|And([x]) -> andsOrs x
			|Or([x]) -> andsOrs x
			|And(condList) -> let newCondList1 = List.map (fun c -> andsOrs c) condList in
						Printf.printf "push and\n";
						Printf.printf "%i\n" (List.length newCondList1);
						let  newCondList2 = pushAnd newCondList1 (List.length newCondList1) in 
						Printf.printf "still alive and well";And(newCondList2)
			|Or(condList) -> let newCondList1 = List.map (fun c -> andsOrs c) condList in
						Printf.printf "push or";
						Printf.printf "%i\n" (List.length newCondList1);
						let  newCondList2 = pushOr newCondList1 in Or(newCondList2);;


let sensVarsSize sensVars = List.fold_left (fun acc (_,possList) -> acc * (List.length possList +1)) 1 sensVars;;

exception TooMuchWork;;
let allValuations sensVars =  let rec accAll values = 
				match values with
					[] -> []
					|[(v,t)] -> []::(List.map (fun x -> [(v,x)]) t )
					|(v,[])::t -> accAll t
					|(v,(h::t))::t2 -> (List.map (fun l -> (v,h)::l) (accAll t2)) @ (accAll ((v,t)::t2))  
			in let svs = sensVarsSize sensVars in 
				if svs > 12 or svs <1 then raise TooMuchWork else 
					Printf.printf "%i\n" (sensVarsSize sensVars); accAll sensVars;;




let rec eval cond vals = match cond with 
				Equals(var,value) -> (try
							match List.find (fun (var1,_) -> var = var1) vals with
								(_,value2) -> value = value2
						    with Not_found -> false)
				|And([]) -> true
				|Or([]) -> false
				|And(h::t) -> if eval h vals then eval (And(t)) vals else false
				|Or(h::t) -> if eval h vals then true else eval (Or(t)) vals;;		


let testIfEquivalent cond1 cond2 =let rec equivalent cond1 cond2 vals = 
						match vals with
						[] -> true
						|(h::t) -> if eval cond1 h = eval cond2 h then equivalent cond1 cond2 t
								else false
					in try let vals = allValuations (allSensibleVars2 cond1 cond2) in
						 equivalent cond1 cond2 vals
						with TooMuchWork -> cond1 = cond2;;					 


type termOrder = Equal | FirstImplies | SecondImplies | Contradictory | Nothing;;

let orderTerms cond1 cond2 =let rec order cond1 cond2 vals info = 
					match (vals,info) with
						(_,(false, false, false,false)) -> Nothing
						|([],(s1,s2,s3,s4)) -> if s1 then Equal	 
								    else if s2 then FirstImplies
								    else if s3 then SecondImplies
								    else if s4 then Contradictory
								    else Nothing 
						|(h::t,(s1,s2,s3,s4)) -> 
							let t1 = eval cond1 h and t2 = eval cond2 h in 
								if (t1 = t2) then order cond1 cond2 t (s1,s2,s3,s4 && not t1)
								else if t1 then order cond1 cond2 t (false,false,s3,s4)
								else order cond1 cond2 t (false,s2,false,s4)
				in try  let vals = allValuations (allSensibleVars2 cond1 cond2) in 
						order cond1 cond2 vals (true,true,true,true)
					with TooMuchWork -> if cond1 = cond2 then Equal else Nothing;; 
		
exception ContradictoryInAnd;;
let reduce1 condList ifOr = let (first, second) = if ifOr then (SecondImplies, FirstImplies) else (FirstImplies, SecondImplies) in 
			let rec oneTest h t = match t with 
					    [] -> (true,[])
					    |(h2::t2) ->  let r = orderTerms h h2 in 
								if r = Contradictory && not ifOr then raise ContradictoryInAnd 
									else ();
								if r = Equal or r = first then oneTest h t2
								else if r = second then (false,h2::t2)
								else let (tOrN,res) = oneTest h t2 in (tOrN,h2::res)
		        in let rec doForOnes condList = 
				match condList with
					[] -> []
					|(h::t) -> let (tOrN, res) = oneTest h t in 
							if tOrN then h::(doForOnes res)
							else doForOnes res
		        in try doForOnes condList
				with ContradictoryInAnd -> [Or([])];;		
			


  
let testIfFalse cond = 	let rec check cond vals = 
					match vals with 
					   [] -> Or([])
					   |(h::t) -> if eval cond h then cond else check cond t
				in try let vals = allValuations (allSensibleVars cond) in  check cond vals
					with TooMuchWork -> cond;;

exception ElemFound of int;;
let rec findTheSame cond1 cond2 where = match cond1 with 
					[] -> []
		    			|(h::t) -> try (List.fold_left (fun iter elem -> if orderTerms h elem = Equal then 
											raise (ElemFound iter) else iter+1) 1 cond2;
							findTheSame t cond2 (where+1))
						   with ElemFound(iter) -> (where,iter)::(findTheSame t cond2 (where+1));;



let rec takeElems indexList elems = let (_,same,part) = List.fold_left (fun (i,same,part) elem -> if List.mem i indexList then 
							(i+1,elem::same,part) else (i+1,same,elem::part)) (1,[],[]) elems 
				    in (same,part);;
						 
					
exception RuleNotApplied;;
let applyRule1 cond1 cond2 = if not (cond1 = cond2) then 
				match (cond1,cond2) with
				(And(condList1),And(condList2)) -> let (l1,l2) = List.split (findTheSame condList1 condList2 1) in
								    if l1 = [] then raise RuleNotApplied
								    else
								    let (same,part1) = takeElems l1 condList1 
									and (_,part2) = takeElems l2 condList2 in
									(* tu sprawdzenie, która czesc lepiej wziac,
									moze tez poprawic tego ora *)
									Printf.printf "applyrule and\n";
									let result = 	And((andsOrs (Or(part1@part2)))::same) 
										in Printf.printf "applay rule back\n";result
								      

				|(Or(condList1),Or(condList2)) ->let (l1,l2) = List.split (findTheSame condList1 condList2 1) in
								 if l1 = [] then raise RuleNotApplied
								 else   
								 let (same,part1) = takeElems l1 condList1 
									and (_,part2) = takeElems l2 condList2 in
									(* tu sprawdzenie, która czesc lepiej wziac *)
									Or((andsOrs (And(part1@part2)))::same)
				| _ -> raise RuleNotApplied
			     else raise RuleNotApplied;;

let oneWalk x cond1 allOldsRef = let changed = ref false in 
				 let rec oneWalk2 x cond1 allOldsRef = 
				    match cond1 with
				      [] -> changed := false;[]
				      |((refOld,h)::t) -> if not (x = h) then try let condRef = ref ((ref true,applyRule1 x h)::t) in
									         allOldsRef := false;
										 changed := true;
										 refOld := false;
										 !condRef
					                          with RuleNotApplied -> (ref false,h)::(oneWalk2 x t allOldsRef)
						else (ref false,h)::(oneWalk2 x t allOldsRef)
				in
				 let condResult = oneWalk2 x cond1 allOldsRef in 
						if !changed then List.filter (fun (_,y) -> not (y=x)) condResult else condResult;;

let reduce2 condList = let (newsList,equalsList) = List.partition (fun x -> match x with Equals(_,_) -> false 
											 |_ -> true) condList in 
			  
			let allOlds = ref false and cond1 = ref (List.map (fun x -> (ref true,x)) newsList) in 
			if !cond1 <> [] then (
				while not !allOlds do 
					allOlds := true;
					Printf.printf "%i" (List.length !cond1);
					cond1 := List.fold_left (fun acc (isNew,h)-> if !isNew then (
								Printf.printf "foundSomething new!";
								oneWalk h acc allOlds) else acc) !cond1 !cond1
					done)
			else cond1 := !cond1;
				let (_,result) = List.split (!cond1) in equalsList@result;;	

let rec simpl cond2 = let cond1 = testIfFalse cond2 in
				match cond1 with
				Equals(_,_) as c -> c
				|And(condList) -> Printf.printf "found and";
						  let newCondList = reduce2 (reduce1 (List.map (fun x -> simpl x) condList) false) in 
							(match newCondList with 
								[x] -> x	
								|newCondList -> And(newCondList))
				|Or(condList) ->  Printf.printf "found or";
						  let newCondList = reduce2 (reduce1 (List.map (fun x -> simpl x) condList) true) in 
							(match newCondList with 
								[x] -> x
								|newCondList -> Or(newCondList));;


let simplify cond = let s = simpl  (cond) in s (*Printf.printf "before second andsors ";andsOrs s*);;
let conds = ref [];;
let rec simplifyElseifs elseifs = match elseifs with
					[] -> []
					|(ElseIf(cond1,stmt1)::t) -> conds:= cond1::(!conds); (ElseIf(simplify cond1, simplifyStatement stmt1))::(simplifyElseifs t)

and  simplifyStatement stmt = match stmt with
			If(cond1,stmt1,elseifs,stmt2) -> conds:= cond1::(!conds);If(simplify cond1, simplifyStatement stmt1, 
								simplifyElseifs elseifs, simplifyStatement stmt2)
			|Case(var,arms,stmt1) -> Case(var, 
						List.map (fun a -> let Arm(valList,stmt2) = a in Arm(valList, simplifyStatement stmt2)) arms,
						simplifyStatement stmt1)
			| _ as c -> c;;
let checkRule r = match r with 
			Rule(rl,If(cond1,stmt1,elseifs,stmt2)) -> Rule(rl,simplifyStatement (If(cond1,stmt1,elseifs,stmt2)))
			| _ as rul -> rul;;
let checkIfWorks ch = match ch with
			Character(ruleList) -> Character(List.map (fun r -> checkRule r) ruleList);;


type impl = Implied | Contr | None;;
exception ImpliedOcc;;
exception ContrOcc;;

let optimizeAccToVals vals cond1 = let rec chVals vals c info = match (vals,info) with
						(_,(false,false)) -> None 
						|([],(s1,s2)) -> if s1 then Implied else if s2 then Contr else None
						|((h::t),(s1,s2)) -> let ev = eval c h in 
										if ev then chVals t c (s1,false)
										else chVals t c (false,s2)
								    
				  in	 
				   let rec 
					optAnd condList = match condList with 
							[] -> []
							|(h::t) -> let newH = optim h in (match newH with
									Or([]) -> raise ContrOcc
									|And([]) -> optAnd t
									|_ as c -> c::(optAnd t))
				    and optOr condList = match condList with 
							[] -> []
							|(h::t) -> let newH = optim h in (match newH with
									And([]) -> raise ImpliedOcc
									|Or([]) -> optOr t
									|_ as c -> c::(optOr t))
				

				    and optim cond1 = match cond1 with 
							And(condList) -> (try let newAnd = And(optAnd condList) in 
										match chVals vals newAnd (true,true) with
											None -> newAnd
											|Implied -> And([])
											|Contr -> Or([])
									  with ContrOcc -> Or([]))
							|Or(condList) -> (try let newOr = Or(optOr condList) in 
										match chVals vals newOr (true,true) with
											None -> newOr
											|Implied -> And([])
											|Contr -> Or([])
									  with ImpliedOcc -> And([]))
							|Equals(v,vValue) as c -> let imp = chVals vals c (true,true) in 
										if imp = None then c 
										else if imp = Implied then And([]) 
										else Or([])
			in optim cond1;;	


let bool1 = And([
		Or([
		     Equals(Var("a"),12);Equals(Var("b"),1);And([Equals(Var("c"),15)])		
		  ]);
		Or([
	             Equals(Var("a"),3);Equals(Var("a"),23)
		  ]);
		Or([
		     Equals(Var("c"),15);Equals(Var("d"),0);Equals(Var("c"),10)
		  ]);
		Equals(Var("a"),23)]
		);;

let bool9 = Or([
		     Equals(Var("a"),12);Equals(Var("b"),1);And([Equals(Var("c"),15)])		
		  ]);;
let bool10 = Or([
	             Equals(Var("a"),3);Equals(Var("a"),23)
		  ]);;
let bool11 = Or([
		     Equals(Var("c"),15);Equals(Var("d"),0);Equals(Var("c"),10);And([
											Equals(Var("d"),0);
											Equals(Var("a"),3)
										    ])
		  ]);;
let bool12 = And([Equals(Var("a"),23);Equals(Var("h"),1)]);;
let bool13 = And([Equals(Var("a"),23);Equals(Var("a"),1)]);;
let bool8 = And([
		bool9;
		bool10;
		bool11;
		bool12;
		]);;

let bool2 = Or([Equals(Var("b"),2) ; Equals(Var("f"),1)]);;
let bool3 = And([
		Or([
			Equals(Var("a"),12);
			Equals(Var("b"),1)
			]);
		Or([
			Equals(Var("a"),12);
			Equals(Var("c"),1)
			])
			
		]);;
let bool4 = Or([
		Equals(Var("a"),12);
		And([
			Equals(Var("b"),1);
			Equals(Var("c"),1)
		
		])
	      ]);;

let bool5 = Equals(Var("a"),12);;

let bool6 = And([Equals(Var("a"),12);Equals(Var("b"),3)]);;
		
let bool7 = Or([Equals(Var("a"),12);Equals(Var("b"),3)]);;

let badBool1 = And([
			Equals(Var("a"),12);Equals(Var("b"),3)
			]);;
let badBool2 = And([
			Equals(Var("a"),12);Equals(Var("c"),3)
			]);;
let badBool3 = And([
			Equals(Var("a"),12);Equals(Var("g"),3)
			]);; 
let l = [badBool1;badBool2;badBool3];;
let badBool = Or(l);;
