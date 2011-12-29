open Parser
open Printf
open MachineLexer

let main fileName = 

	let cin = 
		if String.length fileName > 0
		then open_in fileName
		else stdin
	in
	let lexbuf = Lexing.from_channel cin in
	let characta = Parser.input MachineLexer.token lexbuf in 
	       characta
	


    
 

