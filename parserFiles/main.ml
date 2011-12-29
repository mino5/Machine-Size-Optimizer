open Parser
open Printf
open MachineLexer

let main () = 
	let lexbuf = Lexing.from_channel stdin in
	let characta = Parser.input MachineLexer.token lexbuf in 
	characta

 

