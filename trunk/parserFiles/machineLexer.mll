{
open Parser
open Printf
}
let digit = ['0'-'9']
let nonZeroDigit = ['1'-'9']
let integer = nonZeroDigit digit* | '0' 
let napis = '"' [^ '"' '\\']* '"'
rule token = parse
      | [' ' '\t' '\n']   {token lexbuf} 
      | integer as num
			  {
				INT (int_of_string num) }
      | napis as nap      {  let len = String.length nap in
				let subnap = String.sub nap 1 (len-2) in
				STRING (subnap)} 
      |'('                {LPAR}
      |')'                {RPAR}
      |"IF"               {IF}
      |"CASE"             {CASE}
      |"ELSEIF"           {ELSEIF}
      |"VAR"              {VAR}
      |"OR"               {OR}
      |"AND"              {AND}
      |"EQUALS"           {EQUALS}
      |"DECISION"         {DECISION}
      |"ARM"              {ARM}
      |'_'                {WILDCARD}
      | _           { token lexbuf}
      | eof         {raise End_of_file} 


