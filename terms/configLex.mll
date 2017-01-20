{
 open ConfigParse

 let line = ref 1

 let batom = ref false

 let buffer = ref ""
 let flush () = buffer := ""
 let add_to_buffer str = buffer := (!buffer) ^ str
}

let digitDec     = ['0' - '9']
let boolean      = "true" | "false"
let numeral      = ['1' - '9'] digitDec*
let digitDecSeq  = digitDec+
let integerDec   = "~" digitDecSeq | digitDecSeq
let natDec       = digitDecSeq
let esc          = ['\t' ' ']+
let letter       = ['a' - 'z' 'A' - 'Z']
let colon        = ":"
let star         = "*"
let plus         = "+"
let prime        = "'"
let minus        = "-"
let at           = "@"
let append       = "++"
let presymb      = ['!' '%' '&' '$' '#' '/' '<' '=' '?' '\\' '~' '^' '|' '>' '-']
let symbol       = colon | plus | at | presymb | star
let symboltc     = colon | plus | at | presymb
let symbolstc    = symboltc | symbol symbol+
let alphadigit   = letter | digitDec | "_" | prime | minus
let tycon        = letter alphadigit* | symbolstc
let ident        = letter alphadigit* | symbol+
let stident      = (alphadigit | plus | at | star | presymb)+

rule token = parse
  | "\n"           {line := !line + 1; flush (); token lexbuf}
  | esc            {();
	 	    if !batom
	 	    then let x = !buffer in (flush (); ID (x))
	 	    else (flush (); token lexbuf)}
  | "["            {LLIST}
  | "]"            {RLIST}
  | "{"            {LBRACE}
  | "}"            {RBRACE}
  | ","            {COMMA}
  | ";"            {SEMICOLON}
  | ":"            {COLON}
  | "("            {LPAREN}
  | ")"            {RPAREN}
  | star           {STAR}
  | "="            {EQUAL}
  | "->"           {ARROW}
  | "\\"           {LAMBDA}
  | "."            {DOT}
  | "and"          {AND}
  | "if"           {IF}
  | "then"         {THEN}
  | "else"         {ELSE}
  | "LOC"          {LOC}
  | "DEQ"          {DEQ}
  | "TYPE"         {TYPE}
  | "%locations"   {LOCATIONS}
  | "%parameters"  {PARAMETERS}
  | "%messages"    {MESSAGES}
  | "%databases"   {EOF}
  | "%connections" {CONNECTIONS}
  | "external"     {EXTERNAL}
  | numeral as n   {();
		    if !batom
		    then (add_to_buffer n; token lexbuf)
		    else NUM (n)}
  | natDec as n    {();
		    if !batom
		    then (add_to_buffer n; token lexbuf)
		    else INT (n)}
  | ident as i     {();
		    if !batom
		    then (add_to_buffer i; token lexbuf)
		    else ID (i)}
  | "``"           {();
		    if !batom
		    then (batom := false; let x = !buffer in (flush (); RATOMS (x)))
		    else (batom := true; LATOMS)}
  | plus    as s   {();
		    if !batom
		    then (add_to_buffer (String.make 1 s); token lexbuf)
		    else failwith ((Lexing.lexeme lexbuf) ^ ": mistake at line " ^ string_of_int !line)}
  | at      as s   {();
		    if !batom
		    then (add_to_buffer (String.make 1 s); token lexbuf)
		    else failwith ((Lexing.lexeme lexbuf) ^ ": mistake at line " ^ string_of_int !line)}
  | star    as s   {();
		    if !batom
		    then (add_to_buffer (String.make 1 s); token lexbuf)
		    else failwith ((Lexing.lexeme lexbuf) ^ ": mistake at line " ^ string_of_int !line)}
  | presymb as s   {();
		    if !batom
		    then (add_to_buffer (String.make 1 s); token lexbuf)
		    else failwith ((Lexing.lexeme lexbuf) ^ ": mistake at line " ^ string_of_int !line)}
  | _              {failwith ((Lexing.lexeme lexbuf) ^ ": mistake at line " ^ string_of_int !line)}
