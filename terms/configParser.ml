module T = NuprlTerms

type location    = string
type host        = string
type port        = int
type address     = location * host * port
type group_name  = string
type member      = bool
type group       = group_name * member * address list
type conn        = group_name * group_name
type parameter   = string * T.nuprl_term
type message     = T.nuprl_term * T.nuprl_term * T.nuprl_term
type loc_message = location * message

let parse input =
  let inch   = open_in input in
  let lexbuf = Lexing.from_channel inch in
  let out    = ConfigParse.config ConfigLex.token lexbuf () in
  let _      = close_in inch in
  out

let parseString str =
  let lexbuf = Lexing.from_string str in
  let out    = ConfigParse.config ConfigLex.token lexbuf () in
  out
