type location    = string
type host        = string
type port        = int
type address     = location * host * port
type group_name  = string
type member      = bool
type group       = group_name * member * address list
type conn        = group_name * group_name
type parameter   = string * NuprlTerms.nuprl_term
type message     = NuprlTerms.nuprl_term * NuprlTerms.nuprl_term * NuprlTerms.nuprl_term
type loc_message = location * message

val parse :
    string     (* file to parse *)
  -> (group list * conn list * parameter list * loc_message list)

val parseString :
    string     (* string to parse *)
  -> (group list * conn list * parameter list * loc_message list)
