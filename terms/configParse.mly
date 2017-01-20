%{
module T = NuprlTerms

module KEY  = struct type t = string let compare = String.compare end
module VARS = Set.Make(KEY)

type 'a t  = unit   -> 'a
type 'a u  = VARS.t -> 'a

type location    = string
type host        = string
type port        = int
type address     = location * host * port
type group_name  = string (* name of a group *)
type member      = bool   (* true if group is a member of the system *)
type group       = group_name * member * address list
type conn        = group_name * group_name
type parameter   = string * T.nuprl_term
type message     = T.nuprl_term * T.nuprl_term * T.nuprl_term
type loc_message = location * message

let get_locs_from_groups groups =
  List.fold_right
    (fun (_,_,locs) set ->
      List.fold_right
	(fun (loc,_,_) set -> VARS.add loc set)
	locs
	set)
    groups
    VARS.empty

%}


%token EOL EOF
%token LLIST
%token RLIST
%token LBRACE
%token RBRACE
%token COMMA
%token COLON
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LOC
%token DEQ
%token TYPE
%token LOCATIONS
%token PARAMETERS
%token MESSAGES
%token ARROW
%token STAR
%token LAMBDA
%token EQUAL
%token IF
%token THEN
%token ELSE
%token DOT
%token AND
%token LATOMS
%token EXTERNAL
%token CONNECTIONS
%token <string> RATOMS
%token <string> ID
%token <string> NUM
%token <string> INT

%start config

%type <unit -> ((string * bool * (string * string * int) list) list * (string * string) list * (string * NuprlTerms.nuprl_term) list * (string * (NuprlTerms.nuprl_term * NuprlTerms.nuprl_term * NuprlTerms.nuprl_term)) list)> config
%%


config :
| conf_groups conf_conns conf_params conf_msgs
    {fun () ->
      let grps  = $1 () in
      let conns = $2 (get_locs_from_groups grps) in
      let prams = $3 () in
      let msgs  = $4 () in
      (grps, conns, prams, msgs)}
| message
    {fun () -> ([], [], [], [("", $1 ())])}
| ID COLON message
    {fun () -> ([], [], [], [($1, $3 ())])}

conf_groups :
| conf_group
    {fun () -> [$1 ()]}
| conf_group conf_groups
    {fun () -> ($1 ()) :: ($2 ())}

conf_group :
| LOCATIONS ID locations
    {fun () -> ($2, true, $3 ())}
| LOCATIONS EXTERNAL ID locations
    {fun () -> ($3, false, $4 ())}

conn :
| ID ARROW ID
    {fun set ->
      if VARS.mem $3 set
      then failwith "error in configuration file: an external group can only connect to another group"
      else ($1, $3)}

conns :
| conn
    {fun set -> [$1 set]}
| conn conns
    {fun set -> ($1 set) :: ($2 set)}

conf_conns :
| CONNECTIONS conns {$2}

conf_params :
| PARAMETERS parameters {$2}

conf_msgs :
| {fun () -> []}
| MESSAGES messages {$2}

numeral :
| NUM {fun () -> $1}
| INT {fun () -> $1}

ip :
| ID {fun () -> $1}
| numeral DOT numeral DOT numeral DOT numeral
    {fun () ->
      let n1 = $1 () in
      let n2 = $3 () in
      let n3 = $5 () in
      let n4 = $7 () in
      n1 ^ "." ^
      n2 ^ "." ^
      n3 ^ "." ^
      n4}

location :
| ID COLON ip numeral {fun () -> ($1, $3 (), int_of_string ($4 ()))}

locations :
| location {fun () -> [$1 ()]}
| location locations {fun () -> ($1 ()) :: ($2 ())}

parameters :
| {fun () -> []}
| ID COLON TYPE LPAREN typ1 RPAREN parameters
    {fun () -> ($1, $5 ()) :: ($7 ())}
| ID COLON exp parameters
    {fun () -> ($1, $3 VARS.empty) :: ($4 ())}

atomlist :
| {fun () -> []}
| ID atomlist {fun () -> $1 :: ($2 ())}

atoms :
| LATOMS atomlist RATOMS {fun () -> ($2 ()) @ [$3]}

message :
| LPAREN atoms COMMA typ1 COMMA exp RPAREN
    {fun () ->
      let lst = List.map T.mk_regular_token_term ($2 ()) in
      let hdr = T.mk_nuprl_finite_list_term lst in
      (hdr, $4 (), $6 VARS.empty)}

messages :
| {fun () -> []}
| ID COLON message messages
    {fun () ->
	let (hdr, typ, exp) = $3 () in
	($1, (hdr, typ, exp)) :: ($4 ())}

explist1 :
| exp
    {fun vars -> [$1 vars]}
| exp SEMICOLON explist1
    {fun vars -> ($1 vars) :: ($3 vars)}

explist :
| {fun vars -> []}
| explist1 {$1}

exp :
| ID
    {fun vars ->
      if VARS.mem $1 vars
      then T.mk_variable_term $1
      else T.mk_nuprl_simple_term $1 []}
| atoms
    {fun vars ->
      T.mk_nuprl_finite_list_term (List.map T.mk_regular_token_term ($1 ()))}
| expint
    {fun vars -> $1 ()}
| exploc
    {fun vars -> $1 ()}
| expdeq
    {fun vars -> $1 ()}
| expeq
    {fun vars -> $1 ()}
| LLIST explist RLIST
    {fun vars -> T.mk_nuprl_finite_list_term ($2 vars)}
| LBRACE explist RBRACE
    {fun vars -> T.mk_nuprl_finite_list_term ($2 vars)}
| LAMBDA ID DOT exp
    {fun vars -> T.mk_lambda_term $2 ($4 (VARS.add $2 vars))}
| IF exp THEN exp ELSE exp
    {fun vars -> T.mk_nuprl_ite_term ($2 vars) ($4 vars) ($6 vars)}
| LPAREN exp COMMA exp RPAREN
    {fun vars -> T.mk_pair_term ($2 vars) ($4 vars)}
| LPAREN RPAREN
    {fun vars -> T.mk_nuprl_it_term}

expeq :
| expint EQUAL expint
    {fun () -> T.mk_nuprl_eq_int_term ($1 ()) ($3 ())}
| ID EQUAL expint
    {fun () -> T.mk_nuprl_eq_int_term (T.mk_variable_term $1) ($3 ())}
| expint EQUAL ID
    {fun () -> T.mk_nuprl_eq_int_term ($1 ()) (T.mk_variable_term $3)}
| exploc EQUAL exploc
    {fun () -> T.mk_nuprl_eq_loc_term ($1 ()) ($3 ())}
| ID EQUAL exploc
    {fun () -> T.mk_nuprl_eq_loc_term (T.mk_variable_term $1) ($3 ())}
| exploc EQUAL ID
    {fun () -> T.mk_nuprl_eq_loc_term ($1 ()) (T.mk_variable_term $3)}

expdeq :
| DEQ LPAREN typ1 RPAREN
    {fun () -> T.toDeq ($3 ())}

exploc :
| LOC LPAREN ID RPAREN
    {fun () -> T.mk_mkid_term $3}

expint :
| numeral {fun () -> T.mk_nuprl_small_integer_term (int_of_string ($1 ()))}

atty :
| ID
    {fun () ->
      match $1 with
	"Loc"    -> T.mk_nuprl_loc_term
      | "Int"    -> T.mk_nuprl_int_term
      | "Unit"   -> T.mk_nuprl_unit_term
      | "Bool"   -> T.mk_nuprl_bool_term
      | "Tok"    -> T.mk_nuprl_atom_term
      | "Atom"   -> T.mk_nuprl_atom_term
      | _        -> T.mk_nuprl_simple_term $1 []}
| LPAREN typ1 RPAREN
    {fun () -> $2 ()}

typ1 :
| typ {$1}
| typ ARROW typ
    {fun () -> T.mk_nuprl_fun_term ($1 ()) ($3 ())}
| typ STAR typ
    {fun () -> T.mk_nuprl_prod_term ($1 ()) ($3 ())}

typ :
| atty {$1}
| atty ID
    {fun () ->
      let t = $1 () in
      match $2 with
	"List" -> T.mk_nuprl_list_term t
      | _      -> T.mk_nuprl_simple_term $2 [t]}
