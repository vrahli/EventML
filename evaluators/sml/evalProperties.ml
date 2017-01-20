module T = NuprlTerms

module KEY = struct type t = string let compare = String.compare end
module SET = Set.Make(KEY)

let addListSET lst set = List.fold_right SET.add lst set
let null lst = (try List.hd lst; false with _ -> true)

let all_set = addListSET ["callbyvalueall";"eq_term"] SET.empty

let set1 =
  addListSET
    ["limited_type_case";"minus";"isinr";"isinl";
     "ispair";"isint";"islambda";"isatom2";"isaxiom";
     "spread";"decide";"apply";
     "callbyvalue";"callbyvalueall";
     "ind";"!wait"]
    SET.empty

let set2 =
  addListSET
    ["add";"subtract";"multiply";"divide";"remainder";
     "less";"int_eq";"atom_eq";"eq_term"]
    SET.empty

let set3 = addListSET ["!abstraction"] SET.empty

let set_all1 =
  addListSET
    ["inl";"inr";"function";"product";
     "isect";"tunion";"set";"quotient"]
    SET.empty

let set_all2 =
  addListSET
    ["pair";"union";"subtype_rel"]
    SET.empty

let set_all3 = addListSET ["equal"] SET.empty

let is_eval_all id = SET.mem id all_set

let num_principals id =
  if SET.mem id set3
  then 3
  else if SET.mem id set2
  then 2
  else if SET.mem id set1
  then 1
  else 0

let num_principal_all id =
  if SET.mem id set_all3
  then 3
  else if SET.mem id set_all2
  then 2
  else if SET.mem id set_all1
  then 1
  else 0

let dummy_opid = "dummy"
let dummy_term = T.mk_nuprl_simple_term dummy_opid []

let abs_ref = ref ("", dummy_term, dummy_term)

let tof_ref = ref dummy_term

let found_user : T.nuprl_term option ref = ref None

let set_found_user t = found_user := Some t

let get_found_user u =
  let t = !found_user in
  let _ = found_user := None in
  match t with
    Some x -> x
  | None -> u

let is_empty_user_lib term = null (T.subterms term)

let mk_ilibrary t u =
  if is_empty_user_lib u
  then t
  else T.mk_nuprl_simple_term "!library" [t; u]

let rec strip_ilib term =
  let ((opid, params), bterms) = T.dest_term term in
  if opid = "!library"
  then
    let (vars,rterm) = List.hd bterms in
    strip_ilib (T.rterm2term rterm)
  else
    let lst = List.map (fun (vars, t) -> (vars, strip_ilib (T.rterm2term t))) bterms in
    T.mk_nuprl_term (opid, params) lst

let rec search_in_user user opid =
  match T.dest_simple_term user with
    (_, [lhs; rhs; tail]) ->
      let lhs  = T.rterm2term lhs   in
      let rhs  = T.rterm2term rhs   in
      let tail = T.rterm2term tail  in
      let id   = T.opid_of_term lhs in
      if id = opid
      then (abs_ref := ("", lhs, rhs);
	    set_found_user tail;
	    true)
      else search_in_user tail opid
  | _ -> false

let search_in_nuprl (abs : T.abs_lib) opid term =
  try
    let {T.id; sign; obid; lhs; rhs; wfs} = T.find_sign abs term in
    (abs_ref := (obid, T.rterm2term lhs, T.rterm2term rhs); true)
  with _ -> false

let search_in_tof (tof : T.tof_lib) obid =
  try (tof_ref := T.get (T.MAP.find obid (!tof)); true)
  with _ -> false

let is_abstraction_term ({T.abs; tof}, user) term =
  let opid = T.opid_of_term term in
  search_in_user user opid
|| search_in_nuprl abs opid term

let is_termof_term ({T.abs; tof}, user) term =
  if T.is_nuprl_term "TERMOF" term
  then
    let params = T.parameters_of_term term in
    let obidop = T.get_obid_parameters params in
    match obidop with
      Some obid -> search_in_tof tof obid
    | None -> false
  else false

let unfold_abs clos term =
  let (obid, lhs, rhs) = !abs_ref in
  T.unfold_ab clos term lhs rhs

let ct_unfold_abs clos term =
  let (obid, lhs, rhs) = !abs_ref in
  T.ct_unfold_ab clos term lhs rhs

let unfold_tof term = !tof_ref
