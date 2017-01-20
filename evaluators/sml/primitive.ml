module B = Tools
module T = NuprlTerms

(**** DO PRIMITIVE FUNCTIONS ****)

module KEY = struct type t = string let compare = String.compare end
module SET = Set.Make(KEY)

let addListSET lst set = List.fold_right SET.add lst set
let null lst = (try List.hd lst; false with _ -> true)

let primitive_values =
  addListSET
    ["pair";"lambda";"inl";"inr";
     "natural_number";"axiom";"token";"equal";
     "union";"product";"function";"isect";
     "int";"atom";"atomn";"set";"rec";"quotient";
     "subtype_rel";"tunion"]
    SET.empty

let primitive_test_op =
  addListSET ["inl"; "inr"; "pair"; "lambda"; "token"; "axiom"] SET.empty

let destruct_ut2_parameter param =
  if T.type_of_parameter param = "ut2"
  then T.value_of_parameter param
  else failwith "improper-parameter-type"

let do_primitive_test test value =
  let opid = T.opid_of_term value in
  if SET.mem opid (primitive_test_op) || T.is_nuprl_integer_term value
  then
    if test = "isint"
    then T.is_nuprl_integer_term value
    else if test = "isatom2"
    then
      match T.parameters_of_term value with
	[param] -> opid = "token" && B.can destruct_ut2_parameter param
      | _ -> failwith "do_primitive_test"
    else
      let list = [("ispair",  "pair");
		  ("isinl",   "inl");
		  ("isinr",   "inr");
		  ("isatom2", "token");
		  ("isaxiom", "axiom")] in
      opid = B.apply_alist list test
  else failwith ("do_primitive_test:" ^ test ^ "-" ^ opid ^ "(no_primitive_test)")

let is_primitive_value_prim term = SET.mem (T.opid_of_term term) primitive_values
let is_primitive_value_num  term = T.is_nuprl_minus_natural_number_term term

let is_primitive_value term =
  is_primitive_value_prim term || is_primitive_value_num term

let rec is_complete_primitive_value term =
  if T.is_nuprl_term "!closure" term
  then is_complete_primitive_value (T.subtermn 1 term)
  else if T.is_ct term
  then let (x,y) = T.dest_ct term in is_complete_primitive_value x
  else
    is_primitive_value term
      &&
    let (opr,bterms) = T.dest_term term in
    let unbound_subterms =
      List.fold_left
	(fun lst (vars,term) ->
	  if null vars
	  then (T.rterm2term term) :: lst
	  else lst)
	[]
	bterms in
    List.for_all is_complete_primitive_value unbound_subterms

let compare_atomn n value1 value2 =
  let op_id1 = T.opid_of_term value1 in
  let op_id2 = T.opid_of_term value2 in
  match T.parameters_of_term value1, T.parameters_of_term value2 with
    [param1], [param2] ->
      (* NOTE: both values should only have one parameter *)
      let ptype = T.type_of_parameter param1 in
      if op_id1 = "token"
	  &&
	op_id1 = op_id2
	  (* NOTE: both values have to be tokens *)
	  &&
	ptype = T.type_of_parameter param2
	  (* NOTE: both value have to have the the same kind of parameters *)
	  &&
	(match n with (* NOTE: what are these: *)
	  0 -> (ptype = "token" || ptype = "t")
	| 1 -> ptype = "ut1"
	| 2 -> ptype = "ut2"
	| _ -> false)
      then T.equal_parameters param1 param2
      else failwith ("compare_atomn - "
		     ^ op_id1 ^ " "
		     ^ T.opid_of_term value2 ^ " "
		     ^ ptype ^ " "
		     ^ T.type_of_parameter param2 ^ " "
		     ^ Pervasives.string_of_int n)
  | ps1, ps2 ->
      failwith ("compare_atomn("
		^ op_id1 ^ ":"
		^ Pervasives.string_of_int (List.length ps1) ^ ","
		^ op_id2 ^ ":"
		^ Pervasives.string_of_int (List.length ps2) ^ ")")
