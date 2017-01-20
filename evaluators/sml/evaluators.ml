module T   = NuprlTerms
module EV2 = Evaluator2
module P   = ParserNuprlAscii

let mapref = ref (T.emlib ())

let start_session prt libop =
  match libop with
    Some lib ->
      let terms = P.parse prt [] lib false in
      let map   = T.terms2map terms in
      let _     = mapref := map in
      ()
  | None -> ()

let start_session_lib lib = mapref := lib

let add_to_session terms =
  let map =
    List.fold_left
      (fun map term ->
	if T.is_nuprl_iabstraction_term term
	then
	  let (_, rlhs, rrhs) = T.dest_iabstraction term in
	  let lhs  = T.rterm2term rlhs in
	  let sign = T.getSignature lhs (*([], [])*) in
	  let id   = T.opid_of_term lhs in
	  let obid = "" in
	  let opid = id in
	  (*val _ = print ("--" ^ id ^ "\n")*)
	  let item = T.mk_item id sign obid rlhs rrhs [] in
	  (*let _    = print_endline ("[adding " ^ id ^ " to session]") in*)
	  T.insert_abs map opid item
	else
	  (*let _ = print_endline ("[will not add " ^ T.opid_of_term term ^ " to session, not an abstraction]")
	  in*) map)
      (!mapref)
      terms
  in mapref := map

let get_lib () = !mapref

let reset_lib () = mapref := T.emlib ()

let end_session = reset_lib

let rec terms2lib_rev lst =
  match lst with
    [] -> T.mk_nuprl_simple_term "!library" []
  | (abs, wf) :: terms ->
      let (t1, lhs, rhs) = T.dest_iabstraction abs in
      T.mk_nuprl_ref_simple_term "!library" [lhs; rhs; T.mk_rterm (terms2lib_rev terms)]

let terms2lib terms = terms2lib_rev (List.rev terms)

(* evaluator with closures *)
let run_ev2_map ts steps term =
  let lib = terms2lib ts in
  EV2.evaluator2 false (!mapref, lib) steps term

(* evaluator with closures
 * + closures instead of subst when unfolding *)
let run_ev2b_map ts steps term =
  let lib = terms2lib ts in
  EV2.evaluator2 true (!mapref, lib) steps term

let run_ev1_map  (lst : (T.nuprl_term * T.nuprl_term) list) (n : int) (t : T.nuprl_term) = failwith "unimplemented"
let run_ev2c_map _ _ _ = failwith "unimplemented"
let run_ev2d_map _ _ _ = failwith "unimplemented"
let run_ev3_map  _ _ _ = failwith "unimplemented"
let run_ev3b_map _ _ _ = failwith "unimplemented"
let run_ev3c_map _ _ _ = failwith "unimplemented"
let run_ev3d_map _ _ _ = failwith "unimplemented"
let run_ev4_map  _ _ _ = failwith "unimplemented"
let run_ev4b_map _ _ _ = failwith "unimplemented"
let run_ev5_map  _ _ _ = failwith "unimplemented"
let run_ev5b_map _ _ _ = failwith "unimplemented"
