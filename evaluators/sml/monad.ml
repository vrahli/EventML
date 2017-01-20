module B = Tools
module T = NuprlTerms

type 's state = int * 's
type ('a, 's) m = 's state -> 'a * 's state

let decr x (n,s) = (x, (B.decr_steps n,s))
let unit x s = (x,s)
let exec f (t,e) s = try f s with _ -> (print_string ("++failed(" ^ T.opid_of_term t ^ ")\n"); ((t,e), s))
let bind (m, k) =
  fun s ->
    let (x,s') = m s
	(*val _ = (fn (steps,_) => print (">" ^ Int.toString steps ^ "\n")) s'*)
    in k x s'
