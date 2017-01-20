val can : ('a -> 'b) -> 'a -> bool

val apply_alist : (string * 'a) list -> string -> 'a

val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b

val decr_steps : int -> int

val split : int -> 'a list -> 'a list * 'a list

val accumulate2 : ('a -> 'b -> 'c -> 'a)
  -> 'a
    -> 'b list
      -> 'c list
	-> 'a

val all2 : ('a -> 'b -> bool)
  -> 'a list
    -> 'b list
      -> bool

val get1 : int -> 'a list -> 'a
val get2 : int -> 'a list -> 'a * 'a
val get3 : int -> 'a list -> 'a * 'a * 'a
val get4 : int -> 'a list -> 'a * 'a  *'a * 'a

val get1_0bound   : int -> ('a list * 'b) list -> 'b
val get1_1bound   : int -> ('a list * 'b) list -> 'a * 'b
val get1_2bound   : int -> ('a list * 'b) list -> 'a * 'a * 'b
val get2_0bound   : int -> ('a list * 'b) list -> 'b * 'b
val get2_1bound   : int -> ('a list * 'b) list -> 'a * 'b * 'a * 'b
val get2_02bound  : int -> ('a list * 'b) list -> 'b * 'a * 'a * 'b
val get2_03bound  : int -> ('a list * 'b) list -> 'b * 'a * 'a * 'a * 'b
val get3_202bound : int -> ('a list * 'b) list -> 'a * 'a * 'b * 'b * 'a * 'a * 'b
