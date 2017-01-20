type 's state = int * 's
type ('a, 's) m = 's state -> 'a * 's state

val decr : 'a -> ('a, 's) m

val unit : 'a -> ('a, 's) m

val bind : ('a, 's) m * ('a -> ('b, 's) m) -> ('b, 's) m
