structure MLton = struct

structure GC = struct
fun collect () = ()
end

structure Weak = struct
type 'a t = 'a ref
fun get r = SOME (!r)
fun new x = ref x
end

structure Signal = struct
structure Handler = struct
type t = unit
fun simple f = ()
end
fun setHandler (signal, handler) = ()
end

end
