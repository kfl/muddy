structure FinalizableXP : FinalizableXP =
struct

type 'a t = 'a Finalizable.t
type 'a destructor = 'a -> unit


val finalizedMemory = ref 0.0
fun reset () = finalizedMemory := 0.0
(*
val _ = MLton.Signal.handleGC reset
*)
val adjustSpeed : {used: int, max: int} -> unit =
    fn {used, max} =>
       if used = 0 then ()
       else let val max  = if max = 0 then 1 else max
                val used = if used > max then max else used
                val delta = real used / real max
            in  finalizedMemory := !finalizedMemory + delta
              ; if !finalizedMemory > 1.0 then (MLton.GC.collect (); reset())
                else ()
            end     

val new : {used: int, max: int} -> 'a -> 'a t =
    fn adjust => fn value =>
       Finalizable.new value before adjustSpeed adjust

end
