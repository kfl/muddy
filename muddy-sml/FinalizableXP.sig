signature FinalizableXP =
sig
    type 'a t = 'a Finalizable.t


    val adjustSpeed : {used: int, max: int} -> unit
    val new         : {used: int, max: int} -> 'a -> 'a t
end

(* 

   [adjustSpeed {used, max}] Use this function to tell the major GC to
   speed up when you use finalized blocks to automatically deallocate
   extra-heap stuff.  The GC will do at least one cycle every [max]
   allocated words; [used] is the number of words allocated this time.
   Note that only [used/max] is relevant.  You can use numbers of bytes
   (or kilobytes, ...) instead of words.  You can change units between
   calls to [adjustSpeed].



*)
