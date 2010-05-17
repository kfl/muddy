(* Copyright (C) 1997-2001 by Ken Friis Larsen and Jakob Lichtenberg. *)

fun pathsToTrue b =
    let val (root, tab) = bdd.nodetable b
	fun high i = #3(Vector.sub(tab, i)) 
	fun low i = #2(Vector.sub(tab, i))
	val memory = Array.array(Vector.length tab, NONE)
	fun lookup i = Array.sub(memory, i)
	fun update i d = Array.update(memory, i, SOME d)
	val eq = bdd.equal

	fun count i =
	    case lookup i of 
		SOME n => n
	      | NONE   => let val n = count(low i) + count(high i)
			  in  update i n;
			      n
			  end

    in  update 0 0;
	update 1 1;
	if eq b bdd.TRUE orelse eq b bdd.FALSE then 0
	else count root
    end

fun count b = 
    let val sat    = bdd.satcount b
	val total  = Real.fromInt(bdd.getVarnum())
	val sup    = bdd.scanset(bdd.support b)
	val numsup = Real.fromInt(Vector.length sup)
	val free   = total - numsup
    in  if bdd.equal b bdd.TRUE then 0.0
	else sat / Math.pow(2.0, free)
    end
