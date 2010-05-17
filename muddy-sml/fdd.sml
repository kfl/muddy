(* Structure fdd implements Finite Domain Blocks in MuDDy BDD ML Interface.  *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

structure fdd :> fdd =
struct
	
    open MuddyCore
	
    type precision = int
    type domain = int
    type fddvar = int
	
    val extDomain_ : int vector -> fddvar = app1 (symb "mlfdd_extdomain") 
	    
    fun mkList(start, stop) = 
            if start > stop then []
	    else start::mkList(start+1, stop)

    fun extDomain l = 
	    let val i = extDomain_ (Vector.fromList(l))
	    in mkList(i,i-1+List.length l) end

    val clearAll: unit -> unit = app1 (symb "mlfdd_clearall") 
    val domainNum: unit -> int = app1 (symb "mlfdd_domainnum") 
    val domainSize: fddvar -> int = app1 (symb "mlfdd_domainsize") 
    val varNum: fddvar -> int = app1 (symb "mlfdd_varnum") 
	
    fun vectorToList v = Vector.foldl op:: nil v

    fun compose__ f g = (fn x => f(g(x)))

    val vars__: fddvar -> bdd.varnum Vector.vector =
	app1 (symb "mlfdd_vars") 
    val vars: fddvar -> bdd.varnum list = 
	    compose__ vectorToList vars__

    val ithSet: fddvar -> bdd.varSet = app1 (symb "mlfdd_ithset")
    val domain: fddvar -> bdd.bdd = app1 (symb "mlfdd_domain")
	
    val makeSet__: fddvar Vector.vector -> bdd.varSet =
	app1 (symb "mlfdd_makeset")
    val makeSet: fddvar list -> bdd.varSet =
	 compose__ makeSet__ Vector.fromList
	
    val setPairs_: int vector -> int vector -> bdd.pairSet =
	    app2 (symb "mlfdd_setpairs")

    fun setPairs l = 
	    let val (l1,l2) = ListPair.unzip l
	    in setPairs_ (Vector.fromList l1) (Vector.fromList l2) end

    val scanallvars_ = app3 (symb "mlfdd_scanallvars")
    fun scanallvars (os,n,b) = 
        let val v = scanallvars_ os n b
        in ((* print (Int.toString (Vector.length v)); *)
            if not (Vector.length v =n)
            then NONE
            else SOME v)
        end

    val addvarblock : fddvar -> fddvar -> bdd.fixed -> unit
     = app3 (symb "mlfdd_intaddvarblock")
end
