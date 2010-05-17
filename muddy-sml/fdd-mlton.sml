(* Structure fdd implements Finite Domain Blocks in MuDDy BDD ML Interface.  *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

(* MLton version. Ken Friis Larsen 2003 *)


structure fdd :> fdd =
struct
    type precision = int
    type domain = int
    type fddvar = int
	

    fun failwith s = raise Fail s

    fun buddyError code = failwith ("BuDDy error code: "^Int.toString code)

    fun checkError code =
        if code < 0 then buddyError code
        else code


    val extDomain_ = _import "fdd_extdomain" : int vector * int -> fddvar; 
	    
    fun extDomain l = 
	    let val v = vector l
                val offset = checkError(extDomain_(v, Vector.length v))
	    in  List.tabulate(Vector.length v, fn i => offset+i)
            end

    val clearAll = _import "fdd_clearall" : unit -> unit;
    val domainNum_ = _import "fdd_domainnum" : unit -> int;
    val domainNum = checkError o domainNum_


    val domainSize_ = _import "fdd_domainsize" :  fddvar -> int; 
    val domainSize = checkError o domainSize_

    val varNum_ = _import "fdd_varnum" :  fddvar -> int ; 
    val varNum = checkError o varNum_
	

    val ithvar_ = _import "fdd_ithvar" : fddvar * int -> bdd.root;
    fun ithvar var value = bdd.create(ithvar_(var, value))

    val equals_ = _import "fdd_equals"  : fddvar * fddvar -> bdd.root;
    fun equals v1 v2 = bdd.create(equals_(v1, v2))



    val vars_ = _import "mlfdd_vars" : fddvar * bdd.varnum array * int -> unit;
    fun vars var =
        let val size = varNum var
            val arr  = Array.array(size, 0)
        in  vars_(var, arr, size)
          ; Array.foldl op:: [] arr
        end

    
    val setpairs_ = _import "fdd_setpairs" 
                  : bdd.bddPair * fddvar vector * fddvar vector * int -> int;

    fun setPairs pl = 
        let val (old,new) = ListPair.unzip pl
            val old = vector old
            val new = vector new
        in  bdd.withNewPair(fn pair =>
              ignore(checkError(setpairs_(pair, old, new, Vector.length old))))
        end

    exception FDD_NOT_IMPLEMENTED
    fun notImplemented x = raise FDD_NOT_IMPLEMENTED

    val domain = notImplemented
    val ithSet = notImplemented


(*    fun vectorToList v = Vector.foldl op:: nil v

    fun compose__ f g = (fn x => f(g(x)))

    val vars__: fddvar -> bdd.varnum Vector.vector = app1 (symb "mlfdd_vars") 
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
*)

    val scanallvars_ = _import "mlfdd_scanallvars" 
                     : int array * int * int * bdd.root -> unit;

    fun scanallvars (os,n,b) =
        if n < 0 orelse n+os > domainNum() then raise Subscript
        else if bdd.equal b bdd.FALSE orelse n = 0 then NONE
        else let val res = Array.array(n, 0)
             in  bdd.withRoot(b, fn b => scanallvars_(res, os, n, b))
               ; SOME(Array.vector res)
             end

    val addvarblock_ = _import "fdd_intaddvarblock" 
                     : fddvar * fddvar * bdd.fixed -> int;
    fun addvarblock first last fixed =
        ignore(checkError(addvarblock_(first, last, fixed)))

end
