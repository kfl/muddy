(* Structure bvec implements Boolean Vectorss in MuDDy BDD ML Interface.     *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

structure bvec :> bvec =
struct
	
    open MuddyCore
	
    prim_type bvec
    type const = int
	
    val bvectrue: fdd.precision -> bvec = app1 (symb "mlbvec_true")
    val bvecfalse: fdd.precision -> bvec = app1 (symb "mlbvec_false")
    val con: fdd.precision -> const -> bvec = app2 (symb "mlbvec_con")
	
    val var: fdd.precision -> bdd.varnum -> int -> bvec =
	 app3 (symb "mlbvec_var")
    val varfdd: fdd.fddvar -> bvec = app1 (symb "mlbvec_varfdd")
	
    val coerce: fdd.precision -> bvec -> bvec = app2 (symb "mlbvec_coerce")
	
    val isConst: bvec -> bool = app1 (symb "mlbvec_isconst")
    val getConst: bvec -> const = app1 (symb "mlbvec_getconst")
	
    fun lookupConst bvec = 
	if isConst bvec then SOME(getConst bvec) else NONE
	    
    val add: bvec * bvec -> bvec = cur2 (symb "mlbvec_add")
    val sub: bvec * bvec -> bvec = cur2 (symb "mlbvec_sub")

    val mul     : bvec * bvec -> bvec = cur2 (symb "mlbvec_mul")
    val mulfixed: bvec * const -> bvec = cur2 (symb "mlbvec_mulfixed")

    val op div : bvec * bvec -> bvec * bvec = cur2 (symb "mlbvec_div")
    val divfixed: bvec * const -> bvec * bvec = cur2 (symb "mlbvec_divfixed")

    fun compose__ f g = (fn x => f(g(x)))

    val divi      = compose__ (fn (x,_)=>x) op div
    val divifixed = compose__ (fn (x,_)=>x) divfixed
    val modu      = compose__ (fn (_,x)=>x) op div
    val modufixed = compose__ (fn (_,x)=>x)  divfixed

    val shl     : bvec -> bvec -> bdd.bdd -> bvec = app3 (symb "mlbvec_shl")
    val shlfixed: bvec -> int -> bdd.bdd -> bvec = 
	app3 (symb "mlbvec_shlfixed")

    val shr     : bvec -> bvec -> bdd.bdd -> bvec = app3 (symb "mlbvec_shr")
    val shrfixed: bvec -> int  -> bdd.bdd -> bvec = 
	app3 (symb "mlbvec_shrfixed")

    val lth: bvec * bvec -> bdd.bdd = cur2 (symb "mlbvec_lth")
    val lte: bvec * bvec -> bdd.bdd = cur2 (symb "mlbvec_lte")
    val gth: bvec * bvec -> bdd.bdd = cur2 (symb "mlbvec_gth")
    val gte: bvec * bvec -> bdd.bdd = cur2 (symb "mlbvec_gte")
    val equ: bvec * bvec -> bdd.bdd = cur2 (symb "mlbvec_equ")
    val neq: bvec * bvec -> bdd.bdd = cur2 (symb "mlbvec_neq")
end
