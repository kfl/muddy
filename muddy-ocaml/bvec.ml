(* Structure bvec implements Boolean Vectorss in MuDDy BDD ML Interface.     *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

    type bvec
    type const = int
	
    let cur2 f (a,b) = f a b

    external bvectrue: Fdd.precision -> bvec = "mlbvec_true"
    external bvecfalse: Fdd.precision -> bvec = "mlbvec_false"
    external con: Fdd.precision -> const -> bvec = "mlbvec_con"
	
    external var: Fdd.precision -> Bdd.varnum -> int -> bvec =
	 "mlbvec_var"
    external varfdd: Fdd.fddvar -> bvec = "mlbvec_varfdd"
	
    external coerce: Fdd.precision -> bvec -> bvec = "mlbvec_coerce"
	
    external isConst: bvec -> bool = "mlbvec_isconst"
    external getConst: bvec -> const = "mlbvec_getconst"
	
    let lookupConst bvec = 
	if isConst bvec then Some(getConst bvec) else None
	    
    external add_: bvec -> bvec -> bvec = "mlbvec_add"
    let add = cur2 add_
    external sub_: bvec -> bvec -> bvec = "mlbvec_sub"
    let sub = cur2 sub_

    external mul_     : bvec -> bvec -> bvec = "mlbvec_mul"
    let mul = cur2 mul_
    external mulfixed_: bvec -> const -> bvec = "mlbvec_mulfixed"
    let mulfixed = cur2 mulfixed_

    external div_ : bvec -> bvec -> bvec * bvec = "mlbvec_div"
    let div = cur2 div_
    external divfixed_: bvec -> const -> bvec * bvec = "mlbvec_divfixed"
    let divfixed = cur2 divfixed_

    let compose__ f g = function x -> f(g(x))

    let divi      = compose__ (function (x,_)->x) div
    let divifixed = compose__ (function (x,_)->x) divfixed
    let modu      = compose__ (function (_,x)->x) div
    let modufixed = compose__ (function (_,x)->x) divfixed

    external shl     : bvec -> bvec -> Bdd.bdd -> bvec = "mlbvec_shl"
    external shlfixed: bvec -> int -> Bdd.bdd -> bvec = 
	"mlbvec_shlfixed"

    external shr     : bvec -> bvec -> Bdd.bdd -> bvec = "mlbvec_shr"
    external shrfixed: bvec -> int  -> Bdd.bdd -> bvec = 
	"mlbvec_shrfixed"

    external lth_: bvec -> bvec -> Bdd.bdd = "mlbvec_lth"
    let lth = cur2 lth_
    external lte_: bvec -> bvec -> Bdd.bdd = "mlbvec_lte"
    let lte = cur2 lte_
    external gth_: bvec -> bvec -> Bdd.bdd = "mlbvec_gth"
    let gth = cur2 gth_
    external gte_: bvec -> bvec -> Bdd.bdd = "mlbvec_gte"
    let gte = cur2 gte_
    external equ_: bvec -> bvec -> Bdd.bdd = "mlbvec_equ"
    let equ = cur2 equ_
    external neq_: bvec -> bvec -> Bdd.bdd = "mlbvec_neq"
    let neq = cur2 neq_
