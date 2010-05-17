(* Structure bvec implements Boolean Vectorss in MuDDy BDD ML Interface.     *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

(* MLton version. Ken Friis Larsen 2003 *)

structure bvec :> bvec =
struct
    structure F = Finalizable
    structure FXP = FinalizableXP

    type bddptr = word (*really a pointer, but we need a dummy value sometimes*)
    type bv = int * bddptr
    type bvec = bv F.t
    type const = int

    val NULL = 0w2108


    val finalize_bvec_ = _import "mlbdd_finalize_bvec" : int * bddptr -> unit;


    fun create root = let val bvec = FXP.new {used = 0, max = 1} root
                      in  F.addFinalizer(bvec, finalize_bvec_)
                        ; bvec
                      end
                      


	
    exception BVEC_NOT_IMPLEMENTED
    fun notImplemented x = raise BVEC_NOT_IMPLEMENTED

    val bvectrue_ = _import "mlbvec_true" : fdd.precision * int ref * bddptr ref -> unit;
    val bvecfalse_ = _import "mlbvec_false" : fdd.precision * int ref * bddptr ref -> unit;

    fun bvectrue precision = 
        let val bits = ref 0
            val bv = ref NULL 
        in  bvectrue_(precision, bits, bv)
          ; create(!bits, !bv)
        end

    fun bvecfalse precision = 
        let val bits = ref 0
            val bv = ref NULL
        in  bvecfalse_(precision, bits, bv)
          ; create(!bits, !bv)
        end


    val con_ = _import "mlbvec_con" : fdd.precision * const * int ref * bddptr ref -> unit;
    fun con precision const = 
        let val bits = ref 0
            val bv = ref NULL
        in  con_(precision, const, bits, bv)
          ; create(!bits, !bv)
        end

    val var: fdd.precision -> bdd.varnum -> int -> bvec = notImplemented

    val varfdd_ = _import "mlbvec_con" : fdd.fddvar * int ref * bddptr ref -> unit;
    fun varfdd var = 
        let val bits = ref 0
            val bv = ref NULL
        in  varfdd_(var, bits, bv)
          ; create(!bits, !bv)
        end


	
    val coerce: fdd.precision -> bvec -> bvec = notImplemented
	
    val isConst: bvec -> bool = notImplemented
    val getConst: bvec -> const = notImplemented
    val lookupConst: bvec -> const option = notImplemented
	
    val add: bvec * bvec -> bvec = notImplemented
    val sub: bvec * bvec -> bvec = notImplemented

    val mul     : bvec * bvec -> bvec = notImplemented
    val mulfixed: bvec * const -> bvec = notImplemented

    val op div     : bvec * bvec -> bvec * bvec = notImplemented
    val divfixed: bvec * const -> bvec * bvec = notImplemented

    val divi     : bvec * bvec -> bvec = notImplemented
    val divifixed: bvec * const -> bvec = notImplemented
 
    val modu     : bvec * bvec -> bvec = notImplemented
    val modufixed: bvec * const -> bvec = notImplemented

    val shl     : bvec -> bvec -> bdd.bdd -> bvec = notImplemented
    val shlfixed: bvec -> int -> bdd.bdd -> bvec = notImplemented

    val shr     : bvec -> bvec -> bdd.bdd -> bvec = notImplemented
    val shrfixed: bvec -> int -> bdd.bdd -> bvec = notImplemented
	
    val lth: bvec * bvec -> bdd.bdd = notImplemented
    val lte: bvec * bvec -> bdd.bdd = notImplemented
    val gth: bvec * bvec -> bdd.bdd = notImplemented
    val gte: bvec * bvec -> bdd.bdd = notImplemented

    val equ_ = _import "mlbvec_equ" : int * bddptr * int * bddptr -> bdd.root;
    fun equ (bv1, bv2) =
        F.withValue(bv1, fn (n1, b1) =>
        F.withValue(bv2, fn (n2, b2) =>
        bdd.create(equ_(n1, b1, n2, b2))))

    val neq: bvec * bvec -> bdd.bdd = notImplemented

end
