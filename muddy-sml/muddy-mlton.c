/* muddy.c is the layer between the MuDDy ML library and the BuDDy C library.*/
/* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* BDD stuff */
#include <bdd.h>
#include <fdd.h>
#include <bvec.h>

/* MLton stuff */
#include <platform.h>

/* Reduced Ordered Binary Decision Diagrams: interface to
   Jørn Lind-Nielsen's <buddy@it.edu> BuDDy library.
   Made by Ken Friis Larsen <kfl@it.edu>

   MLton version

/* To make DLL on Windows we need non-standard annotations */
#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#define INLINE
#else
#define EXTERNML
#define INLINE inline
#endif



#define DEBUG 0

#if DEBUG
#define DEBUG_MSG(x) x 
#else
#define DEBUG_MSG(x)
#endif

static char* pregc     = NULL;
static char* postgc    = NULL;
static int printgc = 0; /* Invariant: if printgc != 0 then will the
                           two strings above point to valid strings */
static void mlbdd_gc(int num, bddGbcStat* foo)
{
  if(num==1 && printgc) { printf ("%s", pregc); fflush(stdout); }
  else if(num==0 && printgc) { 
    printf("%s", postgc); 
    printf("nodes: %d freenodes: %d time: %ld sumtime: %ld num: %d\n",
	  foo->nodes,foo->freenodes,foo->time,foo->sumtime,foo->num);
    fflush(stdout); 
  }
  
}

static void mlbdd_freegcstrings () {
  if(printgc) {
    free(pregc);
    free(postgc);
    printgc = 0;
  }
}

/* ML type: bool -> root */
EXTERNML int mlbdd_fromBool(Bool b) /* ML */
{
  return b ? bddtrue : bddfalse;
}


/* ML type: bool -> string -> string -> unit */
EXTERNML void mlbdd_setprintgc(Bool print, char *pre, char *post) /* ML */
{
  mlbdd_freegcstrings();
  if(print) { 
    pregc=strdup(pre); 
    postgc=strdup(post); 
    printgc=1; 
    bdd_gbc_hook(mlbdd_gc); 
  }
  else bdd_gbc_hook(NULL);
}

EXTERNML void mlbdd_constants(Int32 *arr) {
  arr[0] = bddop_and;
  arr[1] = bddop_xor;
  arr[2] = bddop_or;
  arr[3] = bddop_nand;
  arr[4] = bddop_nor;
  arr[5] = bddop_imp;
  arr[6] = bddop_biimp;
  arr[7] = bddop_diff;
  arr[8] = bddop_less;
  arr[9] = bddop_invimp;
  arr[10] = BDD_REORDER_FIXED;
  arr[11] = BDD_REORDER_FREE;
  arr[12] = BDD_REORDER_WIN2;
  arr[13] = BDD_REORDER_WIN2ITE;
  arr[14] = BDD_REORDER_SIFT;
  arr[15] = BDD_REORDER_SIFTITE;
  arr[16] = BDD_REORDER_RANDOM;
  arr[17] = BDD_REORDER_NONE;
  return;
}


EXTERNML void mlbdd_bdd_stats(Int32 *arr) {
  static bddStat stat;
  bdd_stats(& stat);

  arr[0] = stat.produced;
  arr[1] = stat.nodenum;
  arr[2] = stat.maxnodenum;
  arr[3] = stat.freenodes;
  arr[4] = stat.minfreenodes;
  arr[5] = stat.varnum;
  arr[6] = stat.cachesize;
  arr[7] = stat.gbcnum;

  return;
}



EXTERNML void mlfdd_scanallvars(Int32 *res, Int32 offset, Int32 n, Word32 r) /* ML */
{
  int *v, *vo, i;

  v = fdd_scanallvar(r);
  vo = v + offset;

  for (i = 0; i < n; i++) {
    res[i] = vo[i];
  }
  free(v);

  return res;
}


/* ML type: fddvar * varnum array * int -> unit */
EXTERNML void mlfdd_vars(Int32 var, Int32 *arr, Int32 size) /* ML */
{
  int *v, i;

  v = fdd_vars(var);
  
  for (i = 0; i < size; i++) {
      arr[i] = v[i];
  }

  return;
}



/* BVEC FUNCTIONS */

#define bvecbitnum_val(x) ((int)  (Field(x, 1)))
#define bvecbitvec_val(x) ((BDD*) (Field(x, 2)))

/* static INLINE BVEC BVEC_val(value obj) { */
/*   BVEC t; */
/*   t.bitnum=bvecbitnum_val(obj); */
/*   t.bitvec=bvecbitvec_val(obj); */
/*   return t; */
/* } */

/* When the bvec becomes unreachable from the ML process, it will be
   garbage-collected, mlbdd_finalize_bvec() will be called on the bvec,
   which will do the necessary bvec-bookkeeping.  */
void mlbdd_finalize_bvec(Int32 bits, BDD *bitvec) 
{
  BVEC bv;
  printf("bitnum = %d, bitvec = %d\n", bits, *bitvec);
  bv.bitnum = bits;
  bv.bitvec = bitvec; 
  bvec_free(bv);
}

/* Creation of a bvec makes a finalized pair (mlbdd_finalize, bitnum, bitvec) */
#if 0
EXTERNML value mlbdd_make_bvec(BVEC v) 
{
  value res;
  res = mlbdd_alloc_final(3, &mlbdd_finalize_bvec);
  bvecbitnum_val(res) = v.bitnum; 
  bvecbitvec_val(res) = v.bitvec;  /* Hopefully a pointer fits in a long */
  return res;
}
#endif

EXTERNML void mlbvec_true(Int32 bits, Int32 *bitnum, BDD **bitvec) {
  BVEC bv;

  bv = bvec_true(bits);
  *bitnum = bv.bitnum;
  *bitvec = bv.bitvec;
}

EXTERNML void mlbvec_false(Int32 bits, Int32 *bitnum, BDD **bitvec) {
  BVEC bv;

  bv = bvec_false(bits);
  *bitnum = bv.bitnum;
  *bitvec = bv.bitvec;
}


EXTERNML void mlbvec_con(Int32 bits, Int32 val, Int32 *bitnum, BDD **bitvec) /* ML */
{
  BVEC bv;

  bv = bvec_con(bits, val);
  *bitnum = bv.bitnum;
  *bitvec = bv.bitvec;
}

#if 0
/* ML type: precision -> varnum -> int -> bvec */
EXTERNML value mlbvec_var(value bits, value var, value step) /* ML */
{
  return mlbdd_make_bvec(bvec_var(Int_val(bits), Int_val(var), Int_val(step)));
}
#endif

EXTERNML void mlbvec_varfdd(Int32 var, Int32 *bitnum, BDD **bitvec) /* ML */
{
  BVEC bv;

  bv = bvec_varfdd(var);
  *bitnum = bv.bitnum;
  *bitvec = bv.bitvec;
}

#if 0
/* ML type: precision -> bvec -> bvec */
EXTERNML value mlbvec_coerce(value bits, value v) /* ML */
{
  return mlbdd_make_bvec(bvec_coerce(Int_val(bits), BVEC_val(v)));
}

/* ML type: bvec -> bool */
EXTERNML value mlbvec_isconst(value v) /* ML */
{
  return bvec_isconst(BVEC_val(v)) ? Val_true : Val_false;
}

/* ML type: bvec -> bool */
EXTERNML value mlbvec_getconst(value v) /* ML */
{
  if(bvec_isconst(BVEC_val(v))) {
    return Val_int(bvec_val(BVEC_val(v)));
  }
  else {
    failwith("The bvec does not represent a single constant.");
    return Val_unit; /* unreachable, here to prevent warnings */
  }
}

/* ML type: bvec -> bvec -> bvec */
EXTERNML value mlbvec_add(value s1, value s2) /* ML */
{
  return mlbdd_make_bvec(bvec_add(BVEC_val(s1), BVEC_val(s2)));
}

/* ML type: bvec -> bvec -> bvec */
EXTERNML value mlbvec_sub(value s1, value s2) /* ML */
{
  return mlbdd_make_bvec(bvec_sub(BVEC_val(s1), BVEC_val(s2)));
}

/* ML type: bvec -> const -> bvec */
EXTERNML value mlbvec_mulfixed(value s1, value con) /* ML */
{
  return mlbdd_make_bvec(bvec_mulfixed(BVEC_val(s1), Int_val(con)));
}

/* ML type: bvec -> bvec -> bvec */
EXTERNML value mlbvec_mul(value s1, value s2) /* ML */
{
  return mlbdd_make_bvec(bvec_mul(BVEC_val(s1), BVEC_val(s2)));
}

#ifdef CUDDY
/* ML type: bvec -> const -> bvec * bvec */
EXTERNML value mlbvec_divfixed(value s1, value con) /* ML */
{
  CAMLparam2(s1, con);
  CAMLlocal1(result);
  BVEC res, rem;
  bvec_divfixed(BVEC_val(s1), Int_val(con), &res, &rem);
  result = alloc_tuple(2);
  Store_field(result, 0, mlbdd_make_bvec(res)); 
  Store_field(result, 1, mlbdd_make_bvec(rem)); 
  CAMLreturn(result);
}

/* ML type: bvec -> bvec -> bvec * bvec  */
EXTERNML value mlbvec_div(value s1, value s2) /* ML */
{
  CAMLparam2(s1, s2);
  CAMLlocal1(result);
  BVEC res, rem;
  bvec_div(BVEC_val(s1), BVEC_val(s2), &res, &rem);
  result = alloc_tuple(2);
  Store_field(result, 0, mlbdd_make_bvec(res)); 
  Store_field(result, 1, mlbdd_make_bvec(rem)); 
  CAMLreturn(result);
}

#else
/* ML type: bvec -> const -> bvec * bvec */
EXTERNML value mlbvec_divfixed(value s1, value con) /* ML */
{
  BVEC res, rem;
  Push_roots(result, 1);
    bvec_divfixed(BVEC_val(s1), Int_val(con), &res, &rem);
    result[0] = alloc_tuple(2);
    Field(result[0], 0) = 0;
    Field(result[0], 1) = 0;
    Field(result[0], 0) = mlbdd_make_bvec(res); 
    Field(result[0], 1) = mlbdd_make_bvec(rem); 
  Pop_roots();
  return result[0];
}

/* ML type: bvec -> bvec -> bvec * bvec  */
EXTERNML value mlbvec_div(value s1, value s2) /* ML */
{
  BVEC res, rem;
  Push_roots(result, 1);
    bvec_div(BVEC_val(s1), BVEC_val(s2), &res, &rem);
    result[0] = alloc_tuple(2);
    Field(result[0], 0) = 0;
    Field(result[0], 1) = 0;
    Field(result[0], 0) = mlbdd_make_bvec(res); 
    Field(result[0], 1) = mlbdd_make_bvec(rem); 
  Pop_roots();
  return result[0];
}
#endif

/* ML type: bvec -> bvec -> bdd -> bvec */
EXTERNML value mlbvec_shl(value s1, value c, value b) /* ML */
{
  return mlbdd_make_bvec(bvec_shl(BVEC_val(s1), BVEC_val(c), Bdd_val(b)));
}

/* ML type: bvec -> const -> bdd -> bvec */
EXTERNML value mlbvec_shlfixed(value s1, value c, value b) /* ML */
{
  return mlbdd_make_bvec(bvec_shlfixed(BVEC_val(s1), Int_val(c), Bdd_val(b)));
}

/* ML type: bvec -> bvec -> bdd -> bvec */
EXTERNML value mlbvec_shr(value s1, value c, value b) /* ML */
{
  return mlbdd_make_bvec(bvec_shr(BVEC_val(s1), BVEC_val(c), Bdd_val(b)));
}

/* ML type: bvec -> const -> bdd -> bvec */
EXTERNML value mlbvec_shrfixed(value s1, value c, value b) /* ML */
{
  return mlbdd_make_bvec(bvec_shrfixed(BVEC_val(s1), Int_val(c), Bdd_val(b)));
}

/* ML type: bvec -> bvec -> bdd */
EXTERNML value mlbvec_lth(value s1, value s2) /* ML */
{
  return mlbdd_make(bvec_lth(BVEC_val(s1), BVEC_val(s2)));
}

/* ML type: bvec -> bvec -> bdd */
EXTERNML value mlbvec_lte(value s1, value s2) /* ML */
{
  return mlbdd_make(bvec_lte(BVEC_val(s1), BVEC_val(s2)));
}

/* ML type: bvec -> bvec -> bdd */
EXTERNML value mlbvec_gth(value s1, value s2) /* ML */
{
  return mlbdd_make(bvec_gth(BVEC_val(s1), BVEC_val(s2)));
}

/* ML type: bvec -> bvec -> bdd */
EXTERNML value mlbvec_gte(value s1, value s2) /* ML */
{
  return mlbdd_make(bvec_gte(BVEC_val(s1), BVEC_val(s2)));
}
#endif

EXTERNML BDD mlbvec_equ(Int32 bits1, BDD *b1, Int32 bits2, BDD *b2) /* ML */
{
  BVEC bv1;
  BVEC bv2;

  bv1.bitnum = bits1;
  bv1.bitvec = b1; 

  bv2.bitnum = bits2;
  bv2.bitvec = b2; 

  return bvec_equ(bv1, bv2);
}


#if 0
/* ML type: bvec -> bvec -> bdd */
EXTERNML value mlbvec_neq(value s1, value s2) /* ML */
{
  return mlbdd_make(bvec_neq(BVEC_val(s1), BVEC_val(s2)));
}

#endif
