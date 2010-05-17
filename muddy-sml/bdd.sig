(* Structure bdd is the core structure of the MuDDy BDD ML Interface.        *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

signature bdd =
sig
    (* Basic types: *)
    type bdd
    type varnum = int


    (* Basic administration: *)
    val init      : int -> int -> unit
    val setVarnum : varnum -> unit
    val getVarnum : unit -> varnum
    val isRunning : unit -> bool
    val bdone      : unit -> unit


    (* Basic operations: *)
    val toBool   : bdd -> bool
    val fromBool : bool -> bdd

    val TRUE : bdd
    val FALSE : bdd

    val equal    : bdd -> bdd -> bool

    val ithvar  : varnum -> bdd
    val nithvar : varnum -> bdd

    val var     : bdd -> varnum
    val low     : bdd -> bdd
    val high    : bdd -> bdd
    

    (* Basic Boolean operations: *)
    datatype bddop =
	And | Xor | Or | Nand | Nor | Imp | Biimp
      | Diff | Lessth | Invimp

    val apply  : bdd -> bdd -> bddop -> bdd

    val DIFF   : bdd * bdd -> bdd
    val IMP    : bdd * bdd -> bdd
    val LESSTH : bdd * bdd -> bdd
    val BIIMP  : bdd * bdd -> bdd
    val OR    : bdd * bdd -> bdd
    val INVIMP : bdd * bdd -> bdd
    val NAND   : bdd * bdd -> bdd
    val NOR    : bdd * bdd -> bdd
    val AND   : bdd * bdd -> bdd
    val XOR    : bdd * bdd -> bdd

    val NOT : bdd -> bdd

    val ITE : bdd -> bdd -> bdd -> bdd


    (* Quantification: *)
    type varSet
    val makeset  : varnum list -> varSet
    val makeset_ : varnum vector -> varSet
    val scanset  : varSet -> varnum vector
    val fromSet  : varSet -> bdd
    val toSet_   : bdd -> varSet

    val support  : bdd -> varSet

    val exist  : varSet -> bdd -> bdd
    val forall : varSet -> bdd -> bdd
    val appex  : bdd -> bdd -> bddop -> varSet -> bdd
    val appall : bdd -> bdd -> bddop -> varSet -> bdd


    (* Replacement: *)
    type pairSet
    val makepairSet : (varnum * varnum) list -> pairSet
    val replace : bdd -> pairSet -> bdd


    (* Composition: *)
    type composeSet
    val compose    : (varnum * bdd) -> bdd -> bdd
    val composeSet : (varnum * bdd) list -> composeSet
    val veccompose : composeSet -> bdd -> bdd


    (* Assignment: *)
    type assignment
    val assignment : (varnum * bool) list -> assignment
    val fromAssignment : assignment -> bdd
    val toAssignment_ : bdd -> assignment
    val getAssignment : assignment -> (varnum * bool) list 

    val restrict : bdd -> assignment -> bdd

    val satone : bdd -> assignment


    (* Simplification: *)
    val simplify  : bdd -> bdd -> bdd


    (* Variable Reordering: *)
    eqtype fixed
    val fixed : fixed
    val free  : fixed

    val addvarblock  : varnum -> varnum -> fixed -> unit
    val clrvarblocks : unit -> unit

    eqtype method
    val WIN2         : method
    val WIN2ITE      : method
    val SIFT         : method
    val SIFTITE      : method
    val RANDOM       : method
    val REORDER_NONE : method

    val reorder          : method -> unit
    val autoReorder      : method -> method
    val autoReorderTimes : method -> int -> method

    val getMethod : unit -> method
    val getTimes  : unit -> int

    val disableReorder : unit -> unit
    val enableReorder  : unit -> unit

    type level = int
    val varToLevel : varnum -> level
    val varAtLevel : level -> varnum


    (* Pretty printing and files: *)
    val printdot   : bdd -> unit
    val fnprintdot : string -> bdd -> unit
    val printset   : bdd -> unit
    val fnprintset : string -> bdd -> unit

    val bddSave : string -> bdd -> unit
    val bddLoad : string -> bdd

 
    (* Miscellaneous bdd operations: *)
    val satcount : bdd -> real
    val nodecount : bdd -> int
    val hash     : bdd -> int
    
    type nodetable = int * (varnum * int * int) Vector.vector
    val nodetable  : bdd -> nodetable 


    (* More administration *)
    val setMaxincrease : int -> int
    val setCacheratio  : int -> int
    val verbosegc      : (string * string) option -> unit

    type stats = {produced     : int,
		  nodenum      : int,
		  maxnodenum   : int,
		  freenodes    : int,
		  minfreenodes : int,
		  varnum       : int,
		  cachesize    : int,
		  gbcnum       : int}

    val stats    : unit -> stats

    (* The next is aliases for compatibility with MuDDy OCaml: *)

    val tt     : bdd
    val ff     : bdd

    val diff   : bdd * bdd -> bdd
    val imp    : bdd * bdd -> bdd
    val lessth : bdd * bdd -> bdd
    val biimp  : bdd * bdd -> bdd
    val bor    : bdd * bdd -> bdd
    val invimp : bdd * bdd -> bdd
    val nand   : bdd * bdd -> bdd
    val nor    : bdd * bdd -> bdd
    val band  : bdd * bdd -> bdd
    val xor    : bdd * bdd -> bdd

    val not : bdd -> bdd

    val ite : bdd -> bdd -> bdd -> bdd

    type ordering = method
    val win2         : method
    val win2ite      : method
    val sift         : method
    val siftite      : method
    val random       : method
    val reorder_none : method

end

(* Documentation:

   A couple of general comments, might make your tour'de bdd a more
   comfortable ride:
     * functions starting with an underscore _ are optimized versions
       of their non-underscored cousin.  They are, however, unsafe:
       certain constraints on parameters are not checked and you are
       responsible for making sure the constraints are satisfied.  In
       the documentation for the function you can see the constraints.

       If actual parameters doesn't satisfy the constraints "weird
       things" may happen, and your ML program may even "core dump"!

     * We have done our best to choose names that are: 1) close to the
       original buddy names, 2) usable in both O'Caml and SML, and 3)
       are practical for every day bdd hacking...  This obviously
       leads to compromises...


   Type [bdd] is the abstract type of bdds.

   Type [varnum] is an alias for the int type.  A varnum is a bdd
   variable number.  The bdd variables are numbered from 0 up to
   vars-1, where vars is the number of variables set with setVarnum,
   see below.

   [init nodesize cachesize] initiates the bdd package and must be
   called before any bdd operations are done.  nodesize is the initial
   number of nodes in the nodetable and cachesize is the fixed size of
   the internal caches.  Typical values for nodesize are 10000 nodes
   for small test examples and up to 1000000 nodes for large examples.
   A cache size of 10000 seems to work good even for large examples,
   but lesser values should do it for smaller examples.

   The initial number of nodes is not critical for any bdd operation
   as the table will be resized whenever there are too few nodes left
   after a garbage collection.  But it does have some impact on the
   efficency of the operations.

   init and done should only be called once per session.

   [setVarnum num] is used to define the number of variables used in
   the bdd package. It may be called more than one time, but only to
   increase the number of variables. num is the number of variables to
   use.

   [getVarnum ()] returns the number of variables used in the bdd
   package. See setVarnum above.

   [bdone ()] frees all memory used by the bdd package and resets the
   package to its initial state.  Notice that you should not call init
   later in the session.

   [toBool r] return the Boolean represented by r.  Raises Fail if r
   can not be represented as true or false.

   [fromBool b] gives the bdd representing b.

   [TRUE] the bdd that represents a tautology.

   [FALSE] the bdd that represents a contradiction.

   [equal x y] is true if x and y represent equivalent Boolean
   expressions.

   [ithvar i] gives the bdd representing the i'th variable.  The
   requested variable must be in the range define by setVarnum
   starting with 0 being the first.

   [nithvar i] gives the bdd representing the negation of the i'th
   variable.  The requested variable must be in the range defined by
   setVarnum starting with 0 being the first.

   [var r] gets the variable labeling the non-terminal bdd r.

   [low r] gets the false branch of the non-terminal bdd r.
   
   [high r] gets the true branch of the non-terminal bdd r.

   Type [bddop] represent the binary Boolean operator which can be used
   when constructing bdds. Truth tables:

   x y  |  And  Xor  Or  Nand  Nor  Imp  Biimp  Diff  Lessth  Invimp
  ------|------------------------------------------------------------
   T T  |   T    F   T    F     F    T     T     F      F       T
   T F  |   F    T   T	  T     F    F     F     T      F       T
   F T  |   F    T   T	  T     F    T     F     F      T       F
   F F  |   F    F   F	  T     T    T     T     F      F       T

   [apply x y opr] constructs the bdd using the binary Boolean
   operator opr applied to x and y.

   The following functions call apply; they are suitable for
   making infix operators:
     [DIFF(x,y)] equivalent to 'apply x y Diff', 
     [IMP(x,y)] equivalent to 'apply x y Imp',
     [LESSTH(x,y)] equivalent to 'apply x y Lessth',
     [BIIMP(x,y)] equivalent to 'apply x y Biimp',
     [OR(x,y)] equivalent to 'apply x y Or',
     [INVIMP(x,y)] equivalent to 'apply x y Invimp',
     [NAND(x,y)] equivalent to 'apply x y Nand',
     [NOR(x,y)] equivalent to 'apply x y Nor',
     [AND(x,y)] equivalent to 'apply x y And',
     [XOR(x,y)] equivalent to 'apply x y Xor',

   [NOT r] construct the negation of r.

   [ITE x y z] equivalent to 'bor(band(x, y), band(not x, z))'; but
   more efficient.  Corresponds to the common if-then-else notion
   'x->y, z'.

   Type [varSet] is an effective representation of sets of variables.

   [makeset varlist] makes the varSet with the elements in varlist.
   There is no constraint on varlist: it may be empty, contain duplets,
   or negative numbers.  Duplets and negative numbers will just be
   filtered out.

   [makeset_ varvector] makes the varSet with the elements in
   varvector.  There are many constraints on varvector: it must be
   sorted in increasing order, it may not contain duplets nor negative
   numbers.  This function is more effictive than makeset, but it is
   highly unsafe and you should only use it when you can guarantee
   that the input satisfies the constraints.

   [scanset varset] gives the vector of the variables in varset.

   [fromSet varset] gives the bdd representing the conjunction of
   all the variables in varset.

   [toSet_ r] converts the bdd r to a varSet; no checks are performed
   to check that r really represents a varSet therefore it should be
   used with care.

   [support r] gives the set of variables that r depends on.

   [exist varset r] constructs the existential quantification over all
   the variables in the varset of r.

   [forall varset r] constructs the universal quantification over all
   the variables in the varset of r.

   [appex x y opr varset] equivalent to 'exist varset (apply x y opr)'
   but more efficient.

   [appall x y opr varset] equivalent to 'forall varset (apply x y opr)'
   but more efficient.

   Type [pairSet] is used to represent sets of pairs of variables.
   Such sets are used by the replace algorithm below.

   [makepairSet [(x,x'),(y,y'),...,(z,z')]] creates the substitution
   which substitute x' for x, y' for y, ..., and z' for z.

   [replace r pairset] perfoms the substitution pairset on r.

   Type [composeSet] is used to represent sets of pairs.  Each pair
   (x,r) is a bdd variable and a bdd.  Such sets are used by the
   veccompose algorithm below.

   [compose (x1, r1) r] substitute the bdd r1 for the bdd variable x
   in the bdd r.

   [composeSet [(x1,r1),(x2,r2),...),(xn,rn)]] creates the composition
   which substitutes r1 for x1, r2 for x2, ..., and rn for vn.

   [veccompose compSet r] performs the composition of comSet on r.

   Type [assignment] represents an assignment of variables.  There are
   not less than three ways of representing assignments:
     * As an assignment list [(x1,true), (x3,false), (x2, true)],
     * As the abstract assignment type,

     * Using a bdd that obey special rules: 1) It consist of exactly
       one 1 path.  2) Variables where the high spine is followed
       corresponds to true assignments.  3) Variables where the low
       spine is followed corresponds to false assignments.

       For example the bdd for the expression 'band(x1,band(not x3,
       x2))' represents the assignment list shown above.

   [assignment assList] returns the assignment for assList.

   [fromAssignment ass] returns the bdd representing the assignment ass.

   [toAssignment_ r] returns the assignment represented by the bdd r.
   This is an unsafe function, make sure that the input is a proper
   representation of an assignment, see the assignment type above.

   [getAssignment ass] returns the assignment list for ass.

   [restrict r assign] restrict the variables in assign to tt or ff in
   r.

   [satone r] finds a satisfying variable assignment.  Raises Fail
   if r is equal to the bdd ff.

   [simplify r dom] tries to simplify r by restricting it to domain d,
   ie. 'r band dom = dom band (simplify r dom)'.  

   Type [fixed] Not documented, see the Buddy documentation.
   [fixed] Not documented, see the Buddy documentation.
   [free] Not documented, see the Buddy documentation.

   [addvarblock  v1 v2 fix] Not documented, see the Buddy documentation.
   [clrvarblocks ()] clears all variable blocks.

    Type [ordering] is the type of bdd variable orderings.  The
    following orderings exists:
       [WIN2]        
       [WIN2ITE]     
       [SIFT]    
       [SIFTITE]     
       [RANDOM]
       [REORDER_NONE]

   [reorder ord] reorders using the ordering ord.
   [autoReorder ord] Not documented, see the Buddy documentation.
   [autoReorderTimes ord n] Not documented, see the Buddy documentation.

   [getMethod ()] Not documented, see the Buddy documentation.
   [getTimes ()] Not documented, see the Buddy documentation.

   [disableReorder()] disaples automatic reordering.  Reordering is
   enabled by default as soon as variable blocks have been defined.

   [enableReorder()] Re-enables reordering after a call to
   disableReorder.

   Type [level] Not documented, see the Buddy documentation.
   [varToLevel v] Not documented, see the Buddy documentation.
   [varAtLevel l] Not documented, see the Buddy documentation.

   [printdot r] prints r in a format suitable for use by the graph drawing
   program dot, on standard output 
   (dot can be obtained from http://www.research.att.com/sw/tools/graphviz/)

   [fnprintdot fname r] prints r in a format suitable for use by the graph
   drawing program dot in the file fname.

   [printset r] prints all the truth assignments for r that would
   satisfy r, on standard output.

   [fnprintset fname r] prints all the truth assignments for r that would
   satisfy r, in the file fname.

   [bddSave name r] saves r in the file name.

   [bddLoad name] returns the bdd saved in the file name.

   [satcount r] returns how many possible variable assignments there
   exist such that r is satisfied, taking all defined variables into
   account.

   [nodecount r] returns the number of nodes in r.

   [hash r] returns a hash value for the bdd r.

   Type [nodetable] is a bdd node table represented by standard ML
   values.  The node table consist of a pair (i,t), where t is the
   actual node table consisting of triples (v,l,h).  v is the actual
   variable, l is the low index, h is the high index.  i is the
   top index for the node table.

   [nodetable r] returns the nodetable for r.


   [setMaxincrease n] tells BuDDy that the maximum of new nodes added
   when doing an expansion of the nodetable should be n.  Returns the
   old maximum.

   [setCacheratio n] sets the cache ratio to n.  For example, if n is
   four then the internal caches will be 1/4th the size of the
   nodetable.  Returns the old cache ratio.

   [verbosegc(SOME(pregc,postgc))] instructs BuDDy to print pregc when
   a BuDDy GC is initiated and print postgc when the BuDDy GC is
   completed. NONE is used to disable verbose garbage collection.

   Type [stats] represents various statistical information from the
   underlying BDD package:
     produced     : total number of new nodes ever produced.
     nodenum      : currently allocated number of bdd nodes.
     maxnodenum   : user defined maximum number of bdd nodes.
     freenodes    : number of currently free nodes.
     minfreenodes : minimum number of nodes that should be left after
                    a garbage collection (in the bdd library).
     varnum       : number of defined bdd variables,
     cachesize    : number of entries in the internal caches.
     gbcnum       : number of garbage collection done in the bdd library.

   [stats ()] returns the currents stats.

*)
