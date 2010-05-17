(* Structure bdd is the core structure of the MuDDy BDD ML Interface.        *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

(* MLton version. Ken Friis Larsen 2003 *)

structure bdd :> bdd =
struct
	
    type cptr = MLton.Pointer.t

    structure F = Finalizable
    structure FXP = FinalizableXP

    type root = int
    type bdd = root F.t
    type varSet = bdd
    type assignment = bdd	
    type bddPair = cptr
    type pairSet = bddPair F.t
    type composeSet = pairSet (* not the entire truth *)
    
    type varnum = int

    fun failwith s = raise Fail s

    fun checkError code =
        if code < 0 then failwith ("BuDDy error code: "^Int.toString code)
        else ()

    val setprintgc_ = _import "mlbdd_setprintgc" :  bool * string * string -> unit;
    val maxnum : int option ref = ref(SOME 5000)

    fun gcRatio new = let val old = !maxnum
                      in  old before maxnum := new
                      end

    fun verbosegc NONE           = setprintgc_(false, "", "")
      | verbosegc (SOME(s1, s2)) = setprintgc_(true, s1, s2)

    val init_ = _import "bdd_init" : int * int -> int;
    fun init nodesize cachesize = ( checkError(init_(nodesize, cachesize))
                                  ; verbosegc NONE
                                  )
        
    val bdone = _import "bdd_done" : unit -> unit;
    val isRunning_ = _import "bdd_isrunning" :  unit -> int;
    fun isRunning () = isRunning_() = 1

    val setVarnum_ = _import "bdd_setvarnum" : varnum -> int;
    fun setVarnum no = checkError(setVarnum_ no)

    val getVarnum = _import "bdd_varnum" : unit -> varnum;

    val delref_ = _import "bdd_delref" : root -> unit;
    val addref_ = _import "bdd_addref" : root -> unit;


    fun create root = ( addref_ root
                      ; let val new = case !maxnum of
                                          NONE => F.new
                                        | SOME n => FXP.new {used = 1, max = n}
                            val bdd = new root
                        in  F.addFinalizer(bdd, delref_)
                          ; bdd
                        end
                      )
    fun cheap root = F.new root



    val withRoot = F.withValue

    val ithvar_ = _import "bdd_ithvar" : varnum -> root;
    val ithvar = cheap o ithvar_

    val nithvar_ = _import "bdd_nithvar" : varnum -> root;
    val nithvar = cheap o nithvar_


    val fromBool_ = _import "mlbdd_fromBool" : bool -> root;
    val fromBool = cheap o fromBool_

    val TRUE = fromBool true
    val FALSE = fromBool false

(*
    val trueroot = _import "bddtrue" : root;
    val falseroot = _import "bddfalse" : root;	
    

    val TRUE = create trueroot
    val FALSE = create falseroot

    fun fromBool b = if b then TRUE else FALSE
*)                   
    datatype bddop =
	And | Xor | Or | Nand | Nor | Imp | Biimp | Diff | Lessth | Invimp 


    val constants_ = _import "mlbdd_constants" : int array -> unit;

    val (bddop_and, bddop_xor, bddop_or, bddop_nand, 
	 bddop_nor, bddop_imp, bddop_biimp, bddop_diff, 
	 bddop_less, bddop_invimp,
	 fixed, free, WIN2, WIN2ITE, SIFT, SIFTITE, RANDOM, REORDER_NONE)
	= let val arr = Array.array(18, 0)
          in  constants_ arr
            ;( Array.sub(arr,0),
               Array.sub(arr,1),
               Array.sub(arr,2),
               Array.sub(arr,3),
               Array.sub(arr,4),
               Array.sub(arr,5),
               Array.sub(arr,6),
               Array.sub(arr,7),
               Array.sub(arr,8),
               Array.sub(arr,9),
               Array.sub(arr,10),
               Array.sub(arr,11),
               Array.sub(arr,12),
               Array.sub(arr,13),
               Array.sub(arr,14),
               Array.sub(arr,15),
               Array.sub(arr,16),
               Array.sub(arr,17))
          end

	    
    val opr2i = 
	fn And    => bddop_and
	 | Xor    => bddop_xor
	 | Or     => bddop_or
	 | Nand   => bddop_nand
	 | Nor    => bddop_nor
	 | Imp    => bddop_imp
	 | Biimp  => bddop_biimp
	 | Diff   => bddop_diff
	 | Lessth => bddop_less
	 | Invimp => bddop_invimp
	
    val apply_ = _import "bdd_apply" : root * root * int -> root;
    fun apply r1 r2 opr = 
        F.withValue(r1, fn r1 =>
        F.withValue(r2, fn r2 =>
          create(apply_(r1, r2, opr2i opr))))

    val NOT_ = _import "bdd_not" : root -> root;
    fun NOT r = F.withValue(r, create o NOT_)

    val ITE_ = _import "bdd_ite" : root * root * root -> root;
    fun ITE r1 r2 r3 =
        F.withValue(r1, fn r1 =>
        F.withValue(r2, fn r2 =>
        F.withValue(r3, fn r3 =>
          create(ITE_(r1, r2, r3)))))


    fun AND    (r1, r2) = apply r1 r2 And
    fun XOR    (r1, r2) = apply r1 r2 Xor
    fun OR     (r1, r2) = apply r1 r2 Or
    fun NAND   (r1, r2) = apply r1 r2 Nand
    fun NOR    (r1, r2) = apply r1 r2 Nor
    fun IMP    (r1, r2) = apply r1 r2 Imp
    fun BIIMP  (r1, r2) = apply r1 r2 Biimp
    fun DIFF   (r1, r2) = apply r1 r2 Diff
    fun LESSTH (r1, r2) = apply r1 r2 Lessth
    fun INVIMP (r1, r2) = apply r1 r2 Invimp


    val exist_ = _import "bdd_exist" :  root * root -> root;
    val forall_ = _import "bdd_forall" :  root * root -> root;
    val appall_ = _import "bdd_appall" :  root * root * int * root -> root ;
    val appex_ = _import "bdd_appex" :  root * root * int * root -> root ;

    fun exist vs r = 
        F.withValue(vs, fn vs =>
        F.withValue(r, fn r =>
          create(exist_(r, vs))))

    fun forall vs r =
        F.withValue(vs, fn vs =>
        F.withValue(r, fn r =>
          create(forall_(r, vs))))

    fun appall r1 r2 opr varset = 
        F.withValue(r1, fn r1 =>
        F.withValue(r2, fn r2 =>
        F.withValue(varset, fn varset =>
          create(appall_(r1, r2, opr2i opr, varset)))))

    fun appex r1 r2 opr varset  = 
        F.withValue(r1, fn r1 =>
        F.withValue(r2, fn r2 =>
        F.withValue(varset, fn varset =>
          create(appex_(r1, r2, opr2i opr, varset)))))


    fun makesetV vector = if Vector.length vector = 0 then FALSE
			  else Vector.foldl (fn(i, res) => AND(ithvar i, res))
                                            TRUE vector
   
    fun makeset [] = FALSE
      | makeset vs = List.foldl (fn(i, res) => AND(ithvar i, res))
                                TRUE vs

    val root : bdd -> int = fn r => F.withValue(r, fn r => r)
    val hash = root

    fun equal r1 r2 = 	
        F.withValue(r1, fn r1 =>
        F.withValue(r2, fn r2 => r1 = r2))


    val var_  = _import "bdd_var"  : root -> varnum;
    val low_  = _import "bdd_low"  : root -> root;
    val high_ = _import "bdd_high" : root -> root;

    fun var r = F.withValue(r, var_)
    fun low r = F.withValue(r, create o low_)
    fun high r = F.withValue(r, create o high_)

    fun toBool r =
        if equal r TRUE then true
        else if equal r FALSE then false
        else failwith 
          "The input to bdd.toBool must be one of the constants TRUE or FALSE."


    fun assign((v,b), res) = 
        apply res (if b then ithvar v else nithvar v) And 
	
    val assignment = List.foldl assign TRUE
    val fromAssignment = fn x => x
    val toAssignment_ = fn x => x

    fun isBool bdd = equal bdd TRUE orelse equal bdd FALSE

    fun getAssignment bdd =
        let fun loop bdd acc =
                if isBool bdd then acc
                else let val var = var bdd
                         val low = low bdd
			 val high = high bdd
                     in  if equal low FALSE then loop high ((var,true) :: acc)
                         else loop low ((var,false) :: acc)
                     end
        in  loop bdd [] end

    val fromSet = fn x => x
    val toSet_  = fn x => x


    val support_ = _import "bdd_support" : root -> root;
    fun support r = F.withValue(r, fn r => create(support_ r))


    fun scanset bdd =
        let fun loop bdd acc =
                if isBool bdd then vector acc
                else loop (high bdd) (var bdd :: acc)
        in  loop bdd [] end

    val freepair_ = _import "bdd_freepair" : bddPair -> unit;
    val newpair_  = _import "bdd_newpair" : unit -> bddPair;
    val setpairs_ = _import "bdd_setpairs" 
                  : bddPair * int vector * int vector * int -> int;
    val setbddpairs_ = _import "bdd_setbddpairs" 
                     : bddPair * root vector * root vector * int -> int;


    fun createPair bp = let val p = FXP.new {used = 0, max = 1} bp
                        in  F.addFinalizer(p, freepair_)
                          ; p
                        end

    fun withNewPair f =
        let val pair = createPair(newpair_())
        in  F.withValue(pair, f)
          ; pair
        end

    fun makepairSet pl = 
        let val (old,new) = ListPair.unzip pl
            val old = vector old
            val new = vector new
        in  withNewPair(fn pair =>
              checkError(setpairs_(pair, old, new, Vector.length old)))
        end

    fun makebddpairset_ pl = 
        let val (old,new) = ListPair.unzip pl
            val old = vector old
            val new = vector new
        in  withNewPair(fn pair =>
              checkError(setbddpairs_(pair, old, new, Vector.length old)))
        end

    fun composeSet pl =
        let fun getToTheButtom ([], acc) = makebddpairset_(rev acc)
              | getToTheButtom ((old,new) :: rest, acc)  =

                F.withValue(new, fn new => 
                  getToTheButtom(rest, (old,new) :: acc))
        in  getToTheButtom(pl, [])
        end

    val compose_ = _import "bdd_compose" :  root * root * varnum -> root ;
    fun compose (v,r2) r1 = 
        F.withValue(r1, fn r1 =>
        F.withValue(r2, fn r2 => create(compose_(r1, r2, v))))
                   

    val veccompose_ = _import "bdd_veccompose" : root * bddPair -> root;
    fun veccompose cs r =
        F.withValue(cs, fn cs =>
        F.withValue(r, fn r => create(veccompose_(r, cs))))

    val restrict_ = _import "bdd_restrict" :  root * root -> root;
    fun restrict r1 r2 = 	
        F.withValue(r1, fn r1 =>
        F.withValue(r2, fn r2 => create(restrict_(r1, r2))))
     

    val satone_ = _import "bdd_satone" :  root -> root ;
    fun satone r = if equal r FALSE then failwith "Not satisfiable"
                   else F.withValue(r, fn r => create(satone_ r))


    val replace_ = _import "bdd_replace" :  root * bddPair -> root ;
    fun replace r ps = 
        F.withValue(r, fn r => 
        F.withValue(ps, fn ps =>
          create(replace_(r, ps))))




    type stats = {produced     : int,
		  nodenum      : int,
		  maxnodenum   : int,
		  freenodes    : int,
		  minfreenodes : int,
		  varnum       : int,
		  cachesize    : int,
		  gbcnum       : int}

    val stats_ = _import "mlbdd_bdd_stats" : int array -> unit;
    fun stats unit =
	let val arr = Array.array(8, 0) 
	in  stats_ arr
          ; { produced     = Array.sub(arr, 0),
              nodenum      = Array.sub(arr, 1),
              maxnodenum   = Array.sub(arr, 2),
              freenodes    = Array.sub(arr, 3),
              minfreenodes = Array.sub(arr, 4),
              varnum       = Array.sub(arr, 5),
              cachesize    = Array.sub(arr, 6),
              gbcnum       = Array.sub(arr, 7)
             }
	end


    val satcount_ = _import "bdd_satcount" :  root -> real ;
    fun satcount r = F.withValue(r, satcount_)

    val nodecount_ = _import "bdd_nodecount" :  root -> int ;
    fun nodecount r = F.withValue(r, nodecount_)

    
    val simplify_ = _import "bdd_simplify" :  root * root -> root;
    fun simplify r1 r2 =
        F.withValue(r1, fn r1 =>
        F.withValue(r2, fn r2 => create(simplify_(r1, r2))))


    val printdot_ = _import "bdd_printdot" :  root -> unit;
    fun printdot r = F.withValue(r, printdot_)


    val fnprintdot_ = _import "bdd_fnprintdot" :  string * root -> unit ;
    fun fnprintdot filename r =
        let val cstring = filename ^ "\000"
        in  F.withValue(r, fn r => fnprintdot_(cstring, r))
        end


    val printset_ = _import "bdd_printset" :  root -> unit;
    fun printset r = F.withValue(r, printset_)

    val fnprintset_ = _import "bdd_fnprintset" :  string * root -> unit;
    fun fnprintset filename r =
        let val cstring = filename ^ "\000"
        in  F.withValue(r, fn r => fnprintset_(cstring, r))
        end

    val bddSave_ = _import "bdd_fnsave" :  string * root -> unit;
    fun bddSave filename r =
        let val cstring = filename ^ "\000"
        in  F.withValue(r, fn r => bddSave_(cstring, r))
        end

    val bddLoad_ = _import "bdd_fnload" :  string -> root;
    fun bddLoad filename =
        let val cstring = filename ^ "\000"
        in  create(bddLoad_ cstring)
        end



    type nodetable = int * (varnum * int * int) Vector.vector


    (* Three helper functions, used to normalize rootno *)
    fun table size = (size, ref 0, Array.array(size,[]))    
    fun mem i []           = NONE
      | mem i ((k,d) :: t) = if i = k then SOME d
			     else mem i t  
    fun add (size,next,tab) r = 
	let val code = r mod size
	    val slot = Array.sub(tab,code)
	in
	    case mem r slot of
		NONE => let val n = !next
			in  next := n + 1;
			    Array.update(tab, code, (r,n) :: slot);
			    n
			end
	      | SOME n => n
	end
    
    fun nodetable r =
	let val nc = nodecount r + 2
	    val rootno = add (table nc) o root
	    val tab = Array.array(nc,NONE)
	    fun peek i = Array.sub(tab, i)
	    fun update i d = Array.update(tab,i,SOME d)
		
	    fun node r =
		let val root = rootno r
		in
		    case peek root of
			NONE   => let val low = low r
				      val high = high r
				  in  update root (var r, 
						   rootno low, 
						   rootno high);
				      node low;
				      node high
				  end
		      | SOME _ => ()
		end
	in
	    update (rootno TRUE) (0,0,0);
	    update (rootno FALSE) (0,0,0);
	    node r;
	    (rootno r,
	     Vector.tabulate(nc, fn i => valOf(Array.sub(tab,i))))
	end	

    (* BuDDy tuning stuff *)
    val setMaxincrease = _import "bdd_setmaxincrease" :  int -> int ;
    val setCacheratio = _import "bdd_setcacheratio" :  int -> int ;


    (* Variable reordering stuff *)
    type fixed = int
    val addvarblock_ = _import "bdd_intaddvarblock" 
                     : varnum * varnum * fixed -> unit;
    fun addvarblock v1 v2 fix = addvarblock_(v1, v2, fix)

    val clrvarblocks = _import "bdd_clrvarblocks" :  unit -> unit ;


    type method = int
    val reorder = _import "bdd_reorder" :  method -> unit ;
    val autoReorder = _import "bdd_autoreorder" :  method -> method ;
    val autoReorderTimes_ = _import "bdd_autoreorder_times" 
                          :  method * int -> method ;
    fun autoReorderTimes method times = autoReorderTimes_(method, times)

    val getMethod = _import "bdd_getreorder_method" :  unit -> method;
    val getTimes = _import "bdd_getreorder_times" :  unit -> int;

    val disableReorder = _import "bdd_disable_reorder" :  unit -> unit ;
    val enableReorder = _import "bdd_enable_reorder" :  unit -> unit ;

    type level = int
    val varToLevel = _import "bdd_var2level" :  varnum -> level ;
    val varAtLevel = _import "bdd_level2var" :  level -> varnum ;


    (* The next is aliases for compatibility with MuDDy OCaml: *)

    val tt           = TRUE   	     
    val ff           = FALSE  	     
		       		    	     
    val diff         = DIFF   	     
    val imp          = IMP    	     
    val lessth       = LESSTH 	     
    val biimp        = BIIMP  	     
    val bor          = OR     	     
    val invimp       = INVIMP 	     
    val nand         = NAND   	     
    val nor          = NOR    	     
    val band         = AND    	     
    val xor          = XOR    	     
		       			     
    val not          = NOT           
		       		    	     
    val ite          = ITE           
		       		    	     
    type ordering    = method
    val win2         = WIN2          
    val win2ite      = WIN2ITE       
    val sift         = SIFT          
    val siftite      = SIFTITE       
    val random       = RANDOM        
    val reorder_none = REORDER_NONE  
    
end
