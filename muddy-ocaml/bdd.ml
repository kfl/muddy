(* Structure bdd is the core structure of the MuDDy BDD ML Interface.        *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

    type bdd
    type varSet = bdd
    type assignment = bdd
    type pairSet
    type composeSet = pairSet (* not the entire truth *)
    
    type varnum = int
    
    type bddop =
	And | Xor | Or | Nand | Nor | Imp | Biimp | Diff | Lessth | Invimp 
	
    
    type constants = int * int * int
       * int * int * int * int * int
       * int * int * int * int * int
       * int * int * int * int * int

    external constants_ : unit -> constants
	           = "mlbdd_constants"

    let (bddop_and, bddop_xor, bddop_or, bddop_nand, 
	 bddop_nor, bddop_imp, bddop_biimp, bddop_diff, 
	 bddop_less, bddop_invimp,
	 fixed, free, win2, win2ite, sift, siftite, random, reorder_none)
	= constants_ ()
	    
    let opr2i = 
	function And   -> bddop_and
	      | Xor    -> bddop_xor
	      | Or     -> bddop_or
	      | Nand   -> bddop_nand
	      | Nor    -> bddop_nor
	      | Imp    -> bddop_imp
	      | Biimp  -> bddop_biimp
	      | Diff   -> bddop_diff
	      | Lessth -> bddop_less
	      | Invimp -> bddop_invimp
	
    external apply_ : bdd -> bdd -> int -> bdd = "mlbdd_bdd_apply"
    external exist_  : bdd -> varSet -> bdd = "mlbdd_bdd_exist"
    external forall_ : bdd -> varSet -> bdd = "mlbdd_bdd_forall"
    external appall_ : bdd -> bdd -> int -> varSet -> bdd
	                                  = "mlbdd_bdd_appall"
    external appex_ : bdd -> bdd -> int -> varSet -> bdd ="mlbdd_bdd_appex"

    external mkset_ : varnum array -> varSet  = "mlbdd_makeset"


    external stats_ : unit -> int * int * int * int * int * int * int * int 
	                                  = "mlbdd_bdd_stats"


    external makepairset_ : varnum array -> varnum array -> pairSet
	                                  = "mlbdd_makepairset"

    external makebddpairset_ : varnum array -> bdd array -> pairSet
	                                  = "mlbdd_makebddpairset"


    external setVarnum : varnum -> unit = "mlbdd_bdd_setvarnum"
    external getVarnum : unit -> varnum = "mlbdd_getvarnum"

    external root : bdd -> int = "mlbdd_root"
    let hash = root
	
    external init : int -> int -> unit         = "mlbdd_bdd_init"
    external bdone : unit -> unit              = "mlbdd_bdd_done"
    external isRunning : unit -> bool          = "mlbdd_bdd_isrunning"

    external ithvar : varnum -> bdd  = "mlbdd_bdd_ithvar"
    external nithvar : varnum -> bdd = "mlbdd_bdd_nithvar"
    
    external fromBool : bool -> bdd = "mlbdd_fromBool"

    external var  : bdd -> varnum = "mlbdd_bdd_var"
    external low  : bdd -> bdd    = "mlbdd_bdd_low"
    external high : bdd -> bdd    = "mlbdd_bdd_high"

    let tt = fromBool true
    let ff = fromBool false

    external equal : bdd -> bdd -> bool = "mlbdd_equal"

    (* external toBool : bdd -> bool   = "mlbdd_toBool"
       FIXME Todo: I (Ken) like this definition better, but needs to make
              sure that the right exception is thrown.
    *)
    let toBool bdd = 
      if bdd = tt then true 
      else if bdd = ff then false 
      else failwith 
          "The input to bdd.toBool must be one of the constants TRUE or FALSE."


    external simplify : bdd -> bdd -> bdd = "mlbdd_bdd_simplify"

    external printdot : bdd -> unit             = "mlbdd_bdd_printdot"
    external fnprintdot : string -> bdd -> unit = "mlbdd_bdd_fnprintdot"

    external printset : bdd -> unit             = "mlbdd_bdd_printset"
    external fnprintset : string -> bdd -> unit = "mlbdd_bdd_fnprintset"

    external bddSave : string -> bdd -> unit    = "mlbdd_bdd_fnsave"
    external bddLoad : string -> bdd            = "mlbdd_bdd_fnload"


    external satcount : bdd -> float = "mlbdd_bdd_satcount"

    external nodecount : bdd -> int = "mlbdd_bdd_nodecount"
	
    type stats          = {produced     : int;
			    nodenum      : int;
			    maxnodenum   : int;
			    freenodes    : int;
			    minfreenodes : int;
			    varnum       : int;
			    cachesize    : int;
			    gbcnum       : int}


    let stats unit =
	let (p,nn,mn,fnum,minn,vn,cs,gn) = stats_ unit
	in 
	    {produced     = p;
	     nodenum      = nn;
	     maxnodenum   = mn;
	     freenodes    = fnum;
	     minfreenodes = minn;
	     varnum       = vn;
	     cachesize    = cs;
	     gbcnum       = gn}

    let makeset_ vector = if Array.length vector = 0 then (*Obj.magic*) ff
			  else mkset_ vector

    let rec nodup pre = function
	[] -> []
      |	(h :: tail) -> if pre = h then nodup pre tail
		       else h :: nodup h tail
    let makeset varlist =
	let positive = List.filter (function i -> i >= 0) varlist in 
	let sorted = Sort.list (<=) positive in
	let nodup  =
	  match sorted with
	    [] -> []
	  | h :: tail -> h :: nodup h tail
	in makeset_ (Array.of_list(nodup))

    external scanset : varSet -> varnum array = "mlbdd_bdd_scanset"

    external support : bdd -> varSet = "mlbdd_bdd_support"

    let fromSet = function x -> x
    let toSet_  = function x -> x

    let apply r1 r2 opr         = apply_ r1 r2 (opr2i opr)
    let exist vs r              = exist_ r vs
    let forall vs r             = forall_ r vs
    let appall r1 r2 opr varset = appall_ r1 r2 (opr2i opr) varset
    let appex r1 r2 opr varset  = appex_ r1 r2 (opr2i opr) varset



    external not : bdd -> bdd  = "mlbdd_bdd_not"
    external ite : bdd -> bdd -> bdd -> bdd = "mlbdd_bdd_ite"

    let band (r1, r2)    = apply_ r1 r2 bddop_and
    let xor (r1, r2)    = apply_ r1 r2 bddop_xor
    let bor (r1, r2)     = apply_ r1 r2 bddop_or
    let nand (r1, r2)   = apply_ r1 r2 bddop_nand
    let nor (r1, r2)    = apply_ r1 r2 bddop_nor
    let imp (r1, r2)    = apply_ r1 r2 bddop_imp
    let biimp (r1, r2)  = apply_ r1 r2 bddop_biimp
    let diff (r1, r2)   = apply_ r1 r2 bddop_diff
    let lessth (r1, r2) = apply_ r1 r2 bddop_less
    let invimp (r1, r2) = apply_ r1 r2 bddop_invimp

    external replace : bdd -> pairSet -> bdd = "mlbdd_bdd_replace"

    let makepairSet pl = 
        let (old,_new) = List.split pl
        in makepairset_ (Array.of_list old) (Array.of_list _new)

    external compose_ : bdd -> bdd -> varnum -> bdd 
	= "mlbdd_bdd_compose"

    let compose (v,r2) r1 = compose_ r1 r2 v

    external veccompose : pairSet -> bdd -> bdd = "mlbdd_bdd_veccompose"

    let composeSet pl =
        let (old, _new) = List.split pl
        in  makebddpairset_ (Array.of_list old) (Array.of_list _new)


    let assign res (v,b) = apply res (if b then ithvar v else nithvar v) And
    let assignment = (List.fold_left assign) tt
    let fromAssignment = function x -> x
    let toAssignment_ = function x -> x

    let isBool bdd = bdd = tt || bdd = ff

    let getAssignment bdd =
        let rec loop bdd acc =
                if isBool bdd then acc
                else let var = var bdd
                     and low = low bdd
		     and high = high bdd
                     in  if equal low ff then loop high ((var,true) :: acc)
                         else loop low ((var,false) :: acc)
        in  loop bdd []

    external restrict : bdd -> bdd -> bdd = "mlbdd_bdd_restrict"
    
    external satone_ : bdd -> assignment = "mlbdd_bdd_satone"
    let satone r = if equal r ff then failwith "Not satisfiable"
                   else satone_ r

    type nodetable = int * (varnum * int * int) array


    (* Three helper functions, used to normalize rootno *)
    let table size = (size, ref 0, Array.make size [])    
    let rec mem i lst = 
      match lst with
        [] -> None
      | ((k,d) :: t) -> if i = k then Some d
			     else mem i t  
    let add (size,next,tab) r = 
	let code = r mod size in
	let slot = Array.get tab code
	in
	    match mem r slot with
	      None -> let n = !next
			in  next := n + 1;
			    Array.set tab code ((r,n) :: slot);
			    n
	    | Some n -> n
    
    
    let valOf x = 
      match x with
	Some y -> y 
      | None -> raise (Invalid_argument "Internal error")

    let _compose f g = function x -> f(g(x))

    let nodetable r =
	let nc = nodecount r + 2 in
	let rootno = _compose (add (table nc)) root in
	let tab = Array.make nc None in
	let peek i = Array.get tab i in
	let update i d = Array.set tab i (Some d) in

	let rec node r =
		let root = rootno r
		in
		    match peek root with
			None   -> let low = low r
				  and high = high r
				  in  update root (var r, 
						   rootno low, 
						   rootno high);
				      node low;
				      node high
		      | Some _ -> ()
	in
	    update (rootno tt) (0,0,0);
	    update (rootno ff) (0,0,0);
	    node r;
	    (rootno r,
	     Array.init nc (function i -> valOf(Array.get tab i)))

    (* BuDDy tuning stuff *)
    external setMaxincrease : int -> int = "mlbdd_bdd_setmaxincrease"
    external setCacheratio  : int -> int = "mlbdd_bdd_setcacheratio"

    external setprintgc : bool -> string -> string -> unit = "mlbdd_setprintgc"

    let verbosegc lst =
      match lst with
	None -> setprintgc false "" ""
      | (Some(s1, s2)) -> setprintgc true s1 s2


    (* Variable reordering stuff *)
    type fixed = int
    external addvarblock  : varnum -> varnum -> fixed -> unit 
        = "mlbdd_bdd_intaddvarblock"
    external clrvarblocks  : unit -> unit = "mlbdd_bdd_clrvarblocks"


    type ordering = int
    external reorder     : ordering -> unit = "mlbdd_bdd_reorder"
    external autoReorder : ordering -> ordering = "mlbdd_bdd_autoreorder"
    external autoReorderTimes : ordering -> int -> ordering = "mlbdd_bdd_autoreorder_times"

    external getMethod : unit -> ordering =  "mlbdd_bdd_getreorder_method"
    external getTimes  : unit -> int    = "mlbdd_bdd_getreorder_times"

    external disableReorder : unit -> unit = "mlbdd_bdd_disable_reorder"
    external enableReorder  : unit -> unit = "mlbdd_bdd_enable_reorder"

    type level = int
    external varToLevel : varnum -> level = "mlbdd_bdd_var2level"
    external varAtLevel : level -> varnum = "mlbdd_bdd_level2var"
