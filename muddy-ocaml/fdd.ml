(* Structure fdd implements Finite Domain Blocks in MuDDy BDD ML Interface.  *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

    type precision = int
    type domain = int
    type fddvar = int
	
    external extDomain_ : int array -> fddvar = "mlfdd_extdomain"
  
    let rec mkList(start, stop) = 
	    if start > stop then []
	    else start::mkList(start+1, stop)

    let extDomain l = 
	    let i = extDomain_ (Array.of_list(l))
	    in mkList(i,i-1+List.length l)

    external clearAll: unit -> unit = "mlfdd_clearall"
    external domainNum: unit -> int = "mlfdd_domainnum"
    external domainSize: fddvar -> int = "mlfdd_domainsize"
    external varNum: fddvar -> int = "mlfdd_varnum"

    let compose__ f g = function x -> f(g(x))
	
    external vars__: fddvar -> Bdd.varnum array = "mlfdd_vars"
    let vars = compose__ Array.to_list vars__

    external ithSet: fddvar -> Bdd.varSet = "mlfdd_ithset"
    external domain: fddvar -> Bdd.bdd = "mlfdd_domain"
	
    external makeSet__: fddvar array -> Bdd.varSet = "mlfdd_makeset"
    let makeSet = compose__ makeSet__ Array.of_list
	
    external setPairs_: int array -> int array -> Bdd.pairSet =
      "mlfdd_setpairs"

    let setPairs l = 
	    let (l1,l2) = List.split l
	    in setPairs_ (Array.of_list l1) (Array.of_list l2)

    external scanallvars_ : fddvar -> int -> Bdd.bdd -> int array = "mlfdd_makeset"

    let scanallvars (os,n,b) =
       let v= scanallvars_ os n b in
       if not(Array.length v=n)
       then None
       else Some v

   external addvarblock: fddvar -> fddvar -> Bdd.fixed -> unit = "mlfdd_intaddvarblock"
