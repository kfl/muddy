(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg. *)
let _ = ( Bdd.init 100 100
        ; Bdd.setVarnum 10
        ) 

let l = Fdd.extDomain [256;256;256;256;256;256;256;256;256;256]
let [v0;v1;v2;v3;v4;v5;v6;v7;v8;v9] = List.map Bvec.varfdd l
let [c0;c1;c2;c3;c4;c5;c6;c7;c8;c9] = List.map (Bvec.con 8) [0;1;2;3;4;5;6;7;8;9]

let r42 = Bvec.mulfixed (c8, 7) (* r = 42 *)
let rh = Bvec.add (v1,r42)
let res = Bvec.equ (v0,rh)

let _ = Bdd.fnprintdot("fddtest.dot") res (* v0 = v1+42 *)

