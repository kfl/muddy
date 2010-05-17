(* Copyright (C) 1997-2001 by Ken Friis Larsen and Jakob Lichtenberg. *)
let _ = Bdd.init 10000 10000


let l = Fdd.extDomain [256,256,256,256,256,256,256,256,256,256]
let [V0,V1,V2,V3,V4,V5,V6,V7,V8,V9] = map varfdd l
let [C0,C1,C2,C3,C4,C5,C6,C7,C8,C9] = map (con 8) [0,1,2,3,4,5,6,7,8,9]

let r1 = mulfixed (C8, 1)
let r2 = equ (C8,r1)
let R2 = toBool r2

let r3 = sub (C6,C1)
let r4 = equ (C5,r3)
let R4 = toBool r4

let _ = fnprintdot("XequalsYplus42.dot") r2

