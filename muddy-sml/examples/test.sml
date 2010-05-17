(* Copyright (C) 1997-2001 by Ken Friis Larsen and Jakob Lichtenberg. *)
; load "bvec"; open bdd; open fdd; open bvec; init 10000 10000;

val l = extDomain [256,256,256,256,256,256,256,256,256,256];
val [V0,V1,V2,V3,V4,V5,V6,V7,V8,V9] = map varfdd l
val [C0,C1,C2,C3,C4,C5,C6,C7,C8,C9] = map (con 8) [0,1,2,3,4,5,6,7,8,9]

val r1 = mulfixed (C8, 1)
val r2 = equ (C8,r1)
val R2 = toBool r2

val r3 = sub (C6,C1)
val r4 = equ (C5,r3)
val R4 = toBool r4

val _ = fnprintdot("XequalsYplus42.dot") r2

