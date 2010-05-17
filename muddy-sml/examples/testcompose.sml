load"bdd";

val _ = (bdd.init 100 100; bdd.setVarnum 10)

val x0 = bdd.ithvar 0
val x1 = bdd.ithvar 1
val x2 = bdd.ithvar 2
val x3 = bdd.ithvar 3

val b1 = bdd.AND(x0,x1) 
val b2 = bdd.AND(x2,x3) 

val comp = bdd.composeSet [(0,x2),(1,x3)]
val b3 = bdd.veccompose comp b1
val test1 = bdd.equal b2 b3
val b4 = bdd.compose (0,b2) b1
val test2 = bdd.equal b4 (bdd.AND(x1,b2))
