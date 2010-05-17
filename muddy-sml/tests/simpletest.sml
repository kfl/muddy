val _ = ( bdd.init 100 100
        ; bdd.setVarnum 10
        )

val var = bdd.ithvar 

infix AND OR IMP ==
val (op AND, op OR, op IMP) = (bdd.AND, bdd.OR, bdd.IMP)
fun x == y = bdd.equal x y

val NOT = bdd.NOT

val (s, t, u, v, w, x, y, z) = (var 0, var 1, var 2, var 3, 
                                var 4, var 5, var 6, var 7)

val p = x AND NOT y
val q = x OR NOT y
val r = p IMP q

val ass = bdd.satone p
val assL = bdd.getAssignment ass

val test1 = if r == bdd.TRUE then "OK" else "ERROR"
val test2 = 
  if assL = [(6, false), (5, true)] orelse assL = [(5, true), (6, false)]
  then "OK" else "ERROR"

val _ = 
  List.app (fn s => print(s^"\n"))
    [ test1
    , test2
    ] 
