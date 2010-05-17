let _ = ( Bdd.init 100 100
        ; Bdd.setVarnum 10
        )

let var = Bdd.ithvar 

let (&&&) x y = Bdd.band(x, y)
let (|||) x y = Bdd.bor(x,y)
let (=>) x y = Bdd.imp(x,y)
let (<=>) x y = Bdd.biimp(x,y)
let nott = Bdd.not

let s, t, u, v, w, x, y, z = var 0, var 1, var 2, var 3, 
                             var 4, var 5, var 6, var 7

let p = x &&& nott y
let q = x ||| nott y
let r = p => q

let ass = Bdd.satone p
let assL = Bdd.getAssignment ass

let test1 = if r = Bdd.tt then "OK" else "ERROR"
let test2 = 
  if assL = [(6, false); (5, true)] || assL = [(5, true); (6, false)]
  then "OK" else "ERROR"

let _ = 
  List.iter print_endline
    [ test1
    ; test2
    ] 
