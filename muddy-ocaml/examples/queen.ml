(* Basic shorthands: *)
let ( &&& ) x y = Bdd.band   (x,y)
let ( ||| ) x y = Bdd.bor    (x,y)
let ( <-- ) x y = Bdd.invimp (x,y)
let ( --> ) x y = Bdd.imp    (x,y)
let ( <-> ) x y = Bdd.biimp  (x,y)
let ( </> ) x y = Bdd.xor    (x,y)

let tt = Bdd.tt
let ff = Bdd.ff

let foldn n c a =
  let v = ref a in
    for k = 0 to n-1 do
      v := (c (k,!v))
    done; !v


(* A couple of hacked lines: handle args, provide help, get parameter n: *)
let n = 
  let stop() = 
    (print_string "Usage: 'queen n', where n is between 1 and 16.\n";
     exit 1) in
  let v = try (match Sys.argv with [|_;n|] -> int_of_string n| _ -> stop()) 
  with _ -> stop() in
  if v >=1 && v <= 16 then v else stop()


(* Relationship between chess board (row,col)'s and corresponding bdd variable,
   0<=r<n, 0<=c<n *)

let var  (r,c)= (n*r + c)
let pvar (r,c)= Bdd.ithvar (var(r,c))
let nvar (r,c)= Bdd.nithvar(var(r,c))
let pos  i = (i / n, i mod n) (* pos is the inverse of var *)


(* solve: int -> Bdd.bdd builds a bdd representing all solutions to queen n *)
let solve () =
  let foldOr  f = foldn n (function (n,a)->(f(n) ||| a)) ff
  and foldAnd f = foldn n (function (n,a)->(f(n) &&& a)) tt
  and v = ref tt in

    for i = 0 to n-1 do (* iterate over all rows *)

      for j = 0 to n-1 do (* iterate over all cols *)
        (* If a queen is on (i,j) then: *)
        v := ((!v) &&& (pvar(i,j) --> (

          (* No other queens on row i: *)
          (foldAnd (function k->
	    if k=i then tt else nvar(i,k))) &&&

          (* No other queens on col j: *)
          (foldAnd (function k-> 
	    if k=j then tt else nvar(k,j))) &&&

          (* No other queens on diagonal LL -> UR: *)
          (foldAnd (function k->
	    if k=0 then tt else nvar((i+k) mod n,(j+k) mod n))) &&&

          (* No other queens on diagonal LL -> UR: *)
          (foldAnd (function k->
	    if k=0 then tt else nvar((n+i-k) mod n,(n+j-k) mod n))) &&&

        tt )))
      done;

      (* There must be a queen on each row i *)

      v := ((!v) &&& foldOr (function k->pvar(i,k)))

    done; !v

let _ =
  (Bdd.init 10000 10000;
   Bdd.setVarnum (n*n))

let solutions = solve()

let _ = print_string "Size of board:       ";
        print_int n;
        print_string "\n";
        print_string "Number of solutions: ";
        print_float (Bdd.satcount (solutions));
        print_string "\n";

(*
let exa () = 
  List.map (function (a,b) -> (pos a, b))
    (Bdd.getAssignment(Bdd.satone(solutions)))

let exa2 () = 
  List.map (function (a,b) -> (pos a))
    (List.filter (function (a,b) -> b)
       (Bdd.getAssignment(Bdd.satone(solutions))))
*)
