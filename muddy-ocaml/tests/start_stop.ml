open Bdd
let check () =
  ((if isRunning () then print_string "Buddy is running :-)" 
    else print_string "Buddy is not running :-("); print_newline())
let _ = check()
let _ = init 10000 10000
let _ = check ()
let _ = bdone ()
let _ = check ()
