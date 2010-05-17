(* Structure MuddyCore provides internals for the MuDDy BDD ML Interface.    *)
(* Copyright (C) 1997-2002 by Ken Friis Larsen and Jakob Lichtenberg.        *)

open Dynlib

local
  val muddylibname="muddy.so"
  val path = case Process.getEnv "MUDDYHOME" of
                SOME p => Path.concat (p, 
				       Path.concat ("muddy-sml", muddylibname))
              | NONE   => muddylibname
	
  val hdl  = dlopen {lib = path, flag = RTLD_NOW, global = false}
in
  val symb = dlsym hdl
  fun cur2 h (a,b) = app2 h a b
  fun cur3 h (a,b,c) = app3 h a b c
  fun cur23 h a (b,c) = app3 h a b c
end
