all:		queen.exe

clean:
		rm -f *~ *.cmi *.cmx *.exe *.exp *.lib *.obj

queen.exe:	queen.ml
		ocamlopt -I .. muddy.cmxa queen.ml -o queen.exe
