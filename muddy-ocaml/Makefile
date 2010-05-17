include ../config

# The targets we wish to build:
TARGETS = \
	muddy-ocaml$(EXE_SUFFIX) muddy.cma muddy.cmxa libmuddy.$(LIB_SUFFIX) \
	bdd.cmi fdd.cmi bvec.cmi

# A couple of extra files are build:
#   foo.cmx  also gives you foo.$(OBJ_SUFFIX)
#   foo.cmxa also gives you foo.$(LIB_SUFFIX)
INSTALL_FILES = $(TARGETS) muddy.$(LIB_SUFFIX) bdd.mli fdd.mli bvec.mli

MUDDYOCAML_INSTALLDIR = c:/slam/muddy

.SUFFIXES 	:
.SUFFIXES	: .ml .mli .cmi .cmx .cmo
.PHONY		: all tests clean

all:		$(TARGETS)	

install:	all
		mkdir -p $(MUDDYOCAML_INSTALLDIR)
		cp $(INSTALL_FILES) $(MUDDYOCAML_INSTALLDIR)

uninstall:
		cd $(MUDDYOCAML_INSTALLDIR); rm -f $(INSTALL_FILES)

tests:		all
		$(MAKE) -C tests


muddy-ocaml$(EXE_SUFFIX):	muddy.cma libmuddy.$(LIB_SUFFIX)
		ocamlmktop -cclib -L. muddy.cma -o muddy-ocaml$(EXE_SUFFIX)

muddy.cmxa:	bdd.cmx fdd.cmx bvec.cmx 
		ocamlopt -ccopt "$(CFLAGS)" -a -o muddy.cmxa -cclib -lmuddy bdd.cmx fdd.cmx bvec.cmx 

muddy.cma:	bdd.cmo fdd.cmo bvec.cmo 
		ocamlc -a -o muddy.cma -custom -ccopt "$(CFLAGS)" -cclib -lmuddy bdd.cmo fdd.cmo bvec.cmo 

libmuddy.$(LIB_SUFFIX):	muddy.$(OBJ_SUFFIX) $(BUDDYLIB)
		cp $(BUDDYLIB) libmuddy.$(LIB_SUFFIX)
		$(LINK) libmuddy.$(LIB_SUFFIX) muddy.$(OBJ_SUFFIX)

muddy.$(OBJ_SUFFIX):	muddy.c
		ocamlopt -c -ccopt "$(CFLAGS)" -I $(BUDDYDIR) -I $(CAMLRUNTIME) muddy.c

.mli.cmi:
		ocamlopt -c -ccopt "$(CFLAGS)" $<

.ml.cmx:
		ocamlopt -c -ccopt "$(CFLAGS)" $<

.ml.cmo:
		ocamlc -c -ccopt "$(CFLAGS)" $<

clean:
		rm -f *.cm* *.$(OBJ_SUFFIX) *.$(LIB_SUFFIX) *~ muddy.c *.exe *.exp $(TARGETS)
		make -C tests clean

include ../common.mk

# Dependencies:

fdd.cmi:	bdd.cmi
bvec.cmi:	fdd.cmi

bdd.cmx:	bdd.cmi
fdd.cmx:	fdd.cmi
bvec.cmx:	bvec.cmi

bdd.cmo:	bdd.cmi
fdd.cmo:	fdd.cmi
bvec.cmo:	bvec.cmi
