include ../config

CFLAGS += -I$(MOSMLRUNTIME) -I$(BUDDYDIR)

CFILES=muddy.c
COBJS=muddy.$(OBJ_SUFFIX)

SMLS=MuddyCore.sml bdd.sml bvec.sml fdd.sml
SIGS=bdd.sig bvec.sig fdd.sig

SMLOBJ=MuddyCore.uo bdd.uo bvec.uo fdd.uo
SIGOBJ=bdd.ui bvec.ui fdd.ui

MUDDYLIBNAME=muddy.$(DYN_SUFFIX)

.SUFFIXES 	:
.SUFFIXES 	: .sml .sig .ui .uo .c .$(OBJ_SUFFIX)
.PHONY		: all tests clean


all: $(SMLOBJ) $(SIGOBJ)  $(MUDDYLIBNAME)

tests: all
	make -C tests

$(MUDDYLIBNAME): $(COBJS) $(BUDDYLIB)
ifneq ($(OS),Windows_NT)
	$(DYNLD) -o $@ $(COBJS) -L$(BUDDYDIR) -lbdd
else
	$(DYNLD) /out:$@ $(COBJS) $(BUDDYLIB)
endif

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

.c.$(OBJ_SUFFIX):
	$(CC) -c $(CFLAGS) -o $@ $<

clean:
	make -C tests clean
	rm -f *.u? *~ *.exp *.lib muddy.c
	rm -f *.$(OBJ_SUFFIX) *.$(DYN_SUFFIX)
	rm -f Makefile.bak

depend : 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak;	 \
         gcc -MM $(CFLAGS) *.c) > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

include ../common.mk

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
muddy.$(OBJ_SUFFIX): muddy.c
fdd.uo: fdd.ui bdd.ui MuddyCore.uo 
fdd.ui: bdd.ui 
bvec.uo: bvec.ui bdd.ui fdd.ui MuddyCore.uo 
bvec.ui: bdd.ui fdd.ui 
bdd.uo: bdd.ui MuddyCore.uo 
