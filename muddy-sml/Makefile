# Makefile for MuDDy

###
# Point this to where you unpacked mosml
MOSMLHOME=$(HOME)/projects/mosml2.10
# The 2.01 standard install:
# MOSMLHOME=/usr/lib/mosml

###
# Where you have installed BuDDy, leave undefined if you installed in a standard location like /usr
BUDDYDIR=$(HOME)/projects/muddy/buddy

###
# Your C compiler
CC=gcc

####
# How to link on your platform

## For Mac OS X the following works
LD=gcc -Wl,-rpath -Wl,${MOSMLHOME}/lib
DYNLD=gcc -bundle -undefined dynamic_lookup -bundle_loader ${MOSMLHOME}/bin/camlrunm


#### If everything goes well you shouldn't need to change anything below here
OPTS=-Wall -O3
CINCLUDE=-I$(MOSMLRUNTIME) -I$(BUDDYDIR)/include
CFLAGS=$(OSFLAGS) $(OPTS) $(CINCLUDE)
#LIBS=$(EXTRALIBS)

MOSMLBIN=$(MOSMLHOME)/bin
#MOSMLRUNTIME=$(MOSMLHOME)/src/runtime
MOSMLRUNTIME=$(MOSMLHOME)/include
MOSMLTOOLS=$(MOSMLBIN)/camlrunm $(MOSMLHOME)/tools
MOSMLC=$(MOSMLBIN)/mosmlc -c
MOSMLL=$(MOSMLBIN)/mosmlc 
MOSMLLEX=$(MOSMLBIN)/mosmllex
MOSMLYACC=$(MOSMLBIN)/mosmlyac

CFILES=muddy.c
COBJS=muddy.o

BUDDYSTATICLIB=$(BUDDYDIR)/lib/libbdd.a

SMLS=MuddyCore.sml bdd.sml bvec.sml fdd.sml
SIGS=bdd.sig  bvec.sig  fdd.sig

SMLSRC=$(SMLLS)
SIGSRC=$(SIGLS)

SMLOBJ=MuddyCore.uo bdd.uo bvec.uo fdd.uo
SIGOBJ=bdd.ui  bvec.ui  fdd.ui

.SUFFIXES :
.SUFFIXES : .sml .sig .ui .uo .c .o

.PHONY: muddy-mosml muddy-mlton

muddy-stamp: $(SMLOBJ) $(SIGOBJ) muddy-mlton.o muddy.so 
	touch muddy-stamp

muddy.c: ../muddy.c
	@echo "Getting a read-only copy of 'muddy.c'..."
	@cp -f ../muddy.c .
	@chmod a-w muddy.c

muddy-mlton.o: muddy-mlton.c 
	mlton -stop o -cc-opt -g -cc-opt -I./$(BUDDYDIR)/include muddy-mlton.c

muddy-mosml: $(SMLOBJ) $(SIGOBJ) muddy.so

muddy.so: $(COBJS)
	$(DYNLD) -o $@ $(COBJS) $(BUDDYSTATICLIB) $(LIBS)

$(BUDDYLIB):
	cd $(BUDDYDIR); make

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<

clean:
	rm -f *.u? 
	rm -f *.o *.so
	rm -f Makefile.bak
	rm -f muddy-stamp
	cd $(BUDDYDIR); make clean
	cd examples; make clean

depend : 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak;	 \
         gcc -MM $(CINCLUDE) $(OPTS) *.c) > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

pack:
	tar czC .. --exclude CVS --exclude .cvsignore -f ../muddy.tgz muddy


### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
muddy.o: muddy.c
fdd.uo: fdd.ui bdd.ui MuddyCore.uo 
fdd.ui: bdd.ui 
bvec.uo: bvec.ui bdd.ui fdd.ui MuddyCore.uo 
bvec.ui: bdd.ui fdd.ui 
bdd.uo: bdd.ui MuddyCore.uo 
