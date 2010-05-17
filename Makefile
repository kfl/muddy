##############################################################################
# MuDDy Main make file.

.PHONY: all install clean \
	buddy muddy-sml muddy-ocaml \
	install-buddy install-muddy-sml install-muddy-ocaml \
	clean-buddy clean-muddy-sml clean-muddy-ocaml \
	dist rpm 

##############################################################################
# Top-level targets:
all: 			buddy muddy-sml muddy-ocaml

install:		install-buddy install-muddy-sml muddy-ocaml

clean:			clean-it


##############################################################################
# Build targets:
buddy:
			$(MAKE) -C $@

muddy-sml:		buddy
			$(MAKE) -C $@

muddy-ocaml:		buddy
			$(MAKE) -C $@

##############################################################################
# Install targets:

install-buddy:		buddy
			$(MAKE) -C buddy install

install-muddy-sml:	muddy-sml
			$(MAKE) -C muddy-sml install

install-muddy-ocaml:	muddy-ocaml
			$(MAKE) -C muddy-ocaml install

##############################################################################
# Clean targets:
clean-it:
			@rm -f muddy.tgz *~
			$(MAKE) -C buddy clean
			$(MAKE) -C muddy-sml clean
			$(MAKE) -C muddy-ocaml clean

##############################################################################
# Administrative targets:

# Package distribution
EXCLUDE = --exclude CVS --exclude .cvsignore --exclude *~ 
dist:			clean
			rm -f /tmp/muddy.tgz
			tar czf /tmp/muddy.tgz $(EXCLUDE) -C .. muddy
			mv  /tmp/muddy.tgz .

# Linux RPM building:
RPM_SOURCES = $(HOME)/rpm/SOURCES
rpm:			dist
			rpm -ta muddy.tgz
