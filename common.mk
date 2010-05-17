##############################################################################
# General muddy stuff, used in muddy-sml and muddy-ocaml

muddy.c: ../muddy.c
	@echo "Getting a read-only copy of 'muddy.c'..."
	@cp -f ../muddy.c .
	@chmod a-w muddy.c

$(BUDDYLIB):
	@echo "Building '$(BUDDYLIB)'..."
	@cd $(BUDDYDIR); make
