TESTS = simpletest.dif start_stop.dif

all: $(TESTS)
	@echo "Diff start {"
	@cat $(TESTS)
	@echo "Diff stop }"


.SUFFIXES	: .dif .out .exe .ml

.out.dif:
		@if diff $< $(@:.dif=.ref) | tee -a $@; \
		    then echo "$@ OK"; \
                    else echo "$@ Failed"; \
                    fi

.exe.out:
		./$< > $@

.ml.exe:
		ocamlopt  -I .. muddy.cmxa $<  -o $@

clean:
	rm -f *.cm* *.mli *.o *~ simpletest simpletest.out *.exe *.exp *.lib *.obj *.dif *.out

