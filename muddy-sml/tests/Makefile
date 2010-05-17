MUDDYHOME=..

all:
	mosmlc -I .. simpletest.sml -o simpletest
	MUDDYHOME=../..  ./simpletest >simpletest.out
	diff simpletest.ref simpletest.out

clean:
	rm -f simpletest *~ *.ui *.uo *.out


