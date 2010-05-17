MuDDy - ML interface to BuDDy
=============================

Homepage:
    http://github.com/kfl/muddy

Authors:
    Ken Friis Larsen  <ken@friislarsen.net> and Jakob Lichtenberg

Copyright (C) 1997-2010 by Ken Friis Larsen and Jakob Lichtenberg.


This README file contains:

1. WHAT IS MUDDY
2. ML IMPLEMENTATIONS AND PLATFORMS
3. REQUIREMENTS
4. INSTALLATION
5. STARTUP
6. DOCUMENTATION
7. EXAMPLES
8. HISTORY AND ACKNOWLEDGEMENTS



1. WHAT IS MUDDY
----------------

MuDDy is a ML interface to the Binary Decision Diagrams package BuDDy
written in C.  BuDDy is written by Jørn Lind-Nielsen <jln@itu.dk> and
is available at the homepage:
  http://www.itu.dk/research/buddy

Newer versions are at:
  http://sourceforge.net/projects/buddy/

MuDDy is used by a small set of people, and the functionality and
design of the interface corresponds to these people's need.  Please
comment on the design of the interface to the authors.

MuDDy-SML is the SML interface and MuDDy-OCaml is the O'Caml
interface.


2. ML IMPLEMENTATIONS AND PLATFORMS
-----------------------------------

From MuDDy 2.01 we support three ML implementations:
  * MuDDy-SML is the Moscow ML and MLton SML interface and
  * the MuDDy-OCaml is the O'Caml interface.

And we support three platforms:
  * Mac OS-X
  * Linux and
  * Windows.

3. REQUIREMENTS
---------------

###On all platforms:

Make:  MuDDy needs a non-broken make to install.

We recommend GNU make from Free Software Foundation. GNU make is included in
most Linux distributions, and is furthermore available from:
  http://www.gnu.org/software/make/make.html


###On Linux platforms:

MuDDy is developed under Linux and is not tested on other
UNIX-like systems.  However, it should work without problems on
other UNIX-like systems.  MuDDy/Mosml requires a UNIX-like system
with Moscow ML dynlib support.  Please report back if you are
able to compile MuDDy on other UNIX-like systems.


###On Windows platform

MuDDy is (was once) tested on:

* Windows XP,
* Using Visual Studio 6 SP5 (native) (compiler: cl and linker: link)
* GNU Make included in CygWin


###To use MuDDy-SML:

We use Moscow ML's foreign function interface to call the C functions
in BuDDy.  Therefore, MuDDy can only be used with Moscow ML (version
1.44 or newer).  (However, we are very interested if somebody wants to
port MuDDy to other SML systems and are willing to provide information
and help.)

This version of MuDDy is only tested under Moscow ML 2.00 and newer
compiled with dynlib support, however, the code should work on version
1.44 or newer compiled with dynlib support.  Moscow ML is available
from:
  http://www.dina.kvl.dk/~sestoft/mosml.html


###To use MuDDy-OCaml:

We have tested with various O'Caml versions starting from 3.04 to
3.11.1. O'Caml is available from:
  http://www.ocaml.org

Windows: We have only tested the native distribution.


4. INSTALLATION
---------------

### 4.1. Extracting the sources
To extract the package:
    tar xzf muddy.tgz
    cd muddy

The directory you are in now will be refered to as the MuDDy home dir.

    Todo: config-file

### 4.2. Building BuDDy (required)

First build BuDDy with:
    make buddy

The result of this process is:
    buddy/src/libbdd.a                # BuDDy static bdd library, Linux
    buddy/src/libbdd.lib              # BuDDy static bdd library, Windows
    buddy/src/{bdd,fdd,bvec}.h        # BuDDy header files

Documentation can be found in:
    buddy/doc/buddy.ps
    buddy/doc/bddnotes.ps
    buddy/doc/tech.txt


### 4.3. Building MuDDy-SML (optional)

After BuDDy is build you can build MuDDy-SML:
    make muddy-sml

The result of this process is the following files:
    muddy-sml/{bdd,fdd,bvec,MuddyCore}.ui  # Compiled signatures
    muddy-sml/{bdd,fdd,bvec,MuddyCore}.uo  # Compiled byte-code
    muddy-sml/muddy.so                     # Dynamic library, Linux
    muddy-sml/muddy.dll                    # Dynamic library, Windows

Comment: 1) The bdd, fdd and bvec SML modules all utilizes the
(internal) MuddyCore SML module.  2) The MuddyCore SML modules loads
the dynamic C library muddy.{so,dll}. 3) The dynamic C library
muddy.{so,dll} contains the interface code and the BuDDy static
library libbdd.{a,lib}.

The documentation is a README.sml file and three signature files:
    README.sml
    muddy-sml/{bdd,fdd.bvec}.sig
    muddy-sml/examples


------------------------------------------------------------------------------
### 4.4 Building MuDDy-OCaml (optional)

After BuDDy is build you can build MuDDy-OCaml:
    make muddy-ocaml

The result of this process is the following files:
    muddy-ocaml/{bdd,fdd,bvec}.cmi # Compiled signatures
    muddy-ocaml/{bdd,fdd,bvec}.cmo # Compiled Byte-code
    muddy-ocaml/{bdd,fdd,bvec}.cmx # Compiled native-code cmx files
    muddy-ocaml/{bdd,fdd,bvec}.o   # Compiled native-code objects (Linux)
    muddy-ocaml/{bdd,fdd,bvec}.obj # Compiled native-code objects (Windows)

    Todo:
    muddy-ocaml/libmuddy.a         # Todo
    muddy-ocaml/libmuddy.lib       # Todo
    muddy-ocaml/muddy.a            # Todo
    muddy-ocaml/muddy.cma          # Todo
    muddy-ocaml/muddy.cmxa         # Todo


The documentation is a README.ocaml file and three signature files:
    README.ocaml
    muddy-ocaml/{bdd,fdd.bvec}.mli
    muddy-ocaml/examples



5. STARTUP
----------

### 5.1 MuDDy-SML

To use the MuDDy-SML package, the Moscow ML runtime system must have
access to:
 * The shared library `muddy.so`
 * The compiled ML modules.

These files are all available in the MuDDy-SML dir after build.

If the `MUDDYHOME` environment variable points to the MuDDy home dir, MuDDy
will be able to find the shared library.

Therefore, assuming MuDDy home dir is the current directory, and you
use the bash shell:
    export MUDDYHOME=`pwd`

Or, if you use tcsh shell:
    setenv MUDDYHOME `pwd`

Now, when you use Moscow ML you must remember to tell it where the
compiled SML modules can found.  For example, to invoke the
interactive environment do:
    mosml -I $MUDDYHOME/muddy-sml

(If the `MUDDYHOME` environment variable is not set, the shared library
must be placed in the dynamic library load path, controlled by the
LD_LIBRARY_PATH environment variable, and you must still provide the
path to the SML modules when invoking Moscow ML.  For more information
on these matters refer to the Moscow ML documentation.)


### 5.2 MuDDy-OCaml

To build a new MuDDy enabled top level environment with MuDDy:
    ocamlmktop -I $MUDDYHOME/muddy-ocaml muddy.cma -o ocamlmuddy

To byte-code compile a file `foo.ml` that uses the MuDDy library:
    ocamlc -I $MUDDYHOME/muddy-ocaml muddy.cma foo.ml -o foo

To native-code compile a file foo.ml that uses the MuDDy library:
    ocamlopt -I $MUDDYHOME/muddy-ocaml muddy.cmxa foo.ml -o foo




----------------------------------------------------------------------------

6. DOCUMENTATION
----------------

Since MuDDy is just an interface to BuDDy, the documentation is based
on the documentation for BuDDy, available in $MUDDYHOME/buddy/doc/buddy.ps

MuDDy consists of three modules:
    * `bdd` implementing the functionality of the BuDDy `bdd.h` header
    * `fdd` implementing the functionality of the BuDDy `fdd.h` header
    * `bvec` implementing the functionality of the BuDDy `bvec.h` header

Each module has a sig/mli file.  In this file you can find:
    * Short documentation of the types and values.
    * A comparison to the BuDDy C types and function declarations.

You should be aware, that there is the general difference between
MuDDy and BuDDy that when you use MuDDy, reference counting of BDD
nodes are managed automatically.

Identifiers are almost equivalent between MuDDy-SML and MuDDy-OCaml.
Since SML and O'Caml disagrees about what exactly makes up an
identifier there is however minor differences:

Muddy-OCaml:  MuddySML:

tt            TRUE
ff            FALSE

diff          DIFF
imp           IMP
lessth        LESSTH
biimp         BIIMP
bor           OR
invimp        INVIMP
nand          NAND
nor           NOR
band          AND
xor           XOR

not           NOT

ite           ITE

ordering      method      (this is a type)
win2          WIN2
win2ite       WIN2ITE
sift          SIFT
siftite       SIFTITE
random        RANDOM
reorder_none  REORDER_NONE


Some tutorial material is includes in

 University of Cambridge Computer Laboratory Technical Report No. 481
    Combining the Hol98 Proof Assistant with the BuDDy BDD package

by Mike Gordon and Ken Friis Larsen.  The report is available at
	http://www.cl.cam.ac.uk/~mjcg/BDD/#TR


----------------------------------------------------------------------------
7. EXAMPLES
-----------

Surprisingly, the examples are in the examples directories!  In
muddy-sml/examples you find SML examples and in muddy-ocaml/examples
you find O'Caml examples.

(For now, we only provide one example: test.sml / test.ml)

To run the test example under MuDDy-SML, start Moscow ML in the
$MUDDYHOME/muddy-sml/examples dir, and write:

    use "test.sml";

Remember to follow the instructions in the STARTUP section regarding
the MUDDYHOME variable, and the -I $MUDDYHOME directive.

To run the test example under MuDDy-OCaml, start your MuDDy enabled
top environment in the $MUDDYHOME/muddy-ocaml/examples dir, and write:

    #use "test.ml";;

Remember to follow the instructions in the STARTUP section to see how
to make a MuDDy enabled top environment.



----------------------------------------------------------------------------
8. HISTORY AND ACKNOWLEDGEMENTS
-------------------------------

The first version of MuDDy was written by Ken Friis Larsen while
visiting Mike Gordon at Computer Lab. at University of Cambridge (UK),
in autumn 1997 and spring 1998.  Jakob Lichtenberg then extended MuDDy
to cope with the new BuDDy features: Finite Domain Blocks (`fdd`s) and
Boolean Vectors (`bvec`s).  Recently (2001, 2002) we added support for
O'Caml.

It should be stressed that MuDDy is only a type safe SML wrapping
around Jørn Lind-Nielsen's <jln@itu.dk> great BuDDy package, and that
all the "hard work" is done by the BuDDy package.  Jørn Lind-Nielsen
has answered lots of BuDDy questions (and BDD questions), and have
been willing to change the BuDDy package to make the SML wrapping more
easy.

A special thanks should also go to Peter Sestoft who have answered
tons of questions about the Moscow ML/Caml Light runtime system, and
he even audited the C code at one point to help finding a bug.

The wrapping of C functions caused surprisingly few problems, this is
mainly due to the magnificent designed Caml Light runtime system that
Moscow ML is based upon.  Caml Light is created by Xavier Leroy and
Damien Doligez and is available from:
  http://pauillac.inria.fr/caml/overview-caml-light-eng.html

The first usage of MuDDy was in the Hol98 theorem prover.  This usage
has influenced lots of decisions about design of the interface.

We would also like to thanks the following people who have provided
lots of good feedback and encouragement (listed in alphabetic order):

    Henrik Reif Andersen (IT University of Copenhagen)
    Mike Gordon          (Computer Lab. Univerity of Cambridge (UK))
    Jesper Møller        (IT University of Copenhagen)
    Michael Norris       (Computer Lab. Univerity of Cambridge (UK))
    Konrad Slind         (Computer Lab. Univerity of Cambridge (UK))



COPYRIGHT NOTICE for MuDDy
--------------------------

MuDDy - a SML interface to the Binary Decision Diagrams package BuDDy
Copyright (c) 1997-2010 Ken Friis Larsen and Jakob Lichtenberg

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
