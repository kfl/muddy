0. Install BuDDy
    * Download BuDDy from <http://sourceforge.net/projects/buddy/>
    * `$ configure --prefix=BUDDYDIR`
    * `$ make`
    * `$ make install`

1. Compile Moscow ML version of MuDDy 
   (in `muddy-sml` dir):
    * Edit `Makefile`, most important is the `BUDDYDIR` and `MOSMLHOME` variables
    * `$ make muddy-mosml`
    * Now test your installation
    * ``$ export MUDDYHOME=`pwd` ``
    * `$ cd test`
    * `$ mosmlc -I $MUDDYHOME simpletest.sml -o simpletest`
    * `$ simpletest > simpletest.out`
    * Compare your output in `simpletest.out` with `simpletest.ref`

2. Compile MLton version of MuDDy
   (in `muddy-sml` dir)
    * Edit `Makefile`, most important is the `BUDDYDIR` variable
    * ... TODO ...
