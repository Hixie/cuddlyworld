# fpc test.pas      -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -g -gt -gl -gh -Sa -OoDFA -veiwnhb -FE../bin/ -Fulib -Filib && ../bin/test && rm -f test test.o; exit;

# Generate helper files
perl -w regen.pl &&

# DEBUGGING
# add -B to recompile everything
fpc cuddlyworld.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -g -gt -gl -gh -Sa -OoDFA -veiwnhb -FE../bin/ -Fulib -Filib 2>&1 | ./lib/filter.pl &&
fpc genesis.pas     -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -g -gt -gl -gh -Sa -OoDFA -veiwnhb -FE../bin/ -Fulib -Filib 2>&1 | ./lib/filter.pl &&
fpc tests.pas       -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -g -gt -gl -gh -Sa -OoDFA -veiwnhb -FE../bin/ -Fulib -Filib 2>&1 | ./lib/filter.pl &&
echo &&
../bin/tests &&
echo &&
echo &&
# rm -f world.dat &&
# ../bin/genesis &&
echo &&
echo &&
../bin/cuddlyworld
