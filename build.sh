
# fpc test.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -Sa -veiwnhb && ./test; exit;

# Generate helper files
perl -w regen.pl &&

# TESTING
# ~/bin/fpc/bin/fpc test.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gh -Sa -veiwnhb && ./test && exit;

# DEBUGGING
# add -B to recompile everything
fpc cuddlyworld.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gh -Sa -veiwnhb -FE../bin/ -Fulib -Filib 2>&1 | ./lib/filter.pl &&
fpc genesis.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gh -Sa -veiwnhb -FE../bin/ -Fulib -Filib 2>&1 | ./lib/filter.pl &&
fpc tests.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gh -Sa -veiwnhb -FE../bin/ -Fulib -Filib 2>&1 | ./lib/filter.pl &&
echo &&
../bin/tests &&
echo &&
echo &&
rm -f world.dat &&
../bin/genesis &&
echo &&
echo &&
../bin/cuddlyworld
