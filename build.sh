# TESTING
# ~/bin/fpc/bin/fpc test.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gh -Sa -veiwnhb && ./test && exit;

# DEBUGGING
# add -B to recompile everything
~/bin/fpc/bin/fpc cuddlyworld.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gh -Sa -veiwnhb -FE../bin/ 2>&1 | ./filter.pl &&
~/bin/fpc/bin/fpc genesis.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gh -Sa -veiwnhb -FE../bin/ 2>&1 | ./filter.pl &&
~/bin/fpc/bin/fpc tests.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gh -Sa -veiwnhb -FE../bin/ 2>&1 | ./filter.pl &&
echo &&
../bin/tests &&
echo &&
echo &&
#rm -f world.dat &&
#../bin/genesis &&
echo &&
echo &&
../bin/cuddlyworld
