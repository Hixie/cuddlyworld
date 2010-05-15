# TESTING
# ~/bin/fpc/bin/fpc test.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gc -gh -Sa -veiwnhb && ./test

# DEBUGGING
~/bin/fpc/bin/fpc cuddlyworld.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gc -gh -Sa -veiwnhb -FE../bin/ 2>&1 | ./filter.pl &&
echo &&
echo &&
~/bin/fpc/bin/fpc genesis.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gc -gh -Sa -veiwnhb -FE../bin/ 2>&1 | ./filter.pl &&
echo &&
echo &&
~/bin/fpc/bin/fpc tests.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gc -gh -Sa -veiwnhb -FE../bin/ 2>&1 | ./filter.pl &&
echo &&
echo &&
../bin/tests &&
echo &&
echo &&
rm -f world.dat &&
../bin/genesis &&
echo &&
echo &&
../bin/cuddlyworld
