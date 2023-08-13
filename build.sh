#MODE=RELEASE MAIN=temp lib/compile.sh; exit

#rm -rf ../bin/*;fpc test.pas -B -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -g -gt -gl -gh -Sa -OoDFA -veiwnhb -FE../bin/ -Fulib -Filib && ../bin/test; exit

# Generate helper files
perl -w regen.pl || exit 1

MODE="DEBUG"

# DEFINES="-dPLAY_IN_TEST_EDEN"
# DEFINES="-dPLAY_IN_TEST_DOORLAND"
# DEFINES="-dPLAY_IN_TEST_STAIRLAND -B"
# play as Flathead, password zorkmid
# (the world doesn't support adding players, you need to hard-code them in)
# (don't forget to comment out genesis below)

pushd . > /dev/null
# DEFINES="-dVERBOSE"
MAIN="tests" MODE="$MODE" PATHS="-Futests" DEFINES="$DEFINES" lib/compile.sh || exit 1
popd > /dev/null
echo

rm -f world.dat
pushd . > /dev/null
MAIN="genesis" MODE=$MODE DEFINES="$DEFINES" lib/compile.sh || exit 1
popd > /dev/null
echo

pushd . > /dev/null
TESTCMD="echo ready!" CUDDLYWORLDPORT="10001" MAIN="cuddlyworld" MODE=$MODE DEFINES="$DEFINES" lib/compile.sh || exit 1
popd > /dev/null
echo
