#rm -rf ../bin/*; fpc test.pas -B -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -g -gt -gl -gh -Sa -OoDFA -veiwnhb -FE../bin/ -Fulib -Filib && ../bin/test; exit

# Generate helper files
perl -w regen.pl &&

MODE="DEBUG"

MAIN="tests"
pushd . > /dev/null
. ${SRC}lib/compile.sh
popd > /dev/null
unset TESTCMD
echo

rm -f world.dat
MAIN="genesis"
pushd . > /dev/null
. ${SRC}lib/compile.sh
popd > /dev/null
unset TESTCMD
echo

MAIN="cuddlyworld"
pushd . > /dev/null
. ${SRC}lib/compile.sh
popd > /dev/null
unset TESTCMD
echo
