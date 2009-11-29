# TESTING
# ~/bin/fpc/bin/fpc test.pas && ./test

# DEBUGGING
~/bin/fpc/bin/fpc cuddlyworld.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gc -gh -Sa -veiwnhb -FE../bin/ &&
echo &&
echo &&
~/bin/fpc/bin/fpc genesis.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -gt -gl -gc -gh -Sa -veiwnhb -FE../bin/ &&
echo &&
echo &&
rm -f world.dat &&
../bin/genesis &&
echo &&
echo &&
../bin/cuddlyworld

# RELEASE
# ~/bin/fpc/bin/fpc cuddlyworld.pas -dRELEASE -Ci -Nu -O3 -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoLOOPUNROLL -OoTAILREC -OpPENTIUM4 -Si -Xs -XS -XX -B -v0einbf -FE../bin-release/ && ~/bin/fpc/bin/fpc genesis.pas -dRELEASE -Ci -Nu -O3 -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoLOOPUNROLL -OoTAILREC -OpPENTIUM4 -Si -Xs -XS -XX -B -v0einbf -FE../bin-release/ && echo && echo && ../bin-release/cuddlyworld
