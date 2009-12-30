# RELEASE
# when upgrading to 2.4, add -OoLOOPUNROLL 
~/bin/fpc/bin/fpc cuddlyworld.pas -dRELEASE -Ci -Nu -O3 -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OpPENTIUM4 -Si -Xs -XS -XX -B -v0einbf -FE../bin-release/ &&
~/bin/fpc/bin/fpc genesis.pas     -dRELEASE -Ci -Nu -O3 -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OpPENTIUM4 -Si -Xs -XS -XX -B -v0einbf -FE../bin-release/ &&
~/bin/fpc/bin/fpc tests.pas       -dRELEASE -Ci -Nu -O3 -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OpPENTIUM4 -Si -Xs -XS -XX -B -v0einbf -FE../bin-release/ &&
echo &&
echo &&
../bin-release/tests &&
echo &&
echo &&
rm -f world.dat &&
../bin-release/genesis &&
echo &&
echo &&
../bin-release/cuddlyworld
