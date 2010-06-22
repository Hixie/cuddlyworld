# RELEASE
# Add -OWAll / -OwAll options once the compiler doesn't generate broken code with those options!
~/bin/fpc/bin/fpc cuddlyworld.pas -dRELEASE -Ci -O3 -OoLOOPUNROLL -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OWSYMBOLLIVENESS -Si -Xs- -XS -XX -B -v0einbf -FW../bin-release/opt-feedback -FE../bin-release/ &&
~/bin/fpc/bin/fpc cuddlyworld.pas -dRELEASE -Ci -O3 -OoLOOPUNROLL -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OwSYMBOLLIVENESS -Si -Xs -XS -XX -B -v0einbf -Fw../bin-release/opt-feedback -FE../bin-release/ &&
echo &&
~/bin/fpc/bin/fpc genesis.pas     -dRELEASE -Ci -O3 -OoLOOPUNROLL -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OWSYMBOLLIVENESS -Si -Xs- -XS -XX -B -v0einbf -FW../bin-release/opt-feedback -FE../bin-release/ &&
~/bin/fpc/bin/fpc genesis.pas     -dRELEASE -Ci -O3 -OoLOOPUNROLL -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OwSYMBOLLIVENESS -Si -Xs -XS -XX -B -v0einbf -Fw../bin-release/opt-feedback -FE../bin-release/ &&
echo &&
~/bin/fpc/bin/fpc tests.pas       -dRELEASE -Ci -O3 -OoLOOPUNROLL -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OwSYMBOLLIVENESS -Si -Xs- -XS -XX -B -v0einbf -Fw../bin-release/opt-feedback -FE../bin-release/ &&
~/bin/fpc/bin/fpc tests.pas       -dRELEASE -Ci -O3 -OoLOOPUNROLL -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OwSYMBOLLIVENESS -Si -Xs -XS -XX -B -v0einbf -Fw../bin-release/opt-feedback -FE../bin-release/ &&
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
