# fpc test.pas -dDEBUG -Ci -Co -CO -Cr -CR -Ct -O- -g -gt -gl -gh -Sa -OoDFA -veiwnhb -FE../bin/ -Fulib -Filib && ../bin/test; exit

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


# RELEASE TEST
# rm -rf ../bin/*
# fpc test.pas -dRELEASE -O3 -OoLOOPUNROLL -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OWSYMBOLLIVENESS -OoDFA -Si -Xs- -XS -XX -B -v0einf -FW../bin/opt-feedback -FE../bin/ -Fulib -Filib 2>&1 &&
# fpc test.pas -dRELEASE -O3 -OoLOOPUNROLL -OoREGVAR -OoSTACKFRAME -OoPEEPHOLE -OoASMCSE -OoTAILREC -OwSYMBOLLIVENESS -OoDFA -Si -Xs  -XS -XX -B -v0einf -Fw../bin/opt-feedback -FE../bin/ -Fulib -Filib 2>&1 &&
# rm -f ../bin/*.o ../bin/*.ppu &&
# time ../bin/test
