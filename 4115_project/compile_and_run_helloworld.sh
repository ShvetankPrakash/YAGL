#!/bin/bash
make clean && make all
echo "---------------SRC--------------"
cat "${1}.ygl"
echo "---------------AST--------------"
./yagl.native -a "${1}.ygl"
echo "---------------SAST--------------"
./yagl.native -s "${1}.ygl"
echo "---------------LLVM IR--------------"
./yagl.native -l "${1}.ygl"
./yagl.native -l "${1}.ygl" > "${1}.ll"
llc -relocation-model=pic "${1}.ll" > "${1}.s"
echo "---------------ASSEMBLY--------------"
cat "${1}.s"
cc -c printbig.c
cc -o "${1}.exe" "${1}.s" printbig.o
echo "---------------EXE OUTPUT--------------"
"./${1}.exe"
rm printbig.o
