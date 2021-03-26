#!/bin/bash
make clean && make all
echo "---------------SRC--------------"
cat helloworld.ygl
echo "---------------AST--------------"
./yagl.native -a helloworld.ygl
echo "---------------SAST--------------"
./yagl.native -s helloworld.ygl
echo "---------------LLVM IR--------------"
./yagl.native -l helloworld.ygl
./yagl.native -l helloworld.ygl > helloworld.ll
llc -relocation-model=pic helloworld.ll > helloworld.s
echo "---------------ASSEMBLY--------------"
cat helloworld.s
cc -c printbig.c
cc -o helloworld.exe helloworld.s printbig.o
echo "---------------EXE OUTPUT--------------"
./helloworld.exe
rm printbig.o
