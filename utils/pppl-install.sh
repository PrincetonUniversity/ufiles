#!/bin/bash

rm -rf builds

export MAKE_SO=1

export COMPILER=GCC ; export COMPILER_VER=11.2.0 ; source environ
make --no-print-directory
mv builds/gcc/11.2.0 builds/gcc/11.2

export COMPILER=GCC ; export COMPILER_VER=9.3.0 ; source environ
make --no-print-directory
mv builds/gcc/9.3.0 builds/gcc/9.3

export COMPILER=ICC ; export COMPILER_VER=2019 ; source environ
make --no-print-directory

export COMPILER=ICC ; export COMPILER_VER=2022 ; source environ
make --no-print-directory

export COMPILER=PGC ; export COMPILER_VER=19.10 ; source environ
make --no-print-directory
