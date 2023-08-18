@echo off
del ast
del module.o
del module.s
python Compiler\Parser\parser.py lib.lang
Compiler.exe ast
llc module -o module.s
clang module.s -c -o module.o
del lib.o
ren module.o lib.o
del module
clang -c binding.c -o binding.o
PAUSE