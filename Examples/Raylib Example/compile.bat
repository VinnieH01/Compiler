@echo off
del prgm.exe
del ast
del module.o
del module.s
python Compiler\Parser\parser.py test.lang
Compiler.exe ast
llc module -o module.s
clang module.s -c -o module.o
clang module.o lib.o binding.o lib/raylib.lib lib/opengl32.lib lib/gdi32.lib lib/user32.lib lib/kernel32.lib lib/winmm.lib lib/winspool.lib lib/comdlg32.lib lib/advapi32.lib lib/shell32.lib lib/ole32.lib lib/uuid.lib lib/odbc32.lib lib/odbccp32.lib lib/oleaut32.lib lib/msvcrt.lib -o prgm.exe
del module
prgm.exe