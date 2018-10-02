**to use the compiler the backend must be turned into a library by invoking "python buildLib.py", this will generate the 'library' file required by the program**

The program is written in Standard ML, and is intended to be run using the Standard ML of New Jersey, which can be found at https://www.smlnj.org/ . A precompiled version is available in both the Debian and Ubuntu PPAs. To build the program, while in the project directory, run 'sml compiler.cm'. This should bring you into an interactive environment containing the Compiler structure, which is used to process files containing a stack language loosely resembling Forth. (The program should nominally work under Poly/ML, however it currently makes use of CM, which is specific to SMLNJ)

The compiler supports the basic stack language we used in class with the exception of while loops. additionally function definitions are now structured as ":name { def }" where the spaces around the braces are necessary.

compiler.cm provides the actual compilation tools. When built it provides the function "Compiler.write name input lib" where name input and lib are strings which respectivley represent the name of the file you want to output, the file you want to compile, and the name of the library output by buildLib (which should be lib.asm). For example, 'Compiler.write "output" "test.txt" "lib.asm"' should generate the file output.asm, a valid MIPS assembly file.

the output can be run in the MARS emulator
