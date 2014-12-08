to use the compiler the backend must be turned into a library by invoking "python buildLib.py"

The compiler supports the basic stack language we used in class with the exception of while loops. additionally function definitions are now structured as ":name { def }" where teh spaces around the braces are necessary.

compiler.cm provides the actual compilation tools. When built it provides the function "Compiler.write name input lib" where name input and lib are strings which respectivley represent the name of the file you want to ouotput, the file you want to compile, and the name of the library output by buildLib (which should be lib.asm).

the output can be run in the MARS emulator
