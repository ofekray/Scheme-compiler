# Scheme-compiler
Scheme to CISC bootstrapped compiler 


This is a Scheme->CISC compiler written in Scheme.

The compiler is the final project in the [Compiler Principles course](https://www.cs.bgu.ac.il/~comp171/wiki.files/proj.pdf) taken in Ben-Gurion University.

The final project objective is to create a fully functional compiler, that compiles Scheme code into CISC code.

CISC is a CISC-like assembly language for a general register architecture created in C, and is basically composed using C-macros.

## Usage
1. Open any scheme compiler (like 'Chez Scheme') then use: `(load "compiler.scm")`
2. Now you can compile your scheme file using: `(compile-scheme-file "input_file.scm" "output_file.scm")`
3. This will create `output_file.c` which is a CISC file!
4. If you want, you can compile the CISC file using gcc: `(gcc -o output output_file.c)`
5. And then simply run the file using: `(./output)`
