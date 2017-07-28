## Compiler for GoLite

GoLite is a subset of Go carved out by Vincent Foley for McGill University's COMP520 compiler class,
taught by Prof. Laurie Hendren in 2015.
This OCaml implementation was the joint work of Rohan Jacob-Rao, Shawn Otis and Steven Thephsourinthone.
The compiler consists of a lexer and parser, type checker, weeders and C code generator.
Since the original project I have cleaned up the code so it can be extended.

This project uses a mix of OCaml and ReasonML (https://reasonml.github.io/).
Reason can be installed using OPAM (`opam install reason.1.13.6`).
The project relies on Jane Street's Core library (also available on OPAM) and a modification of the
bundled `corebuild` script that uses Reason's `rebuild` instead of `ocamlbuild`
(https://github.com/Khady/reason-corebuild).

With the dependencies installed you can test the compiler using the following steps.

Step 1: Compile the project using `make` from the `src/` directory.  
This will create an executable called 'main.native'.

Step 2: Translate a Go program in the supported syntax set.  
Examples will be added to the `test-programs/` directory.
For example, run `src/main.native test-programs/triangles/triangles.go`.

Step 3: Compile and run the generated C code.
Generated files are stored at the location of the input program.  
Change to that directory: `cd test-programs/triangles/`.
Compile the C output file: `gcc -o triangles triangles.c`.  
Run it: `./triangles`.

Experiment with parameters in the main function of the Go source file and repeat Steps 2 and 3.  
(Note that imported packages are currently not supported by our compiler. Hence the example programs
do not `import "fmt"` and instead translate `print` and `println` functions as if they were Go primitives.)
