================================
Author: Chris Bak (November 3 2014)
GP2: Graph Programming
================================

GP2 is a graph programming language. The programmer creates graph programs in a graphical editor which are executed by an underlying machine. The editor generates two text files passed to the compiler: one representing the program and one representing the input graph.

The code contains a fully functional parser and semantic analyser for GP2 programs. There is also a code generator, currently capable of generating a C module for each rule in a GP2 program and a module to build the host graph.

Build Instructions
---------------------
To build an executable for a GP2 program, run:

> make all F1=path/to/gp2_program_file F2=path/to/host_graph_file

To execute only the parser and code generator, run:

> make compile 
> ./GP2-compile path/to/gp_program_file path/to/gp_graph_file

To execute only the runtime system, run:

> make runtime
> ./GP2-run
