==================================
GP 2 Compiler
Author: Chris Bak (June 29th 2015)
==================================

Build Instructions
---------------------

To build the GP 2 compiler, run

> cd src

> make build

This builds the library object files and creates the binary `GP2-compile` which is called from the *src* directory in one of three ways:

> ./GP2-compile \[-o output-file-name\] /path/to/program-file /path/to/host-graph-file

> ./GP2-compile -vp /path/to/program-file

> ./GP2-compile -vh /path/to/host-graph-file

The last two options allow the user to run only the parser and semantic checker on the program file or on the host graph file without calling the code generator. 

By default, the `GP2-run` binary will write its output to the file **gp2.output** in the top-level directory. The output file can be changed with the `-o` flag.

While in the *src* directory, running

> make prog=/path/to/program-file host=/path/to/host-graph-file

builds the compiler, calls it (without the -o flag) on the passed program and host graph files, and builds the runtime system in the *runtime* directory.

To execute the compiled GP 2 program, run the binary `GP2-run` in the *runtime* directory.
