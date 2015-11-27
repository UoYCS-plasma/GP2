GP 2 Compiler
=============
Author: Chris Bak (November 27th 2015)

Build Instructions
---------------------

To build the GP 2 compiler from source, run

> cd src

> make build

This builds the library object files and creates the binary `GP2-compile` in the *src* directory.

> ./GP2-compile \[-o /output-directory\] /path/to/program-file /path/to/host-graph-file

> ./GP2-compile -p /path/to/program-file

> ./GP2-compile -h /path/to/host-graph-file

-o specifies a directory for the generated files and the output. The default is */tmp/gp2*.
-p runs the parser and the semantic checker on the program file for validation. No code is generated.
-h run the parser on the host graph file for validation. No code is generated.

While in the *src* directory, running

> make prog=/path/to/program-file host=/path/to/host-graph-file

builds the compiler, calls it (without the -o flag) on the passed program and host graph files, and builds the runtime system in the */tmp/gp2* directory. To execute the compiled GP 2 program, execute `GP2-run`.
