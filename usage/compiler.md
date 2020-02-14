# Usage: Build From Source Code

You can compile GP 2 programs using the `gp2`. From the `bin` folder, run
```
./gp2 [-d] [-f] [-g] [-m] [-n] [-q] [-l <libdir>] [-o <outdir>] <program_file>
```

Compiles *gp2-program* into C code. The generated code is placed in
*/tmp/gp2* unless an alternate location is specified with the **-o** flag. 

To execute the generated code, run `./build.sh` and
`./gp2run <graph_file>` from */tmp/gp2*.

This will generate a file *"gp2.output"* containing the output graph.

Before executing, you may need to copy library files from the source code.
```
cp <path_to_source_code_directory>/lib/*.{c,h} ./gp2_code_temp/
```

If GP 2 is installed in a non-standard directory, use the **-l** option to 
ensure the generated code can be compiled and executed.

The following is the list of compiler flags:

- **-d** - Compile program with debugging flags.
- **-f** - Compile in fast shutdown mode.
- **-g** - Compile with minimal garbage collection (requires fast shutdown).
- **-m** - Compile with root reflecting matches.
- **-n** - Compile without graph node lists.
- **-q** - Compile program quickly without optimisations.
- **-l** - Specify directory of lib source files.
- **-o** - Specify directory for generated code and program output.

The compiler can also be used to validate GP 2 source files.
- Run `gp2 -p <program_file>` to validate a program.
- Run `gp2 -r <rule_file>` to validate a rule.
- Run `gp2 -h <host_file>` to validate a host graph.
