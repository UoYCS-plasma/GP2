# Usage: Pre-built

In the directory containing your bash file, simply call
```
./gp2c <flags> <program> <input graph>
```
where ``<flags>`` consists of the flags you wish to use seperated by spaces, ``<program>`` is the relative path to your GP 2 program, and ``<input graph>`` the relative path to your input graph.

These are the flags:

- **-d** - Compile program with debugging flags.
- **-f** - Compile in fast shutdown mode.
- **-g** - Compile with minimal garbage collection (requires fast shutdown).
- **-m** - Compile with root reflecting matches.
- **-n** - Compile without graph node lists.
- **-q** - Compile program quickly without optimisations.
- **-l** - Specify directory of lib source files.
- **-o** - Specify directory for generated code and program output.

Additionally, you can use the **-c** flag to keep the generated C code and the log.
