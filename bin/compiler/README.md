Make sure to make the bash file executable by calling ``chmod u+x gp2c`` or ``chmod u+x gp2docker``.

Use ``gp2c`` with after you built the GP 2 compiler. Edit the variables at the top of the file. ``install_dir`` should be the path of the directory containing the built compiler, and ``source_dir`` the path of the compiler's source code. To use the compiler flags, add them to the command in step 2 of the bash file. Run the compiler as follows:
```
./gp2c <program> <input graph>
```
where ``<program>`` is the path to your GP 2 program, and ``<input graph>`` the path to your input graph.

Call ``gp2docker`` to use GP 2 via Docker. Use the flags by adding ``-e GP2_FLAGS='<flags>'`` inbetween ``data`` and ``registry``, where ``<flags>`` is the string of flags you wish to use. Run GP 2 using the following command:
```
./gp2docker <program> <input graph>
```

These are the flags:

- **-d** - Compile program with debugging flags.
- **-f** - Compile in fast shutdown mode.
- **-g** - Compile with minimal garbage collection (requires fast shutdown).
- **-m** - Compile with root reflecting matches.
- **-n** - Compile without graph node lists.
- **-q** - Compile program quickly without optimisations.
- **-l** - Specify directory of lib source files.
- **-o** - Specify directory for generated code and program output.
