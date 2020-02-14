Make sure to make the bash file executable by calling ``chmod u+x gp2docker``.

Call ``gp2docker`` to use GP 2 via Docker. Use the flags by adding ``-e GP2_FLAGS='<flags>'`` inbetween ``data`` and ``registry``, where ``<flags>`` is the string of flags you wish to use. Run GP 2 using the following command:
```
./gp2docker <program> <input graph>
```
where ``<program>`` is the path to your GP 2 program, and ``<input graph>`` the path to your input graph.

These are the flags:

- **-d** - Compile program with debugging flags.
- **-f** - Compile in fast shutdown mode.
- **-g** - Compile with minimal garbage collection (requires fast shutdown).
- **-m** - Compile with root reflecting matches.
- **-n** - Compile without graph node lists.
- **-q** - Compile program quickly without optimisations.
- **-l** - Specify directory of lib source files.
- **-o** - Specify directory for generated code and program output.
