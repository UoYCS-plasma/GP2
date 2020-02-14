# Usage

To set up the compiler, follow these steps:

1. Move the files in this directory to a directory of your choice.
1. In the ``gp2c`` file, edit the variable ``install_dir`` at the top to be the path to said directory.
1. Make ``gp2c`` executable by running the command ``chmod u+x gp2c`` as a root user.

If you're on Ubuntu, you can move ``gp2c`` to your home folder to make it available as a terminal command.

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
