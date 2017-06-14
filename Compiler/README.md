## About GP 2

GP 2 (Graph Programs 2) is a rule-based, nondeterministic programming language for solving graph problems at a high level of abstraction, freeing programmers from handling low-level data structures. The core of GP 2 consists of four constructs: single-step application of a set of conditional graph-transformation rules, sequential composition, branching and iteration. The language has a small structural operational semantics and a visual editor for writing GP 2 programs, running them, and tracing their execution.

## The GP 2 Compiler

The GP 2 compiler translates a GP 2 program into executable C code.
The generated code is executable with the support of the GP 2 library.

Default usage:
`gp2 [-c] [-d] [-l <rootdir>] [-o <outdir>] <gp2-program_file>`

Compiles *gp2-program* into C code. The generated code is placed in
*/tmp/gp2* unless an alternate location is specified with the **-o** flag. 

To execute the generated code, run `make` and `./gp2run <host-graph-file>`
from */tmp/gp2*.

If GP 2 is installed in a non-standard directory, use the **-l** option to 
ensure the generated code can be compiled and executed. See Installation 
for more information.

Options:

**-c** - Enable graph copying.

**-d** - Compile program with GCC debugging flags.

**-l** - Specify root directory of installed files.

**-o** - Specify directory for generated code and program output.

The compiler can also be used to validate GP 2 source files.

Run `gp2 -p <program_file>` to validate a program.

Run `gp2 -r <rule_file>` to validate a rule.

Run `gp2 -h <host_file>` to validate a host graph.

## Installation

Superusers install GP 2 as follows: 

1. Run `./configure` from the top-level directory to generate `config.h` and `Makefile`.

2. Run `make`.

3. Run `sudo make install`. 

   This command will install files into the following directories:
   * `/usr/local/bin`
   * `/usr/local/lib`
   * `/usr/local/include`

If you are not a superuser, install GP 2 locally as follows:

1. Run `./configure --prefix={dest-dir}' from the top-level directory.

2. Run `make`.

3. Run `make install`.

   This command will install files into the following directories:
   * `{dest-dir}/bin`
   * `{dest-dir}/lib`
   * `{dest-dir}/include`

Call the compiler with `-l {dest-dir}` to ensure that the generated code compiles.

## Copying

See the file [COPYING](COPYING).

## Authors

The GP 2 language was designed by Detlef Plump.

The GP 2 compiler and runtime library was developed by Christopher Bak.
