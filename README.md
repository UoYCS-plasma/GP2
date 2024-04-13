# Versions

Source's latest update: *13th April 2024*

Docker's latest update: *24th February 2021*

# Getting Started

The GP 2 compiler translates a GP 2 program into executable C code. The generated code is executable with the support of the GP 2 library. There are two ways to set it up. You can build the compiler using *make*. However, the setup process is not guaranteed to be stable on all Linux machines or on machines running MacOS or Windows. We, therefore, provide an installation via Docker as an alternative for Linux and MacOS.

## Installation

To run the compiler, you need to install the C library [Judy](http://judy.sourceforge.net/index.html), which you may find in your distribution's package manager. In the Ubuntu repositories, you can find it under ``libjudy-dev``.

There are several ways to install the compiler:

- [**Pre-built**](https://uoycs-plasma.github.io/GP2/usage/pre-built) - Use a version of the compiler that has already been built. This might not work correctly on every Linux distribution, but it does work on Ubuntu 18.04, for instance.
- [**Build from source code**](https://github.com/UoYCS-plasma/GP2/blob/master/GP_2_Compilation_Tutorial.pdf) - (Recommended to get the latest version.) Follow the link for a guide on how to build the compiler yourself.
- [**Docker**](https://uoycs-plasma.github.io/GP2/installation/docker) - Follow the link for instructions on how to install the compiler via Docker. This method is recommended for MacOS users.
- **University of York** - In the Department of Computer Science, the compiler is installed on most Linux machines.

## Usage

Usage depends on which installation method you used:

- **Pre-built** - [bash file usage](https://uoycs-plasma.github.io/GP2/usage/pre-built)
- **Build from source code** - [usage guide](https://github.com/UoYCS-plasma/GP2/blob/master/GP_2_Compilation_Tutorial.pdf)
- **Docker** - [bash file usage](https://github.com/UoYCS-plasma/GP2/blob/master/bin/gp2docker)
- **University of York** - [usage guide](https://uoycs-plasma.github.io/GP2/usage/uoy)


## Example: Editing, Compiling, and Running a Transitive Closure Program

For this section, we assume that we can run GP 2 programs with using the command `gp2c <program_file> <graph_file>`.

The program we'll look at is this:

![test](images/link.png "A program for computing the transitive closure of a graph.")

This program computes the **transitive closure** of a graph. The transitive closure of a graph is the smallest extension of that graph that is **transitive**.
A graph is transitive when for every pair of nodes *v1, v2* with a path from *v1* to *v2*, there is an edge directly from *v1* to *v2*.

For example, this graph is *not* transitive:

![test](images/notrans.png "A non-transitive graph.")

There is a path from the leftmost node to the rightmost node, but there is no edge directly from the leftmost node to the rightmost node. 
The purpose of the program we're reviewing is to *transform* this graph into the smallest extension of this graph, which *is* transitive. 

Firstly, let's get this graph in a usable form. Try writing the graph out as:

```
[
	(0, empty)
	(1, empty)
	(2, empty)
	(3, empty)
	|
	(4, 0, 1, empty)
	(5, 1, 2, empty)
	(6, 2, 3, empty)
	(7, 3, 0, empty)
]
```

What does this mean? Well, the square brackets `[ ... ]` surround the entire graph's structure. Then the nodes are listed. For example, `(0, empty)` indicates that there is
a node which we will identify as node 0, and this node is unlabelled (its label is 'empty'). After the nodes are listed, there is a divider, `|`, and then the edges are
listed. The edge `(4, 0, 1, empty)` describes an edge from node 0 to node 1, which is also unlabelled. 

Save this graph as *"cycle.host"*. 


Try writing this program out in text form:

```
Main = link!

link(a, b, c, d, e : list)
[
	(n1, a)
	(n2, c)
	(n3, e)
	|
	(e1, n1, n2, b)
	(e2, n2, n3, d)
]
=>
[
	(n1, a)
	(n2, c)
	(n3, e)
	|
	(e1, n1, n2, b)
	(e2, n2, n3, d)
	(e3, n1, n3, empty)
]
interface = 
{
	n1, n2, n3
}
where not edge(n1, n3)
```

and saving it as *"transitive_closure.gp2"*. The general form of a program in text form is:

```
Main = [PROGRAM CODE]

[RULE 1]
[RULE 2]
[RULE 3]
```

And an individual rule is of the form:

```
[RULENAME]
(
	[VARIABLES]
)
[LEFT-HAND SIDE GRAPH]
=>
[RIGHT-HAND SIDE GRAPH]
interface = { [INTERFACE] }
[CONDITION]
```

Where graphs are of the same form as the graph we saw earlier. 

So what does our program mean? The only line of the main program is `Main = link!`. This `!`  means that the rule `link` will be applied as long as possible - e.g. it will be applied
until it is no longer applicable. Applying the rule `link` firstly searches for a match for its left-hand side; 3 adjacent nodes where there is not an edge
from the 1st node to the 3rd node. Then, once a match is found, the left-hand side is transformed into the right-hand side by inserting an edge from the 1st
node to the 3rd node. The 'interface' describes which nodes survive; none of the 3 matched nodes `n1, n2, n3` are deleted. 

Now we can compile our program by calling:

```
gp2c PATH/TO/transitive_closure.gp2 PATH/TO/cycle.host
```
This will output the following graph:

![test](images/trans.png "A transitive graph.")

## Using `gp2c`

Use ``gp2c`` with after you built the GP 2 compiler. Edit the variables at the top of the file. ``install_dir`` should be the path of the directory containing the built compiler, and ``source_dir`` the path of the compiler's source code. To use the compiler flags, add them to the command in step 2 of the bash file. Run the compiler as follows:
```
./gp2c <program> <input graph>
```
where ``<program>`` is the path to your GP 2 program, and ``<input graph>`` the path to your input graph.

These are the flags:

- **-d** - Compile the program with debugging flags.
- **-f** - Compile in fast shutdown mode.
- **-g** - Compile with minimal garbage collection (requires fast shutdown).
- **-m** - Compile with root reflecting matches.
- **-n** - Compile without graph node lists.
- **-q** - Compile program quickly without optimisations.
- **-l** - Specify the directory of lib source files.
- **-o** - Specify a directory for generated code and program output.

## Using `gp2docker`

Call ``gp2docker`` to use GP 2 via Docker. Use the flags by adding ``-e GP2_FLAGS='<flags>'``in-between ``data`` and ``registry``, where ``<flags>`` is the string of flags you wish to use. Run GP 2 using the following command:
```
./gp2docker <program> <input graph>
```
where ``<program>`` is the path to your GP 2 program, and ``<input graph>`` the path to your input graph.

These are the flags:

- **-d** - Compile the program with debugging flags.
- **-f** - Compile in fast shutdown mode.
- **-g** - Compile with minimal garbage collection (requires fast shutdown).
- **-m** - Compile with root reflecting matches.
- **-n** - Compile without graph node lists.
- **-q** - Compile program quickly without optimisations.
- **-l** - Specify the directory of lib source files.
- **-o** - Specify a directory for generated code and program output.

## GP 2 Home Page

The GP 2 home page can be found [here](https://uoycs-plasma.github.io/GP2/).

## Licence and Copying

GP 2 is licensed under the GNU General Public License v3.0. See the file [COPYING](https://github.com/UoYCS-plasma/GP2/blob/master/COPYING).

## Authors

The GP 2 language was designed by [Detlef Plump](http://www-users.cs.york.ac.uk/~det/).

Christopher Bak developed the GP 2 compiler and runtime library. In 2020, [Graham Campbell](https://gjcampbell.co.uk/) and [Jack Romo](http://jackromo.com/) implemented additional improvements to the compiler, described [here](https://arxiv.org/pdf/2002.02914.pdf), as well as [Ziad Ismaili Alaoui](https://www.cs.york.ac.uk/people/?group=Research%20Students&username=zismali) in 2024.
