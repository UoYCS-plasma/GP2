# Getting Started

The GP 2 compiler translates a GP 2 program into executable C code. The generated code is executable with the support of the GP 2 library. There are two ways to set it up. You can build the compiler using make. However, the setup process is not guaranteed to be stable on all Linux machines, or on machines running MacOS or Windows. We therefore provide an installation via Docker as an alternative for Linux and MacOS.

## Installation

To run the compiler, you need to install the C library [Judy](http://judy.sourceforge.net/index.html) which you may find in your distribution's package manager. In the Ubuntu repositories, you can find it under ``libjudy-dev``.

- You can find a pre-built version of the compiler [here](https://github.com/UoYCS-plasma/GP2/blob/master/Pre-built).
- Follow [this guide](https://uoycs-plasma.github.io/GP2/installation/buildcompiler) if you want to build the compiler yourself.
- To use GP 2 via Docker, take a look at [this guide](https://uoycs-plasma.github.io/GP2/installation/docker).
- In the University of York Department of Computer Science, the compiler is installed on most Linux machines.

## Usage

In order to use the GP 2 Compiler with Docker or after building it yourself, take a look at these [bash files](https://github.com/UoYCS-plasma/GP2/blob/master/Compiler/bin).

Alternatively, [This guide](https://uoycs-plasma.github.io/GP2/usage) gives a step-by-step exmplanation of how to use the compiler.

On University of York Linux machines, simply call
```
gp2c <flags> <program> <input graph>
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


## Example: Editing, Compiling, and Running a Transitive Closure Program

For this section, we assume to have a bash file `gp2bash` which we can run GP 2 programs with using `./gp2bash <program_file> <graph_file>`

The program we'll look at is this:

![test](images/link.png "A program for computing the transitive closure of a graph.")

This program computes the **transitive closure** of a graph. The transitive closure of a graph is the smallest extension of that graph that is **transitive**.
A graph is transitive when for every pair of nodes *v1, v2* with a path from *v1* to *v2*, there is an edge directly from *v1* to *v2*.

For example, this graph is *not* transitive:

![test](images/notrans.png "A non-transitive graph.")

There is a path from the left-most node to the right-most node but there is no edge directly from the left-most node to the right-most node. 
The purpose of the program we're looking at is the *transform* this graph into the smallest extension of this graph which *is* transitive. 

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

What does this mean? Well the square brackets `[ ... ]` surround the entire graph's structure. Then the nodes are listed, for example; `(0, empty)` indicates that there is
a node which we will identify as node 0 and this node is unlabelled (its label is 'empty'). After the nodes are listed, there is a divider; `|`, and then edges are
listed. The edge `(4, 0, 1, empty)` describes an edge from node 0 to node 1 which is also unlabelled. 

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
where not edge(1, 3)
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
[LEFT HAND SIDE GRAPH]
=>
[RIGHT HAND SIDE GRAPH]
interface = { [INTERFACE] }
[CONDITION]
```

Where graphs are of the same form as the graph we saw earlier. 

So what does our program mean? The only line of the main program is `Main = link!`. This `!`  means that the rule `link` will be applied as long as possible - e.g. it will be applied
until it is no longer applicable. Applying the rule `link` firstly searches for a match for its left hand side; 3 adjacent nodes where there is not an edge
from the 1st node to the 3rd node. Then, once a match is found, the left hand side is transformed into the right hand side by inserting an edge from the 1st
node to the 3rd node. The 'interface' describes which nodes survive; none of the 3 matched nodes `n1, n2, n3` are deleted. 

Now we can compile our program by calling:

```
./gp2bash PATH/TO/transitive_closure.gp2 PATH/TO/cycle.host
```
This will output the following graph:

![test](images/trans.png "A transitive graph.")

## Copying

See the file [COPYING](COPYING).

## Authors

The GP 2 language was designed by [Detlef Plump](http://www-users.cs.york.ac.uk/~det/).

The GP 2 compiler and runtime library was developed by Christopher Bak. Recent improvements to the compiler were implemented by [Graham Campbell](https://gjcampbell.co.uk/) and [Jack Romo](http://jackromo.com/).
