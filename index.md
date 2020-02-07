## What is GP 2?

GP 2 is a visual, rule-based, non-deterministic graph programming language which frees programmers from handling low-level data structures for graphs. The main motivation behind GP 2 is facilitating formal reasoning about programs while supporting practical problem solving.

A GP 2 program algorithmically transforms an input graph into an output graph by sequentially applying graph transformation rules. GP 2 is computationally complete in that every computable partial function on labelled graphs can be programmed. There are a number of results on formally verifying graph programs using Hoare logics, and on checking programs for confluence (see *Verification of Graph Programs* and *Checking Graph Programs for Confluence* on the [publications page](https://uoycs-plasma.github.io/GP2/publications)). 

The core of GP 2 consists of four constructs: single-step application of a set of conditional graph transformation rules, sequential composition, branching and iteration. The language has a small structural operational semantics.

Our existing compiler generates raw C code from a GP 2 program's source code. This source code, which is concise and optimised, is capable of competing with textbook C programs for some graph problems (see [here](https://link.springer.com/chapter/10.1007%2F978-3-319-40530-8_7)).

GP 2 programs combine the speed of low-level C programming with the expressiveness of graph transformation. If you want to transform graphs, GP 2 should be your go-to tool.

In addition, we have a probabilistic version of GP 2, called P-GP 2, which has been applied to problems in evolutionary computation (see *Probabilistic GP 2* on the [publications page](https://uoycs-plasma.github.io/GP2/publications)).

## How do I get started?

Head over to [this page](https://uoycs-plasma.github.io/GP2/gettingstarted) for instructions on installing GP 2 and running your first program. 

## How can I find out more?

You can [contact us](https://uoycs-plasma.github.io/GP2/contact) at any time or take a look at our various [publications](https://uoycs-plasma.github.io/GP2/publications) for more information.

