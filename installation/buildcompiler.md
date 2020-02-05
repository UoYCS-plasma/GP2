To build the compiler, first download the files from the [GP 2 Github Page](https://github.com/UoYCS-plasma/GP2/). Then in the folder `GP2/Compiler` run the following commands.

1. `autoreconf -i; autoconf -i; automake -a`
1. `./configure`
1. `make dist`
1. `tar -xzvf gp2-1.0.tar.gz; cd gp2-1.0`
1. `./configure --prefix=<path_to_build_directory>` where `<path_to_build_directory>` is where you want the compiler to reside.
1. `cp ../lib/*.{c,h} lib/`
1. `make`
1. `make install`
