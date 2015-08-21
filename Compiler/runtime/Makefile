# Runtime objects.
ROBJECTS := $(patsubst %.c, %.o, $(wildcard *.c)) $(patsubst %.c, %.o, $(wildcard host/*.c)) 
# Library objects.
LOBJECTS = ../lib/debug.o ../lib/graph.o ../lib/graphStacks.o ../lib/label.o ../lib/morphism.o \
           ../lib/hostParser.o ../lib/hostLexer.o
           
OBJECTS = $(ROBJECTS) $(LOBJECTS)

CC = gcc 
# CFLAGS for debugging with gdb.
# CFLAGS = -g -I../lib -I../inc -Wall -Wextra 

# CFLAGS for profiling with gprof.
# CFLAGS = -pg -I../lib -I../inc -O2 -Wall -Wextra 

# CLFAGS for optimised code.
CFLAGS = -I../lib -I../inc -O2 -fomit-frame-pointer -Wall -Wextra 

default:	$(OBJECTS)
		$(CC) $(OBJECTS) $(CFLAGS) -o GP2-run 		

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Delete all files except Makefile (this file). 
clean:		
		ls | grep -v 'Makefile' | xargs rm
		if [ -e "../gp2.log" ]; then rm ../gp2.log; fi
		if [ -e "../gp2.trace" ]; then rm ../gp2.trace; fi
