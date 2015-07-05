OBJECTS = debug.o graph.o graphStacks.o hostLexer.o hostParser.o label.o morphism.o

CC = gcc
# CFLAGS for debugging with gdb.
# CFLAGS = -g -I../inc -Wall -Wextra 

# CFLAGS for profiling with gprof.
# CFLAGS = -pg -I../inc -O2 -Wall -Wextra 

# CLFAGS for optimised code.
CFLAGS = -I../inc -O2 -Wall -Wextra 

# Builds the library object files. 
default:	$(OBJECTS)

hostParser.c hostParser.h: graph.h hostParser.y
		bison hostParser.y

hostLexer.c:	hostParser.h hostLexer.lex 
		flex hostLexer.lex

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Dependencies
debug.o:	../inc/globals.h graph.h debug.h
graph.o: 	../inc/globals.h label.h graph.h
graphStacks.o:	../inc/globals.h graph.h label.h graphStacks.h
hostLexer.o:	../inc/globals.h hostParser.h
hostParser.o:	../inc/globals.h graph.h label.h hostParser.h
label.o:	../inc/globals.h label.h
morphism.o:	../inc/globals.h label.h morphism.h

# Cleanup
clean:
		rm *.o

clean-all:
		rm hostLexer.c hostParser.h hostParser.c
		rm *.o
