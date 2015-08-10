COLOBJECTS = graph.o hostLexer.o hostParser.o 2col.o
SORTOBJECTS = graph.o hostLexer.o hostParser.o topsort.o

CC = gcc
# CFLAGS for debugging with gdb.
#CFLAGS = -g -I../inc -Wall -Wextra 

# CFLAGS for profiling with gprof.
# CFLAGS = -pg -I../inc -O2 -Wall -Wextra 

# CLFAGS for optimised code.
CFLAGS = -I../inc -O2 -Wall -Wextra 

2col:	        $(COLOBJECTS)
		$(CC) $(COLOBJECTS) $(CFLAGS) -o 2col
	
2col-debug:	$(COLOBJECTS)
		make 2col
		valgrind --leak-check=full ./2col $(host)

topsort:        $(SORTOBJECTS)
		$(CC) $(SORTOBJECTS) $(CFLAGS) -o topsort
	
topsort-debug:	$(SORTOBJECTS)
		make topsort
		valgrind --leak-check=full ./topsort $(host)

hostParser.c hostParser.h: graph.h hostParser.y
		bison hostParser.y

hostLexer.c:	hostParser.h hostLexer.lex 
		flex hostLexer.lex

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Dependencies
graph.o: 	graph.h
hostLexer.o:	graph.h hostParser.h
hostParser.o:	graph.h hostParser.h
main.o:		graph.h hostParser.h

# Cleanup
clean:
		rm *.o
