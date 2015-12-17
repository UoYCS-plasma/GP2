COLOBJECTS = graph.o hostLexer.o hostParser.o 2col.o
SORTOBJECTS = graph.o hostLexer.o hostParser.o topsort.o

CC = gcc
CFLAGS = -I../inc -O2 -Wall -Wextra 

2col:	        $(COLOBJECTS)
		$(CC) $(COLOBJECTS) $(CFLAGS) -o 2col

topsort:        $(SORTOBJECTS)
		$(CC) $(SORTOBJECTS) $(CFLAGS) -o topsort

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
		rm *.o 2col topsort hostParser.c hostParser.h hostLexer.c
