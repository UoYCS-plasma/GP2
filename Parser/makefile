P = gpparse
OBJECTS = gpparser.tab.o lex.yy.o ast.o pretty.o seman.o main.o
CC = gcc
CFLAGS = -g -Wall -Wextra `pkg-config --cflags --libs glib-2.0`

# Builds executable gpparse, runs it on an extensionless file and generates AST images.
# Usage: make F=<filename>
default:        $(OBJECTS)
		$(CC) $(CFLAGS) $(OBJECTS) -o $(P) 	
		gpparse $(F)	       	
		dot -Tjpeg $(F).dot -o $(F).jpeg 	
		dot -Tjpeg $(F)_F.dot -o $(F)_F.jpeg

# Builds executable gpparse.
# Usage: make gpparse
$(P):		$(OBJECTS)
		$(CC) $(CFLAGS) $(OBJECTS) -o $(P)

gpparser.tab.o: gpparser.tab.c gpparser.tab.h
		$(CC) $(CFLAGS) -c gpparser.tab.c

gpparser.tab.c: gpparser.y ast.h
		bison -dtv gpparser.y

lex.yy.o: 	lex.yy.c
		$(CC) $(CFLAGS) -c lex.yy.c

lex.yy.c:	gplexer.l 
		flex gplexer.l

main.o:         main.c pretty.h
		$(CC) $(CFLAGS) -c main.c

ast.o: 		ast.c ast.h
		$(CC) $(CFLAGS) -c ast.c

pretty.o:       pretty.c pretty.h ast.h
		$(CC) $(CFLAGS) -c pretty.c

seman.o:	seman.c seman.h ast.h
		$(CC) $(CFLAGS) -c seman.c

clean:
		- rm *.o gpparser.tab.c gpparser.tab.h lex.yy.c gpparse

