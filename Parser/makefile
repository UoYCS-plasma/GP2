# F must be declared with no extension on the command line e.g. make F=test 

CC = gcc
CFLAGS = -Wall -Wextra `pkg-config --cflags --libs glib-2.0`

# builds gpparse, runs it on an extensionless file and generates an AST image.
default: gpparser.tab.o lex.yy.o ast.o pretty.o seman.o main.o
	 $(CC) -o gpparse $(CFLAGS) gpparser.tab.o lex.yy.o ast.o pretty.o seman.o main.o
	 gpparse $(F) 
	 dot -Tjpeg $(F).dot -o $(F).jpeg 

# only builds executable gpparse.
gpparse: gpparser.tab.o lex.yy.o ast.o pretty.o seman.o main.o
	 $(CC) -o gpparse $(CFLAGS) gpparser.tab.o lex.yy.o ast.o pretty.o seman.o main.o

# builds executable gpparse for debugging with gdb.
gpdebug: gpparser.tab.c lex.yy.c ast.c pretty.c seman.c main.c
	 $(CC) $(CFLAGS) -o gpparse $(CFLAGS) -g gpparser.tab.c lex.yy.c ast.c pretty.c seman.c main.c

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

