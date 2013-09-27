CC = gcc

gpparse: gpparser.tab.o lex.yy.o ast.o pretty.o 
	 $(CC) -o gpparse gpparser.tab.o lex.yy.o ast.o pretty.o

gpdebug: gpparser.tab.c lex.yy.c ast.c pretty.c
	 $(CC) -g -o gpparse gpparser.tab.c lex.yy.c ast.c pretty.c

gpparser.tab.o: gpparser.tab.c gpparser.tab.h
		$(CC) -c gpparser.tab.c

gpparser.tab.c: gpparser.y ast.h
		bison -dtv gpparser.y

lex.yy.o: 	lex.yy.c
		$(CC) -c lex.yy.c

lex.yy.c:	gplexer.l 
		flex gplexer.l

ast.o: 		ast.c ast.h
		$(CC) -c ast.c

pretty.o:       pretty.c pretty.h ast.h
		$(CC) -c pretty.c

clean:
		- rm *.o gpparser.tab.c gpparser.tab.h lex.yy.c gpparse

