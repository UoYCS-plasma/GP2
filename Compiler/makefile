P = gpparse
OBJECTS = gpparser.tab.o lex.yy.o ast.o pretty.o seman.o main.o
CC = gcc
#CFLAGS = -g -Wall -Wextra `pkg-config --cflags --libs glib-2.0`
CFLAGS = -g -Wall -Wextra -I/local/d0p6/chrisbak/root/include/glib-2.0 -I/local/d0p6/chrisbak/root/lib/glib-2.0/include
LFLAGS = -lglib-2.0


# Builds executable gpparse, runs it on an extensionless file and generates AST images.
# Usage: make F=<filename>
default:        $(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o $(P) 	
		./gpparse $(F1)	$(F2)       	
		dot -Tjpeg $(F1).dot -o $(F1).jpeg 	
		dot -Tjpeg $(F1)_F.dot -o $(F1)_F.jpeg
		dot -Tjpeg $(F2).dot -o $(F2).jpeg

# Builds executable gpparse.
# Usage: make gpparse
$(P):		$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o $(P)

gpparser.tab.o: gpparser.tab.c gpparser.tab.h
		$(CC) $(CFLAGS) -c gpparser.tab.c

gpparser.tab.c gpparser.tab.h: gpparser.y ast.h
		bison -dtv gpparser.y

lex.yy.o: 	lex.yy.c 
		$(CC) $(CFLAGS) -c lex.yy.c

lex.yy.c:	gplexer.lex gpparser.tab.h 
		flex gplexer.lex

main.o:         main.c pretty.h ast.h seman.h
		$(CC) $(CFLAGS) -c main.c

ast.o: 		ast.c ast.h
		$(CC) $(CFLAGS) -c ast.c

pretty.o:       pretty.c pretty.h ast.h seman.h
		$(CC) $(CFLAGS) -c pretty.c

seman.o:	seman.c seman.h ast.h
		$(CC) $(CFLAGS) -c seman.c

clean:
		rm *.o gpparser.tab.c gpparser.tab.h lex.yy.c gpparse

