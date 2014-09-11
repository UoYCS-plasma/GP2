P = runGP
OBJECTS = gpparser.tab.o lex.yy.o ast.o rule.o pretty.o seman.o graph.o match.o staticsearch.o main.o
CC = gcc
CFLAGS = -g -Wall -Wextra `pkg-config --cflags --libs glib-2.0`
#CFLAGS = -g -Wall -Wextra -I/local/d0p6/chrisbak/root/include/glib-2.0 -I/local/d0p6/chrisbak/root/lib/glib-2.0/include
LFLAGS = -lglib-2.0


# Builds executable gpparse and runs it on extensionless files.
# Usage: make F1=<program_filename> F2=<graph_filename>
default:        $(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o $(P) 	
		./$(P) $(F1) $(F2)       	

debug:		$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o $(P) 	
		G_SLICE=always-malloc G_DEBUG=gc-friendly valgrind --tool=memcheck --leak-check=full --leak-resolution=high --track-origins=yes --suppressions=GNOME.supp/glib.supp ./$(P) $(F1) $(F2)

# Testing file.
test:		ast.o graph.o match.o rule.o staticsearch.o test.o
		$(CC) ast.o graph.o match.o rule.o staticsearch.o test.o $(LFLAGS) -o testGP
		./testGP

test-debug:	ast.o graph.o match.o rule.o staticsearch.o test.o
		$(CC) ast.o graph.o match.o rule.o staticsearch.o test.o $(LFLAGS) -o testGP
		G_SLICE=always-malloc G_DEBUG=gc-friendly valgrind --tool=memcheck --leak-check=full --leak-resolution=high --track-origins=yes ./testGP



# Builds executable runGP.
# Usage: make runGP
$(P):		$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o $(P)

gpparser.tab.o: gpparser.tab.c gpparser.tab.h
		$(CC) $(CFLAGS) -c gpparser.tab.c

gpparser.tab.c gpparser.tab.h: gpparser.y ast.h
		bison -dtv gpparser.y

lex.yy.o: 	lex.yy.c 
		$(CC) $(CFLAGS) -c lex.yy.c

lex.yy.c:	gplexer.lex gpparser.tab.h ast.h 
		flex gplexer.lex

main.o:         main.c pretty.h
		$(CC) $(CFLAGS) -c main.c

ast.o: 		ast.c ast.h
		$(CC) $(CFLAGS) -c ast.c

rule.o:		rule.c rule.h
		$(CC) $(CFLAGS) -c rule.c

pretty.o:       pretty.c pretty.h 
		$(CC) $(CFLAGS) -c pretty.c

seman.o:	seman.c seman.h 
		$(CC) $(CFLAGS) -c seman.c

graph.o:	graph.c graph.h 
		$(CC) $(CFLAGS) -c graph.c

match.o:	match.c match.h 
		$(CC) $(CFLAGS) -c match.c

staticsearch.o:	staticsearch.c staticsearch.h 
		$(CC) $(CFLAGS) -c staticsearch.c

test.o:		test.c staticsearch.h
		$(CC) $(CFLAGS) -c test.c
clean:
		rm *.o gpparser.tab.c gpparser.tab.h lex.yy.c runGP

