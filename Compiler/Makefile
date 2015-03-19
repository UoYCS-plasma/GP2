OBJECTS = parser.o lex.yy.o debug.o error.o ast.o pretty.o seman.o \
          transform.o label.o graph.o rule.o stack.o searchplan.o \
          genHost.o genRule.o genProgram.o main.o
CC = gcc
CFLAGS = -g -Wall -Wextra `pkg-config --cflags --libs glib-2.0`
LFLAGS = -lglib-2.0 
VALGRIND = G_SLICE=always-malloc G_DEBUG=gc-friendly valgrind --tool=memcheck \
           --leak-check=full --leak-resolution=high --track-origins=yes \

# Builds the parser and code generator, runs it on the passed GP2 program/host graph files,
# and executes the generated code.
# Usage: make all F1=<path_to_program_file> F2=<path_to_host_graph_file>
default:	
		make compile 
		./GP2-compile $(F1) $(F2) 
		cd runtime && make

run:	
		make compile 
		./GP2-compile $(F1) $(F2) 
		cd runtime && make && ./GP2-run

debug:		
		make compile-debug $(F1) $(F2) 
		cd runtime && make && $(VALGRIND) --suppressions=../GNOME.supp/glib.supp ./GP2-run 

# Builds the parser and the code generator.
compile:	$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o GP2-compile

# Builds the parser and the code generator. Runs the executable with a call to valgrind.
compile-debug:	$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o GP2-compile 	
		$(VALGRIND) --suppressions=GNOME.supp/glib.supp ./GP2-compile $(F1) $(F2)

clean:
		rm *.o parser.c parser.h lex.yy.c GP2-compile
		cd runtime && make clean

parser.c parser.h: gpparser.y ast.h error.h
		bison gpparser.y

lex.yy.c:	gplexer.lex parser.h ast.h error.h
		flex gplexer.lex

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Dependencies
main.o:		error.h globals.h genHost.h genProgram.h genRule.h parser.h seman.h
parser.o:	parser.h lex.yy.c
debug.o:	error.h globals.h 
ast.o: 		ast.h error.h globals.h
pretty.o:       pretty.h ast.h error.h globals.h seman.h
seman.o:	seman.h ast.h error.h globals.h
label.o:	label.h error.h globals.h
graph.o:	error.h globals.h label.h stack.h graph.h 
rule.o:		error.h globals.h graph.h rule.h
transform.o:	ast.h error.h globals.h graph.h label.h rule.h transform.h 
stack.o:	error.h globals.h stack.h
searchplan.o:	graph.h globals.h searchplan.h
genHost.o:	ast.h error.h globals.h transform.h genHost.h
genRule.o:	error.h globals.h rule.h searchplan.h transform.h genRule.h
genProgram.o:	ast.h error.h globals.h genProgram.h

