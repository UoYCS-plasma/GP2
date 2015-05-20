OBJECTS = parser.o lex.yy.o debug.o error.o ast.o pretty.o seman.o symbol.o \
          transform.o label.o graph.o graphStacks.o rule.o searchplan.o \
          analysis.o genHost.o genLabel.o genRule.o genProgram.o main.o
CC = gcc
CFLAGS = -g -Wall -Wextra `pkg-config --cflags --libs glib-2.0`
LFLAGS = -lglib-2.0 
VALGRIND = G_SLICE=always-malloc G_DEBUG=gc-friendly valgrind --tool=memcheck \
           --leak-check=full --leak-resolution=high --track-origins=yes \

# Builds the compiler, generates code, and builds the runtime system.
# Usage: make F1=<path_to_program_file> F2=<path_to_host_graph_file>
default:	$(OBJECTS)
		make build
		./GP2-compile $(prog) $(host)
		cd runtime && make

program:	
		./GP2-compile -p $(prog)
		cd runtime && make

host:	
		./GP2-compile -h $(host)
		cd runtime && make

# Builds the executable GP2-compile.
build:		$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o GP2-compile 	

# Builds everything and runs valgrind on the runtime executable.
debug:		$(OBJECTS)	
		./GP2-compile $(prog) $(host) 
		cd runtime && make && $(VALGRIND) ./GP2-run 

# Builds the executable GP2-compile and runs it with valgrind.
compile-debug:	$(OBJECTS)
		make build
		$(VALGRIND) --suppressions=GNOME.supp/glib.supp ./GP2-compile $(prog) $(host)

clean:
		cd runtime && make clean

clean-obj:
		rm *.o 

clean-all:
		make clean-obj
		make clean

parser.c parser.h: gpparser.y ast.h error.h
		bison gpparser.y

lex.yy.c:	gplexer.lex parser.h ast.h error.h
		flex gplexer.lex

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Dependencies
main.o:		analysis.h error.h globals.h genHost.h genProgram.h genRule.h parser.h pretty.h seman.h
parser.o:	parser.h lex.yy.c
debug.o:	error.h globals.h 
ast.o: 		ast.h error.h globals.h
pretty.o:       pretty.h ast.h error.h globals.h seman.h
seman.o:	seman.h ast.h error.h globals.h symbol.h
symbol.o:	error.h globals.h symbol.h
label.o:	error.h globals.h label.h
graph.o:	error.h globals.h label.h graph.h 
graphStacks.o:  error.h globals.h graph.h graphStacks.h
rule.o:		error.h globals.h graph.h rule.h
transform.o:	ast.h error.h globals.h graph.h label.h rule.h transform.h 
searchplan.o:	graph.h globals.h searchplan.h
analysis.o:	ast.h globals.h pretty.h analysis.h
genHost.o:	ast.h error.h genLabel.h globals.h transform.h genHost.h
genLabel.o:     error.h globals.h label.h genLabel.h
genRule.o:	error.h genLabel.h globals.h label.h rule.h searchplan.h transform.h genRule.h
genProgram.o:	ast.h error.h globals.h genProgram.h

