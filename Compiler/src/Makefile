OBJECTS = analysis.o ast.o error.o genCondition.o genLabel.o genProgram.o \
	  genRule.o lexer.o main.o parser.o pretty.o rule.o searchplan.o \
	  seman.o symbol.o transform.o 

CC = gcc
CFLAGS = -g -I ../inc -Wall -Wextra `pkg-config --cflags --libs glib-2.0`
LFLAGS = -lglib-2.0 
VALGRIND = G_SLICE=always-malloc G_DEBUG=gc-friendly valgrind --tool=memcheck \
           --leak-check=full --leak-resolution=high --track-origins=yes \

# Builds the compiler, generates code, and builds the runtime system.
# Usage: make prog=<path_to_program_file> host=<path_to_host_graph_file>
default:	$(OBJECTS)
		make build
		./gp2compile $(prog) $(host)
		cd /tmp/gp2 && make

# Builds everything and runs valgrind on the runtime executable.
debug:		$(OBJECTS)	
		make build
		./gp2compile $(prog) $(host) 
		cd /tmp/gp2 && make && $(VALGRIND) ./gp2run 

# As above, but the -c flag is passed to gp2compile.
copy:		$(OBJECTS)
		make build
		./gp2compile -c $(prog) $(host)
		cd /tmp/gp2 && make

copy-debug:	$(OBJECTS)	
		make build
		./gp2compile -c $(prog) $(host) 
		cd /tmp/gp2 && make && $(VALGRIND) ./gp2run 

# Builds the executable gp2compile.
build:		$(OBJECTS)
		$(CC) $(OBJECTS) $(LFLAGS) -o gp2compile 
		cd ../lib && make && cd ../src


# Builds the executable gp2compile and runs it with valgrind.
compile-debug:	$(OBJECTS)
		make build
		$(VALGRIND) --suppressions=GNOME.supp/glib.supp ./gp2compile $(prog) $(host)


parser.c parser.h: ast.h gpParser.y
		bison -t gpParser.y

lexer.c:	parser.h gpLexer.lex 
		flex gpLexer.lex

%.o:		%.c
		$(CC) -c $(CFLAGS) -o $@ $<

# Dependencies
analysis.o:	../inc/globals.h ast.h analysis.h
ast.o: 		../inc/globals.h ast.h error.h
error.o:	../inc/globals.h
genCondition.o:	../inc/globals.h genLabel.h rule.h genCondition.h
genLabel.o:	../inc/globals.h rule.h genLabel.h
genProgram.o:	../inc/globals.h ast.h genProgram.h
genRule.o:	../inc/globals.h genCondition.h genLabel.h rule.h searchplan.h transform.h genRule.h
lexer.o:	../inc/globals.h parser.h
main.o:		../inc/globals.h analysis.h error.h genProgram.h genRule.h libheaders.h parser.h pretty.h seman.h
parser.o:	../inc/globals.h lexer.c parser.h 
pretty.o:       ../inc/globals.h ast.h symbol.h pretty.h 
rule.o:		../inc/globals.h rule.h
searchplan.o:	../inc/globals.h rule.h searchplan.h
seman.o:	../inc/globals.h ast.h symbol.h seman.h 
symbol.o:	../inc/globals.h symbol.h
transform.o:	../inc/globals.h ast.h rule.h transform.h 

#Cleanup
clean:
		rm *.o 
		if [ -e "gp2.log" ]; then rm gp2.log; fi
		if [ -e "gp2.trace" ]; then rm gp2.trace; fi

clean-all:
		cd ../lib && make clean
		cd ../src
		rm parser.c parser.h lexer.c
		make clean
