gpp: mainpp.o parser.o help/pretty.o ast.o gplex.o help/prettyGraph.o translate.o
	g++ -o gpp mainpp.o parser.o help/pretty.o ast.o gplex.o help/prettyGraph.o translate.o

parser.c parser.h: gpparser.y
	bison gpparser.y

gplex.c: gplexer.lex
	flex -ogplex.c gplexer.lex

mainpp.o: main.cpp help/error.h help/globals.h parser.h help/pretty.h ast.h help/prettyGraph.h ../parsertypes.hpp translate.hpp
	g++ -c main.cpp -std=c++0x -o mainpp.o

translate.o: translate.hpp translate.cpp
	g++ -c translate.cpp -std=c++0x -o translate.o

parser.o: parser.c parser.h
	gcc -c parser.c

pretty.o: help/pretty.c help/pretty.h
	cd help
	gcc -c pretty.c
	cd ..

prettyGraph.o: help/prettyGraph.c help/prettyGraph.h
	cd help 
	gcc -c prettyGraph.c
	cd ..

ast.o: ast.c ast.h
	gcc -c ast.c

clean:
	rm parser.c parser.h *.o help/*.o gpp gplex.c

