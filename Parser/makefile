parser: gpparser.h gpparser.y gplexer.lex
	bison -d gpparser.y
	flex gplexer.lex
	gcc -o gpparse lex.yy.c
	
	
  
