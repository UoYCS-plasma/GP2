parser: gpparser.h gpparser.y gplexer.lex
	bison -dtv gpparser.y
	flex gplexer.lex
	gcc -o gpparse lex.yy.c
	
	
  
