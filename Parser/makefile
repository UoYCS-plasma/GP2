parser: gpparser.h gpparser.y gplexer.l
	bison -dtv gpparser.y
	flex gplexer.l
	gcc -o gpparse lex.yy.c
	
	
  
