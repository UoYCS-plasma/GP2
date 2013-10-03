/*////////////////////////////////////////////////////////////////////////////

                                       main.c       
                               
  This is the compiler for GP2, a graph programming language. It takes as input
  two text files. One contains a GP2 graph program and the second contains a 
  host graph. The program parses the files with a Bison/Flex parser, creates
  an abstract syntax tree and prints the tree.

  The makefile for the project is in the same directory as this file. Build
  with the command 'make'.

  Compiled with GCC 4.7.1, GNU Bison 2.5.1 and Flex 2.5.35.
 
    

                           Created on 2/10/2013 by Chris Bak 

/////////////////////////////////////////////////////////////////////////// */ 

#include <stdio.h>  /* printf, fprintf, fopen */
#include "pretty.h" /* pretty printer function declarations */

int main(int argc, char** argv) {

  if(argc > 1 && !strcmp(argv[1], "-d")) { 
    yydebug = 1; 	/* yydebug controls generation of the debugging file gpparser.output. */
    argc--; argv++;	/* Effectively removing "-d" from the command line call. */
  }

  if(argc != 2) {
    fprintf(stderr, "ERROR: filename required\n");
    return 1;
  }

  if(!(yyin = fopen(argv[1], "r"))) {  /* The lexer reads from yyin. */
     perror(argv[1]);
     yylineno = 1;	
     return 1;
  }

  file_name = argv[1];
  printf("Processing %s\n", file_name);

  if(!yyparse()) {
    printf("GP2 parse succeeded\n\n");
    printf("GP Program %s\n\n", file_name); 
    print_list(gp_program);  
  }
  else printf("GP2 parse failed\n");
 
  fclose(yyin);  

  return 0;
}
