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
#include <string.h> /* strcmp */
#include "pretty.h" /* pretty printer function declarations */
#include "seman.h" /* semantic analysis functions */
#define DRAW_TREE  
#define DRAW_TABLE

int main(int argc, char** argv) {

  /* Creates a new hashtable with strings as keys. g_str_equal is a string
   * hashing function built into GLib. 
   */	

  GHashTable *gp_symbol_table = NULL;	

  if(argc > 1 && !strcmp(argv[1], "-d")) { 
    yydebug = 1; 	/* yydebug controls generation of the debugging file gpparser.output. */
    argc--; argv++;	/* Effectively removing "-d" from the command line call. */
  }

  if(argc != 2) {
    fprintf(stderr, "Usage: gpparse [-dg] <filename>\n");
    return 1;
  }

  if(!(yyin = fopen(argv[1], "r"))) {  /* The lexer reads from yyin. */
     perror(argv[1]);
     yylineno = 1;	
     return 1;
  }

  file_name = argv[1];
  printf("Processing %s...\n\n", file_name);

  if(!yyparse()) {
    printf("GP2 parse succeeded\n\n");
    gp_program = reverse(gp_program); /* ast.c*/
    #ifdef DRAW_TREE
       print_dot_ast(gp_program, file_name); /* pretty.c */ 
    #endif
    gp_symbol_table = g_hash_table_new(g_str_hash, g_str_equal);
    declaration_scan(gp_program, gp_symbol_table, "Global"); /* seman.c */
    #ifdef DRAW_TABLE
       print_symbol_table(gp_symbol_table); /* pretty.c */
    #endif
  }
  else printf("GP2 parse failed\n");
 
  fclose(yyin);  

  return 0;
}
