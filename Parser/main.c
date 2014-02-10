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

#include "ast.h" /* struct List */
#include "pretty.h" /* pretty printer function declarations */
#include "seman.h" /* semantic analysis functions */
#include <stdio.h>  /* printf, fprintf, fopen */
#include <stdlib.h> /* free */
#include <string.h> /* strcmp */

#define DRAW_ORIGINAL_TREE /* print_dot_ast before semantic_check */
#define DRAW_FINAL_TREE /* print_dot_ast after semantic_check */
#define DRAW_TABLE

FILE *log_file;
char *file_name = NULL; 
int abort_scan = 0;

/* The parser points this to the root of the AST. */
struct List *gp_program = NULL; 

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

  /* The global variable FILE *yyin is declared in lex.yy.c. It must be 
   * pointed to the file to be read by the parser.
   */

  if(!(yyin = fopen(argv[1], "r"))) {  
     perror(argv[1]);
     yylineno = 1;	
     return 1;
  }

  /* Open the file gp.log for writing. */

  if(!(log_file = fopen("gp.log", "w"))) { 
     perror(argv[1]);
     return 1;
  }

  char *file_name = argv[1];
  printf("\nProcessing %s...\n\n", file_name);

  if(!yyparse()) {
    printf("GP2 parse succeeded\n\n");

    /* Reverse the global declaration list at the top of the generated AST. */
    gp_program = reverse(gp_program);

    /* Create a new GHashTable with strings as keys.
     * g_str_hash is glib's default string hashing function.
     * g_str_equal is glib's default function for comparing strings for hash
     * lookups.
     * free is the function called by glib to free keys during hash table
     * insertions and in the g_hash_table_destroy function.
     * free_symbol_list is defined in seman.c. This is called by glib to
     * free hash table values during insertions and in the destroy function.
     */

    gp_symbol_table = g_hash_table_new_full(g_str_hash, g_str_equal, free,
					    (GDestroyNotify)free_symbol_list);    

    /* The lexer and parser can set the abort_scan flag */

    if(!abort_scan) {
      abort_scan = declaration_scan(gp_program, gp_symbol_table, "Global");
                     /* seman.c */
      #ifdef DRAW_ORIGINAL_TREE
         print_dot_ast(gp_program, file_name); /* pretty.c */ 
      #endif
    }

    /* declaration_scan returns 1 if there is a name clash among the 
     * rule and procedure declarations.
     */

    if(!abort_scan) {
       semantic_check(gp_program, gp_symbol_table, "Global"); /* seman.c */
       #ifdef DRAW_FINAL_TREE
          /* create the string <file_name>_F as an argument to print_dot_ast */
          int length = strlen(file_name)+2;
          char alt_name[length];
          strcpy(alt_name,file_name);
          strcat(alt_name,"_F"); 
          print_dot_ast(gp_program, alt_name); /* pretty.c */ 
       #endif
     

    #ifdef DRAW_TABLE
       print_symbol_table(gp_symbol_table); /* pretty.c */
    #endif
    }

  }
  else fprintf(stderr,"GP2 parse failed.\n\n");
 
  /* Garbage collection */
  free_ast(gp_program); /* defined in ast.c */
  /* g_hash_table_destroy uses free and free_symbol_list, passed to 
   * g_hash_table_new_full, to free the dynamically allocated keys and values
   * respectively.
   */
  g_hash_table_destroy(gp_symbol_table); 
  fclose(yyin);  
  fclose(log_file);

  return 0;
}
