/* ////////////////////////////////////////////////////////////////////////////

  ===============================
  main.c - Chris Bak (02/10/2013)
  ===============================     
                               
  This is the compiler for GP2, a graph programming language. It takes as input
  two text files. One contains a GP2 graph program and the second contains a 
  host graph. The program parses the files with a Bison parser, creates an
  abstract syntax tree and a symbol table, and performs some semantic analysis.
  Pretty printing of the AST and the symbol table is possible by defining the
  appropriate macros.

  The makefile for the project is in the same directory as this file. Build
  with the command 'make gpparse.'

  Compiled with GCC 4.6.4, GNU Bison 2.5 and Flex 2.5.35.

/////////////////////////////////////////////////////////////////////////// */ 

#include "ast.h" /* struct List, struct GPGraph */
#include "pretty.h" /* Pretty printer functions */
#include "seman.h" /* Semantic analysis functions */
#include <stdbool.h>
#include <stdio.h> 
#include <stdlib.h>
#include <string.h>


/* Macros to control debugging features. */
#undef PARSER_TRACE 		/* Assign yydebug to 1 */
#define DRAW_ORIGINAL_AST 	/* Call printDotAST before semanticCheck. */
#define DRAW_FINAL_AST 		/* Call printDotAST after semanticCheck. */
#define PRINT_SYMBOL_TABLE 	/* Call printSymbolTable after semanticCheck. */
#define DRAW_HOST_GRAPH_AST     /* Call printGraph after second call to 
                                   yyparse. */


/* The Bison parser has two separate grammars. The grammar that is parsed is 
 * determined by the first token it receives. If Bison receives GP_PROGRAM
 * then it will parse using the grammar for GP2 programs. If Bison receives
 * GP_GRAPH then it will parse using the grammar for GP2 host graphs.
 * The variable parse_target is passed to the lexer which in turn sends
 * the appropriate token to the parser. 
 */ 

#define GP_PROGRAM 1 		
#define GP_GRAPH 2	
int parse_target = 0; 

FILE *log_file;  /* File to contain verbose errors for developers */
string file_name = NULL; /* The name of the file being parsed */
bool abort_scan = false; /* If set to true, semantic checking does not occur. */
bool abort_compilation = false; /* If set to true, code generation does not occur. */

/* The parser points this to the root of the program AST. */
struct List *gp_program = NULL; 

/* The parser points this to the root of the host graph's AST. */
struct GPGraph *host_graph = NULL; 

/* The symbol table is created inside main */
GHashTable *gp_symbol_table = NULL;	



/* Usage: gpparse [-dg] <program_file> <host_graph_file> */
int main(int argc, char** argv) {

  if(argc != 3) {
    print_to_console( "Usage: gpparse <program_file> <host_graph_file>\n");
    return 1;
  }

  /* Open the log file for writing. */
  file_name = argv[1]; 
  int length = strlen(file_name) + 4; 
  char log_file_name[length];
  strcpy(log_file_name, file_name);
  strncat(log_file_name, ".log", 4);
  if(!(log_file = fopen(log_file_name, "w"))) { 
     perror(log_file_name);
     return 1;
  }
  print_to_log("%s\n\n",log_file_name);

  /* The global variable FILE *yyin is declared in lex.yy.c. It must be 
   * pointed to the file to be read by the parser. argv[1] is the file 
   * containing the GP program text file.
   */

  if(!(yyin = fopen(argv[1], "r"))) {  
     perror(argv[1]);
     yylineno = 1;	
     return 1;
  }

  #ifdef PARSER_TRACE
  yydebug = 1; /* When yydebug is set to 1, Bison outputs a trace of its 
                * parse to stderr. */
  #endif

  /* Bison parses with the GP2 program grammar */
  parse_target = GP_PROGRAM;

  printf("\nProcessing %s...\n\n", file_name);

  if(!yyparse()) {
     print_to_log("GP2 program parse succeeded\n\n");
     #ifdef DRAW_ORIGINAL_AST
        printDotAST(gp_program, file_name); /* Defined in pretty.c */ 
     #endif
  }

  else print_to_log("GP2 program parse failed.\n\n");

  /* Point yyin to the file containing the host graph. */

  if(!(yyin = fopen(argv[2], "r"))) {  
     perror(argv[1]);
     yylineno = 1;	
     return 1;
  }

  file_name = argv[2];

  /* Bison parses with the host graph grammar */
  parse_target = GP_GRAPH;

  printf("\nProcessing %s...\n\n", file_name);
 
  if(!yyparse()) {
     print_to_log("GP2 graph parse succeeded\n\n");    
  
     reverseGraphAST(host_graph); /* Defined in seman.c */

     #ifdef DRAW_HOST_GRAPH_AST
        printDotHostGraph(host_graph, file_name); /* Defined in pretty.c */
     #endif
  }
  else print_to_log("GP2 graph parse failed.\n\n");     

  /* The lexer and parser set the abort_scan flag if a syntax error is
   * encountered. */

  if(abort_scan) abort_compilation = true; 
  else {
    /* Reverse the global declaration list at the top of the generated AST. */
    gp_program = reverse(gp_program);

    /* Create a new GHashTable with strings as keys.
     * g_str_hash is glib's default string hashing function.
     * g_str_equal is glib's default function for comparing strings for hash
     * lookups.
     * free is the function called by glib to free keys during hash table
     * insertions and in the g_hash_table_destroy function.
     * The fourth argument is a function to free values. I do this manually
     * with the function freeSymbolList defined in seman.c: I do not pass a 
     * value-freeing function to g_hash_table_new_full.
     */
    gp_symbol_table = g_hash_table_new_full(g_str_hash, g_str_equal, free, NULL);    

    /* declarationScan is defined in seman.c. It returns true if there is a
     * name clash among the rule and procedure declarations.
     */
    abort_scan = declarationScan(gp_program, gp_symbol_table, "Global");
  }
    
  if(abort_scan) abort_compilation = true;
  else {
     /* semanticCheck is defined in seman.c. It returns true if there is a
      * semantic error.
      */
     abort_compilation = semanticCheck(gp_program, gp_symbol_table, "Global"); 

     #ifdef DRAW_FINAL_AST
        /* create the string <file_name>_F as an argument to printDotAST */
        int length = strlen(argv[1])+2;
        char alt_name[length];
        strcpy(alt_name,argv[1]);
        strcat(alt_name,"_F"); 
        printDotAST(gp_program, alt_name); /* Defined in pretty.c */ 
     #endif
     
     #ifdef PRINT_SYMBOL_TABLE
        printSymbolTable(gp_symbol_table, argv[1]); /* Defined in pretty.c */
     #endif
  }

  if(!abort_compilation) print_to_console("Proceed with code generation.\n"); 
  else print_to_console("\nBuild aborted. Please consult the file %s.log for "
                        "a detailed error report.\n", argv[1]);   
 
  /* Garbage collection */
  fclose(yyin);
  fclose(log_file);
  if(gp_program) freeAST(gp_program); /* Defined in ast.c */
  if(host_graph) freeASTGraph(host_graph); /* Defined in ast.c */

  /* The call to g_hash_table_foreeach frees all the hash table values,
   * linked lists of struct Symbols, with the function freeSymbolList 
   * defined in seman.c.
   * g_hash_table_destroy uses the key-freeing function passed to 
   * g_hash_table_full (free) to free the dynamically allocated keys.
   */
  if(gp_symbol_table) {
    g_hash_table_foreach(gp_symbol_table, freeSymbolList, NULL);
    g_hash_table_destroy(gp_symbol_table); 
  }

}
