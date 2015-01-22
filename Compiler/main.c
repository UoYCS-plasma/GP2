/* ////////////////////////////////////////////////////////////////////////////
                               
  This is the code generator for GP2, a graph programming language. It takes as
  input two text files. The first is a textual specification of GP2 graph
  program; the second contains a textual description of a host graph. 
  The program parses the files with a Bison parser to creates two abstract
  syntax trees and a symbol table for the program, and performs some semantic 
  analysis.
  If there are no significant syntactic or semantic faults with the program,
  code is generated to execute the GP2 program on the input host graph.

  Pretty printing of the AST and the symbol table is enabled by defining the
  appropriate macros.

/////////////////////////////////////////////////////////////////////////// */ 

#include "ast.h" 
#include "debug.h"
#include "error.h"
#include "globals.h"
#include "generate.h"
#include "parse.h"
#include "rule.h"
#include "seman.h" 
#include "stack.h"
#include "transform.h"

/* Macros to control debugging features. */
#undef PARSER_TRACE 		/* Assign yydebug to 1 */
#undef DRAW_ORIGINAL_AST 	/* Call printDotAST before semanticCheck. */
#undef DRAW_FINAL_AST 		/* Call printDotAST after semanticCheck. */
#undef PRINT_SYMBOL_TABLE 	/* Call printSymbolTable after semanticCheck. */
#undef DRAW_HOST_GRAPH_AST     /* Call printGraph after second call to 
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

bool abort_scan = false; /* If set to true, semantic checking does not occur. */

/* The parser points this to the root of the program AST. */
struct List *gp_program = NULL; 

/* The parser points this to the root of the host graph's AST. */
struct GPGraph *ast_host_graph = NULL; 

Stack *rule_stack = NULL;

int main(int argc, char** argv)
{
   if(argc != 3) {
     print_to_console("Usage: GP2-compile <program_file> <host_graph_file>\n");
     return 1;
   }
   openLogFileC(argv[1]);

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

   printf("\nProcessing %s...\n\n", argv[1]);

   if(!yyparse()) {
      print_to_log("GP2 program parse succeeded\n\n");
      #ifdef DRAW_ORIGINAL_AST
         /* create the string <>_first as an argument to printDotAST */
         int length = strlen(argv[1])+6;
         char alt_name[length];
         strcpy(alt_name, argv[1]);
         strcat(alt_name, "_first"); 
         printDotAST(gp_program, alt_name);
      #endif
   }

   else print_to_log("GP2 program parse failed.\n\n");

   /* Point yyin to the file containing the host graph. */

   if(!(yyin = fopen(argv[2], "r"))) {  
      perror(argv[1]);
      yylineno = 1;	
      return 1;
   }

   /* Bison parses with the host graph grammar */
   parse_target = GP_GRAPH;

   printf("\nProcessing %s...\n\n", argv[2]);
  
   if(!yyparse()) {
      print_to_log("GP2 graph parse succeeded\n\n");    
   
      reverseGraphAST(ast_host_graph);

      #ifdef DRAW_HOST_GRAPH_AST
         printDotHostGraph(ast_host_graph, argv[2]);
      #endif
   }
   else print_to_log("GP2 graph parse failed.\n\n");     

   /* Reverse the program's global declaration list. */
   gp_program = reverse(gp_program);

   #ifdef DEBUG
     bool valid_program = analyseProgram(gp_program, true, argv[1]);
   #else
     bool valid_program = analyseProgram(gp_program, false, NULL);
   #endif

   if(valid_program) 
   {
      print_to_console("Generating code...\n\n"); 
      generateRuntimeCode(gp_program);
      generateHostGraphCode(ast_host_graph);
   }
   else print_to_console("Build aborted. Please consult the file %s.log for "
                         "a detailed error report.\n", argv[1]);   

   fclose(yyin);
   if(gp_program) freeAST(gp_program); 
   if(ast_host_graph) freeASTGraph(ast_host_graph); 

   closeLogFile();

   return 0;
}
