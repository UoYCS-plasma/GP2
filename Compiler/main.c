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

#include "analysis.h"
#include "error.h"
#include "globals.h"
#include "genHost.h"
#include "genProgram.h"
#include "genRule.h"
#include "parser.h"
#include "seman.h" 

#define DEBUG
#undef PARSER_TRACE 	

/* The Bison parser has two separate grammars. The grammar that is parsed is 
 * determined by the first token it receives. If Bison receives GP_PROGRAM
 * then it will parse using the grammar for GP2 programs. If Bison receives
 * GP_GRAPH then it will parse using the grammar for GP2 host graphs.
 * The variable parse_target is passed to the lexer which in turn sends
 * the appropriate token to the parser. */ 
#define GP_PROGRAM 1 		
#define GP_GRAPH 2	
int parse_target = 0; 

int main(int argc, char **argv)
{
   if(argc < 2 || argc > 3) {
     print_to_console("Usage: GP2-compile [<program_file>] <host_graph_file>\n");
     return 1;
   }
   openLogFile();
   /* GP2 program compilation is optional. */
   if(argc == 3)
   {
      string program_file_name = argv[1];

      /* Open the GP2 program file to be read by the parser. */
      if(!(yyin = fopen(program_file_name, "r"))) 
      {  
         perror(program_file_name);
         return 1;
      }
      #ifdef PARSER_TRACE 
      yydebug = 1; /* Bison outputs a trace of its parse to stderr. */
      #endif
      parse_target = GP_PROGRAM;
      printf("\nProcessing %s...\n\n", program_file_name);

      if(yyparse() == 0) print_to_console("GP2 program parse succeeded.\n\n");
      else 
      {
         print_to_console("GP2 program parse failed.\n\n");     
         fclose(yyin);
         return 0;
      }
      gp_program = reverse(gp_program);
      #ifdef DEBUG
      bool valid_program = analyseProgram(gp_program, true, program_file_name);
      #else
      bool valid_program = analyseProgram(gp_program, false, NULL);
      #endif
      if(valid_program && !syntax_error) 
      {
         print_to_console("Generating code...\n\n");
         generateRules(gp_program);
         #ifdef DEBUG
            staticAnalysis(gp_program, true, program_file_name);   
         #else
            staticAnalysis(gp_program, false, NULL);
         #endif
         generateRuntimeCode(gp_program);
      }
      else print_to_console("Build aborted. Please consult the file gp2.log "
                            "for a detailed error report.\n");   
   }

   /* Open the GP2 host graph file to be read by the parser. */
   string host_graph_file = argc == 3 ? argv[2] : argv[1];
   if(!(yyin = fopen(host_graph_file, "r"))) {  
      perror(host_graph_file);
      return 1;
   }
   parse_target = GP_GRAPH;
   printf("\nProcessing %s...\n\n", host_graph_file);
  
   if(yyparse() == 0) print_to_console("GP2 graph parse succeeded.\n\n");
   else 
   {
      print_to_console("GP2 graph parse failed.\n\n");
      fclose(yyin);
      if(gp_program) freeAST(gp_program); 
      return 0;
   }   
   reverseGraphAST(ast_host_graph);
   #ifdef DEBUG 
      printDotHostGraph(ast_host_graph, host_graph_file);
   #endif
   generateHostGraphCode(ast_host_graph);

   fclose(yyin);
   if(gp_program) freeAST(gp_program); 
   if(ast_host_graph) freeASTGraph(ast_host_graph); 
   closeLogFile();
   return 0;
}
