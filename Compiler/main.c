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
#include "pretty.h"
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
   string const usage = "Usage:\n"
                        "GP2-compile <program_file> <host_file>\n"
                        "GP2-compile -h <host_file>\n"
                        "GP2-compile -p <program_file>\n";
   if(argc != 3) {
     print_to_console("%s", usage);
     return 0;
   }
   openLogFile();

   /* 0 - Build both program and host graph. 
    * 1 - Build only host graph.
    * 2 - Build only program. */
   int mode = 0;
   string program_file = NULL, host_file = NULL;

   /* Check for options. */
   if(argv[1][0] != '-')
   {
     program_file = argv[1];
     host_file = argv[2];
   }
   else
   {
      if(argv[1][1] == 'h')
      {
         mode = 1;
         host_file = argv[2];
      }
      else if(argv[1][1] == 'p')
      {
         mode = 2;
         program_file = argv[2];
      }
      else
      {
        print_to_console("%s", usage);
        return 0;
      }
   }
   
   if(mode != 1)
   {
      /* Set up and run the GP 2 program parser. */
      if(!(yyin = fopen(program_file, "r"))) 
      {  
         perror(program_file);
         return 1;
      }
      #ifdef PARSER_TRACE 
         yydebug = 1; /* Bison outputs a trace of its parse to stderr. */
      #endif
      parse_target = GP_PROGRAM;
      printf("\nProcessing %s...\n\n", program_file);

      if(yyparse() == 0) print_to_console("GP2 program parse succeeded.\n\n");
      else 
      {
         print_to_console("GP2 program parse failed.\n\n");     
         fclose(yyin);
         return 0;
      }
      gp_program = reverse(gp_program);
      #ifdef DEBUG
         /* analyseProgram prints the symbol table before exiting. */
         bool valid_program = analyseProgram(gp_program, true, program_file);
      #else
         bool valid_program = analyseProgram(gp_program, false, NULL);
      #endif
      if(valid_program && !syntax_error) 
      {
         print_to_console("Generating program code...\n\n");
         generateRules(gp_program);
         staticAnalysis(gp_program);   
         #ifdef DEBUG
            printDotAST(gp_program, program_file, "_2");
         #endif
         generateRuntimeCode(gp_program);
      }
      else print_to_console("Build aborted. Please consult the file gp2.log "
                            "for a detailed error report.\n");   
   }

   if(mode == 2) generateHostGraphCode(NULL); 
   else
   {
      /* Set up and run the host graph parser. */
      if(!(yyin = fopen(host_file, "r"))) {  
         perror(host_file);
         return 1;
      }
      parse_target = GP_GRAPH;
      printf("\nProcessing %s...\n\n", host_file);
   
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
         printDotHostGraph(ast_host_graph, host_file);
      #endif
      print_to_console("Generating host graph code...\n\n");
      generateHostGraphCode(ast_host_graph);
   }

   fclose(yyin);
   if(gp_program) freeAST(gp_program); 
   if(ast_host_graph) freeASTGraph(ast_host_graph); 
   closeLogFile();
   return 0;
}
