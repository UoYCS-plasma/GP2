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

/* The Bison parser has two separate grammars. The grammar that is parsed is 
 * determined by the first token it receives. If Bison receives GP_PROGRAM
 * then it will parse using the grammar for GP2 programs. If Bison receives
 * GP_GRAPH then it will parse using the grammar for GP2 host graphs.
 * The variable parse_target is passed to the lexer which in turn sends
 * the appropriate token to the parser. */ 
#define GP_PROGRAM 1 		
#define GP_GRAPH 2	
int parse_target = 0; 

static bool validateProgram(string program_file)
{
   yyin = fopen(program_file, "r");
   if(yyin == NULL) 
   {  
      perror(program_file);
      return false;
   }
   #ifdef PARSER_TRACE 
      yydebug = 1; /* Bison outputs a trace of its parse to stderr. */
   #endif
   parse_target = GP_PROGRAM;

   /* yyparse sets the global flag syntax_error to true if any syntax errors
    * exist in the program. Some syntax errors are cleanly handled by the parser,
    * resulting in a valid AST. Hence, if syntax errors are encountered, semantic
    * analysis can still be performed. */
   bool valid_program = (yyparse() == 0);
   if(!valid_program) return false;
   gp_program = reverse(gp_program);
   #ifdef DEBUG_PROGRAM
      /* analyseProgram prints the symbol table before exiting. */
      bool semantic_error = analyseProgram(gp_program, true, program_file);
   #else
      bool semantic_error = analyseProgram(gp_program, false, NULL);
   #endif
   return (!syntax_error && !semantic_error);
}

static bool validateHostGraph(string host_file)
{
   yyin = fopen(host_file, "r");
   if(yyin == NULL) 
   {  
      perror(host_file);
      return false;
   }
   parse_target = GP_GRAPH;
   return (yyparse() == 0);
}

int main(int argc, char **argv)
{
   string const usage = "Usage:\n"
                        "GP2-compile <program_file> <host_file>\n"
                        "GP2-compile -vp <program_file>\n"
                        "GP2-compile -vh <host_file>\n";
   if(argc != 3) 
   {
      print_to_console("%s", usage);
      return 0;
   }
   openLogFile("gp2.log");

   /* If true, only parsing and semantic analysis executed on the GP2 source files. */
   bool validate;
   string program_file = NULL, host_file = NULL;

   if(strcmp(argv[1], "-vp") == 0)
   {
      validate = true;
      program_file = argv[2];
   }
   else if(strcmp(argv[1], "-vh") == 0)
   {
      validate = true;
      host_file = argv[2];
   }
   else 
   {
      validate = false;
      program_file = argv[1];
      host_file = argv[2];
   }

   if(validate)
   {
      if(program_file != NULL) 
      {
         bool result = validateProgram(program_file);
         if(result) print_to_console("Program %s is valid.\n\n", program_file);   
         else print_to_console("Program %s is invalid.\n\n", program_file);   
      }
      if(host_file != NULL)
      {
         bool result = validateHostGraph(host_file);
         if(result) print_to_console("Host graph %s is valid.\n\n", host_file);   
         else print_to_console("Host graph %s is invalid.\n\n", host_file);   
      }
      if(yyin != NULL) fclose(yyin);
      if(gp_program) freeAST(gp_program); 
      closeLogFile();
      return 0;
   }
   else
   {
      bool valid_program = validateProgram(program_file);
      bool valid_host_graph = validateHostGraph(host_file);
      if(!valid_program)
         print_to_console("Program %s is invalid. Build aborted.\n\n", program_file);   
      if(!valid_host_graph)
         print_to_console("Host graph %s is invalid. Build aborted.\n\n", host_file);  
      if(!valid_program || !valid_host_graph)
      {
         if(yyin != NULL) fclose(yyin);
         if(gp_program) freeAST(gp_program); 
         closeLogFile();
         return 0;
      }
      else
      {
         print_to_console("Generating program code...\n\n");
         generateRules(gp_program);
         staticAnalysis(gp_program);   
         #ifdef DEBUG_PROGRAM
            printDotAST(gp_program, program_file);
         #endif
         generateRuntimeMain(gp_program, host_file, host_nodes, host_edges);
      }
   }
   if(yyin != NULL) fclose(yyin);
   if(gp_program) freeAST(gp_program); 
   closeLogFile();
   return 0;
}
