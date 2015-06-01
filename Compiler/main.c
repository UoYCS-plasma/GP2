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

int main(int argc, char **argv)
{
   string const usage = "Usage:\n"
                        "GP2-compile [-v] <program_file> <host_file>\n"
                        "GP2-compile -[v]h <host_file>\n"
                        "GP2-compile -[v]p <program_file>\n";

   if(argc != 3 && argc != 4) {
      print_to_console("%s", usage);
      return 0;
   }
   openLogFile("gp2.log");

   /* If true, only parsing and semantic analysis executed on the GP2 source files. */
   bool validate = false;

   /* 0 - Takes both the program and the host graph source files. 
    * 1 - Takes only the program source file.
    * 2 - Takes only the host graph source file. */
   int mode = 0;
   string program_file = NULL, host_file = NULL;

   /* Check for command-line options. */
   if(argv[1][0] == '-')
   {
      if(strcmp(argv[1], "-v") == 0)
      {
         if(argc == 4)
         {
            validate = true;
            program_file = argv[2];
            host_file = argv[3];
         }
         else
         {
            print_to_console("%s", usage);
            return 0;
         }
      }
      else if(strcmp(argv[1], "-p") == 0)
      {
         mode = 1;
         program_file = argv[2];
      }
      else if(strcmp(argv[1], "-h") == 0)
      {
         mode = 2;
         host_file = argv[2];
      }
      else if(strcmp(argv[1], "-vp") == 0)
      {
         validate = true;
         mode = 1;
         program_file = argv[2];
      }
      else if(strcmp(argv[1], "-vh") == 0)
      {
         validate = true;
         mode = 2;
         host_file = argv[2];
      }
      else
      {
         print_to_console("%s", usage);
         return 0;
      }
   }
   else /* No options provided. */
   {
     program_file = argv[1];
     host_file = argv[2];
   }

   /* Program code is generated in modes 0 and 1. */
   if(mode == 0 || mode == 1)
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
      if(yyparse() == 0) print_to_console("Program parse succeeded.\n");
      else  
      {
         if(validate)
              print_to_console("Program %s is invalid.\n", program_file);   
         else print_to_console("Program is invalid. Build aborted.\n");     
         fclose(yyin);
         if(gp_program) freeAST(gp_program); 
         closeLogFile();
         return 0;
      }
      gp_program = reverse(gp_program);
      #ifdef DEBUG_PROGRAM
         /* analyseProgram prints the symbol table before exiting. */
         bool semantic_error = analyseProgram(gp_program, true, program_file);
      #else
         bool semantic_error = analyseProgram(gp_program, false, NULL);
      #endif
      bool valid_program = !syntax_error && !semantic_error;
      if(valid_program && !validate) 
      {
         print_to_console("Generating program code...\n\n");
         generateRules(gp_program);
         staticAnalysis(gp_program);   
         #ifdef DEBUG_PROGRAM
            printDotAST(gp_program, program_file, "_2");
         #endif
         generateRuntimeMain(gp_program);
      }
      if(!valid_program && validate)
         print_to_console("Program %s is invalid.\n", program_file);   
      if(!valid_program && !validate)
         print_to_console("Program is invalid. Build aborted.\n");   
      if(valid_program && validate)
         print_to_console("Program %s is valid.\n", program_file);
   }

   /* Host graph code is generated in modes 0 and 2. */
   if(mode == 0 || mode == 2)
   {
      /* Set up and run the host graph parser. */
      if(!(yyin = fopen(host_file, "r"))) {  
         perror(host_file);
         return 1;
      }
      parse_target = GP_GRAPH;
      bool valid_graph = yyparse() == 0;
      if(valid_graph && !validate)
      {
         print_to_console("Host graph parse succeeded.\n");
         reverseGraphAST(ast_host_graph);
         #ifdef DEBUG_HOST 
            printDotHostGraph(ast_host_graph, host_file);
         #endif
         print_to_console("Generating host graph code...\n\n");
         generateHostGraphCode(ast_host_graph);
      }
      if(!valid_graph && validate)
         print_to_console("Host graph %s is invalid.\n", host_file);   
      if(!valid_graph && !validate)
         print_to_console("Host graph is invalid.\n");
      if(valid_graph && validate)
         print_to_console("Host graph %s is valid.\n", host_file);
   }
   /* If a host graph is already generated (checked by the existence of the
    * directory runtime/host), do nothing, otherwise generate code for the
    * empty host graph. */
   if(mode == 1)
   {
      /* UNIX-dependent code. */
      DIR *dir = opendir("runtime/host");
      /* Opendir succeeded: directory exists. */
      if(dir != NULL) closedir(dir);
      /* Directory does not exist. */
      else if(errno == ENOENT) generateHostGraphCode(NULL); 
      /* Opendir failed for another reason. */
      else
      {
         print_to_log("Error (main): opendir failure.\n");
         exit(1);
      }
   }
   fclose(yyin);
   if(gp_program) freeAST(gp_program); 
   if(ast_host_graph) freeASTGraph(ast_host_graph); 
   closeLogFile();
   return 0;
}
