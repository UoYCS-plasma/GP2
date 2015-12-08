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
#include "genProgram.h"
#include "genRule.h"
#include "libheaders.h"
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
#define GP_RULE 3	
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
      if(semantic_error) printDotAST(gp_program, program_file);
   #else
      bool semantic_error = analyseProgram(gp_program, false, NULL);
   #endif
   return (!syntax_error && !semantic_error);
}

static bool validateRule(string rule_file)
{
   yyin = fopen(rule_file, "r");
   if(yyin == NULL) 
   {  
      perror(rule_file);
      return false;
   }
   #ifdef PARSER_TRACE 
      yydebug = 1; /* Bison outputs a trace of its parse to stderr. */
   #endif
   parse_target = GP_RULE;
   return (yyparse() == 0);
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

#define LIB_HEADERS 7
unsigned char *headers[LIB_HEADERS] = {globals_h, debug_h, graph_h, graphStacks_h,
                                       hostParser_h, label_h, morphism_h};

string file_names[LIB_HEADERS] = {"globals.h", "debug.h", "graph.h", "graphStacks.h", 
                                  "hostParser.h", "label.h", "morphism.h"};

void makeLibHeaders(string output_dir, unsigned char **headers, string *file_names,
                    int header_count)
{
   int i;
   for(i = 0; i < header_count; i++)
   {
      int length = strlen(output_dir) + strlen(file_names[i]) + 1;
      char file_name[length];
      strcpy(file_name, output_dir);
      strcat(file_name, "/");
      strcat(file_name, file_names[i]);
      
      FILE *file = fopen(file_name, "w");
      if(file == NULL) { 
         perror(file_name);
         exit(1);
      }  
      fprintf(file, "%s", headers[i]);
      fclose(file);
   }
}

/* Controls the CFLAGS in the generated makefile. */
bool debug_flags = false;

void printMakeFile(string output_dir)
{
   int length = strlen(output_dir) + 9;
   char makefile_name[length];
   strcpy(makefile_name, output_dir);
   strcat(makefile_name, "/");
   strcat(makefile_name, "Makefile");
   FILE *makefile = fopen(makefile_name, "w");
   if(makefile == NULL) { 
      perror("Makefile");
      exit(1);
   }
   
   char current_dir[1024];
   if(getcwd(current_dir, sizeof(current_dir)) == NULL) {
      perror("getcwd() error");
      exit(1);
   }

   fprintf(makefile, "LIB=%s\n", current_dir);
   fprintf(makefile, "OBJECTS := $(patsubst %%.c, %%.o, $(wildcard *.c))\n");  
   fprintf(makefile, "CC=gcc\n\n");

   if(debug_flags) fprintf(makefile, "CFLAGS = -g -L$(LIB) -Wall -Wextra -lgp2debug\n\n");
   else fprintf(makefile, "CFLAGS = -L$(LIB) -fomit-frame-pointer -O2 -Wall -Wextra -lgp2\n\n");

   fprintf(makefile, "default:\t$(OBJECTS)\n\t\t$(CC) $(OBJECTS) $(CFLAGS) -o GP2-run\n\n");
   fprintf(makefile, "%%.o:\t\t%%.c\n\t\t$(CC) -c $(CFLAGS) -o $@ $<\n\n");
   fprintf(makefile, "clean:\t\n\t\trm *\n");
   fclose(makefile);
} 

   
bool graph_copying = false;

int main(int argc, char **argv)
{
   string const usage = "Usage:\n"
                        "GP2-compile [-c] [-d] [-o <outdir>] <program_file> <host_file>\n"
                        "GP2-compile -p <program_file>\n"
                        "GP2-compile -r <rule_file>\n"
                        "GP2-compile -h <host_file>\n\n"
                        "Flags:\n"
                        "-c - Enable graph copying.\n"
                        "-d - Compile program with GCC debugging flags.\n"
                        "-r - Validate a GP 2 rule.\n"
                        "-p - Validate a GP 2 program.\n"
                        "-h - Validate a GP 2 host graph.\n"
                        "-o - Specify directory for generated code and program output.\n\n";

   /* If true, only parsing and semantic analysis executed on the GP2 source files. */
   bool validate = false;
   string program_file = NULL, host_file = NULL, rule_file = NULL, output_dir = NULL;
   openLogFile("/tmp/gp2-compile.log");

   if(argc < 2)
   {
      print_to_console("%s", usage);
      return 0; 
   }

   if(strcmp(argv[1], "-p") == 0)
   {
      if(argc != 3)
      {
         print_to_console("%s", usage);
         return 0; 
      }
      validate = true;
      program_file = argv[2];
   }
   else if(strcmp(argv[1], "-h") == 0)
   {
      if(argc != 3)
      {
         print_to_console("%s", usage);
         return 0; 
      }
      validate = true;
      host_file = argv[2];
   }
   else if(strcmp(argv[1], "-r") == 0)
   {
      if(argc != 3)
      {
         print_to_console("%s", usage);
         return 0; 
      }
      validate = true;
      rule_file = argv[2];
   }
   else
   {
      /* The following code assumes all options are listed separately before the 
       * program file and the host file. */
      int argv_index;
      for(argv_index = 1; argv_index < argc; argv_index++)
      {
         string parameter = argv[argv_index];
         if(parameter[0] != '-') break;
         switch(parameter[1])
         {
            case 'c':
                 graph_copying = true;
                 break;

            case 'd':
                 debug_flags = true;
                 break;

            case 'o':
                 argv_index++;
                 if(argv_index == argc)
                 {
                    print_to_console("%s", usage);
                    return 0; 
                 }
                 output_dir = argv[argv_index];
                 break;

            default:
                 print_to_console("Error: invalid option \"%s\".\n", parameter);
                 return 0;
         }
      }
      /* The remaining parameters are the program file and the host graph file. */
      if(argc - argv_index != 2)
      {
         print_to_console("%s", usage);
         return 0; 
      }
      program_file = argv[argv_index++];
      host_file = argv[argv_index];
   }

   /* If no output directory specified, make a directory in /tmp. */
   if(output_dir == NULL) 
   {
      mkdir("/tmp/gp2", S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
      output_dir = "/tmp/gp2";
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
      if(rule_file != NULL)
      {
         bool result = validateRule(rule_file);
         if(result) print_to_console("Rule %s is valid.\n\n", rule_file);   
         else print_to_console("Rule %s is invalid.\n\n", rule_file);   
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
         generateRules(gp_program, output_dir);
         staticAnalysis(gp_program);   
         #ifdef DEBUG_PROGRAM
            printDotAST(gp_program, program_file);
         #endif
         generateRuntimeMain(gp_program, host_nodes, host_edges, host_file, output_dir);
         makeLibHeaders(output_dir, headers, file_names, LIB_HEADERS);
         printMakeFile(output_dir);
      }
   }
   if(yyin != NULL) fclose(yyin);
   if(gp_program) freeAST(gp_program); 
   closeLogFile();
   return 0;
}

