/* ////////////////////////////////////////////////////////////////////////////

  Copyright 2015-2017 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>.
                               
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

#include "error.h"
#include "common.h"
#include "genProgram.h"
#include "genRule.h"
#include "parser.h"
#include "pretty.h"
#include "seman.h" 

#include <sys/stat.h>
//#include <unistd.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 
#include <ctype.h> 
#include <limits.h>

/* Toggle tracing of the Bison parser. The trace is printed to stderr. */
#undef PARSER_TRACE 

/* Toggle debugging of the compiler through AST printing before and after 
 * static analysis, and through printing of the symbol table. The output files
 * are placed in the same directory as the input program with filenames
 * <program>.dot and <program>.tab. */
#undef DEBUG_PROGRAM

/* The Bison parser has two separate grammars. The grammar that is parsed is 
 * determined by the first token it receives. If Bison receives GP_PROGRAM
 * then it will parse using the grammar for GP2 programs. If Bison receives
 * GP_GRAPH then it will parse using the grammar for GP2 host graphs.
 * The variable parse_target is passed to the lexer which in turn sends
 * the appropriate token to the parser. */

/* The values for the tokens GP_PROGRAM, GP_RULE, and GP_GRAPH are defined in
 * parser.h which is generated by Bison. */ 
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

/* Controls the CFLAGS in the generated makefile. */
bool debug_flags = false;

void printMakeFile(string output_dir, string install_dir)
{
   int length = strlen(output_dir) + 9;
   char makefile_name[length];
   strcpy(makefile_name, output_dir);
   strcat(makefile_name, "/");
   strcat(makefile_name, "Makefile");
   FILE *makefile = fopen(makefile_name, "w");
   if(makefile == NULL)
   { 
      perror("Makefile");
      exit(1);
   }
   
   if(install_dir != NULL) 
   { 
      fprintf(makefile, "INCDIR=%s/include\n", install_dir);
      fprintf(makefile, "LIBDIR=%s/lib\n", install_dir);
   }
   fprintf(makefile, "OBJECTS := $(patsubst %%.c, %%.o, $(wildcard *.c))\n");  
   fprintf(makefile, "CC=gcc\n\n");

   if(debug_flags) fprintf(makefile, "CFLAGS = -g -I$(INCDIR) -L$(LIBDIR) -Og -Wall -Wextra -lgp2\n\n");
   else fprintf(makefile, "CFLAGS = -I$(INCDIR) -L$(LIBDIR) -O3 -Wall -Wextra -lgp2\n\n");
   fprintf(makefile, "default:\t$(OBJECTS)\n\t\t$(CC) $(OBJECTS) $(CFLAGS) -o gp2run\n\n");
   fprintf(makefile, "%%.o:\t\t%%.c\n\t\t$(CC) -c $(CFLAGS) -o $@ $<\n\n");
   fprintf(makefile, "clean:\t\n\t\trm *\n");
   fclose(makefile);
} 

   
bool graph_copying = false;

int main(int argc, char **argv)
{
   string const usage = "Usage:\n"
                        "gp2 [-c] [-d] [-l <rootdir>] [-o <outdir>] <program_file>\n"
                        "gp2 -p <program_file>\n"
                        "gp2 -r <rule_file>\n"
                        "gp2 -h <host_file>\n\n"
                        "Flags:\n"
                        "-c - Enable graph copying.\n"
                        "-d - Compile program with GCC debugging flags.\n"
                        "-p - Validate a GP 2 program.\n"
                        "-r - Validate a GP 2 rule.\n"
                        "-h - Validate a GP 2 host graph.\n"
                        "-l - Specify root directory of installed files.\n"
                        "-o - Specify directory for generated code and program output.\n"
                        "--max-nodes - Specify maximum number of nodes in a host graph.\n"
                        "--max-edges - Specify maximum number of edges in a host graph.\n";

   /* If true, only parsing and semantic analysis executed on the GP2 source files. */
   bool validate = false;
   string program_file = NULL, host_file = NULL, rule_file = NULL, 
          install_dir = NULL, output_dir = NULL;

   unsigned int max_nodes = HOST_NODE_SIZE;
   unsigned int max_edges = HOST_EDGE_SIZE;
   char **p = NULL;

   if(argc < 2)
   {
      print_to_console("%s", usage);
      exit(EXIT_FAILURE);
   }

   if(strcmp(argv[1], "-p") == 0)
   {
      if(argc < 3)
      {
         print_to_console("%s", usage);
         exit(EXIT_FAILURE);
      }
      validate = true;
      program_file = argv[2];
   }
   else if(strcmp(argv[1], "-h") == 0)
   {
      if(argc < 3)
      {
         print_to_console("%s", usage);
         exit(EXIT_FAILURE);
      }
      validate = true;
      host_file = argv[2];
   }
   else if(strcmp(argv[1], "-r") == 0)
   {
      if(argc < 3)
      {
         print_to_console("%s", usage);
         exit(EXIT_FAILURE);
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
         if (strcmp(argv[argv_index], "--max-nodes") == 0)
         {
             if(argc < argv_index+2)
             {
                 print_to_console("%s", usage);
                 exit(EXIT_FAILURE);
             }
             long inp = strtol(argv[++argv_index], p, 10);
             if (inp > INT_MAX || inp < 1) {
                print_to_console("Error: invalid size for max_nodes/max_edges\n");
                exit(EXIT_FAILURE);
             }
             max_nodes = (unsigned int) inp;
         }
         else if (strcmp(argv[argv_index], "--max-edges") == 0)
         {
             if(argc < argv_index+2)
             {
                 print_to_console("%s", usage);
                 exit(EXIT_FAILURE);
             }
             long inp = strtol(argv[++argv_index], p, 10);
             if (inp > INT_MAX || inp < 1) {
                print_to_console("Error: invalid size for max_nodes/max_edges\n");
                exit(EXIT_FAILURE);
             }
             max_edges = (unsigned int) inp;
         }
         else {
            switch(parameter[1])
            {
               case 'c':
                    graph_copying = true;
                    break;

               case 'd':
                    debug_flags = true;
                    break;

               case 'l':
                    argv_index++;
                    if(argv_index == argc)
                    {
                       print_to_console("%s", usage);
                       exit(EXIT_FAILURE);
                    }
                    install_dir = argv[argv_index];
                    break;

               case 'o':
                    argv_index++;
                    if(argv_index == argc)
                    {
                       print_to_console("%s", usage);
                       exit(EXIT_FAILURE);
                    }
                    output_dir = argv[argv_index];
                    break;

               default:
                    print_to_console("Error: invalid option \"%s\".\n", parameter);
                    exit(EXIT_FAILURE);
            }
         }
      }
      /* The remaining parameter is the program file. */
      if(argc - argv_index != 1)
      {
         print_to_console("%s", usage);
         exit(EXIT_FAILURE);
      }
      program_file = argv[argv_index];
   }

   /* If no output directory specified, make a directory in /tmp. */
   if(output_dir == NULL) 
   {
      mkdir("/tmp/gp2", S_IRWXU | S_IRWXG | S_IRWXO );
      output_dir = "/tmp/gp2";
      chmod(output_dir, S_IRWXU | S_IRWXG | S_IRWXO );
      openLogFile("/tmp/gp2/gp2-compile.log");
   }
   else
   {
      int length = strlen(output_dir) + strlen("/gp2-compile.log");
      char log_name[length];
      strcpy(log_name, output_dir);
      strcat(log_name, "/gp2-compile.log");
      openLogFile(log_name);
   }

   if(validate)
   {
      if(program_file != NULL) 
      {
         bool result = validateProgram(program_file);
         if(result) print_to_console("Program %s is valid.\n", program_file);   
         else print_to_console("Program %s is invalid.\n", program_file);   
      }
      if(host_file != NULL)
      {
         bool result = validateHostGraph(host_file);
         if(result) print_to_console("Host graph %s is valid.\n", host_file);   
         else print_to_console("Host graph %s is invalid.\n", host_file);   
      }
      if(rule_file != NULL)
      {
         bool result = validateRule(rule_file);
         if(result) print_to_console("Rule %s is valid.\n", rule_file);   
         else print_to_console("Rule %s is invalid.\n", rule_file);   
      }
      if(yyin != NULL) fclose(yyin);
      if(gp_program) freeAST(gp_program);
      closeLogFile();
      return 0;
   }
   else
   {
      bool valid_program = validateProgram(program_file);
      if(!valid_program)
      {
         print_to_console("Program %s is invalid. Build aborted.\n", program_file);   
         if(yyin != NULL) fclose(yyin);
         if(gp_program) freeAST(gp_program); 
         closeLogFile();
         exit(EXIT_FAILURE);
      }
      else
      {
         print_to_console("Generating program code...\n");
         generateRules(gp_program, output_dir);
         generateRuntimeMain(gp_program, output_dir, max_nodes, max_edges);
         printMakeFile(output_dir, install_dir);
      }
   }
   if(yyin != NULL) fclose(yyin);
   if(gp_program) freeAST(gp_program); 
   closeLogFile();
   return 0;
}

