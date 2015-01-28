/* ///////////////////////////////////////////////////////////////////////////

  =======================
  Generate Program Module
  =======================    

  The code generating module. Responsible for generating the main routine
  of the GP 2 runtime system and the code to set up the runtime system, 
  namely the construction of the host graph.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_PROGRAM_H
#define INC_GEN_PROGRAM_H

/* main_header and main_source are the file handles for runtime.h
 * and runtime.c respectively. */
#define printToMainHeader(code, ...)	             \
  do { fprintf(main_header, code, ##__VA_ARGS__); }  \
  while(0) 

#define PTMH printToMainHeader

#define printToMainSource(code, ...)	             \
  do { fprintf(main_source, code, ##__VA_ARGS__); }  \
  while(0) 

#define PTMS printToMainSource

#define printToMainSourceI(code, indent, ...)	         		\
  do { fprintf(main_source, "%*s" code, indent, " ", ##__VA_ARGS__); }  \
  while(0) 

#define PTMSI printToMainSourceI

/* source is the file handle for init_source.c. */
#define printToInitSource(code, ...)	        \
  do { fprintf(source, code, ##__VA_ARGS__); }  \
  while(0) 

#define PTIS printToInitSource

#include "ast.h"
#include "error.h"
#include "genMatch.h"
#include "globals.h"
#include "transform.h"

/* Given the AST of a GP2 host graph, generateHostGraphCode creates the 
 * module init_runtime. A function to create the host graph is written
 * to init_runtime.c. */ 
 void generateHostGraphCode(GPGraph *ast_host_graph);

/* The contexts of a GP2 program determine the code that is generated. In
 * particular, the code generated when a rule match fails is determined by
 * its context. The context also has some impact on graph copying. */
typedef enum {TOP_LEVEL, PROC_BODY, IF_BODY, TRY_BODY, LOOP_BODY} ContextType;

/* The main function for code generation. Creates the runtime module and 
 * populates header and source with their preliminaries. The main code
 * generation phase is initiated with a call to generateDeclarationCode on
 * the passed declaration list. */
void generateRuntimeCode(List *declarations);

/* Walks the program AST, calling generateProgramCode for each command
 * sequence, generateDeclarationCode for each local declaration list, and 
 * makeRule and generateRuleCode for each rule. Also generates code
 * for the main function of the runtime system upon finding the
 * main declaration. */
void generateDeclarationCode(List *declarations);

void generateProgramCode(GPStatement *statement, ContextType context, int indent);
void generateCommandSequence(List *commands, ContextType context, int indent);
void generateRuleCall(string rule_name, ContextType context, int indent);
void generateRuleSetCall(List *rules, ContextType context, int indent);
void generateProcedureCall(string proc_name, ContextType context, int indent);
void generateFailureCode(string rule_name, ContextType context, int indent);

#endif /* INC_GEN_PROGRAM_H */
