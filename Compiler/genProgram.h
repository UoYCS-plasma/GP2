/* ///////////////////////////////////////////////////////////////////////////

  =======================
  Generate Program Module
  =======================    

  The code generating module. Generates the main function of the runtime 
  system from the AST of the control sequence of the GP 2 program. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_PROGRAM_H
#define INC_GEN_PROGRAM_H

/* main_header and main_source are the file handles for runtime.h
 * and main.c respectively. */
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

#include "ast.h"
#include "error.h"
#include "globals.h"

/* The contexts of a GP2 program determine the code that is generated. In
 * particular, the code generated when a rule match fails is determined by
 * its context. The context also has some impact on graph copying. */
typedef enum {MAIN_BODY, PROC_BODY, IF_BODY, TRY_BODY, LOOP_BODY} ContextType;

/* The main function for code generation. Creates the runtime module and 
 * populates header and source with their preliminaries. The main code
 * generation phase is initiated with a call to generateDeclarationCode on
 * the root of the GP2 program AST. */
void generateRuntimeCode(List *declarations);

/* Walks the declaration lists of the AST and acts according to the three
 * declaration types:
 *
 * Main
 * ====
 * Emit code to define the runtime global variables and to print the main
 * function and call generateProgramCode on the body of the Main GP2 procedure.
 * The generated code is placed in the body of the main function.
 *
 * Procedure 
 * =========
 * Emit the function proc_<proc_name> and call generateProgramCode on the
 * procedure body. The generated code is placed in the body of the procedure
 * function. If the procedure has a local declaration list, a recursive call
 * to generateDeclarationCall is made.
 *
 * Rule
 * ====
 * Create the rule data structure by calling makeRule (transform.h) on the
 * AST of the rule and call generateRuleCode on that data structure. This
 * will create a module to match and apply that rule.
 */
void generateDeclarationCode(List *declarations);
void generateMorphismCode(List *declarations, char type);
void generateProgramCode(GPStatement *statement, ContextType context, 
                         int restore_point, int roll_back, int indent);
void generateRuleCall(string rule_name, bool empty_lhs, bool predicate,
                      ContextType context, int restore_point, 
                      int roll_back, bool last_rule, int indent);
void generateRuleSetCall(List *rules, ContextType context, int restore_point, 
                         int roll_back, int indent);
void generateBranchStatement(GPStatement *statement, ContextType context,
                             int restore_point, int roll_back, int indent);

/* Generates code to handle failure, which is context-dependent. There are two
 * types of failure: 
 *
 * (1) A rule fails to match. The name of the rule is passed as the first 
 *     argument. 
 * (2) The fail statement is called. NULL is passed as the first argument.
 *
 * The rule_name argument is used in the MAIN_BODY context to report the nature
 * of the failure before execution terminates. */
void generateFailureCode(string rule_name, ContextType context, 
                         int restore_point, int roll_back, int indent);

#endif /* INC_GEN_PROGRAM_H */
