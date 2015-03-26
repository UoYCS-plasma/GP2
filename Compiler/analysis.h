/* ////////////////////////////////////////////////////////////////////////////

  ======================
  Static Analysis Module  
  ======================                      
 
  Functions to analyse and annotate the AST of a GP2 program to support
  code generation.

//////////////////////////////////////////////////////////////////////////// */

#ifndef INC_ANALYSIS_H
#define INC_ANALYSIS_H

#include "ast.h"
#include "globals.h"
#include "pretty.h"

void staticAnalysis(List *declarations, bool debug, string prefix);

typedef enum {NULL_COMMAND = 0, SINGLE_RULE_CALL, SIMPLE_COMMAND, NORMAL_COMMAND} commandType;

void annotate(GPStatement *statement, int restore_point);

commandType getCommandType(GPStatement *statement, bool copy);

commandType analyseSequence(List *commands, bool copy);

/* A simple statement is a statement that does not necessitate copying the 
 * graph when present in a conditional statement predicate, a loop or a 
 * procedure body. Simple statements are defined recursively:
 * - A rule call is simple.
 * - A rule set call is simple.
 * - skip and fail statements are simple.
 * - A procedure call is simple if the procedure's body is simple.
 * - A command sequence is simple if all commands but the last are predicate 
 *   rule calls (the last command can be anything).
 * - A conditional branch is simple if both its then and else statements are
 *   simple. 
 * - A loop statement is simple if its body is a null statement.
 * - An or statement is simple if both its choices are simple. */
bool simpleStatement(GPStatement *statement);

bool nullStatement(GPStatement *statement);

#endif /* INC_ANALYSIS_H */
