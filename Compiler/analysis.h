/* ////////////////////////////////////////////////////////////////////////////

  ======================
  Static Analysis Module  
  ======================                      
 
  Functions that analyse the AST of a GP2 program to support code generation.

//////////////////////////////////////////////////////////////////////////// */

#ifndef INC_ANALYSIS_H
#define INC_ANALYSIS_H

#include "ast.h"
#include "globals.h"
#include "pretty.h"

void staticAnalysis(List *declarations, bool debug, string prefix);

/* Traverses the AST of the GP 2 program, recursively calling annotate
 * on subcommands. Calls getCommandType on if/try conditions and loop
 * bodies, and annotates the corresponding AST nodes according to their
 * copy type. */
void annotate(GPCommand *command, int restore_point);

/* Returns true if the passed GP 2 command always succeeds. Used to test
 * conditions and loop bodies: if these always succeed, then backtracking
 * is not necessary for try statements and loops. */
bool neverFails(GPCommand *command);

typedef enum {NO_BACKTRACK = 0, RECORD_CHANGES, COPY} copyType;
/* Assigns a copy type to commands that require graph backtracking, namely
 * a command in a loop body or a command in the conditional part of an 
 * if/try statement. In addition to the command in question, the function
 * takes three arguments to handle special cases:
 *
 * (1) The depth of the passed command in terms of nested command sequences.
 *     For example, the rule r2 has depth 0 in r2, depth 1 in (r1; r2), and 
 *     depth 2 in (r1; (r2; r3); r4). Depth 2 may seem superfluous but it is
 *     required for a special case.
 * (2) A flag set to true if the command is in the condition part of an if
 *     statement.
 * (3) A flag set to true if the command is the last command in a sequence.
 *   
 * NO_BACKTRACK is returned if the passed command is a rule call, a rule set
 * call, skip, fail, or an OR statement of any of the above.
 * COPY is returned if the passed command contains a loop except in a couple
 * of cases.
 * RECORD_CHANGES is returned in other cases.
 * Procedure calls, conditional branches and OR statements are evaluated by
 * recursive calls. For conditional branches, if any of its statements 
 * (condition, then branch, else branch) have type COPY, the whole command
 * has type COPY, otherwise it has type RECORD_CHANGES.
 *
 * The above is an informal description of the function's operation. Refer
 * to the source file or GP 2 documentation for a description of the special
 * cases. */
copyType getCommandType(GPCommand *command, int depth, bool if_body, 
                        bool last_command);

#endif /* INC_ANALYSIS_H */
