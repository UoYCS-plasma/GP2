/* ////////////////////////////////////////////////////////////////////////////

  ======================
  Static Analysis Module  
  ======================                      
 
  Functions for static analysis of a GP 2 program. The analysis is responsible
  for determining at which points to copy the host graph and which subprograms 
  should track changes made to the host graph.

//////////////////////////////////////////////////////////////////////////// */

#ifndef INC_ANALYSIS_H
#define INC_ANALYSIS_H

#include "ast.h"
#include "globals.h"
#include "pretty.h"

void staticAnalysis(List *declarations);

/* The static analysis for GP 2 programs annotates the AST with restore points 
 * and a roll back flag. A restore point marks points at which the host graph
 * is copied at runtime. A roll back flag set to true marks subprograms that
 * record modifications to the host graph. This analysis scrutinises bodies
 * of if/try conditionals and loop bodies, for conditional branching and loops
 * are the only GP 2 program constructs that may require an older host graph
 * state to be restored. I call these "critical subprograms".
 *
 * I follow a simple heuristic: if a loop occurs in critical subprograms, the host 
 * graph is copied before entering the critical subprogram. Otherwise, the host
 * graph is recorded. Recording all the changes made by a looped subprogram, and
 * undoing them all later, is likely to be much more expensive than copying the
 * graph to a stack and popping it later. In contrast, for a subprogram of a few
 * sequenced rule applications, it is more cost-effective to track and undo
 * the small amount of host graph modifications made by those rule applications
 * than to copy a potentially large host graph. There are corner  cases that breach
 * this heuristic, some of which are provided in the examples below.
 *
 * Examples
 * ========
 * Let G be the host graph before entering program P. r1-r4 are rules.
 *
 * (1) P = if (r1; r2)! then r3 else r4
 * Q = (r1; r2)! is the critical subprogram. G is copied before entering Q.
 * The if statement will apply r3 or r4 to G, not the graph after applying Q to G.
 *
 * (2) P = (r1; r2; r3)!
 * Q = (r1; r2; r3) is the critical subprogram. Changes to the host graph made by Q
 * are recorded. If r1 or r2 fails, GP 2's semantics states that the loop exits
 * with G as the current host graph.
 *
 * (3) P = try (r1!; r2!) then r3 else r4
 * Q = (r1!; r2!) is the critical subprogram. No graph backtracking is required.
 * This is because loops are guaranteed to succeed, and a try statement does not
 * roll back the host graph when branching to its 'then' branch. 
 *   
 * (4) P = try (r1!; r2; r3!) then r3 else r4
 * Q = (r1!; r2; r3!) is the critical subprogram. G is copied before entering Q.
 * This is because r2 could fail, in which case the sequence fails, and the 'else'
 * branch is taken, applying r4 to G.
 *
 * The above is an informal description of the function's operation. Refer
 * to the source file or GP 2 documentation for a description of the special
 * cases. */

#endif /* INC_ANALYSIS_H */
