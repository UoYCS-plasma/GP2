/* ////////////////////////////////////////////////////////////////////////////

  Copyright 2015-2016 Christopher Bak

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

void staticAnalysis(List *declarations);

/* The static analysis for GP 2 programs annotates the AST nodes of conditional
 * branches and loops by setting some flags according to an analysis of a particular
 * subprogram. Two flags are modified by this analysis.
 * (1) The 'record' flag is set to true if an if/try condition or a loop body 
 *     (a 'critical subprogram') needs to record modifications to the host graph
 *     during its execution in order to restore the graph to a previous state.
 * (2) The 'stop recording' flag is set to true if recording of the host graph is 
 *     no longer needed after a certain point in a command sequence. This flag only
 *     exists on loops.
 *
 * Examples
 * ========
 * Let G be the host graph before entering program P. r1-r4 are rules.
 *
 * (1) P = (r1; r2; r3)!
 * Q = r1; r2; r3 is the critical subprogram. If r1 or r2 fails, GP 2's semantics
 * states that the loop exits with G as the current host graph. Therefore the loop's
 * 'record' flag is set to true.
 *
 * (2) P = try (r1!; r2!) then r3 else r4
 * Q = r1!; r2! is the critical subprogram. If the 'then' branch of a try statement
 * is taken, the working host graph is used. Because a loop can never fail, the 
 * whole try condition can never fail: no graph recording is necessary.

 * (3) P = if (r1!; r2!) then r3 else r4
 * The critical subprogram is the same as (2). The difference is that we have an if
 * statement instead of a try statement. G is required even if the 'then' branch is
 * taken, so the if statement's 'record' flag is set to true.
 *   
 * (4) P = try (r1; r2; r3!; r4!) then r3 else r4
 * Q = r1; r2; r3!; r4! is the critical subprogram which can fail at r1 or r2.
 * The try statement's 'record' flag is set to true. Also, the 'stop recording'
 * flag of the loop r3! is set to true. Once this point is reached, the try 
 * condition is guaranteed to succeed, so continuing to record changes would
 * be wasted effort.
 *
 * (5) P = (r1; r2!; r3!)
 * Q = r1; r2!; r3! is the critical subprogram. Although r1 can fail, no graph
 * recording is required. If r1 succeeds, then the whole loop body will succeed
 * because only loops follow. If r1 fails, then the host graph G has not been
 * modified by the loop body, so the loop can be exited without needing to undo
 * any recorded changes. */

#endif /* INC_ANALYSIS_H */
