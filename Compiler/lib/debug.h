/* ///////////////////////////////////////////////////////////////////////////

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

  ============
  Debug Module 
  ============

  Debugging functions and variables.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_DEBUG_H
#define INC_DEBUG_H

#define print_trace(trace, ...)                       \
  do { fprintf(trace_file, trace, ##__VA_ARGS__); }   \
  while(0)

#include <inc/common.h>
#include "graph.h"

#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 

void openLogFile(string log_file_name);
void closeLogFile(void);

extern FILE *trace_file;
void openTraceFile(string trace_file_name);
void closeTraceFile(void);

/* Checks if a host graph satisfies all the data invariants. The invariants are
 * described in the source file. Currently does not account for bidirectional
 * edges/bidegrees, but these can only occur in rule graphs. */
bool validGraph(Graph *graph);
void printVerboseGraph(Graph *graph, FILE *file);
void printVerboseNode(Node *node, FILE *file);
void printVerboseEdge(Edge *edge, FILE *file);

#endif /* INC_DEBUG_H */
