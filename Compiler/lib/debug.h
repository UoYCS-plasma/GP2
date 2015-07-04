/* ///////////////////////////////////////////////////////////////////////////

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

#include "globals.h"
#include "graph.h"

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
