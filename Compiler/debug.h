/* ///////////////////////////////////////////////////////////////////////////

  ============
  Debug Module 
  ============

  Debugging functions and variables. Not included in production code.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_DEBUG_H
#define INC_DEBUG_H

#include "globals.h"
#include "graph.h"
#include "rule.h"

/* Checks if a host graph satisfies all the data invariants. The invariants are
 * described in the source file. Currently does not account for bidirectional
 * edges/bidegrees, but these can only occur in rule graphs. */
bool validGraph(Graph *graph);
void printVerboseGraph(Graph *graph, FILE *file);
void printVerboseNode(Node *node, FILE *file);
void printVerboseEdge(Edge *edge, FILE *file);

#endif /* INC_DEBUG_H */
