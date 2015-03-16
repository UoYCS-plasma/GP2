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
void printVerboseRule(Rule *rule);
void printVerboseGraph(Graph *graph);
void printVerboseNode(Node *node);
void printVerboseEdge(Edge *edge);


#endif /* INC_DEBUG_H */
