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

bool validGraph(Graph *graph);
void printVerboseRule(Rule *rule);
void printVerboseGraph(Graph *graph);
void printVerboseNode(Node *node);
void printVerboseEdge(Edge *edge);


#endif /* INC_DEBUG_H */
