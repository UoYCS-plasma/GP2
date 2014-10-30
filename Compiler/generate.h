/* ///////////////////////////////////////////////////////////////////////////

  ===================================
  generate.h - Chris Bak (27/10/2014)
  ===================================
                             
  Module for generating C code to execute GP2 programs.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GENERATE_H
#define INC_GENERATE_H

#include "globals.h"
#include "match.h"

extern FILE *match_source;

void generateMatchingCode(Graph *lhs, string rule_name);
void emitNodeMatchingCode(Node *left_node, bool backtracking, int indent);

#endif /* INC_GENERATE_H */

