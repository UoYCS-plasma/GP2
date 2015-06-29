/* ///////////////////////////////////////////////////////////////////////////

  =========================
  AST Transformation Module
  =========================
                             
  Module for transforming the AST into intermediate data structures.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_TRANSFORM_H
#define INC_TRANSFORM_H 

#include "ast.h"
#include "globals.h"
#include "rule.h"

Rule *transformRule(GPRule *ast_rule);

/* Populates the rule's LHS graph from its AST equivalent, including the
 * transformation of AST-labels to the data structure defined in label.h. */
void scanLHS(Rule *rule, GPGraph *ast_lhs);

/* Populates the rule's RHS graph from its AST equivalent, including the
 * transformation of AST-labels to the data structure defined in label.h.
 * Also responsible for pointing rule graph nodes and edges to their 
 * corresponding interface items. Finally, it searches for variables and 
 * degree operators in RHS-labels in order to annotate variables and nodes 
 * with information about their usage by the rule. These annotations are 
 * used to support code generation. */
void scanRHS(Rule *rule, GPGraph *ast_rhs, List *interface);

#endif /* INC_TRANSFORM_H */
