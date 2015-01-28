/* ///////////////////////////////////////////////////////////////////////////

  =====================
  Generate Match Module
  =====================

  Module for generating code to apply a rule. Given a rule, the functions
  in this module generate a C module to match and apply the given rule.
  A searchplan is used to support the generation of C code to match
  a given rule graph. A function is generated for each searchplan operation,
  each function calling the next searchplan function on success.
  A separate function is generated to apply the rule. The function takes
  the host graph and a morphism as input and returns the updated host graph.  

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_MATCH_H
#define INC_GEN_MATCH_H

#define printToRuleHeader(code, ...)	             \
  do { fprintf(rule_header, code, ##__VA_ARGS__); }  \
  while(0) 

#define PTRH printToRuleHeader

#define printToRuleSource(code, ...)	             \
  do { fprintf(rule_source, code, ##__VA_ARGS__); }  \
  while(0) 

#define PTRS printToRuleSource

#define printToRuleSourceI(code, indent, ...)	         		\
  do { fprintf(rule_source, "%*s" code, indent, " ", ##__VA_ARGS__); }  \
  while(0) 

#define PTRSI printToRuleSourceI

#include "error.h"
#include "globals.h"
#include "rule.h"
#include "searchplan.h"

FILE *rule_header;
FILE *rule_source;
struct Searchplan *searchplan;

void generateRuleCode(Rule *rule);

/* Called with rule_name R, emitMatchingCode creates a C module named
 * match_R. It creates the header file, calls generateSearchplan on the 
 * rule's left-hand side graph, and emits the matching code (according to
 * the searchplan) to the source file. Four auxiliary functions emit a C 
 * function to execute a particular kind of searchplan operation. 
 *
 * This should never be called with an empty LHS graph! If the LHS is empty, 
 * the caller skips this function and just calls the rule application code
 * generator. */
void emitMatchingCode(string rule_name, Graph *lhs, ItemList *deleted_nodes,
                      bool is_predicate);
void emitApplicationCode(Rule *rule, bool empty_lhs, bool empty_rhs);

void emitRuleMatcher(string rule_name, SearchOp *first_op, bool is_predicate);

/* The four emitMatcher functions take an LHS item and emit code that searches
 * for a matching host item. The generated code queries the host graph for the
 * appropriate item or list of items from the host graph according to the LHS 
 * item and the searchplan operation from which the code is generated.
 * If a host item matching the LHS item is found, the generated code pushes its
 * index to the appropriate morphism stack and calls the next function in the
 * searchplan. The correct call is generated from the subsequent searchplan
 * operation next_op and the emitNextMatcherCall function.
 */
void emitNodeMatcher(Node *left_node, bool is_root, ItemList *deleted_nodes,
                     SearchOp *next_op);
void emitNodeFromEdgeMatcher(Node *left_node, char type, 
                             ItemList *deleted_nodes, SearchOp *next_op);
void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op);
void emitEdgeFromNodeMatcher(Edge *left_edge, char type, SearchOp *next_op);

/* emitNextMatcherCall writes a call to a matching function according to the
 * passed searchplan operation. If next_op is NULL, then 'return true;' is
 * written to the generated source file. */
bool emitNextMatcherCall(SearchOp* next_op, int indent);

#endif /* INC_GEN_MATCH_H */

