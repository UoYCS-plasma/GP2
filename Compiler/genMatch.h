/* ///////////////////////////////////////////////////////////////////////////

  =====================
  Generate Match Module
  =====================

  Module for generating code to apply a rule. The functions in this module
  take a rule and produce a C module with two principal functions: one to match
  the rule (creating a morphism) and one that takes a morphism and applies the
  rule. 

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

/* generateRuleCode creates a C module to match and apply the rule.
 * It sets up the module environment and makes the appropriate calls to
 * emitMatchingCode and emitApplicationCode depending on the structure
 * of the rule.  */
 void generateRuleCode(Rule *rule);
 
/* Generates the searchplan from the LHS which is used to emit the matching
 * code. The generated matching code is structure as follows:
 * For a searchplan op_1,...,op_n and corresponding generated C functions
 * f_1,...,f_n, the main matching function match_R calls f_1. If f_i fails to
 * find a match, it returns false, causing match_R to return NULL. If f_i
 * finds a match, it updates the appropriate data structures (including the
 * morphism) and calls f_i+1.
 * The subsequent matching functions operate in the same way, apart from the
 * last function f_n. If f_n finds a match it returns true, signalling match_R 
 * to return the morphism.
 *
 * Argument 1: The name of the rule, passed to emitRuleMatcher so that it 
 *             writes the correct function names.
 * Argument 2: The LHS graph of the rule. Used to generate the searchplan.
 * Argument 3: The list of nodes that the rule deletes. The dangling condition
 *             is checked for candidate host graph nodes that match a deleted
 *             node.  */
void generateMatchingCode(string rule_name, int number_of_variables, 
                          Graph *lhs, ItemList *deleted_nodes);


/* Emits the declaration and definition of the main matching function 
 * match_<rule_name>. The body of the function contains a check that returns 
 * false if the LHS has more nodes or more edges than the host graph, and the 
 * call to the first matching function.
 *
 * Argument 1: The name of the rule, used to write the function name.
 * Argument 2: The first operation in the searchplan, used to write the call
 *             to the first matching function.
 * Argument 3: The number of nodes in the LHS.
 * Argument 4: The number of edges in the LHS. */
void emitRuleMatcher(string rule_name, SearchOp *first_op, int left_nodes, 
                     int left_edges, int variables);

/* The four emitMatcher functions take an LHS item and emit a function that 
 * searches for a matching host item. The generated code queries the host graph
 * for the appropriate item or list of items according to the LHS item and the
 * searchplan operation from which the code is generated.
 *
 * Several checks are made by each function to check if a host item matches
 * the LHS-item in the order presented below.
 * The host item must:
 * (1) Not have been matched already (GP2 required injective matching).
 *     A boolean array, indexed by host index, is maintained at runtime to 
 *     facilitate this check. The arrays are named matched_nodes and 
 *     matched_edges.
 * (2) Have the same label class as the LHS-item.
 * (3) Have the same mark as the the LHS-item.
 * (4) [Nodes only] Have degree compatibility with the LHS-node. The host node's
 *     in- and outdegrees must be greater or equal to that of the LHS-node. If
 *     the LHS-node is deleted node, the degrees must be equal.
 * (5) Have label compatibility with the LHS-item.
 *
 * If a valid host item is found, the generated code pushes its index to the
 * appropriate morphism stack and calls the function for the following 
 * searchplan operation (see emitNextMatcherCall). If there are no operations 
 * left, NULL is passed to emitNextMatcherCall, and code is generated to return
 * true. */

/* emitNodeMatcher emits a function corresponding to the searchplan operations 
 * 'n' and 'r'. The generated function gets the list of candidate host nodes. 
 * For 'n', this is the list of nodes whose label class matches that of 
 * left_node. For 'r', this is the list of root nodes in the host graph. Each
 * candidate host node is tested until one matches or the end of the list is 
 * reached. 
 *
 * Argument 1: The LHS node that is matched by the generated function.
 * Argument 2: A flag to control which list of host graph nodes is obtained
 *             by the generated code. 
 * Argument 3: The list of deleted nodes. The presence of left_node in this 
 *             list determines the code emitted to check the host node's 
 *             indegree and outdegree. 
 * Argument 4: The next operation in the searchplan. NULL if this is the
 *             last operation. */
void emitNodeMatcher(Node *left_node, bool is_root, ItemList *deleted_nodes,
                     SearchOp *next_op);

/* emitNodeFromEdgeMatcher emits a function corresponding to the searchplan 
 * operations 'i', 'o' and 'b', those that match a node from an incident edge. 
 *
 * Argument 1: The LHS node that is matched by the generated function.
 * Argument 2: The type of the operation ('i', 'o' or 'b').
 * Argument 3: The list of deleted nodes. The presence of left_node in this 
 *             list determines the code emitted to check the host node's 
 *             indegree and outdegree. 
 * Argument 4: The next operation in the searchplan. NULL if this is the
 *             last operation. */
void emitNodeFromEdgeMatcher(Node *left_node, char type, 
                             ItemList *deleted_nodes, SearchOp *next_op);

/* emitEdgeMatcher emits a function corresponding to the searchplan operation 
 * 'e'. The generated function gets the list of candidate host edges: the list
 * of edges whose label class matches that of left_edge. Each candidate host 
 * edge is tested until one matches or the end of the list is reached. 
 *
 * Argument 1: The LHS edge that is matched by the generated function.
 * Argument 2: The next operation in the searchplan. NULL if this is the
 *             last operation. */
void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op);

/* The following two functions emit a function corresponding to the searchplan
 * operations 's', 't' and 'l', those that match an edge from an incident
 * matched node. The code generated by either function is sufficient to handle
 * looping edges except for a single line, so I pass the loop property as a 
 * parameter to emitEdgeFromNodeMatcher which at one point examines is_bool
 * to generate the appropriate line of code. */
void emitEdgeFromNodeMatcher(Edge *left_edge, bool is_loop, SearchOp *next_op);
void emitEdgeToNodeMatcher(Edge *left_edge, SearchOp *next_op);

/* emitNextMatcherCall writes a call to a matching function according to the
 * passed searchplan operation. If next_op is NULL, then 'return true;' is
 * emitted. */
bool emitNextMatcherCall(SearchOp* next_operation, int indent);

/* Emits the function apply_<rule_name> that makes the necessary changes to
 * the host graph according to the rule and morphism. apply_<rule_name>
 * takes a morphism and the host graph as arguments. In the special cases
 * that either the LHS or RHS is the empty graph, less code needs to be 
 * generated to apply the rule. */
void generateApplicationCode(Rule *rule, bool empty_lhs, bool empty_rhs);

#endif /* INC_GEN_MATCH_H */
