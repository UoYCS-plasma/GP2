/* ///////////////////////////////////////////////////////////////////////////

  ===================================
  generate.h - Chris Bak (27/10/2014)
  ===================================
                             
  Module for generating C code to execute GP2 programs.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GENERATE_H
#define INC_GENERATE_H

#define printToHeader(code, ...)	              \
  do { fprintf(match_header, code, ##__VA_ARGS__); }  \
  while(0) 

#define printToSource(code, ...)	              \
  do { fprintf(match_source, code, ##__VA_ARGS__); }  \
  while(0) 

#define printToSourceI(code, indent, ...)	              		 \
  do { fprintf(match_source, "%*s" code, indent, " ", ##__VA_ARGS__); }  \
  while(0) 

#include "globals.h"
#include "match.h"

extern FILE *match_header;
extern FILE *match_source;
extern struct Searchplan *searchplan;

/* Search operations are categorised by a character as follows:
 * 'n': Non-root node.
 * 'r': Root node.
 * 'i': Non-root node matched from its incoming edge.
 * 'o': Non-root node matched from its outgoing edge.
 * 'b': Non-root matched from an incident bidirectional edge.
 * 'e': Edge.
 * 's': Edge matched from its source.
 * 't': Edge matched from its target.
 *
 * The index of a search operation refers to the index of the node or edge
 * in the graph's corresponding pointer array. 
 */
typedef struct SearchOp {
   bool is_node;
   char type;
   int index;
   struct SearchOp *next;
} SearchOp;

/* Operations are appended to the searchplan, so a pointer to the last
 * searchplan operation is maintained for efficiency. */
typedef struct Searchplan {
   SearchOp *first;
   SearchOp *last;
} Searchplan;

Searchplan *initialiseSearchplan(void);
/* Appends the search operation (type, index) to the plan. The is_node flag
 * is inferred from the type. */
void addSearchOp(Searchplan *plan, char type, int index);
void printSearchplan(Searchplan *searchplan);
void freeSearchplan(Searchplan *searchplan);


/* If k < |E_L| edges are to be deleted, I probably want an array whose
 * first k entries contain the indices of those edges, and the rest to
 * be -1. Then I do not have to iterate |E_L| times over the array
 * (stop when -1 is found). Ditto for the other arrays. */

typedef struct RuleData {
   int *deleted_edges; /* indexed from 0 to |E_L| */
   int *deleted_nodes; /* indexed from 0 to |V_L| */
   int *added_nodes; /* indexed from 0 to |V_R| */
   int *added_edges; /* indexed from 0 to |E_R| */
   int *preserved_nodes; /* indexed from 0 to |V_R| */  
   int *preserved_edges; /* indexed from 0 to |E_R| */
} RuleData;
   

/* generateSearchplan traverses a graph in order to create a searchplan
 * using the following algorithm:
 * (1) For each root node, append the 'r' operation to the searchplan.
 * (2) From each root node, walk the graph depth-first. 
 *     Tag each item, including the initial root node, when it is encountered.
 *     If the next item in the traversal is untagged, tag it and append the
 *     appropriate operation to the searchplan. For instance, if we examine an
 *     outgoing edge of a root node, add the 's' operation to the searchplan.
 *     Once this step is complete, all connected components containing root
 *     nodes have been examined.
 * (3) Scan the node list of the graph, performing step 2 on any untagged nodes.
 *     Unnecessary if the input graph is root-connected.
 *
 * The depth-first search is performed by recursive calls to traverseNode and
 * traverseEdge. These two functions are responsible for checking if items
 * are tagged, tagging items, and adding new operations to the searchplan.
 */     
void generateSearchplan(Graph *lhs);
void traverseNode(Node *node, char match_from, bool *discovered_item,
	          int offset);
void traverseEdge(Edge *edge, char match_from, bool *discovered_item,
		  int offset);

/* Called with rule_name R, generateMatchingCode creates a C module named
 * match_R. It creates the header file, calls generateSearchplan, and emits
 * the matching code (according to the searchplan) to the source file.
 * Four auxiliary functions emit a C function to execute a particular
 * kind of searchplan operation.
 *
 * This should never be called with an empty LHS graph! If the LHS is empty, 
 * the caller skips this function and just calls the rule application code
 * generator. */
void generateMatchingCode(Graph *lhs, bool *dangling_nodes, string rule_name);
void emitMainFunction(string rule_name, SearchOp *first_op);

/* The four emitMatcher functions take an LHS item and emit code that searches
 * for a matching host item. The generated code queries the host graph for the
 * appropriate item or list of items from the host graph according to the LHS 
 * item and the searchplan operation from which the code is generated.
 * If a host item matching the LHS item is found, the generated code pushes its
 * index to the appropriate morphism stack and calls the next function in the
 * searchplan. The correct call is generated from the subsequent searchplan
 * operation next_op and the emitNextMatcherCall function.
 */
void emitNodeMatcher(Node *left_node, bool is_root, bool *dangling_nodes,
                     SearchOp *next_op);
void emitNodeFromEdgeMatcher(Node *left_node, char type, bool *dangling_nodes,
                             SearchOp *next_op);
void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op);
void emitEdgeFromNodeMatcher(Edge *left_edge, char type, SearchOp *next_op);

//void emitRuleApplicationCode(string rule_name, Graph *lhs, Graph *rhs,
//                             RuleData *rule_data);

/* emitNextMatcherCall writes a call to a matching function according to the
 * passed searchplan operation. If next_op is NULL, then 'return true;' is
 * written to the generated source file. */
bool emitNextMatcherCall(SearchOp* next_op, int indent);

#endif /* INC_GENERATE_H */

