/* ///////////////////////////////////////////////////////////////////////////

  =========================
  AST Transformation Module
  =========================
                             
  Module for transforming the AST into intermediate data structures.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_TRANSFORM_H
#define INC_TRANSFORM_H 

#include "ast.h"
#include "error.h"
#include "globals.h"
#include "graph.h"
#include "label.h"
#include "rule.h"

/* Allocates memory for the rule data structure and populates it using the
 * AST representation of the rule and the functions in this module. */
Rule *makeRule(GPRule *rule);

/* Populates the rule's variable list from the variable declaration lists in
 * the AST. */
VariableList *scanVariableList(VariableList *variable_list, List *declarations);

/* Counts the number of nodes/edges in a graph from its AST representation. */
int countNodes(GPGraph *graph);
int countEdges(GPGraph *graph);

/* Generates an appropriate initial node/edge array size for a graph. 
 * Returns the maximum of minimum_size and the smallest power of 2 greater 
 * than the number_of_items in the passed graph. number_of_items is obtained
 * from a call to countNodes or countEdges. */
int getArraySize(int number_of_items, int minimum_size);

/* (1) Create and returns the graph data structure for the LHS graph.
 * (2) Add all LHS-nodes and LHS-edges to node and edge maps. Each map is an
 *     association list storing the item's identifier (from the GP program),
 *     its index in the LHS graph, and the source and target identifiers for
 *     edges.
 * (3) Populate the list of deleted node indices. A deleted node is a node
 *     occurring in the LHS but not in the interface list.
 * (4) Set the is_rooted flag if there exists a root node in the LHS.
 * (5) Set the deletes_nodes flag if the rule deletes a node.
 */
void scanLHS(Rule *rule, GPGraph *ast_lhs, List *interface);

/* (1) Create and returns the graph data structure for the graph containing
 *     the nodes of the RHS graph.
 * (2) Update the node map with the RHS-node indices. New node maps are 
 *     introduced for nodes which do not exist in the LHS.
 * (3) Populate the list of added node indices. An added node is a node
 *     occurring in the RHS but not in the interface list.
 * (4) Populate the list of preserved node index pairs. If a right node
 *     is in the interface, its RHS-index and left-index (obtained from the 
 *     node map) is added to the list. 
 */
void scanRHSNodes(Rule *rule, GPGraph *ast_rhs, List *interface);

/* (1) Add the RHS-edges to the RHS graph.
 * (2) Populate the list of preserved edge index pairs. Preserved edges
 *     are located by first checking if the source and target nodes are in
 *     the interface. If so, the edge map is searched for a LHS-edge with the
 *     appropriate source and target. If one exists, the edge is added to
 *     the preserved edges list. Otherwise, see (3).
 * (3) Populate the list of added edge structures. If the source or the
 *     target node is not in the interface, or if they are and no LHS-edge
 *     exists with the appropriate source and target, the edge is added
 *     to the added edges list with the appropriate information.
 */
void scanRHSEdges(Rule *rule, GPGraph *ast_rhs, List *interface);

/* (1) Update the rule's variable list with the variables that appear in a
 *     RHS label.
 * (2) Update the rule's preserved nodes list with the node that appear in a
 *     degree operator in a RHS label. 
 * The relabelled argument is false if the RHS item containing the atom is
 * preserved and unchanged by the rule. In this case, any variables in this
 * atom are not flagged for runtime code generation, preventing unused 
 * variables in the runtime code. */
void scanRHSAtom(Rule *rule, bool relabelled, Atom atom);

/* Generates a Label from the AST representation of a label. The data
 * structures for atoms are extremely similar, admitting a straightforward
 * translation. One key difference is that the target structure stores
 * an integer (RHS node index) for the indegree and outdegree operations
 * in contrast to the string in the AST. The node map is passed to the two
 * transformation functions to get the appropriate index from the string
 * node identifier. */
Label transformLabel(GPLabel *ast_label, IndexMap *node_map);
int getASTListLength(GPLabel *ast_label);
Atom transformAtom(GPAtom *ast_atom, IndexMap *node_map);

#endif /* INC_TRANSFORM_H */
