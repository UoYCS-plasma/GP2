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
#include "rule.h"

/* Creates the rule data structure from a pointer to a rule in the AST. */
Rule *makeRule(GPRule *rule);

/* Returns the maximum of minimum_size and the smallest power of 2 greater 
 * than the number of nodes or edges in the passed graph. */
int getNodeSize(GPGraph *graph, int minimum_size);
int getEdgeSize(GPGraph *graph, int minimum_size);

/* scanLHS does the following:
 * (1) Creates and returns the graph data structure for the LHS graph.
 * (2) Adds all LHS-nodes and LHS-edges to node and edge maps. Each map is an
 *     association list storing the item's identifier (from the GP program),
 *     its index in the LHS graph, and the source and target identifiers for
 *     edges.
 * (3) Populates the list of deleted node indices. A deleted node is a node
 *     occurring in the LHS but not in the interface list.
 * (4) Sets the is_rooted flag if there exists a root node in the LHS.
 * (5) Sets the deletes_nodes flag if the rule deletes a node.
 */
Graph *scanLHS(GPGraph *ast_lhs, List *interface, IndexMap **node_map, 
               IndexMap **edge_map, ItemList **deleted_nodes,
               unsigned int *is_rooted);

/* scanRHSNodes does the following:
 * (1) Creates and returns the graph data structure for the graph containing
 *     the nodes of the RHS graph.
 * (2) Updates the node map with the RHS-node indices. New node maps are 
 *     introduced for nodes which do not exist in the LHS.
 * (3) Populates the list of added node indices. An added node is a node
 *     occurring in the RHS but not in the interface list.
 * (4) Populates the list of preserved node index pairs. If a right node
 *     is in the interface, its RHS-index and left-index (obtained from the 
 *     node map) is added to the list. 
 */
Graph *scanRHSNodes(GPGraph *ast_rhs, List *interface, IndexMap **node_map,
                    PreservedItemList **nodes, ItemList **added_nodes);

/* scanRHSEdges does the following:
 * (1) Adds the RHS-edges to the RHS graph.
 * (2) Populates the list of preserved edge index pairs. Preserved edges
 *     are located by first checking if the source and target nodes are in
 *     the interface. If so, the edge map is searched for a LHS-edge with the
 *     appropriate source and target. If one exists, the edge is added to
 *     the preserved edges list. Otherwise, see (3).
 * (3) Populates the list of added edge structures. If the source or the
 *     target node is not in the interface, or if they are and no LHS-edge
 *     exists with the appropriate source and target, the edge is added
 *     to the added edges list with the appropriate information.
 */
NewEdgeList *scanRHSEdges(GPGraph *ast_rhs, Graph *rhs, List *interface, 
                          IndexMap *node_map, IndexMap **edge_map,
                          PreservedItemList **edges);

Label *transformLabel(GPLabel *ast_label);

#endif /* INC_TRANSFORM_H */
