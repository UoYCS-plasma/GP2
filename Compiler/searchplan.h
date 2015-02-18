/* ///////////////////////////////////////////////////////////////////////////

  =================
  Searchplan Module
  =================  

  Defines a data structure for searchplans and functions operating on this
  data structure. Also defines a function to construct a static searchplan
  from a graph.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_SEARCHPLAN_H
#define INC_SEARCHPLAN_H

#include "graph.h"
#include "globals.h"

/* Search operations are categorised by a character as follows:
 * 'n': Non-root node.
 * 'r': Root node.
 * 'i': Non-root node matched from its incoming edge.
 * 'o': Non-root node matched from its outgoing edge.
 * 'b': Non-root matched from an incident bidirectional edge.
 * 'e': Edge.
 * 's': Edge matched from its source.
 * 't': Edge matched from its target.
 * 'l': Looping edge matched from its node.
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
Searchplan *generateSearchplan(Graph *lhs);
void traverseNode(Searchplan *searchplan, Graph *lhs, Node *node, char type, 
                  bool *discovered_item, int offset);
void traverseEdge(Searchplan *searchplan, Graph *lhs, Edge *edge, char type, 
                  bool *discovered_item, int offset);

#endif /* INC_SEARCHPLAN_H */
