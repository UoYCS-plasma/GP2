/* ///////////////////////////////////////////////////////////////////////////

  =======================================
  staticsearch.h - Chris Bak (14/08/2014)
  =======================================
                             
  Module for a static searchplan algorithm for matching graphs in GP 2.

/////////////////////////////////////////////////////////////////////////// */

#ifndef STATIC_SEARCH_H
#define STATIC_SEARCH_H

#include "match.h"

/* staticSearchplan conducts a depth-first search for a graph morphism
 * from lhs to host. It returns the first morphism found or, in the case
 * where no morphism exists, NULL.
 *
 * The algorithm uses an array of characters to facilitate backtracking.
 * Each array entry is either the character 'n' (node) or the character 'e'
 * (edge), to record the item that was last matched. An index of the last
 * added item is kept, which is incremented and decremented when items are
 * matched and fail to match respectively. Essentially a crude stack.
 * If a node or edge fails to match against all candidate items in the host 
 * graph, a backtracking flag is set which will examine the last array entry
 * and act accordingly. If the index is reduced to 0, no morphism exists. 
 * If the array is full (it contains a number of characters equal to the sum 
 * of the number of the nodes and the number of edges in lhs), a complete 
 * morphism has been found.
 *
 * An association list of node matches and edge matches is maintained in
 * conjunction with the array. Each time a 'n' ('e') character is added, a node
 * (edge) mapping is added to its list. The lists contain the information to
 * resume searching for an item from a "checkpoint". When the stack is popped,
 * the first item from the appropriate list is queried.
 *
 * The algorithm matches an item (node or edge) one at a time, starting with
 * the root nodes. Matching operations are prioritised in order of determinism.
 *
 * Argument 1: A pointer to the left-hand side graph of a rule. A "copy" of
 *             this graph is to be found in the host graph.
 * Argument 2: A pointer to the host graph.
 * Argument 3: A pointer to the list of variables in the rule. Required to
 *             check if labels match.
 */

Morphism *staticSearchplan(Graph *lhs, Graph *host, VariableList *variables);


/* matchRootNode attempts to match a root node in the rule against each node 
 * in the root node list of the host graph until either a match is found or 
 * all the root nodes have been examined. It returns the index of the matched
 * host node or -1 if no match exists.
 *
 * Argument 1: A pointer to the root node from the rule graph to be matched.
 * Argument 2: A pointer to the host graph.
 * Argument 3: The index of the previous match of the first argument. 
 *             -1 if rule_root is being matched for the first time.
 *             Used while backtracking to find the starting point of search in 
 *             the host graph's root node list. 
 * Argument 4: The list of node matches. Used to check if a host node has
 *             already been matched.       
 * Argument 5: A pointer to the list of variables in the rule. Required for
 *             checking if labels match.       
 */

int matchRootNode(Node *rule_root, Graph *host, int index, 
                  GraphMapping *node_matches, VariableList *variables);


/* matchEdge attempts to match a rule edge from either its source or target
 * node, one of which has already been matched. It gets the image N of the 
 * source/target node in the node mapping and tests the rule edge against
 * edges in N's outgoing/incoming edge list until a match is found or
 * the edges have been exhausted. It returns the index of the matched host
 * edge of -1 if no match exists.
 *
 * Argument 1: A pointer to the edge from the rule graph to be matched.
 * Argument 2: A pointer to the host graph.
 * Argument 3: A flag to specify which direction the edge is being matched
 *             from. This is set in the main matching function when an 
 *             appropriate unmatched edge has been found.
 * Argument 4: The index of the previous match of the first argument. 
 *             -1 if rule_edge is being matched for the first time.
 *             Used while backtracking to find the starting point of search in 
 *             the host graph's edge list.         
 * Argument 5: The list of edge matches. Used to check if a host edge has
 *             already been matched.    
 * Argument 6: A pointer to the list of variables in the rule. Required for
 *             checking if labels match.     
 */

int matchEdge(Edge *rule_edge, Graph *host, bool match_from_source, int index, 
              GraphMapping *node_matches, GraphMapping *edge_matches,
              VariableList *variables);

/* Called directly after a call to matchEdge. matchEdge matches an edge from 
 * one of its endpoints; matchIncidentNode tries to match the other endpoint N
 * if it is not already matched. There are three behaviours this operation can
 * trigger, and hence three return values:
 * (1) N has been matched to the correct host graph node, namely the
 *     corresponding endpoint of the image of the rule edge. Return -2.
 *     No match made but backtracking is not required.
 * (2) N has already been matched to the incorrect host graph node. This
 *     is an invalid morphism. Return -1. No match made and backtracking is
 *     required.
 * (3) N has not been matched and no match exists. Return -1.
 * (4) N has not been matched and a match exists. Return the index of the host
 *     node that N is matched to.
 *
 * Argument 1: A pointer to the edge from the rule graph in question.
 * Argument 2: A pointer to the host edge to which the first argument is mapped.
 * Argument 3: The same value passed to matchEdge. If true, then this function
 *             tries to match rule_edge's target. Otherwise, it tries to match
 *             rule_edge's source.
 * Argument 4: The list of node matches. Used to check if a host node has
 *             already been matched. 
 * Argument 5: A pointer to the list of variables in the rule. Required for
 *             checking if labels match.           
 */

int matchIncidentNode(Edge *rule_edge, Edge *host_edge, bool match_target, 
                      GraphMapping *node_matches, VariableList *variables);


/* matchNode attempts to match a non-root rule node against each node in the 
 * node array of the host graph until either a match is found or all the nodes
 * have been examined. 
 *
 * Argument 1: A pointer to the node from the rule graph to be matched.
 * Argument 2: A pointer to the host graph.
 * Argument 3: The index of the previous match of the first argument. 
 *             -1 if rule_node is being matched for the first time.
 *             Used to find the starting point of search in the host graph's
 *             node array while backtracking.
 * Argument 4: The current list of node matches. Used to check if a host
 *             root node has already been matched.      
 * Argument 5: A pointer to the list of variables in the rule. Required for
 *             checking if labels match. 
 */

int matchNode(Node *rule_node, Graph *host, int index, 
              GraphMapping *node_matches, VariableList *variables);
              

#endif /* STATIC_SEARCH_H*/
