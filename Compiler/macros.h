#ifndef INC_GEN_MACROS_H
#define INC_GEN_MACROS_H

#define MAKE_MATCHED_NODES_ARRAY                \
   int count;                                   \
   int matched_nodes[left_nodes];               \
   for(count = 0; count < left_nodes; count++)  \
       matched_nodes[count] = -1;                

#define MAKE_MATCHED_EDGES_ARRAY                \
   int matched_edges[left_edges];               \
   for(count = 0; count < left_edges; count++)  \
       matched_edges[count] = -1;                

#define CHECK_MATCHED_NODE                         \
   for(index = 0; index < left_nodes; index++)     \
   {                                               \
      if(matched_nodes[index] == host_node->index) \
         node_matched = true;                      \
   }                                               \
      
#define CHECK_MATCHED_EDGE                         \
   for(index = 0; index < left_edges; index++)     \
   {                                               \
      if(matched_edges[index] == host_edge->index) \
         edge_matched = true;                      \
   }                                               \

/* The host node does not match the rule node if:
 * (1) The marks differ and the rule node's mark is not ANY
 *     (ANY is equivalent to 6 in the enumerated type MarkType).
 * (2) The host node's indegree is strictly less than the rule node's indegree.
 * (3) The host node's outdegree is strictly less than the rule node's outdegree.
 * (4) The number of edges incident to the host node is strictly less than the
 *     number of edges incident to the rule node. */
#define IF_INVALID_NODE(nmark, indeg, outdeg, bideg)           \
   if(node_matched ||                                          \
      (host_node->label.mark != (nmark) && (nmark) != 6) ||    \
      host_node->indegree < (indeg) ||                         \
      host_node->outdegree < (outdeg) ||                       \
      ((host_node->outdegree + host_node->indegree             \
        - (indeg) - (outdeg) - (bideg)) < 0))                  \

/* As above, but with the fourth condition made stricter:
 * (4) The number of edges incident to the host node is not equal to the 
 *     number of edges incident to the rule node. Indeed, if it is less,
 *     then standard matching is violated (above). If it is greater,
 *     then the dangling condition is violated. */
#define IF_INVALID_DANGLING_NODE(nmark, indeg, outdeg, bideg)  \
   if(node_matched ||                                          \
      (host_node->label.mark != (nmark) && (nmark) != 6) ||    \
      host_node->indegree < (indeg) ||                         \
      host_node->outdegree < (outdeg) ||                       \
      ((host_node->outdegree + host_node->indegree             \
        - (indeg) - (outdeg) - (bideg)) != 0))                 \

/* Deletes all the host items in the morphism from the host graph. Edges are
 * deleted first so that there is no chance of dangling edges from node
 * deletion. Called when the RHS of a rule is the empty graph. */
#define REMOVE_RHS                                                         \
   do {                                                                    \
   for(count = 0; count < morphism->edges; count++)                        \
      if(record_changes)                                                   \
      {                                                                    \
         Edge *edge = getEdge(host, morphism->edge_map[count].host_index); \
         pushRemovedEdge(false, edge->label, edge->source, edge->target);  \
      }                                                                    \
      removeEdge(host, morphism->edge_map[count].host_index);              \
                                                                           \
   for(count = 0; count < morphism->nodes; count++)                        \
      if(record_changes)                                                   \
      {                                                                    \
         Node *node = getNode(host, morphism->node_map[count].host_index); \
         pushRemovedNode(node->root, node->label);                         \
      }                                                                    \
      removeNode(host, morphism->node_map[count].host_index);              \
   } while(0);                                                             \

#define PROCESS_EDGE_MORPHISMS                                               \
   do {                                                                      \
   for(count = 0; count < morphism->edges; count++)                          \
   {                                                                         \
      left_index = morphism->edge_map[count].left_index;                     \
      host_index = morphism->edge_map[count].host_index;                     \
      if(edge_map[left_index].remove_item == true)                           \
      {                                                                      \
         if(record_changes)                                                  \
         {                                                                   \
            Edge *edge = getEdge(host, host_index);                          \
            pushRemovedEdge(false, edge->label, edge->source, edge->target); \
         }                                                                   \
         removeEdge(host, host_index);                                       \
         continue;                                                           \
      }                                                                      \
      Label *new_label = edge_map[left_index].new_label;                     \
      if(new_label != NULL)                                                  \
      {                                                                      \
         if(record_changes)                                                  \
         {                                                                   \
            Edge *edge = getEdge(host, host_index);                          \
            pushRelabelledEdge(host_index, false, edge->label);              \
         }                                                                   \
         relabelEdge(host, host_index, new_label, false);                    \
      }                                                                      \
   }                                                                         \
   } while(0);                                               


#define PROCESS_NODE_MORPHISMS                                             \
   do {                                                                    \
   for(count = 0; count < morphism->nodes; count++)                        \
   {                                                                       \
      left_index = morphism->node_map[count].left_index;                   \
      host_index = morphism->node_map[count].host_index;                   \
      if(node_map[left_index].remove_item == true)                         \
      {                                                                    \
         if(record_changes)                                                \
         {                                                                 \
            Node *node = getNode(host, host_index);                        \
            pushRemovedNode(false, node->label);                           \
         }                                                                 \
         removeNode(host, host_index);                                     \
         continue;                                                         \
      }                                                                    \
      Node *host_node = getNode(host, host_index);                         \
      Label *new_label = node_map[left_index].new_label;                   \
      bool change_root = host_node->root != node_map[left_index].rhs_root; \
      if(new_label != NULL || change_root)                                 \
      {                                                                    \
         if(record_changes)                                                \
         {                                                                 \
            Node *node = getNode(host, host_index);                        \
            pushRelabelledNode(host_index, change_root, node->label);      \
         }                                                                 \
         relabelNode(host, host_index, new_label, change_root);            \
         node_map[left_index].host_index = host_index;                     \
         continue;                                                         \
      }                                                                    \
      else node_map[left_index].host_index = host_index;                   \
   }                                                                       \
   } while(0);                                               

#endif /* INC_GEN_MACROS */
