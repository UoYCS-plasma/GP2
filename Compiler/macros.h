#ifndef INC_GEN_MACROS_H
#define INC_GEN_MACROS_H

#define GET_GRAPH_AT_RESTORE_POINT                                         \
   do {                                                                    \
   if(restore_index > 0)                                                   \
   {                                                                       \
      int restore_depth = stack_depth - restore_points[restore_index - 1]; \
      host = restoreGraph(host, restore_depth);                            \
      stack_depth -= restore_depth;                                        \
   }                                                                       \
   } while(0);
     
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
   int index;                                      \
   for(index = 0; index < left_nodes; index++)     \
   {                                               \
      if(matched_nodes[index] == host_node->index) \
         node_matched = true;                      \
   }                                               \
      
#define CHECK_MATCHED_EDGE                         \
   int index;                                      \
   for(index = 0; index < left_edges; index++)     \
   {                                               \
      if(matched_edges[index] == host_edge->index) \
         edge_matched = true;                      \
   }                                               \

#define IF_INVALID_NODE(lclass, nmark, indeg, outdeg, bideg)  \
   if(node_matched ||                                         \
      host_node->label_class != (lclass) ||                   \
      host_node->label->mark != (nmark) ||                    \
      host_node->indegree < (indeg) ||                        \
      host_node->outdegree < (outdeg) ||                      \
      ((host_node->outdegree + host_node->indegree            \
        - (indeg) - (outdeg)) < (bideg)))                     \

#define IF_INVALID_DANGLING_NODE(lclass, nmark, indeg, outdeg, bideg)  \
   if(node_matched ||                                                  \
      host_node->label_class != (lclass) ||                            \
      host_node->label->mark != (nmark) ||                             \
      host_node->indegree < (indeg) ||                                 \
      host_node->outdegree < (outdeg) ||                               \
      host_node->outdegree - (outdeg) != (bideg) ||                    \
      host_node->indegree - (indeg) != (bideg))                        \


#define IF_INVALID_EDGE(lclass, emark)       \
   if(edge_matched ||                        \
      host_edge->label_class != (lclass) ||  \
      host_edge->label->mark != (emark))

#define IF_INVALID_LOOP_EDGE(lclass, emark)      \
   if(edge_matched ||                            \
      host_edge->source != host_edge->target ||  \
      host_edge->label_class != (lclass) ||      \
      host_edge->label->mark != (emark))

/* Deletes all the host items in the morphism from the host graph. Edges are
 * deleted first so that there is no chance of dangling edges from node
 * deletion. Called when the RHS of a rule is the empty graph. */
#define REMOVE_RHS                                              \
   do {                                                         \
   for(count = 0; count < morphism->edges; count++)             \
      removeEdge(host, morphism->edge_map[count].host_index);   \
                                                                \
   for(count = 0; count < morphism->nodes; count++)             \
      removeNode(host, morphism->node_map[count].host_index);   \
   } while(0);                                                  \

#define PROCESS_EDGE_MORPHISMS                               \
   do {                                                      \
   for(count = 0; count < morphism->edges; count++)          \
   {                                                         \
      left_index = morphism->edge_map[count].left_index;     \
      host_index = morphism->edge_map[count].host_index;     \
      if(edge_map[left_index].remove_item == true)           \
      {                                                      \
         removeEdge(host, host_index);                       \
         continue;                                           \
      }                                                      \
      if(edge_map[left_index].relabel_item == true)          \
      {                                                      \
         Edge *host_edge = getEdge(host, host_index);        \
         Label *label = edge_map[left_index].new_label;      \
         relabelEdge(host, host_edge, label, true, false);   \
         continue;                                           \
      }                                                      \
   }                                                         \
   } while(0);                                               


#define PROCESS_NODE_MORPHISMS                                 \
   do {                                                        \
   for(count = 0; count < morphism->nodes; count++)            \
   {                                                           \
      left_index = morphism->node_map[count].left_index;       \
      host_index = morphism->node_map[count].host_index;       \
      if(node_map[left_index].remove_item == true)             \
      {                                                        \
         removeNode(host, host_index);                         \
         continue;                                             \
      }                                                        \
      if(node_map[left_index].relabel_item == true)            \
      {                                                        \
         Node *host_node = getNode(host, host_index);          \
         Label *label = node_map[left_index].new_label;        \
         relabelNode(host, host_node, label, true,             \
                     node_map[left_index].change_root);        \
         node_map[left_index].host_index = host_index;         \
         continue;                                             \
      }                                                        \
      else node_map[left_index].host_index = host_index;       \
   }                                                           \
   } while(0);                                               

#endif /* INC_GEN_MACROS */
