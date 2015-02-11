#ifndef INC_GEN_MACROS_H
#define INC_GEN_MACROS_H

#define ADD_HOST_NODE(is_root, node_name)                                \
   do {                                                                  \
   int index = addNode(host, (is_root), NULL);                           \
   node_map = addIndexMap(node_map, node_name, index, -1, NULL, NULL);   \
   } while(0);                                                           \

#define GET_HOST_SOURCE(source_name)                                    \
   do {                                                                 \
   IndexMap *source_map = findMapFromId(node_map, source_name);         \
   if(source_map == NULL)                                               \
   {                                                                    \
      print_to_log("Error (makeHostGraph): Edge's source " source_name  \
                   "not found in the node map.\n");                     \
      exit(1);                                                          \
   }                                                                    \
   source = getNode(host, source_map->left_index);                      \
   } while(0);                                                          \

#define GET_HOST_TARGET(target_name)                                    \
   do {                                                                 \
   IndexMap *target_map = findMapFromId(node_map, target_name);         \
   if(target_map == NULL)                                               \
   {                                                                    \
      print_to_log("Error (makeHostGraph): Edge's target " target_name  \
                   "not found in the node map.\n");                     \
      exit(1);                                                          \
   }                                                                    \
   target = getNode(host, target_map->left_index);                      \
   } while(0);                                                          \

#define MAKE_MATCHED_NODES_ARRAY                           \
   int count;                                              \
   bool matched_nodes[host->next_node_index];              \
   for(count = 0; count < host->next_node_index; count ++) \
      matched_nodes[count] = false;                

#define MAKE_MATCHED_EDGES_ARRAY                           \
   bool matched_edges[host->next_edge_index];              \
   for(count = 0; count < host->next_edge_index; count ++) \
      matched_edges[count] = false;                

#define CHECK_NODE_MATCHED   \
   if(matched_nodes[index])  \
   {                         \
      nodes = nodes->next;   \
      continue;              \
   }                         \

#define CHECK_EDGE_MATCHED   \
   if(matched_edges[index])  \
   {                         \
      edges = edges->next;   \
      continue;              \
   }                         \

#define CHECK_EDGE_IS_LOOP                     \
   if(host_edge->source != host_edge->target)  \
   {                                           \
      index++;                                 \
      continue;                                \
   }                                           \

#define CHECK_NODE_LABEL_CLASS(lclass)    \
   if(host_node->label_class != (lclass)) \
   {                                      \
      nodes = nodes->next;                \
      continue;                           \
   }                                      \

#define CHECK_EDGE_LABEL_CLASS(lclass)    \
   if(host_edge->label_class != (lclass)) \
   {                                      \
      edges = edges->next;                \
      continue;                           \
   }                                      \

#define CHECK_NODE_MARK(nmark)           \
   if(host_node->label->mark != (nmark)) \
   {                                     \
      nodes = nodes->next;               \
      continue;                          \
   }                                     \

#define CHECK_EDGE_MARK(emark)           \
   if(host_edge->label->mark != (emark)) \
   {                                     \
      edges = edges->next;               \
      continue;                          \
   }                                     \

#define CHECK_NODE_DEGREES(indeg, outdeg) \
   if(host_node->indegree < (indeg))      \
   {                                      \
      nodes = nodes->next;                \
      continue;                           \
   }                                      \
                                          \
   if(host_node->outdegree < (outdeg))    \
   {                                      \
      nodes = nodes->next;                \
      continue;                           \
   }                                      \


#define CHECK_DANGLING_NODE_DEGREES(indeg, outdeg) \
   if(host_node->indegree != (indeg))              \
   {                                               \
      nodes = nodes->next;                         \
      continue;                                    \
   }                                               \
                                                   \
   if(host_node->outdegree != (outdeg))            \
   {                                               \
      nodes = nodes->next;                         \
      continue;                                    \
   }                                               \

#define IF_INVALID_NODE(lclass, nmark, indeg, outdeg)  \
   if(matched_nodes[index] ||                          \
      host_node->label_class != (lclass) ||            \
      host_node->label->mark != (nmark) ||             \
      host_node->indegree < (indeg) ||                 \
      host_node->outdegree < (outdeg))                 \

#define IF_INVALID_DANGLING_NODE(lclass, nmark, indeg, outdeg)  \
   if(matched_nodes[index] ||                                   \
      host_node->label_class != (lclass) ||                     \
      host_node->label->mark != (nmark) ||                      \
      host_node->indegree != (indeg) ||                         \
      host_node->outdegree != (outdeg))                         \

#define IF_INVALID_EDGE(lclass, emark)       \
   if(matched_edges[host_edge->index] ||     \
      host_edge->label_class != (lclass) ||  \
      host_edge->label->mark != (emark))

#define IF_INVALID_LOOP_EDGE(lclass, emark)     \
   if(matched_edges[host_edge->index] ||        \
      host_edge->source != host_edge->target || \
      host_edge->label_class != (lclass) ||     \
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
      if(edge_map[left_index] == -1)                         \
      {                                                      \
         removeEdge(host, host_index);                       \
         continue;                                           \
      }                                                      \
      if(edge_map[left_index] == 1)                          \
      {                                                      \
         Edge *host_edge = getEdge(host, host_index);        \
         relabelEdge(host, host_edge, NULL, true, false);    \
         continue;                                           \
      }                                                      \
   }                                                         \
   } while(0);                                               


#define PROCESS_NODE_MORPHISMS                                          \
   do {                                                                 \
   for(count = 0; count < morphism->nodes; count++)                     \
   {                                                                    \
      left_index = morphism->node_map[count].left_index;                \
      host_index = morphism->node_map[count].host_index;                \
      if(node_map[left_index] == -1)                                    \
      {                                                                 \
         removeNode(host, host_index);                                  \
         continue;                                                      \
      }                                                                 \
      if(node_map[left_index] == 1)                                     \
      {                                                                 \
         Node *host_node = getNode(host, host_index);                   \
         relabelNode(host, host_node, NULL, true, false);               \
         node_map[left_index] = host_node->index;                       \
         continue;                                                      \
      }                                                                 \
      if(node_map[left_index] == 0) node_map[left_index] = host_index;  \
   }                                                                    \
   } while(0);                                               

#endif /* INC_GEN_MACROS */
