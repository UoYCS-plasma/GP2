#ifndef INC_GEN_MACROS_H
#define INC_GEN_MACROS_H

#define ADD_HOST_NODE(is_root, node_name)                                      \
   do {                                                                        \
   node = newNode((is_root), NULL);                                            \
   addNode(host, node);                                                        \
   node_map = addIndexMap(node_map, node_name, node->index, -1, NULL, NULL);   \
   } while(0);                                                                 \

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


#define MAKE_MATCHED_NODES_ARRAY                                   \
   do {                                                            \
   matched_nodes = calloc(host->next_node_index, sizeof(bool));    \
   if(matched_nodes == NULL)                                       \
   {                                                               \
      print_to_log("Error: Memory exhausted during matched nodes " \
                   "table construction.\n");                       \
      exit(1);                                                     \
   }                                                               \
   } while(0);                                                     \

#define MAKE_MATCHED_EDGES_ARRAY                                      \
   do {                                                               \
   if(host->number_of_edges > 0)                                      \
   {                                                                  \
      matched_edges = calloc(host->next_edge_index, sizeof(bool));    \
      if(matched_edges == NULL)                                       \
      {                                                               \
         print_to_log("Error: Memory exhausted during matched edges " \
                      "table construction.\n");                       \
         exit(1);                                                     \
      }                                                               \
   }                                                                  \
   } while(0);                                                        \
  
#define CHECK_NODE_MATCHED   \
   if(matched_nodes[index])  \
   {                         \
      nodes = nodes->next;   \
      continue;              \
   }                         \

#define CHECK_EDGE_MATCHED_I \
   if(matched_edges[index])  \
   {                         \
      index++;               \
      continue;              \
   }                         \

#define CHECK_EDGE_MATCHED_L \
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

#define CHECK_EDGE_LABEL_CLASS_I(lclass)  \
   if(host_edge->label_class != (lclass)) \
   {                                      \
      index++;                            \
      continue;                           \
   }                                      \

#define CHECK_EDGE_LABEL_CLASS_L(lclass)  \
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


#define CHECK_EDGE_MARK_I(emark)         \
   if(host_edge->label->mark != (emark)) \
   {                                     \
      index++;                           \
      continue;                          \
   }                                     \

#define CHECK_EDGE_MARK_L(emark)         \
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

#define IF_INVALID_NODE(lclass, nmark, indeg, outdeg) \
   if(matched_nodes[index] ||                         \
      host_node->label_class != (lclass) ||           \
      host_node->label->mark != (nmark) ||            \
      host_node->indegree < (indeg) ||                \
      host_node->outdegree < (outdeg))                \


#define IF_INVALID_DANGLING_NODE(lclass, nmark, indeg, outdeg) \
   if(matched_nodes[index] ||                                  \
      host_node->label_class != (lclass) ||                    \
      host_node->label->mark != (nmark) ||                     \
      host_node->indegree != (indeg) ||                        \
      host_node->outdegree != (outdeg))                        \

/* Creates a (left_index, host_index) pair and pushes it to the node images
 * stack of the morphism. Called when a node match is found. */
#define ADD_NODE_MAP(l_index)                                                 \
   do {                                                                       \
   StackData *mapping = malloc(sizeof(StackData));                            \
   if(mapping == NULL)                                                        \
   {                                                                          \
      print_to_log("Error: Memory exhausted during mapping construction.\n"); \
      exit(1);                                                                \
   }                                                                          \
   mapping->map.left_index = (l_index);                                       \
   mapping->map.host_index = index;                                           \
   push(morphism->node_images, mapping);                                      \
   matched_nodes[index] = true;                                               \
   } while(0);                                                                \

/* Creates a (left_index, host_index) pair and pushes it to the edge images
 * stack of the morphism. Called when a edge match is found. */
#define ADD_EDGE_MAP(l_index)                                                 \
   do {                                                                       \
   StackData *mapping = malloc(sizeof(StackData));                            \
   if(mapping == NULL)                                                        \
   {                                                                          \
      print_to_log("Error: Memory exhausted during mapping construction.\n"); \
      exit(1);                                                                \
   }                                                                          \
   mapping->map.left_index = (l_index);                                       \
   mapping->map.host_index = index;                                           \
   push(morphism->edge_images, mapping);                                      \
   matched_edges[index] = true;                                               \
   } while(0);                                                                \

#define REMOVE_NODE_MAP                          \
   do {                                          \
   StackData *data = pop(morphism->node_images); \
   if(data) free(data);                          \
   matched_nodes[index] = false;                 \
   } while(0);                                   \

#define REMOVE_EDGE_MAP                          \
   do {                                          \
   StackData *data = pop(morphism->edge_images); \
   if(data) free(data);                          \
   matched_edges[index] = false;                 \
   } while(0);                                   \

#define REMOVE_EDGE_MAP_AND_RETURN_FALSE         \
   do {                                          \
   StackData *data = pop(morphism->edge_images); \
   if(data) free(data);                          \
   matched_edges[host_edge->index] = false;      \
   return false;                                 \
   } while(0);                                   \

#define MAKE_NODE_POINTER_MAP(r_nodes)                                    \
   do {                                                                   \
   map = calloc((r_nodes), sizeof(Node *));                               \
   if(map == NULL)                                                        \
   {                                                                      \
      print_to_log("Error: Memory exhausted during map construction.\n"); \
      exit(1);                                                            \
   }                                                                      \
   } while(0);                                                            \


/* Deletes all the host items in the morphism from the host graph. Edges are
 * deleted first so that there is no chance of dangling edges from node
 * deletion. Called when the RHS of a rule is the empty graph. */
#define REMOVE_RHS                                    \
   do {                                               \
   StackData *data = NULL;                            \
   while((data = pop(morphism->edge_images)) != NULL) \
   {                                                  \
      removeEdge(host, data->map.host_index);         \
      free(data);                                     \
   }                                                  \
   while((data = pop(morphism->node_images)) != NULL) \
   {                                                  \
      removeNode(host, data->map.host_index);         \
      free(data);                                     \
   }                                                  \
   } while(0);                                        \


#define MAKE_NODE_MAP(num_items)                                          \
   do {                                                                   \
   node_map = calloc((num_items), sizeof(int));                           \
   if(node_map == NULL)                                                   \
   {                                                                      \
      print_to_log("Error: Memory exhausted during map construction.\n"); \
      exit(1);                                                            \
   }                                                                      \
   } while(0);                                                            \

#define MAKE_EDGE_MAP(num_items)                                          \
   do {                                                                   \
   edge_map = calloc((num_items), sizeof(int));                           \
   if(edge_map == NULL)                                                   \
   {                                                                      \
      print_to_log("Error: Memory exhausted during map construction.\n"); \
      exit(1);                                                            \
   }                                                                      \
   } while(0);                                                            \

#define PROCESS_EDGE_MORPHISMS                                       \
   do {                                                              \
   while((data = pop(morphism->edge_images)) != NULL)                \
   {                                                                 \
      if(edge_map[data->map.left_index] == -1)                       \
      {                                                              \
         removeEdge(host, data->map.host_index);                     \
         free(data);                                                 \
         continue;                                                   \
      }                                                              \
      if(edge_map[data->map.left_index] == 1)                        \
      {                                                              \
         Edge *host_edge = getEdge(host, data->map.host_index);      \
         relabelEdge(host, host_edge, NULL, true, false);            \
         free(data);                                                 \
         continue;                                                   \
      }                                                              \
      free(data);                                                    \
   }                                                                 \
   } while(0);                                                       \


#define PROCESS_NODE_MORPHISMS                                       \
   do {                                                              \
   while((data = pop(morphism->node_images)) != NULL)                \
   {                                                                 \
      if(node_map[data->map.left_index] == -1)                       \
      {                                                              \
         removeNode(host, data->map.host_index);                     \
         free(data);                                                 \
         continue;                                                   \
      }                                                              \
      if(node_map[data->map.left_index] == 1)                        \
      {                                                              \
         Node *host_node = getNode(host, data->map.host_index);      \
         relabelNode(host, host_node, NULL, true, false);            \
         node_map[data->map.left_index] = host_node->index;          \
         free(data);                                                 \
         continue;                                                   \
      }                                                              \
      if(node_map[data->map.left_index] == 0)                        \
         node_map[data->map.left_index] = data->map.host_index;      \
      free(data);                                                    \
   }                                                                 \
   } while(0);                                                       \

#endif /* INC_GEN_MACROS */
