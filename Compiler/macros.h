#ifndef INC_GEN_MACROS_H
#define INC_GEN_MACROS_H

#define MAKE_MATCHED_NODES_ARRAY                                   \
   matched_nodes = calloc(host_nodes, sizeof(bool));               \
   if(matched_nodes == NULL)                                       \
   {                                                               \
      print_to_log("Error: Memory exhausted during matched nodes " \
                   "table construction.\n");                       \
      exit(1);                                                     \
   }                                                               \

#define MAKE_MATCHED_EDGES_ARRAY                                      \
   if(host_edges > 0)                                                 \
   {                                                                  \
      matched_edges = calloc(host_edges, sizeof(bool));               \
      if(matched_edges == NULL)                                       \
      {                                                               \
         print_to_log("Error: Memory exhausted during matched edges " \
                      "table construction.\n");                       \
         exit(1);                                                     \
      }                                                               \
   }                                                                  \
  
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

#define IF_VALID_NODE(lclass, nmark, indeg, outdeg) \
   if(matched_nodes[index] ||                       \
      host_node->label_class != (lclass) ||         \
      host_node->label->mark != (nmark) ||          \
      host_node->indegree < (indeg) ||              \
      host_node->outdegree < (outdeg))              \


#define IF_VALID_DANGLING_NODE(lclass, nmark, indeg, outdeg) \
   if(matched_nodes[index] ||                                \
      host_node->label_class != (lclass) ||                  \
      host_node->label->mark != (nmark) ||                   \
      host_node->indegree != (indeg) ||                      \
      host_node->outdegree != (outdeg))                      \


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

#endif /* INC_GEN_MACROS */
