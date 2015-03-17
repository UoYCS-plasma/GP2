#include "genHost.h"

void generateHostGraphCode(GPGraph *ast_host_graph)
{
   FILE *header = fopen("runtime/init_runtime.h", "w");
   if(header == NULL) { 
     perror("init_runtime.h");
     exit(1);
   }  

   FILE *source = fopen("runtime/init_runtime.c", "w");
   if(source == NULL) { 
     perror("init_runtime.c");
     exit(1);
   }
   
   fprintf(header, "#include \"../graph.h\"\n"
                   "#include \"../macros.h\"\n"
                   "#include \"../rule.h\"\n\n"
 		   "Graph *makeHostGraph(void);\n");

   int host_nodes = countNodes(ast_host_graph);
   int host_edges = countEdges(ast_host_graph);
   int node_buffer_size = host_nodes > BUFFER_SIZE ? BUFFER_SIZE : host_nodes + 1;
   int edge_buffer_size = host_edges > BUFFER_SIZE ? BUFFER_SIZE : host_edges + 1;
   int host_node_size = getArraySize(host_nodes, MIN_HOST_NODE_SIZE);
   int host_edge_size = getArraySize(host_edges, MIN_HOST_EDGE_SIZE);

   PTIS("#include \"init_runtime.h\"\n\n"
        "Graph *makeHostGraph(void)\n"
        "{\n"
        "   Graph *host = newGraph(%d, %d);\n"
        "   int count = 0;\n\n", host_node_size, host_edge_size);
   PTIS("   /* Arrays to store data for adding nodes and edges. */\n");
   PTIS("   bool root_nodes[%d] = {false};\n", node_buffer_size);
   PTIS("   Label *node_labels[%d] = {NULL};\n", node_buffer_size);
   if(host_edges > 0) 
   {
      PTIS("   int edge_sources[%d];\n", node_buffer_size);
      PTIS("   int edge_targets[%d];\n", node_buffer_size);
      PTIS("   Label *edge_labels[%d] = {NULL};\n", node_buffer_size);
   }
   PTIS("\n");

   /* Hash table mapping node identifiers (from the AST) to their indices in 
    * the host graph. The nodes are added to the host graph according to their
    * order in the AST's node list, hence the host graph indices are known
    * at compile time. */
   int node_map[host_nodes], count;
   for(count = 0; count < host_nodes; count++) node_map[count] = -1;

   List *nodes = ast_host_graph->nodes;
   int node_index = 0, node_count = 0;
   /* Populate the runtime arrays with the necessary data to add each node. */
   while(nodes != NULL)
   { 
      GPNode *ast_node = nodes->value.node;
      if(ast_node->root) PTIS("   root_nodes[%d] = true;\n ", node_count);
      if(ast_node->label->mark != NONE ||
         ast_node->label->gp_list->list_type != EMPTY_LIST)
         PTIS("   node_labels[%d] = makeEmptyList(%d);\n", node_count, 
              ast_node->label->mark);
      int map_index = (int)strtol((ast_node->name) + 1, NULL, 0);
      node_map[map_index] = node_index++;   
      if(++node_count == node_buffer_size)
      {
         PTIS("\n");
         PTIS("   for(count = 0; count < %d; count++)\n"
              "      addNode(host, root_nodes[count], node_labels[count]);\n",
              node_buffer_size);
         PTIS("   memset(root_nodes, 0, %d * sizeof(bool));\n", node_buffer_size);
         PTIS("   memset(node_labels, 0, %d * sizeof(Label*));\n\n", node_buffer_size);
         node_count = 0;
      }
      nodes = nodes->next;   
   }
   /* In the likely case that the number of host nodes is not a multiple of 
    * node_buffer_size, add the remaining nodes. */
   if(node_count > 0 && node_count < node_buffer_size)
   {
      PTIS("\n");
      PTIS("   for(count = 0; count < %d; count++)\n"
           "      addNode(host, root_nodes[count], node_labels[count]);\n\n",
           node_count);
   }

   List *edges = ast_host_graph->edges;
   int edge_count = 0;
   while(edges != NULL)
   {
      GPEdge *ast_edge = edges->value.edge;
      int source_index = (int)strtol((ast_edge->source) + 1, NULL, 0);
      int target_index = (int)strtol((ast_edge->target) + 1, NULL, 0);
      PTIS("   edge_sources[%d] = %d; edge_targets[%d] = %d; ",
           edge_count, node_map[source_index], edge_count, node_map[target_index]);
      if(ast_edge->label->mark != NONE ||
         ast_edge->label->gp_list->list_type != EMPTY_LIST)
         PTIS("   edge_labels[%d] = makeEmptyList(%d);\n", edge_count, 
              ast_edge->label->mark);
      else PTIS("\n");
      edge_count++;
      if(edge_count == edge_buffer_size)
      {
         PTIS("\n");
         PTIS("   for(count = 0; count < %d; count++)\n"
              "      addEdge(host, false, edge_labels[count], edge_sources[count],\n"
              "              edge_targets[count]);\n", edge_buffer_size);
         PTIS("   memset(edge_labels, 0, %d * sizeof(Label*));\n\n", edge_buffer_size);
         edge_count = 0;
      }
      edges = edges->next;   
   }
   /* In the likely case that the number of host edged is not a multiple of 
    * edge_buffer_size, add the remaining edges. */
   if(edge_count > 0 && edge_count < edge_buffer_size)
   {
      PTIS("\n");
      PTIS("   for(count = 0; count < %d; count++)\n"
           "      addEdge(host, false, edge_labels[count], edge_sources[count],\n"
           "              edge_targets[count]);\n\n", edge_count);
   }
   PTIS("   return host;\n"
        "}\n");
   fclose(header);
   fclose(source);
}
