#include "genHost.h"

void generateHostGraphCode(GPGraph *ast_host_graph)
{
   FILE *host_file = fopen("runtime/host/host.h", "w");
   if(host_file == NULL) { 
     perror("runtime/host/host.h");
     exit(1);
   }  
   fprintf(host_file, "#ifndef INC_HOST_H\n");
   fprintf(host_file, "#define INC_HOST_H\n\n");
   fprintf(host_file, "extern Graph *host;\n");

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
                   "#include \"host/host.h\"\n"
 		   "Graph *makeHostGraph(void);\n");

   PTIS("#include \"init_runtime.h\"\n\n");
   PTIS("Graph *makeHostGraph(void)\n");
   PTIS("{\n");
   if(ast_host_graph == NULL) 
   {
      PTIS("   return newGraph(%d, %d);\n", MIN_HOST_NODE_SIZE,
           MIN_HOST_EDGE_SIZE);
      PTIS("}\n");
      return;
   }
   int host_nodes = countNodes(ast_host_graph);
   int host_edges = countEdges(ast_host_graph);
   int host_node_size = getArraySize(host_nodes, MIN_HOST_NODE_SIZE);
   int host_edge_size = getArraySize(host_edges, MIN_HOST_EDGE_SIZE);
   int node_file_count = 0, edge_file_count = 0;

   PTIS("   Graph *host = newGraph(%d, %d);\n\n", host_node_size, host_edge_size);

   List *nodes = ast_host_graph->nodes;
   /* Populate the runtime arrays with the necessary data to add each node. */
   if(host_nodes <= BUFFER_SIZE)
   {
      while(nodes != NULL)
      { 
         PTIS("   addNode(host, %d, blank_label);\n", nodes->node->root);
         nodes = nodes->next;   
      }
      PTIS("\n");
   }
   else
   {
      node_file_count = (host_nodes / BUFFER_SIZE) + 1;
      int count;
      for(count = 0; count < node_file_count && count < 1000; count++)
      {
         /* runtime/host/nodes<count>.c, where count is up to 3 digits long. 
          * The character array includes space for the terminating NULL character. */
         char file_name[24];
         snprintf(file_name, 24, "runtime/host/nodes%d.c", count);
         FILE *node_file = fopen(file_name, "w");
         if(node_file == NULL) { 
            perror(file_name);
            exit(1);
         }
         fprintf(node_file, "#include \"../../graph.h\"\n\n");
         fprintf(host_file, "void nodes%d(Graph *host);\n", count);
         fprintf(source, "   nodes%d(host);\n", count);
         fprintf(node_file, "void nodes%d(Graph *host)\n{\n", count);
         /* Emit code to add BUFFER_SIZE nodes. */
         int node_count = 0;
         fprintf(node_file, "   Label blank_label = {NONE, 0, NULL};\n\n");
         while(nodes != NULL && node_count < BUFFER_SIZE)
         {
            fprintf(node_file, "   addNode(host, %d, blank_label);\n", nodes->node->root);
            node_count++;
            nodes = nodes->next;   
         }
         fprintf(node_file, "}\n");
         fclose(node_file);
         if(nodes == NULL) break;
      }
   }

   List *edges = ast_host_graph->edges;
   if(host_edges <= BUFFER_SIZE)
   {
      while(edges != NULL)
      {
         /* As the nodes, assumed to be named n0, n1, ... are added to the host 
         * graph in order, the index of each node in the host graph is its number
         * in its AST identifier. */
         int source_index = (int)strtol((edges->edge->source) + 1, NULL, 0);
         int target_index = (int)strtol((edges->edge->target) + 1, NULL, 0);
         PTIS("   addEdge(host, false, blank_label, %d, %d);\n",
              source_index, target_index);
         edges = edges->next;   
      }
   }
   else
   {
      edge_file_count = (host_edges / BUFFER_SIZE) + 1;
      int count;
      for(count = 0; count < edge_file_count && count < 1000; count++)
      {
         /* runtime/host/edges<count>.c, where count is up to 3 digits long.
          * The array includes space for the terminating NULL character. */
         char file_name[24];
         snprintf(file_name, 24, "runtime/host/edges%d.c", count);
         FILE *edge_file = fopen(file_name, "w");
         if(edge_file == NULL) { 
            perror(file_name);
            exit(1);
         }
         fprintf(edge_file, "#include \"../../graph.h\"\n\n");
         fprintf(host_file, "void edges%d(Graph *host);\n", count);
         fprintf(source, "   edges%d(host);\n", count);
         fprintf(edge_file, "void edges%d(Graph *host)\n{\n", count);
         /* Emit code to add BUFFER_SIZE edges. */
         int edge_count = 0;
         fprintf(edge_file, "   Label blank_label = {NONE, 0, NULL};\n\n");
         while(edges != NULL && edge_count < BUFFER_SIZE)
         {
            int source_index = (int)strtol((edges->edge->source) + 1, NULL, 0);
            int target_index = (int)strtol((edges->edge->target) + 1, NULL, 0);
            fprintf(edge_file, "   addEdge(host, false, blank_label, %d, %d);\n", 
                    source_index, target_index);
            edge_count++;
            edges = edges->next;   
         }
         fprintf(edge_file, "}\n");
         fclose(edge_file);
         if(edges == NULL) break;
      }
   }
   PTIS("\n   return host;\n");
   PTIS("}\n");
   fprintf(host_file, "\n#endif\n");
   fclose(host_file);
   fclose(header);
   fclose(source);
}
