#include "genHost.h"

void generateHostGraphCode(GPGraph *ast_host_graph)
{
   /* UNIX-dependent code. */
   DIR *dir = opendir("runtime/host");
   /* Opendir succeeded: directory exists. */
   if(dir != NULL) closedir(dir);
   /* Directory does not exist. Make it! */
   else if(errno == ENOENT)
   {
      int result = mkdir("runtime/host", 0755);
      if(result == -1)
      {
         print_to_log("Error: mkdir failure.\n");
         exit(1);
      }
   }
   /* Opendir failed for another reason. */
   else
   {
      print_to_log("Error: opendir failure.\n");
      exit(1);
   }

   /* The host header contains the declaration of both the host graph and 
    * any functions that add a batch of nodes or edges to the host graph in
    * case of the host graph being sufficiently large. */
   FILE *host_file = fopen("runtime/host/host.h", "w");
   if(host_file == NULL) { 
      perror("runtime/host/host.h");
      exit(1);
   }  
   fprintf(host_file, "#ifndef INC_HOST_H\n");
   fprintf(host_file, "#define INC_HOST_H\n\n");
   fprintf(host_file, "extern Graph *host;\n");

   /* init_runtime has the code for building the host graph and, in the future,
    * code to gather host graph metrics for dynamic matching strategies. */
   FILE *header = fopen("runtime/buildHost.h", "w");
   if(header == NULL) { 
      perror("init_runtime.h");
      exit(1);
   } 
   FILE *file = fopen("runtime/buildHost.c", "w");
   if(file == NULL) { 
      perror("init_runtime.c");
      exit(1);
   }
   PTH("#include \"../graph.h\"\n"
       "#include \"host/host.h\"\n"
       "Graph *buildHostGraph(void);\n");

   PTF("#include \"buildHost.h\"\n\n");
   PTF("Graph *buildHostGraph(void)\n");
   PTF("{\n");
   if(ast_host_graph == NULL) 
   {
      PTFI("return newGraph(%d, %d);\n", 3, MIN_HOST_NODE_SIZE, MIN_HOST_EDGE_SIZE);
      PTF("}\n");
      return;
   }
   int host_nodes = countNodes(ast_host_graph);
   int host_edges = countEdges(ast_host_graph);
   int host_node_size = getArraySize(host_nodes, MIN_HOST_NODE_SIZE);
   int host_edge_size = getArraySize(host_edges, MIN_HOST_EDGE_SIZE);
   int node_file_count = 0, edge_file_count = 0;
   PTFI("Graph *host = newGraph(%d, %d);\n\n", 3, host_node_size, host_edge_size);
   PTFI("host->node_classes = makeLabelClassTable();\n", 3);
   PTFI("host->edge_classes = makeLabelClassTable();\n", 3);

   List *nodes = ast_host_graph->nodes;
   if(nodes == NULL) 
   {
      PTF("\n   return host;\n");
      PTF("}\n");
      fprintf(host_file, "\n#endif\n");
      fclose(host_file);
      fclose(header);
      fclose(file);
      return;
   }

   /* Flag to prevent repeated writing of "label = blank_label" when
    * consecutive blank nodes are added to the graph. */
   bool blank_label = false;

   /* Build the host graph in init_runtime.c if the number of nodes is small 
    * enough. Otherwise, create new source files to add BUFFER_SIZE nodes
    * to the graph. */
   if(host_nodes <= BUFFER_SIZE)
   {
      PTFI("Label label;\n\n", 3);
      int node_count = 0;
      while(nodes != NULL)
      {
         GPLabel *label = nodes->node->label;
         bool root = nodes->node->root;
         if(label->mark == NONE && getASTListLength(label) == 0)
         {
            if(!blank_label)
            {
               PTFI("label = blank_label;\n", 3);
               blank_label = true;
            }
         }
         else
         {
            generateLabelCode(label, node_count++, file);
            blank_label = false;
         }
         PTFI("addNode(host, %d, label);\n", 3, root);
         nodes = nodes->next;   
      }
      PTF("\n");
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
         /* The "nodes" functions are declared in host.h, called in 
          * buildHost.c, and defined in the corresponding nodes.c file. */
         PTF("   nodes%d(host);\n", count);
         fprintf(host_file, "void nodes%d(Graph *host);\n", count);
         fprintf(node_file, "#include \"../../graph.h\"\n\n");
         fprintf(node_file, "void nodes%d(Graph *host)\n{\n", count);

         /* Emit code to add BUFFER_SIZE nodes. */
         fprintf(node_file, "   Label label;\n\n");
         int node_count = 0;
         blank_label = false;
         while(nodes != NULL && node_count < BUFFER_SIZE)
         {
            GPLabel *label = nodes->node->label;
            bool root = nodes->node->root;
            if(label->mark == NONE && getASTListLength(label) == 0)
            {
               if(!blank_label)
               {
                  fprintf(node_file, "   label = blank_label;\n");
                  blank_label = true;
               }
            }
            else
            {
               generateLabelCode(label, node_count++, node_file);
               blank_label = false;
            }
            fprintf(node_file, "   addNode(host, %d, label);\n", root);
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
      /* label is only declared if host_nodes <= BUFFER_SIZE. */
      if(host_nodes > BUFFER_SIZE) PTF("   Label label;\n\n");
      int edge_count = host_nodes <= BUFFER_SIZE ? host_nodes : 0; 
      while(edges != NULL)
      {
        /* The nodes, assumed to be named n0, n1, ... are added to the host 
         * graph in the order in the text file. Therefore the index of each
         * node in the host graph is the number in its AST identifier.
         * The call to strtol extracts the number after the 'n' in the node
         * IDs of the edge's source and target. */
         int source_index = (int)strtol((edges->edge->source) + 1, NULL, 0);
         int target_index = (int)strtol((edges->edge->target) + 1, NULL, 0);

         GPLabel *label = edges->edge->label;
         if(label->mark == NONE && getASTListLength(label) == 0)
         {
            if(!blank_label)
            {
               PTFI("label = blank_label;\n", 3);
               blank_label = true;
            }
         }
         else 
         {
            generateLabelCode(label, edge_count++, file);
            blank_label = false;
         }
         PTFI("addEdge(host, false, label, %d, %d);\n", 3, source_index, target_index);
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
         /* The "edges" functions are declared in host.h, called in 
          * buildHost.c, and defined in the corresponding edges.c file. */
         PTF("   edges%d(host);\n", count);
         fprintf(host_file, "void edges%d(Graph *host);\n", count);
         fprintf(edge_file, "#include \"../../graph.h\"\n\n");
         fprintf(edge_file, "void edges%d(Graph *host)\n{\n", count);

         /* Emit code to add BUFFER_SIZE edges. */
         fprintf(edge_file, "   Label label;\n\n");
         int edge_count = 0;
         bool blank_label = false;
         while(edges != NULL && edge_count < BUFFER_SIZE)
         {
           /* The call to strtol extracts the number after the 'n' in the node
            * IDs of the edge's source and target. */
            int source_index = (int)strtol((edges->edge->source) + 1, NULL, 0);
            int target_index = (int)strtol((edges->edge->target) + 1, NULL, 0);

            GPLabel *label = edges->edge->label;
            if(label->mark == NONE && getASTListLength(label) == 0)
            {
               if(!blank_label)
               {
                  fprintf(edge_file, "   label = blank_label;\n");
                  blank_label = true;
               }
            }
            else
            {
               generateLabelCode(label, edge_count++, edge_file);
               blank_label = false;
            }
            PTFI("addEdge(host, false, label, %d, %d,);\n", 3, source_index, target_index);
         }
         fprintf(edge_file, "}\n");
         fclose(edge_file);
         if(edges == NULL) break;
      }
   }
   PTF("\n   return host;\n");
   PTF("}\n");
   fprintf(host_file, "\n#endif\n");
   fclose(host_file);
   fclose(header);
   fclose(file);
}

void generateLabelCode(GPLabel *ast_label, int list_count, FILE *file)
{
   int length = 0;
   List *list = ast_label->gp_list;
   while(list != NULL)
   {
      length++;
      list = list->next;
   }
   if(length == 0) PTFI("label = makeEmptyLabel(%d);\n", 3, ast_label->mark);
   else
   {
      PTFI("Atom *list%d = makeList(%d);\n", 3, list_count, length);
      int index = 0;
      list = ast_label->gp_list;
      while(list != NULL)
      {
         GPAtom *atom = list->atom;
         if(atom->type == INTEGER_CONSTANT)
         {
            PTFI("list%d[%d].type = INTEGER_CONSTANT; ", 3, list_count, index);
            PTF("list%d[%d].number = %d;\n", list_count, index, atom->number);
         }
         else if(atom->type == STRING_CONSTANT)
         {
            PTFI("list%d[%d].type = STRING_CONSTANT; ", 3, list_count, index);
            PTF("list%d[%d].string = strdup(\"%s\");\n", 
                list_count, index, atom->string);
         }
         else 
         {
            print_to_log("Error (generateLabelCode): Unexpected host atom "
                         "type %d.\n", atom->type);
            break;
         }
         index++;
         list = list->next;
      }
      PTFI("label = makeHostLabel(%d, %d, list%d);\n", 3,
           ast_label->mark, length, list_count);
   }
}
      
