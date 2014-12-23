/* ///////////////////////////////////////////////////////////////////////////

  ===================================
  generate.c - Chris Bak (28/10/2014)
  ===================================

/////////////////////////////////////////////////////////////////////////// */

#include "generate.h"

FILE *rule_header = NULL;
FILE *rule_source = NULL;
 
Searchplan *searchplan = NULL;


void generateHostGraphCode(GPGraph *ast_host_graph)
{
   FILE *header = fopen("init_runtime.h", "w");
   if(header == NULL) { 
     perror("init_runtime.h");
     exit(1);
   }  

   FILE *source = fopen("init_runtime.c", "w");
   if(source == NULL) { 
     perror("init_runtime.c");
     exit(1);
   }
     
   fprintf(header, "#include \"graph.h\"\n"
                   "#include \"macros.h\"\n"
                   "#include \"rule.h\"\n\n"
 		   "Graph *makeHostGraph(void);\n");

   PTIS("#include \"init_runtime.h\"\n\n"
        "Graph *makeHostGraph(void)\n"
        "{\n"
        "   Graph *host = newGraph();\n"
        "   Node *node, *source, *target = NULL;\n"
        "   Edge *edge = NULL;\n"
        "   IndexMap *node_map = NULL;\n\n");

   List *nodes = ast_host_graph->nodes;

   while(nodes != NULL)
   {
      GPNode *ast_node = nodes->value.node;
      //TODO: Incorporate Label *label = transformLabel(ast_node->label);
      PTIS("   ADD_HOST_NODE(%d, \"%s\")\n", ast_node->root, ast_node->name);
      nodes = nodes->next;   
   }
   PTIS("\n");
   List *edges = ast_host_graph->edges;

   while(edges != NULL)
   {
      GPEdge *ast_edge = edges->value.edge;

      //TODO: Incorporate Label *label = transformLabel(ast_edge->label);
      PTIS("   GET_HOST_SOURCE(\"%s\")\n"
           "   /* If the edge is a loop, no need to get the target node. */\n"
           "   if(!strcmp(\"%s\", \"%s\"))\n"
           "   {\n"
           "      edge = newEdge(%d, NULL, source, source);\n"
           "      addEdge(host, edge);\n"
           "   }\n",
           ast_edge->source, ast_edge->source, ast_edge->target, 
           ast_edge->bidirectional);

      PTIS("   else\n"
           "   {\n"
           "      GET_HOST_TARGET(\"%s\")\n"
           "   }\n"
           "   edge = newEdge(%d, NULL, source, target);\n"
           "   addEdge(host, edge);\n\n",
           ast_edge->target, ast_edge->bidirectional);

      edges = edges->next;   
   }
   PTIS("   if(node_map) freeIndexMap(node_map);\n"
        "   return host;\n"
        "}\n");
}


void generateRuleCode(Rule *rule)
{
   string rule_name = rule->name;
   /* Create files <rule_name>.h and <rule_name>.c */
   int length = strlen(rule_name) + 3;

   string header_name = malloc(length);
   if(header_name == NULL)
   {
      print_to_log("Error: Memory exhausted during file name creation.\n");
      exit(1);
   }

   string source_name = malloc(length);
   if(source_name == NULL)
   {
      print_to_log("Error: Memory exhausted during file name creation.\n");
      exit(1);
   }
   //strcpy(header_name, "runtime/");
   //strcpy(source_name, "runtime/");
   //strcat(header_name, rule_name);
   //strcat(source_name, rule_name);
   strcpy(header_name, rule_name);
   strcpy(source_name, rule_name);
   strcat(header_name, ".h");
   strcat(source_name, ".c");

   /* TODO: open runtime.h and write include rule_name.h. */

   rule_header = fopen(header_name, "w");
   if(rule_header == NULL) { 
     perror(header_name);
     exit(1);
   }  

   rule_source = fopen(source_name, "w");
   if(rule_source == NULL) { 
     perror(source_name);
     exit(1);
   }

   free(header_name);
   free(source_name);

   PTH("#include \"globals.h\"\n"
       "#include \"graph.h\"\n"
       "#include \"macros.h\"\n"
       "#include \"match.h\"\n\n");
   PTS("#include \"%s.h\"\n\n", rule_name);

   /* Assuming at least one of LHS and RHS is non-empty. */
   if(rule->lhs == NULL) emitApplicationCode(rule, true, false);
   else
   {
      PTH("#define LEFT_NODES %d\n"
          "#define LEFT_EDGES %d\n\n"
          "extern Morphism *morphism;\n\n",
          rule->lhs->number_of_nodes, rule->lhs->number_of_edges);
      if(rule->rhs == NULL)
      {
         emitMatchingCode(rule_name, rule->lhs, rule->deleted_nodes, false);
         emitApplicationCode(rule, false, true);
      }
      else
      {
         if(rule->flags.is_predicate)
            emitMatchingCode(rule_name, rule->lhs, rule->deleted_nodes, true);
         else
         {
            emitMatchingCode(rule_name, rule->lhs, rule->deleted_nodes, false);
            emitApplicationCode(rule, false, false);
         }
      }
   }

   fclose(rule_header);
   fclose(rule_source);
   return;
}


void emitApplicationCode(Rule *rule, bool empty_lhs, bool empty_rhs)
{
   Graph *lhs = rule->lhs;
   Graph *rhs = rule->rhs;

   if(empty_lhs)
   {
      PTH("void apply%s(Graph *host);\n", rule->name);
      PTS("void apply%s(Graph *host)\n", rule->name);
      PTS("{\n");

      int index;
      PTS("   Node *host_node = NULL, *source = NULL, *target = NULL;\n"
          "   Edge *host_edge = NULL;\n\n"
          "   /* Array of host node pointers indexed by RHS node index. */\n"
          "   Node **map = NULL;\n"
          "   MAKE_NODE_POINTER_MAP(%d)\n\n", rhs->number_of_nodes);

      for(index = 0; index < rhs->number_of_nodes; index++)
      {
         Node *rule_node = getNode(rhs, index);
         /* TODO: Evaluate rule_node->label. */
         PTSI("host_node = newNode(%d, NULL);\n", 3, rule_node->root);
         PTSI("addNode(host, host_node);\n", 3);
         PTSI("map[%d] = host_node;\n\n", 3, rule_node->index);
      }

      NewEdgeList *iterator = rule->added_edges;
      while(iterator != NULL)
      {
         if(iterator->source_index == iterator->target_index)
         {
            PTSI("source = map[%d];\n", 3, iterator->source_index);
            PTSI("host_edge = newEdge(false, NULL, source, source);\n", 3);
            PTSI("addEdge(host, host_edge);\n\n", 3);
         }
         else
         {
            PTSI("source = map[%d];\n", 3, iterator->source_index);
            PTSI("target = map[%d];\n", 3, iterator->target_index);
            /* TODO: Evaluate rule_edge->label. */
            PTSI("host_edge = newEdge(false, NULL, source, target);\n", 3);
            PTSI("addEdge(host, host_edge);\n\n", 3);
         }
         iterator = iterator->next;
      }     
      PTS("   free(map);\n"
          "   return;\n}\n\n");
      return;
   }

   PTH("void apply%s(Morphism *morphism, Graph *host);\n", rule->name);
   PTS("void apply%s(Morphism *morphism, Graph *host)\n", rule->name);
   PTS("{\n");

   if(empty_rhs)
   {
      PTS("   REMOVE_RHS\n" 
          "   freeMorphism(morphism);\n"
          "   return;\n"
          "}\n\n");
      return;
   }
   
   /* Using preserved items lists, populate the maps in the generated code. */
   PTS("   /* Array of LHS edges marking their status.\n"
       "    * -1 -> delete; 0 -> do nothing; 1 -> relabel */\n"
       "   int *edge_map = NULL;\n"
       "   MAKE_EDGE_MAP(%d)\n", lhs->number_of_edges);
  
   int index;
   for(index = 0; index < lhs->number_of_edges; index++)
   {
      PreservedItemList *item = queryPItemList(rule->preserved_edges, index);
      if(item == NULL) 
      {
         PTSI("edge_map[%d] = -1;\n", 3, index);
         continue;
      }
      else
      {
         if(item->label_change)
              PTSI("edge_map[%d] = 1;\n", 3, index);
         else PTSI("edge_map[%d] = 0;\n", 3, index);
      }
   }

   PTS("\n   /* Array of LHS nodes marking their status.\n"
       "    * -1 -> delete; 0 -> do nothing; 1 -> relabel */\n"
       "   int *node_map = NULL;\n"
       "   MAKE_NODE_MAP(%d)\n", lhs->number_of_nodes);

   PreservedItemList *iterator = rule->preserved_nodes;
   while(iterator != NULL)
   {
      if(iterator->label_change)
           PTSI("node_map[%d] = 1;\n", 3, iterator->left_index);
      else PTSI("node_map[%d] = 0;\n", 3, iterator->left_index);
      iterator = iterator->next;
   }
 
   ItemList *iterator2 = rule->deleted_nodes;
   while(iterator2 != NULL)
   {
      PTSI("node_map[%d] = -1;\n", 3, iterator2->index);
      iterator2 = iterator2->next;
   }

   /* TODO: Pass label of RHS items from rule->preserved_edges/nodes */
   PTS("\n"
       "   StackData *data = NULL;\n\n"
       "   /* Pops all edges in the morphism stacks and deletes/preserves\n"
       "    * host items according to the entries in edge_map and node_map. */\n"
       "   PROCESS_EDGE_MORPHISMS\n" 
       "   PROCESS_NODE_MORPHISMS\n\n"
       "   /* Array of host node pointers indexed by RHS node index. */\n"
       "   Node **map = NULL;\n"
       "   MAKE_NODE_POINTER_MAP(%d)\n\n", rhs->number_of_nodes);

   ItemList *iterator_n = rule->added_nodes;
   if(iterator_n != NULL)
      PTS("   Node *host_node = NULL;\n");
   while(iterator_n != NULL)
   {   
      Node *rule_node = getNode(rhs, iterator_n->index);
      /* TODO: Evaluate rule_node->label. */
      PTSI("host_node = newNode(%d, NULL);\n", 3, rule_node->root);
      PTSI("addNode(host, host_node);\n", 3);
      PTSI("map[%d] = host_node;\n\n", 3, rule_node->index);
      iterator_n = iterator_n->next;
   }   
  
   
   NewEdgeList *iterator_e = rule->added_edges;
   if(iterator_e != NULL)
      PTS("   Node *source = NULL, *target = NULL;\n"
          "   Edge *host_edge = NULL;\n"
          "   int source_index = 0, target_index = 0;\n\n");
   while(iterator_e != NULL)
   {
      if(iterator_e->source_location == 'l')
      {
         PTSI("source_index = node_map[%d];\n", 3, iterator_e->source_index);
         PTSI("source = getNode(host, source_index);\n", 3);
      }
      else PTSI("source = map[%d];\n", 3, iterator_e->source_index);

      if(iterator_e->target_location == 'l')
      {
         PTSI("target_index = node_map[%d];\n", 3, iterator_e->target_index);
         PTSI("target = getNode(host, target_index);\n", 3);
      }
      else PTSI("target = map[%d];\n", 3, iterator_e->target_index);

      /* TODO: Evaluate rule_edge->label. */
      PTSI("host_edge = newEdge(false, NULL, source, target);\n", 3);
      PTSI("addEdge(host, host_edge);\n\n", 3);
   
      iterator_e = iterator_e->next;      
   }

   PTS("   free(edge_map);\n"
       "   free(node_map);\n"
       "   free(map);\n"
       "   freeMorphism(morphism);\n"
       "   return;\n"
       "}\n\n");
}

void emitMatchingCode(string rule_name, Graph *lhs, ItemList *deleted_nodes,
                      bool is_predicate)
{
   /* Create the searchplan. */
   generateSearchplan(lhs); 

   if(searchplan->first == NULL)
   {
      print_to_log("Error: empty searchplan. Aborting.\n");
      freeSearchplan(searchplan);
      return;
   }

   SearchOp *operation = searchplan->first;
   Node *node = NULL;
   Edge *edge = NULL;

   /* The searchplan is iterated over twice. On the first pass, the prototypes
    * of the matching functions are printed to the generated source file. This
    * is necessary since the functions are static. */   
   while(operation != NULL)
   {
      char type = operation->type;

      switch(type) {
        
         case 'n': 

              node = getNode(lhs, operation->index);
              PTS("static bool match_n%d(Graph *host, Morphism *morphism,\n"
                  "                     bool *matched_nodes, bool *matched_edges);\n",
                  node->index);
              break;

         case 'r': 

              node = getNode(lhs, operation->index);
              PTS("static bool match_n%d(Graph *host, Morphism *morphism,\n"
                  "                     bool *matched_nodes, bool *matched_edges);\n",
                  node->index);
              break;

         case 'i': 

         case 'o': 

         case 'b':

              node = getNode(lhs, operation->index);
              PTS("static bool match_n%d(Graph *host, Edge *host_edge, Morphism *morphism,\n"
                  "                     bool *matched_nodes, bool *matched_edges);\n",
                  node->index);
              break;

         case 'e': 

              edge = getEdge(lhs, operation->index);
              PTS("static bool match_e%d(Graph *host, Morphism *morphism,\n"
                  "                     bool *matched_edges, bool *matched_edges);\n",
                  edge->index);
              break;

         case 's': 

         case 't':

         case 'l':

              edge = getEdge(lhs, operation->index);
              PTS("static bool match_e%d(Graph *host, Node *host_node, Morphism *morphism,\n"
                  "                     bool *matched_nodes, bool *matched_edges);\n",
                  edge->index);
              break;
 
         default:
              print_to_log("Error (generateMatchingCode): Unexpected "
                           "operation type %c.\n", operation->type);
              break;
      }
      operation = operation->next;
   }
 
   PTS("\nMorphism *morphism = NULL;\n\n");
   emitRuleMatcher(rule_name, searchplan->first, is_predicate);
   PTS("\n\n");

   /* The second iteration of the searchplan prints the definitions of the 
    * functions that are now declared in the source file. The type of the
    * searchplan operation determines which emitMatcher function is called
    * and which parameters are passed to it. */
   operation = searchplan->first;

   while(operation != NULL)
   {
      char type = operation->type;

      switch(type) {
        
         case 'n': 

              node = getNode(lhs, operation->index);
              emitNodeMatcher(node, false, deleted_nodes, operation->next);
              break;

         case 'r': 

              node = getNode(lhs, operation->index);
              emitNodeMatcher(node, true, deleted_nodes, operation->next);
              break;

         case 'i': 

         case 'o': 

         case 'b':

              node = getNode(lhs, operation->index);
              emitNodeFromEdgeMatcher(node, type, deleted_nodes, 
                                      operation->next);
              break;

         case 'e': 

              edge = getEdge(lhs, operation->index);
              emitEdgeMatcher(edge, operation->next);
              break;

         case 's': 

         case 't':

         case 'l':

              edge = getEdge(lhs, operation->index);
              emitEdgeFromNodeMatcher(edge, type, operation->next);
              break;
         
         default:
              print_to_log("Error (generateMatchingCode): Unexpected "
                           "operation type %c.\n", operation->type);
              break;
      }
      operation = operation->next;
   }

}


void emitRuleMatcher(string rule_name, SearchOp *first_op, bool is_predicate)
{
   /* TODO: Function name should also contain procedural scope. This could be 
    * applied to the rule name at AST transformation time. */
   char item;
   if(first_op->is_node) item = 'n';
   else item = 'e';

   PTH("bool match%s(Graph *host);\n", rule_name);
   PTS("bool match%s(Graph *host)\n"
       "{\n" 
       "   int host_nodes = host->number_of_nodes;\n"
       "   int host_edges = host->number_of_edges;\n\n"
       "   if(LEFT_NODES > host_nodes || LEFT_EDGES > host_edges) return NULL;\n"
       "   morphism = makeMorphism();\n\n"
       "   /* Initialise variables. */\n"
       "   bool *matched_nodes = NULL, *matched_edges = NULL;\n"
       "   MAKE_MATCHED_NODES_ARRAY\n\n"
       "   MAKE_MATCHED_EDGES_ARRAY\n\n"
       "   bool match_found = match_%c%d(host, morphism, matched_nodes,\n"
       "                               matched_edges);\n\n"
       "   if(matched_nodes) free(matched_nodes);\n"
       "   if(matched_edges) free(matched_edges);\n\n", 
       rule_name, item, first_op->index);

   /* If the rule is a predicate, do not emit the call to apply_<rule_name> */
   if(is_predicate) 
   {
      PTS("   if(match_found)\n "
          "   {\n"
          "      freeMorphism(morphism);\n"
          "      return true;\n"
          "   }\n");
   }
   else
   {
      PTS("   if(match_found)\n"
          "   {\n"
          "      apply%s(morphism, host);\n"
          "      return true;\n"
          "   }\n", rule_name);
   }
   PTS("   else freeMorphism(morphism);\n\n"
       "   return false;\n"
       "}\n");
}


void emitNodeMatcher(Node *left_node, bool is_root, ItemList *deleted_nodes,
                     SearchOp *next_op)
{
   int left_index = left_node->index;
   bool dangling_node = queryItemList(deleted_nodes, left_index);

   PTS("static bool match_n%d(Graph *host, Morphism *morphism,\n"
       "                     bool *matched_nodes, bool *matched_edges)\n"
       "{\n", left_index);

   if(is_root) PTSI("GSList *nodes = getRootNodes(host);\n", 3);
   else PTSI("GSList *nodes = getNodesByLabel(host, %d);\n", 3,
             left_node->label_class);

   PTS("   while(nodes != NULL)\n"
       "   {\n"
       "      Node *host_node = (Node *)nodes->data;\n"
       "      int index = host_node->index;\n\n"
       "      CHECK_NODE_MATCHED\n\n");
   
   /* If !is_root, then the candidate nodes are obtained by label class.
    * In that case, there is no need to explicitly check the label class
    * of the host node. */
   if(is_root)
        PTSI("CHECK_NODE_LABEL_CLASS(%d)\n\n", 6, left_node->label_class);

   PTSI("CHECK_NODE_MARK(%d)\n\n", 6, left_node->label->mark);

   if(dangling_node)
        PTSI("CHECK_DANGLING_NODE_DEGREES(%d, %d);\n\n",
             6, left_node->indegree, left_node->outdegree);   
   else PTSI("CHECK_NODE_DEGREES(%d, %d);\n\n", 
             6, left_node->indegree, left_node->outdegree);

   PTSI("/* Label matching code does not exist yet. */\n", 6);
   /* TODO: Call to label matcher goes here. */
   PTSI("bool nodes_match = host_node->label->list_length == 0;\n", 6);
   PTSI("if(nodes_match)\n", 6);
   PTSI("{\n", 6);
   PTSI("ADD_NODE_MAP(%d)\n", 9, left_index);

   /* Emits the call to the next matching function in the searchplan which 
    * assigns its result to a boolean variable result. */
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) 
   {
      PTSI("if(result) return true;\n", 9);
      PTSI("else\n", 9);
      PTSI("{\n", 9);
      PTSI("REMOVE_NODE_MAP\n", 12);
      PTSI("}\n", 9);
   }
   PTSI("}\n", 6);
   PTSI("nodes = nodes->next;\n", 6);
   PTSI("}\n", 3);
   PTSI("return false;\n", 3);
   PTS("}\n\n");
}


void emitNodeFromEdgeMatcher(Node *left_node, char type,
                             ItemList *deleted_nodes, SearchOp *next_op)
{
   int left_index = left_node->index;
   bool dangling_node = queryItemList(deleted_nodes, left_index);

   PTS("static bool match_n%d(Graph *host, Edge *host_edge, Morphism *morphism,\n"
       "                     bool *matched_nodes, bool *matched_edges)\n"
       "{\n", left_index);

   /* First trying the target of the image of a bidirectional edge is
    * completely arbitrary. Might be an idea to code a "coin flip" to
    * choose the incident node. */ 
   if(type == 'i' || type == 'b') PTSI("Node *host_node = getTarget(host_edge);\n", 3);
   else PTSI("Node *host_node = getSource(host_edge);\n", 3);
   PTSI("int index = host_node->index;\n\n", 3);

   PTSI("/* This is the only node to check, so perform all the preliminaries\n"
        "    * in one step.\n"
        "    * Arguments: label class, mark, indegree, outdegree. */\n", 3);
   if(dangling_node)
        PTSI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d)\n", 3,
             left_node->label_class, left_node->label->mark, 
             left_node->indegree, left_node->outdegree);
   else PTSI("IF_INVALID_NODE(%d, %d, %d, %d)\n", 3,
             left_node->label_class, left_node->label->mark, 
             left_node->indegree, left_node->outdegree); 

   if(type == 'b')
   {
      PTSI("{\n", 3); 
      PTSI("/* Matching from bidirectional edge: check the second incident node. */\n", 6);
      if(type == 'i' || type == 'b') PTSI("host_node = getSource(host_edge);\n", 6);
      else PTSI("host_node = getTarget(host_edge);\n", 6);
      PTSI("index = host_node->index;\n\n", 6);
      PTSI("*/ Arguments: label class, mark, indegree, outdegree. */\n", 3);
      if(dangling_node)
           PTSI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d)\n", 3,
                left_node->label_class, left_node->label->mark, 
                left_node->indegree, left_node->outdegree);
      else PTSI("IF_INVALID_NODE(%d, %d, %d, %d)\n", 3,
               left_node->label_class, left_node->label->mark, 
               left_node->indegree, left_node->outdegree); 
      PTSI("{\n", 6); 
      PTSI("REMOVE_EDGE_MAP_AND_RETURN_FALSE\n", 9);
      PTSI("}\n", 6);
      PTSI("}\n\n", 3);
   }
   else 
   {
      PTSI("{\n", 3); 
      PTSI("REMOVE_EDGE_MAP_AND_RETURN_FALSE\n", 6);
      PTSI("}\n\n", 3);
   }
      
   /* TODO: Call to label matcher goes here. */
   PTSI("bool nodes_match = host_node->label->list_length == 0;\n", 3);
   PTSI("if(nodes_match)\n", 3);
   PTSI("{\n", 3);
   PTSI("ADD_NODE_MAP(%d)\n", 6, left_index);
   
   /* Emits the call to the next matching function in the searchplan which 
    * assigns its result to a boolean variable result. */
   bool total_match = emitNextMatcherCall(next_op, 6); 
   if(!total_match)
   {
      PTSI("if(result) return true;\n", 6);
      PTSI("else\n", 6);
      PTSI("{\n", 6);
      PTSI("REMOVE_NODE_MAP\n", 6);
      PTSI("}\n", 6);
   }
   PTSI("}\n", 3);
   PTSI("return false;\n", 3);
   PTS("}\n\n");
}


void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op)
{
   PTS("static bool match_e%d(Graph *host, Morphism *morphism,\n"
       "                     bool *matched_nodes, bool *matched_edges)\n"
       "{\n", left_edge->index);

   PTSI("GSList *edges = getEdgesByLabel(host, %d);\n", 3,
        left_edge->label_class);

   PTSI("while(edges != NULL)\n", 3);
   PTSI("{\n", 3);
   PTSI("Edge *host_edge = (Edge *)edges->data;\n", 6);
   PTSI("int index = host_edge->index;\n\n", 6);

   PTSI("CHECK_EDGE_MATCHED_L\n\n", 6);

   PTSI("CHECK_EDGE_LABEL_CLASS_L(%d)\n\n", 6, left_edge->label_class);

   PTSI("CHECK_EDGE_MARK_L(%d)\n\n", 6, left_edge->label->mark);

   /* TODO: Call to label matcher goes here. */
   PTSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
   PTSI("if(edges_match)\n", 6);
   PTSI("{\n", 6);
   PTSI("ADD_EDGE_MAP(%d)\n", 9, left_edge->index);
   /* Emits the call to the next matching function in the searchplan which 
    * assigns its result to a boolean variable result. */
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match)
   {
      PTSI("if(result) return true;\n", 9);
      PTSI("else\n", 9);
      PTSI("{\n", 9);
      PTSI("REMOVE_EDGE_MAP\n", 12);
      PTSI("}\n", 9);
   }
   PTSI("}\n", 6);
   PTSI("else edges = edges->next;\n", 6);
   PTSI("}\n", 3);
   PTSI("return false;\n", 3);
   PTS("}\n\n");
}


void emitEdgeFromNodeMatcher(Edge *left_edge, char type, SearchOp *next_op)
{
   PTS("static bool match_e%d(Graph *host, Node *host_node, Morphism *morphism,\n"
       "                     bool *matched_nodes, bool *matched_edges)\n"
       "{\n"
       "   int counter;\n", left_edge->index);

   if(type == 's' || type == 'l')
        PTSI("for(counter = 0; counter < host_node->outdegree; counter++)\n", 3);
   else PTSI("for(counter = 0; counter < host_node->indegree; counter++)\n", 3);

   PTSI("{\n", 3);
   if(type == 's' || type == 'l')
        PTSI("Edge *host_edge = getOutEdge(host_node, counter);\n", 6);
   else PTSI("Edge *host_edge = getInEdge(host_node, counter);\n", 6);
   PTSI("int index = host_edge->index;\n\n", 6);

   PTSI("CHECK_EDGE_MATCHED_I\n\n", 6);

   if(type == 'l') PTSI("CHECK_EDGE_IS_LOOP\n\n", 6);

   PTSI("CHECK_EDGE_LABEL_CLASS_I(%d)\n\n", 6, left_edge->label_class);

   PTSI("CHECK_EDGE_MARK_I(%d)\n\n", 6, left_edge->label->mark);

   /* TODO: Call to label matcher goes here. */
   PTSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
   PTSI("if(edges_match)\n", 6);
   PTSI("{\n", 6);
   PTSI("ADD_EDGE_MAP(%d)\n", 9, left_edge->index);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) PTSI("if(result) return true;\n", 9);

   if(left_edge->bidirectional)
   /* Emit code to try and match an edge in the opposite direction. */
   {
      PTSI("}\n", 6);
      PTSI("}\n\n", 3);
      if(type == 's' || type == 'l')
           PTSI("for(counter = 0; counter < host_node->indegree; counter++)\n", 3);
      else PTSI("for(counter = 0; counter < host_node->outdegree; counter++)\n", 3);

      PTSI("{\n", 3);
      if(type == 's' || type == 'l')
           PTSI("Edge *host_edge = getInEdge(host_node, counter);\n", 6);
      else PTSI("Edge *host_edge = getOutEdge(host_node, counter);\n", 6);

      PTSI("int index = host_edge->index;\n\n", 6);

      PTSI("CHECK_EDGE_MATCHED_I\n\n", 3);

      PTSI("CHECK_EDGE_LABEL_CLASS_I(%d)\n\n", 3, left_edge->label_class);

      PTSI("CHECK_EDGE_MARK_I(%d)\n\n", 3, left_edge->label->mark);

      /* TODO: Call to label matcher goes here. */
      PTSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
      PTSI("if(edges_match)\n", 6);
      PTSI("{\n", 6);
      PTSI("ADD_EDGE_MAP(%d)\n", 9, left_edge->index);
   
      /* Emits the call to the next matching function in the searchplan which
       * assigns it result to a boolean variable result. */
      bool total_match = emitNextMatcherCall(next_op, 9);
      if(!total_match) PTSI("if(result) return true;\n", 9);   
   }
   if(!total_match)
   {
      PTSI("else\n", 9);
      PTSI("{\n", 9);
      PTSI("REMOVE_EDGE_MAP\n", 12);
      PTSI("}\n", 9);
   }
   PTSI("}\n", 6);
   PTSI("else index++;\n", 6);
   PTSI("}\n", 3);
   PTSI("return false;\n", 3);
   PTS("}\n\n");
}


bool emitNextMatcherCall(SearchOp *next_op, int indent)
{
   if(next_op == NULL)    
   {
      PTSI("/* All items matched! */\n", indent);
      PTSI("return true;\n", indent);
      return true;
   }
   switch(next_op->type)
   {
      case 'n':
  
      case 'r':

           PTSI("bool result = match_n%d(host, morphism, matched_nodes,\n"
                "                             matched_edges);\n", 
		indent, next_op->index);
           break;

      case 'i':

      case 'o':
 
      case 'b':

           PTSI("bool result = match_n%d(host, host_edge, morphism, matched_nodes,\n"
                "                                matched_edges);\n",
		indent, next_op->index);
           break;
  
      case 'e':

           PTSI("bool result = match_e%d(host, morphism, matched_nodes,\n"
                "                             matched_edges);\n",
                indent, next_op->index);
           break;

      case 's':
 
      case 't':

      case 'l':

           PTSI("bool result = match_e%d(host, host_node, morphism, matched_nodes,\n"
                "                                matched_edges);\n", 
                indent, next_op->index);
           break;

      default:
 
           print_to_log("Error (emitNextMatcherCall): Unexpected "
                           "operation type %c.\n", next_op->type);
           break;
   
   }
   return false;
}

/* void emitRuleApplicationCode(string rule_name, Graph *lhs, Graph *rhs,

                             RuleData *rule_data)
{
   PTH("Graph *apply_%s(Graph *host, Morphism *morphism);\n", rule_name);
   PTS("Graph *apply_%s(Graph *host, Morphism *morphism)\n", rule_name);
   PTS("{\n");
  
   int index = 0;

   if(rule_data->deleted_edges != NULL)
   {
      PTSI(" Delete edges. \n", 3);
      while(rule_data->deleted_edges[index] >= 0)
      {
         PTSI("

         index++;
      }    */ 


void generateSearchplan(Graph *lhs)
{
   int lhs_nodes = lhs->number_of_nodes;
   int lhs_size = lhs_nodes + lhs->number_of_edges;
   int index;

   searchplan = initialiseSearchplan();

   /* Indices 0 to |V_L|-1 are the nodes. Indices |V_L| to |V_L|+|E_L|-1 are the
    * edges. Index |V_L| refers to the LHS edge with index 0. */
   bool *tagged_items = calloc(lhs_size, sizeof(bool));
   if(tagged_items == NULL)
   {
      print_to_log("Error: Memory exhausted during tagged items "
                   "array construction.\n");
      exit(1);
   }
   GSList *left_roots = getRootNodes(lhs); 
   GSList *iterator = left_roots;
   
   /* Add the root nodes to the search plan before traversing the graph.
    * Optimisation: if the rule has no root nodes, this and the next 
    * while loop are unnecessary. */ 
   while(iterator != NULL)
   {
      Node *root = (Node *)iterator->data;
      addSearchOp(searchplan, 'r', root->index);      
      iterator = iterator->next;
   }  
 
   /* Perform a depth-first traversal of the graph from its root nodes. */
   while(left_roots != NULL)
   {
      Node *root = (Node *)left_roots->data;
      if(!tagged_items[root->index]) 
         traverseNode(root, 'r', tagged_items, lhs_nodes);
      left_roots = left_roots->next;
   }

   /* Search for undiscovered nodes. These are nodes not reachable from a root
    * node.
    * Optimisation: check rules beforehand to see if they are left-root-connected.
    * If so, this code fragment is unnecessary. */
   for(index = 0; index < lhs->next_node_index; index++)
   {
      if(!tagged_items[index]) 
      {
         Node *node = getNode(lhs, index);
         traverseNode(node, 'n', tagged_items, lhs_nodes);
      }
   }

   free(tagged_items);      
}

void traverseNode(Node *node, char type, bool *tagged_items, int offset)
{
   tagged_items[node->index] = true;

   /* Root nodes are already in the searchplan. */
   if(type != 'r') 
   {      
      addSearchOp(searchplan, type, node->index);
   }
   
   int index;
   /* Search the node's incident edges for an untagged edge. Outedges
    * are arbitrarily examined first. If no such edges exist, the function
    * exits and control passes to the caller. */
   for(index = 0; index < node->next_out_edge_index; index++)
   {
      Edge *edge = getOutEdge(node, index);
      if(!tagged_items[offset + edge->index])
      {
         if(edge->source == edge->target)
            traverseEdge(edge, 'l', tagged_items, offset);
         else traverseEdge(edge, 's', tagged_items, offset);
      }
   }

   for(index = 0; index < node->next_in_edge_index; index++)
   {
      Edge *edge = getInEdge(node, index);
      if(!tagged_items[offset + edge->index]) 
      {
         if(edge->source == edge->target)
            traverseEdge(edge, 'l', tagged_items, offset);
         else traverseEdge(edge, 't', tagged_items, offset);
      }
   }
}


void traverseEdge(Edge *edge, char type, bool *tagged_items, int offset)
{
   tagged_items[offset + edge->index] = true;

   addSearchOp(searchplan, type, edge->index);

   /* If the edge is a loop, nothing is gained from examining the edge's
    * incident node as it was examined by the caller. */
   if(type == 'l') return;

   /* Check if the edge's source and target are untagged. The target is
    * arbitrarily examined first. If both nodes are tagged, the function exits
    * and control passes to the caller. */
   if(type == 's')
   {
      Node *target = edge->target;
      if(!tagged_items[target->index])
      {
         if(edge->bidirectional) 
              traverseNode(target, 'b', tagged_items, offset);
         else traverseNode(target, 'i', tagged_items, offset);
      }
   }      
   else 
   {
      Node *source = edge->source;
      if(!tagged_items[source->index])
      {
         if(edge->bidirectional) 
              traverseNode(source, 'b', tagged_items, offset);
         else traverseNode(source, 'o', tagged_items, offset);  
      }
   }
}


Searchplan *initialiseSearchplan(void)
{
   Searchplan *new_plan = malloc(sizeof(Searchplan));

   if(new_plan == NULL)
   {
     print_to_log("Error: Memory exhausted during searchplan "
                  "construction.\n");
     exit(1);
   }   

   new_plan->first = NULL;
   new_plan->last = NULL;

   return new_plan;
}

void addSearchOp(Searchplan *plan, char type, int index)
{
   SearchOp *new_op = malloc(sizeof(SearchOp));

   if(new_op == NULL)
   {
     print_to_log("Error: Memory exhausted during search operation "
                  "construction.\n");
     exit(1);
   }   
 
   if(type == 'e' || type == 's' || type == 't' || type == 'l')
        new_op->is_node = false;
   else new_op->is_node = true;
   new_op->type = type;
   new_op->index = index;

   if(plan->last == NULL)
   {
      new_op->next = NULL;
      plan->first = new_op;
      plan->last = new_op;  
   }
   else
   {
      new_op->next = NULL;
      plan->last->next = new_op;
      plan->last = new_op;
   }
}  

void printSearchplan(Searchplan *plan)
{ 
   if(plan->first == NULL) printf("Empty searchplan.\n");
   else
   {
      SearchOp *iterator = plan->first;
      while(iterator != NULL)
      {
         if(iterator->is_node) printf("Node\n");
         else printf("Edge\n");
         printf("Type: %c\nIndex: %d\n\n", iterator->type, iterator->index);
         iterator = iterator->next;  
      }
   }
}

void freeSearchplan(Searchplan *plan)
{
   if(plan == NULL) return;
   if(plan->first == NULL) free(plan);
   else
   {
      SearchOp *iterator = plan->first;
      while(iterator != NULL)
      {
         SearchOp *temp = iterator;
         iterator = iterator->next;  
         free(temp);
      }
      free(plan);
   }
}

