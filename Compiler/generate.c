/* ///////////////////////////////////////////////////////////////////////////

  ===================================
  generate.c - Chris Bak (28/10/2014)
  ===================================

/////////////////////////////////////////////////////////////////////////// */

#define PTH printToHeader
#define PTS printToSource
#define PTSI printToSourceI

#include "generate.h"

FILE *match_header = NULL;
FILE *match_source = NULL;
 
Searchplan *searchplan = NULL;

void generateMatchingCode(Graph *lhs, bool *dangling_nodes, string rule_name,
                          RuleData *rule_data)
{
   /* Create the searchplan. */
   generateSearchplan(lhs); 

   if(searchplan->first == NULL)
   {
      print_to_log("Error: empty searchplan. Aborting.\n");
      freeSearchplan(searchplan);
      return;
   }

   /* Create files match_<rule_name>.h and match_<rule_name>.c */
   int length = strlen(rule_name) + 8;
   char header_name[length];
   char source_name[length];
   strcpy(header_name, "match_");
   strcat(header_name, rule_name);
   strcpy(source_name, header_name);
   strcat(header_name, ".h");
   strcat(source_name, ".c");

   match_header = fopen(header_name, "w");
   if(match_header == NULL) { 
     perror(header_name);
     exit(1);
   }  

   match_source = fopen(source_name, "w");
   if(match_source == NULL) { 
     perror(source_name);
     exit(1);
   }

   PTH("#include \"globals.h\"\n"
      "#include \"graph.h\"\n"
      "#include \"match.h\"\n\n"
      "#define LEFT_NODES %d\n"
      "#define LEFT_EDGES %d\n\n"
      "extern Morphism *morphism;\n\n",
      lhs->number_of_nodes, lhs->number_of_edges);

   PTS("#include \"match_%s.h\"\n\n", rule_name);

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
              PTS("static bool match_n%d(Graph *host, Edge *edge, Morphism *morphism,\n"
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

              edge = getEdge(lhs, operation->index);
              PTS("static bool match_e%d(Graph *host, Node *node, Morphism *morphism,\n"
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
   emitMainFunction(rule_name, searchplan->first, rule_data);
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
              emitNodeMatcher(node, false, dangling_nodes, operation->next);
              break;

         case 'r': 

              node = getNode(lhs, operation->index);
              emitNodeMatcher(node, true, dangling_nodes, operation->next);
              break;

         case 'i': 

         case 'o': 

         case 'b':

              node = getNode(lhs, operation->index);
              emitNodeFromEdgeMatcher(node, type, dangling_nodes, 
                                      operation->next);
              break;

         case 'e': 

              edge = getEdge(lhs, operation->index);
              emitEdgeMatcher(edge, operation->next);
              break;

         case 's': 

         case 't':

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

   emitRuleApplicationCode(string rule_name, RuleData *rule_data);

   fclose(match_header);
   fclose(match_source);
}


void emitMainFunction(string rule_name, SearchOp *first_op)
{
   /* TODO: Function name should also contain procedural scope. This could be 
    * applied to the rule name at AST transformation time. */
   char item;
   if(first_op->is_node) item = 'n';
   else item = 'e';

   PTH("Graph *match_%s(Graph *host);\n", rule_name);
   PTS("Graph *match_%s(Graph *host)\n"
       "{\n" 
       "   morphism = makeMorphism();\n\n"
       "   int host_nodes = host->number_of_nodes;\n"
       "   int host_edges = host->number_of_edges;\n\n"
       "   if(LEFT_NODES > host_nodes || LEFT_EDGES > host_edges)\n"
       "   {\n"
       "      freeMorphism(morphism);\n"
       "      return NULL;\n"
       "   }\n\n"
       "   /* Initialise variables. */\n"
       "   int index = 0;\n"
       "   bool *matched_nodes = calloc(host_nodes, sizeof(bool));\n"
       "   if(matched_nodes == NULL)\n"
       "   {\n"
       "      print_to_log(\"Error: Memory exhausted during matched nodes \"\n"
       "                   \"table construction.\\n\");\n"
       "      exit(1);\n"
       "   }\n"
       "   for(index = 0; index < host_nodes; index++)\n"
       "   {\n"
       "      matched_nodes[index] = false;\n"
       "   }\n\n"
       "   bool *matched_edges = NULL;\n"
       "   if(host_edges > 0)\n"
       "   {\n"
       "      matched_edges = calloc(host_edges, sizeof(bool));\n"
       "      if(matched_edges == NULL)\n"
       "      {\n"
       "         print_to_log(\"Error: Memory exhausted during matched edges \"\n"
       "                      \"table construction.\\n\");\n"
       "         exit(1);\n"
       "      }\n"
       "      for(index = 0; index < host_edges; index++)\n"
       "      {\n"
       "         matched_edges[index] = false;\n"
       "      }\n"
       "   }\n\n"
       "   bool match_found = match_%c%d(host, morphism, matched_nodes,\n"
       "                                matched_edges);\n\n"
       "   free(matched_nodes);\n"
       "   free(matched_edges);\n\n"
       "   if(match_found) return apply_%s(host, morphism);\n"
       "   else freeMorphism(morphism);\n\n"
       "   return NULL;\n"
       "}\n",
       rule_name, item, first_op->index, rule_name);
}


void emitNodeMatcher(Node *left_node, bool is_root, bool *dangling_nodes,
                     SearchOp *next_op)
{
   PTS("static bool match_n%d(Graph *host, Morphism *morphism,\n"
       "                     bool *matched_nodes, bool *matched_edges)\n"
       "{\n", left_node->index);

   if(is_root) PTSI("GSList *nodes = getRootNodes(host);\n", 3);
   else PTSI("GSList *nodes = getNodesByLabel(host, %d);\n", 3,
             left_node->label_class);

   PTSI("while(nodes != NULL)\n", 3);
   PTSI("{\n", 3);
   PTSI("Node *host_node = (Node *)nodes->data;\n", 6);
   PTSI("int index = host_node->index;\n\n", 6);
   PTSI("/* Check if the host node has already been matched. */\n", 6);
   PTSI("if(matched_nodes[index])\n", 6);    
   PTSI("{\n", 6);
   PTSI("nodes = nodes->next;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 
   
   /* If !is_root, then the candidate nodes are obtained by label class.
    * In that case, there is no need to explicitly check the label class
    * of the host node. */
   if(is_root)
   {
      PTSI("/* Check label class, mark and degrees. */\n", 6);
      PTSI("if(host_node->label_class != %d)\n", 6, left_node->label_class);
      PTSI("{\n", 6);
      PTSI("nodes = nodes->next;\n", 9);
      PTSI("continue;\n", 9);
      PTSI("}\n\n", 6); 
   }
   else PTSI("/* Check mark and degrees. */\n", 6);

   PTSI("if(host_node->label->mark != %d)\n", 6, left_node->label->mark);
   PTSI("{\n", 6);
   PTSI("nodes = nodes->next;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 

   /* If the node is dangling, only host nodes with equal degrees are 
    * candidates to be matched. */
   if(dangling_nodes[left_node->index])
   {
      PTSI("/* Matching a node to be deleted: the degrees must agree. */\n", 6);
      PTSI("if(host_node->indegree != %d)\n", 6, left_node->indegree);
   }
   else PTSI("if(host_node->indegree < %d)\n", 6, left_node->indegree);
   PTSI("{\n", 6);
   PTSI("nodes = nodes->next;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 

   if(dangling_nodes[left_node->index])
        PTSI("if(host_node->outdegree != %d)\n", 6, left_node->outdegree);
   else PTSI("if(host_node->outdegree < %d)\n", 6, left_node->outdegree);
   PTSI("{\n", 6);
   PTSI("nodes = nodes->next;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 

   PTSI("/* Label matching code does not exist yet. */\n", 6);
   /* TODO: Call to label matcher goes here. */
   PTSI("bool nodes_match = host_node->label->list_length == 0;\n", 6);
   PTSI("if(nodes_match)\n", 6);
   PTSI("{\n", 6);
   PTSI("StackData *mapping = malloc(sizeof(StackData));\n", 9);
   PTSI("if(mapping == NULL)\n", 9);
   PTSI("{\n", 9);
   PTSI("print_to_log(\"Error: Memory exhausted during mapping construction."
        "\\n\");\n", 12);
   PTSI("exit(1);\n", 12);
   PTSI("}\n", 9);
   PTSI("mapping->map.left_index = %d;\n", 9, left_node->index);
   PTSI("mapping->map.host_index = index;\n", 9);
   PTSI("push(morphism->node_images, mapping);\n", 9);
   PTSI("matched_nodes[index] = true;\n", 9);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) 
   {
      PTSI("if(result) return true;\n", 9);
      PTSI("else\n", 9);
      PTSI("{\n", 9);
      PTSI("pop(morphism->node_images);\n", 12);
      PTSI("matched_nodes[index] = false;\n", 12);
      PTSI("}\n", 9);
   }
   PTSI("}\n", 6);
   PTSI("nodes = nodes->next;\n", 6);
   PTSI("}\n", 3);
   PTSI("return false;\n", 3);
   PTS("}\n\n");
}


void emitNodeFromEdgeMatcher(Node *left_node, char type, bool *dangling_nodes,
                             SearchOp *next_op)
{
   PTS("static bool match_n%d(Graph *host, Edge *edge, Morphism *morphism,\n"
       "                     bool *matched_nodes, bool *matched_edges)\n"
       "{\n", left_node->index);

   /* First trying the target of the image of a bidirectional edge is
    * completely arbitrary. Might be an idea to code a "coin flip" to
    * choose the incident node. */ 
   if(type == 'i' || type == 'b') PTSI("Node *host_node = getTarget(edge);\n", 3);
   else PTSI("Node *host_node = getSource(edge);\n", 3);
   PTSI("int index = host_node->index;\n\n", 3);

   PTSI("/* This is the only node to check, so perform all the preliminaries\n"
        "    * in one step. */\n", 3);
   PTSI("if(matched_nodes[index] ||\n", 3); 
   PTSI("host_node->label_class != %d ||\n", 6, left_node->label_class);
   PTSI("host_node->label->mark != %d ||\n", 6, left_node->label->mark);    
   /* If the node is dangling, only host nodes with equal degrees are 
    * candidates to be matched. */
   if(dangling_nodes[left_node->index])
   {
      PTSI("/* Matching a node to be deleted: the degrees must agree. */\n", 6);
      PTSI("host_node->indegree != %d ||\n" , 6, left_node->indegree);
   }
   else PTSI("host_node->indegree < %d ||\n" , 6, left_node->indegree);
   if(dangling_nodes[left_node->index])
        PTSI("host_node->outdegree != %d ||\n" , 6, left_node->outdegree);
   else PTSI("host_node->outdegree < %d)\n", 6, left_node->outdegree);
   if(type == 'b')
   {
      PTSI("{\n", 3); 
      PTSI("/* Matching from bidirectional edge: check the second incident node. */\n", 6);
      if(type == 'i' || type == 'b') PTSI("host_node = getSource(edge);\n", 6);
      else PTSI("host_node = getTarget(edge);\n", 6);
      PTSI("index = host_node->index;\n\n", 6);
      PTSI("if(matched_nodes[index] ||\n", 6); 
      PTSI("host_node->label_class != %d ||\n", 9, left_node->label_class);
      PTSI("host_node->label->mark != %d ||\n", 9, left_node->label->mark);    
      if(dangling_nodes[left_node->index])
      {
         PTSI("/* Matching a node to be deleted: the degrees must agree. */\n", 6);
         PTSI("host_node->indegree != %d ||\n" , 6, left_node->indegree);
      }
      else PTSI("host_node->indegree < %d ||\n" , 6, left_node->indegree);
      if(dangling_nodes[left_node->index])
           PTSI("host_node->outdegree != %d ||\n" , 6, left_node->outdegree);
      else PTSI("host_node->outdegree < %d)\n", 6, left_node->outdegree);
      PTSI("{\n", 6); 
      PTSI("pop(morphism->edge_images);\n", 9);
      PTSI("matched_edges[edge->index] = false;\n", 9);
      PTSI("return false;\n", 9);
      PTSI("}\n", 6);
      PTSI("}\n\n", 3);
   }
   else 
   {
      PTSI("{\n", 3); 
      PTSI("pop(morphism->edge_images);\n", 6);
      PTSI("matched_edges[edge->index] = false;\n", 6);
      PTSI("return false;\n", 6);
      PTSI("}\n\n", 3);
   }
      

   /* TODO: Call to label matcher goes here. */
   PTSI("bool nodes_match = host_node->label->list_length == 0;\n", 3);
   PTSI("if(nodes_match)\n", 3);
   PTSI("{\n", 3);
   PTSI("StackData *mapping = malloc(sizeof(StackData));\n", 6);
   PTSI("if(mapping == NULL)\n", 6);
   PTSI("{\n", 6);
   PTSI("print_to_log(\"Error: Memory exhausted during mapping construction."
        "\\n\");\n", 9);
   PTSI("exit(1);\n", 9);
   PTSI("}\n", 6);
   PTSI("mapping->map.left_index = %d;\n", 6, left_node->index);
   PTSI("mapping->map.host_index = index;\n", 6);
   PTSI("push(morphism->node_images, mapping);\n", 6);
   PTSI("matched_nodes[index] = true;\n", 6);
   bool total_match = emitNextMatcherCall(next_op, 6); 
   if(!total_match)
   {
      PTSI("if(result) return true;\n", 6);
      PTSI("else\n", 6);
      PTSI("{\n", 6);
      PTSI("pop(morphism->node_images);\n", 9);
      PTSI("matched_nodes[index] = false;\n", 9);
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
   PTSI("Edge *host_edge = (Edge *)edges->data;\n", 3);
   PTSI("int index = host_edge->index;\n\n", 3);

   PTSI("/* Check if the host edge has already been matched. */\n", 6);
   PTSI("if(matched_edges[index])\n", 6);    
   PTSI("{\n", 6);
   PTSI("edges = edges->next;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 

   PTSI("/* Check label class and mark. */\n", 6);
   PTSI("if(host_edge->label_class != %d)\n", 6, left_edge->label_class);
   PTSI("{\n", 6);
   PTSI("edges = edges->next;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 

   PTSI("if(host_edge->label->mark != %d)\n", 6, left_edge->label->mark);
   PTSI("{\n", 6);
   PTSI("edges = edges->next;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 

   /* TODO: Call to label matcher goes here. */
   PTSI("bool edges_match = host_edge->label->list_length == 0;\n", 3);
   PTSI("if(edges_match)\n", 6);
   PTSI("{\n", 6);
   PTSI("StackData *mapping = malloc(sizeof(StackData));\n", 9);
   PTSI("if(mapping == NULL)\n", 9);
   PTSI("{\n", 9);
   PTSI("print_to_log(\"Error: Memory exhausted during mapping construction."
        "\\n\");\n", 12);
   PTSI("exit(1);\n", 12);
   PTSI("}\n", 9);
   PTSI("mapping->map.left_index = %d;\n", 9, left_edge->index);
   PTSI("mapping->map.host_index = index;\n", 9);
   PTSI("push(morphism->edge_images, mapping);\n", 9);
   PTSI("matched_edges[index] = true;\n", 9);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match)
   {
      PTSI("if(result) return true;\n", 9);
      PTSI("else\n", 9);
      PTSI("{\n", 9);
      PTSI("pop(morphism->edge_images);\n", 12);
      PTSI("matched_edges[index] = false;\n", 12);
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
   PTS("static bool match_e%d(Graph *host, Node *node, Morphism *morphism,\n"
       "                     bool *matched_nodes, bool *matched_edges)\n"
       "{\n"
       " int counter;\n", left_edge->index);

   if(type == 's')
      PTSI("for(counter = 0; counter < node->outdegree; counter++)\n", 3);
   else PTSI("for(counter = 0; counter < node->indegree; counter++)\n", 3);

   PTSI("{\n", 3);
   if(type == 's') PTSI("Edge *host_edge = getOutEdge(node, counter);\n", 6);
   else PTSI("Edge *host_edge = getInEdge(node, counter);\n", 6);
   PTSI("int index = host_edge->index;\n\n", 6);
   PTSI("/* Check if the host edge has already been matched. */\n", 6);
   PTSI("if(matched_edges[index])\n", 6);    
   PTSI("{\n", 6);
   PTSI("index++;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 

   PTSI("/* Check label class and mark. */\n", 6);
   PTSI("if(host_edge->label_class != %d)\n", 6, left_edge->label_class);
   PTSI("{\n", 6);
   PTSI("index++;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 

   PTSI("if(host_edge->label->mark != %d)\n", 6, left_edge->label->mark);
   PTSI("{\n", 6);
   PTSI("index++;\n", 9);
   PTSI("continue;\n", 9);
   PTSI("}\n\n", 6); 

   /* TODO: Call to label matcher goes here. */
   PTSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
   PTSI("if(edges_match)\n", 6);
   PTSI("{\n", 6);
   PTSI("StackData *mapping = malloc(sizeof(StackData));\n", 9);
   PTSI("if(mapping == NULL)\n", 9);
   PTSI("{\n", 9);
   PTSI("print_to_log(\"Error: Memory exhausted during mapping construction."
        "\\n\");\n", 12);
   PTSI("exit(1);\n", 12);
   PTSI("}\n", 9);
   PTSI("mapping->map.left_index = %d;\n", 9, left_edge->index);
   PTSI("mapping->map.host_index = index;\n", 9);
   PTSI("push(morphism->edge_images, mapping);\n", 9);
   PTSI("matched_edges[index] = true;\n", 9);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) PTSI("if(result) return true;\n", 9);

   if(left_edge->bidirectional)
   {
      PTSI("}\n", 6);
      PTSI("}\n\n", 3);
      if(type == 's')
         PTSI("for(counter = 0; counter < node->indegree; counter++)\n", 3);
      else PTSI("for(counter = 0; counter < node->outdegree; counter++)\n", 3);

      PTSI("{\n", 3);
      if(type == 's') PTSI("Edge *host_edge = getInEdge(node, counter);\n", 6);
      else PTSI("Edge *host_edge = getOutEdge(node, counter);\n", 6);
      PTSI("int index = host_edge->index;\n\n", 6);
      PTSI("/* Check if the host edge has already been matched. */\n", 6);
      PTSI("if(matched_edges[index])\n", 6);    
      PTSI("{\n", 6);
      PTSI("index++;\n", 9);
      PTSI("continue;\n", 9);
      PTSI("}\n\n", 6); 

      PTSI("/* Check label class and mark. */\n", 6);
      PTSI("if(host_edge->label_class != %d)\n", 6, left_edge->label_class);
      PTSI("{\n", 6);
      PTSI("index++;\n", 9);
      PTSI("continue;\n", 9);
      PTSI("}\n\n", 6); 

      PTSI("if(host_edge->label->mark != %d)\n", 6, left_edge->label->mark);
      PTSI("{\n", 6);
      PTSI("index++;\n", 9);
      PTSI("continue;\n", 9);
      PTSI("}\n\n", 6); 

      /* TODO: Call to label matcher goes here. */
      PTSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
      PTSI("if(edges_match)\n", 6);
      PTSI("{\n", 6);
      PTSI("StackData *mapping = malloc(sizeof(StackData));\n", 9);
      PTSI("if(mapping == NULL)\n", 9);
      PTSI("{\n", 9);
      PTSI("print_to_log(\"Error: Memory exhausted during mapping construction."
         "\\n\");\n", 12);
      PTSI("exit(1);\n", 12);
      PTSI("}\n", 9);
      PTSI("mapping->map.left_index = %d;\n", 9, left_edge->index);
      PTSI("mapping->map.host_index = index;\n", 9);
      PTSI("push(morphism->edge_images, mapping);\n", 9);
      PTSI("matched_edges[index] = true;\n", 9);
      bool total_match = emitNextMatcherCall(next_op, 9);
      if(!total_match) 
      {
         PTSI("if(result) return true;\n", 9);   
         PTSI("else\n", 9);
         PTSI("{\n", 9);
         PTSI("pop(morphism->edge_images);\n", 12);
         PTSI("matched_edges[index] = false;\n", 12);
         PTSI("}\n", 9);
         PTSI("}\n", 6);
         PTSI("else index++;\n", 6);
         PTSI("}\n", 3);
      }
   }
   else
   {  
      PTSI("else\n", 9);
      PTSI("{\n", 9);
      PTSI("pop(morphism->edge_images);\n", 12);
      PTSI("matched_edges[index] = false;\n", 12);
      PTSI("}\n", 9);
      PTSI("}\n", 6);
      PTSI("else index++;\n", 6);
      PTSI("}\n", 3);
   }
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

void emitRuleApplicationCode(string rule_name, Graph *lhs, Graph *rhs,
                             RuleData *rule_data)
{
   PTH("Graph *apply_%s(Graph *host, Morphism *morphism);\n", rule_name);
   PTS("Graph *apply_%s(Graph *host, Morphism *morphism)\n", rule_name);
   PTS("{\n");
  
   int index = 0;

   if(rule_data->deleted_edges != NULL)
   {
      PTSI("/* Delete edges. */\n", 3);
      while(rule_data->deleted_edges[index] >= 0)
      {
         PTSI("

         index++;
      }
      


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
   for(index = 0; index < lhs_size; index++)
   {
      tagged_items[index] = false;
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
   for(index = 0; index < lhs_nodes; index++)
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
   for(index = 0; index < node->outdegree; index++)
   {
      Edge *edge = getOutEdge(node, index);
      if(!tagged_items[offset + edge->index]) 
         traverseEdge(edge, 's', tagged_items, offset);
   }

   for(index = 0; index < node->indegree; index++)
   {
      Edge *edge = getInEdge(node, index);
      if(!tagged_items[offset + edge->index]) 
         traverseEdge(edge, 't', tagged_items, offset);
   }
}


void traverseEdge(Edge *edge, char type, bool *tagged_items, int offset)
{
   tagged_items[offset + edge->index] = true;

   addSearchOp(searchplan, type, edge->index);

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
 
   if(type == 'e' || type == 's' || type == 't') new_op->is_node = false;
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

