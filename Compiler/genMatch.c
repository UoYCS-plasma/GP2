/* ////////////////////////////////////////////////////////////////////////////

 * The functions in this file print various macros. macros.h contains their
 * definitions.

/////////////////////////////////////////////////////////////////////////// */

#include "genMatch.h"

FILE *rule_header = NULL;
FILE *rule_source = NULL;
 
Searchplan *searchplan = NULL;

void generateRuleCode(Rule *rule)
{
   string rule_name = rule->name;
   /* Create files runtime/<rule_name>.h and runtime/<rule_name>.c */
   int length = strlen(rule_name) + 11;

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
   strcpy(header_name, "runtime/");
   strcpy(source_name, "runtime/");
   strcat(header_name, rule_name);
   strcat(source_name, rule_name);
   strcat(header_name, ".h");
   strcat(source_name, ".c");

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

   PTRH("#include \"../globals.h\"\n"
        "#include \"../graph.h\"\n"
        "#include \"../macros.h\"\n"
        "#include \"match.h\"\n"
        "#include \"host.h\"\n"
        "#include \"../stack.h\"\n\n");
   PTRS("#include \"%s.h\"\n\n", rule_name);

   if(rule->lhs == NULL) generateApplicationCode(rule, true, false);
   else
   {
      if(rule->rhs == NULL)
      {
         generateMatchingCode(rule_name, rule->number_of_variables, rule->lhs,
                              rule->deleted_nodes);
         generateApplicationCode(rule, false, true);
      }
      else
      {
         generateMatchingCode(rule_name, rule->number_of_variables, rule->lhs,
                              rule->deleted_nodes);
         generateApplicationCode(rule, false, false);
      }
   }
   fclose(rule_header);
   fclose(rule_source);
   return;
}

void generateMatchingCode(string rule_name, int number_of_variables,
                          Graph *lhs, ItemList *deleted_nodes)
{
   searchplan = generateSearchplan(lhs); 

   if(searchplan->first == NULL)
   {
      print_to_log("Error: empty searchplan. Aborting.\n");
      freeSearchplan(searchplan);
      return;
   }

   SearchOp *operation = searchplan->first;
   Node *node = NULL;
   Edge *edge = NULL;

   /* The searchplan is iterated over twice. On the first iteration, the prototypes
    * of the matching functions are printed to the generated source file. */   
   while(operation != NULL)
   {
      char type = operation->type;

      switch(type) {
        
         case 'n':

         case 'r':

              node = getNode(lhs, operation->index);
              if(operation->next == NULL)
                 PTRS("static bool match_n%d(Morphism *morphism, "
                      "int *matched_nodes);\n", node->index);
              else
                 PTRS("static bool match_n%d(Morphism *morphism, "
                      "int *matched_nodes, int *matched_edges);\n", 
                      node->index);
              break;

         case 'i': 

         case 'o': 

         case 'b':

              node = getNode(lhs, operation->index);
              if(operation->next == NULL)
                 PTRS("static bool match_n%d(Morphism *morphism, "
                      "Edge *host_edge, int *matched_nodes);\n",
                      node->index);
              else
                 PTRS("static bool match_n%d(Morphism *morphism, "
                      "Edge *host_edge, int *matched_nodes, "
                      "int *matched_edges);\n", node->index);
              break;

         case 'e': 

         case 's': 

         case 't':

         case 'l':

              edge = getEdge(lhs, operation->index);
              if(operation->next == NULL)
                 PTRS("static bool match_e%d(Morphism *morphism, "
                      "int *matched_edges);\n", edge->index);
              else
                 PTRS("static bool match_e%d(Morphism *morphism, "
                      "int *matched_nodes, int *matched_edges);\n", 
                      edge->index);
              break;

         default:
              print_to_log("Error (generateMatchingCode): Unexpected "
                           "operation type %c.\n", operation->type);
              break;
      }
      operation = operation->next;
   }
 
   emitRuleMatcher(rule_name, searchplan->first, lhs->number_of_nodes, 
                   lhs->number_of_edges, number_of_variables);
   PTRS("\n\n");

   /* The second iteration of the searchplan prints the definitions of the 
    * functions declared in the first iteration. The type of the searchplan 
    * operation determines which emitMatcher function is called and which
    * parameters are passed. */
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

              edge = getEdge(lhs, operation->index);
              emitEdgeFromNodeMatcher(edge, false, operation->next);
              break;

         case 't':
         
              edge = getEdge(lhs, operation->index);
              emitEdgeToNodeMatcher(edge, operation->next);
              break;

         case 'l':

              edge = getEdge(lhs, operation->index);
              emitEdgeFromNodeMatcher(edge, true, operation->next);
              break;
         
         default:
              print_to_log("Error (generateMatchingCode): Unexpected "
                           "operation type %c.\n", operation->type);
              break;
      }
      operation = operation->next;
   }
   freeSearchplan(searchplan);
}


void emitRuleMatcher(string rule_name, SearchOp *first_op, int left_nodes, 
                     int left_edges, int variables)
{
   char item;
   if(first_op->is_node) item = 'n';
   else item = 'e';

   PTRH("Morphism *match%s(void);\n", rule_name);
   PTRS("\nstatic int left_nodes = %d, left_edges = %d;\n"
        "\nMorphism *match%s(void)\n"
        "{\n" 
        "   if(left_nodes > host->number_of_nodes ||\n"
        "      left_edges > host->number_of_edges) return false;\n\n"
        "   Morphism *morphism = makeMorphism(left_nodes, left_edges, %d);\n\n"
        "   MAKE_MATCHED_NODES_ARRAY\n",
        left_nodes, left_edges, rule_name, variables);

   /* The matched edges array should not be created when there are 0 host
    * edges. Hence the following macro is only generated if there is at least
    * one left edge. If there are 0 host edges, the generated code will exit
    * before this point is reached because left_edges > host_edges. 
    * If there are no left edges, then there is no purpose for a matched edges
    * array regardless of the number of host edges. */
   if(left_edges > 0)
      PTRS("   MAKE_MATCHED_EDGES_ARRAY\n");

   if(first_op->next == NULL)
      PTRS("\n   bool match_found = match_%c%d(morphism, matched_nodes);\n",
           item, first_op->index);
   else
      PTRS("\n   bool match_found = match_%c%d(morphism, matched_nodes, "
           "matched_edges);\n", item, first_op->index);
   PTRS("   if(match_found) return morphism;\n"
        "   freeMorphism(morphism);\n"
        "   return NULL;\n"
        "}\n");
}


void emitNodeMatcher(Node *left_node, bool is_root, ItemList *deleted_nodes,
                     SearchOp *next_op)
{
   int left_index = left_node->index;
   bool dangling_node = queryItemList(deleted_nodes, left_index);
   
   if(next_op == NULL)
      PTRS("static bool match_n%d(Morphism *morphism, int *matched_nodes)\n"
           "{\n", left_index);
   else
      PTRS("static bool match_n%d(Morphism *morphism, int *matched_nodes, "
           "int *matched_edges)\n"
           "{\n", left_index);

   /* Emit code to initialise the iteration over the candidate nodes. 
    * If the left node is rooted, interrogate the root node list of the host
    * graph, otherwise we query the appropriate nodes-by-label-class list. */
    if(is_root) 
       PTRS("   bool node_matched = false;\n"
            "   RootNodes *nodes = NULL;\n"
            "   for(nodes = getRootNodeList(host); nodes != NULL;"
            " nodes = nodes->next)\n"
            "   {\n"
            "      Node *host_node = getNode(host, nodes->index);\n");
    else
       PTRS("   bool node_matched = false;\n"
            "   int count;\n"
            "   LabelClassTable nodes = getNodesByLabel(host, %d);\n"
            "   for(count = 0; count < nodes.index; count++)\n"
            "   {\n"
            "      Node *host_node = getNode(host, nodes.items[count]);\n", 
            left_node->label_class);
    PTRS("      if(host_node == NULL) continue;\n\n"
         "      node_matched = false;\n"
         "      /* Set node_matched to true if the node has already been matched. */\n"
         "      CHECK_MATCHED_NODE\n\n");
   /* Emit code to check the node_matched flag and to test if the candidate 
    * host node is consistent with the left node with respect to label class, 
    * mark, and degrees. If not, the loop will continue without entering the 
    * potentially expensive label matching code. */
   PTRSI("/* Arguments: label class, mark, indegree, outdegree, bidegree. */\n", 6);
   if(dangling_node)
        PTRSI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d, %d) continue;\n\n", 6,
              left_node->label_class, left_node->label->mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree);
   else PTRSI("IF_INVALID_NODE(%d, %d, %d, %d, %d) continue;\n\n", 6,
              left_node->label_class, left_node->label->mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree); 

   PTRSI("/* Label matching code does not exist yet. */\n", 6);
   /* TODO: Call to label matcher goes here. */
   PTRSI("bool nodes_match = host_node->label->list_length == 0;\n", 6);
   PTRSI("if(nodes_match)\n", 6);
   PTRSI("{\n", 6);
   PTRSI("addNodeMap(morphism, %d, host_node->index);\n", 9, left_index);
   PTRSI("matched_nodes[%d] = host_node->index;\n", 9, left_index);

   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) 
   {
      PTRSI("if(result) return true;\n", 9);
      PTRSI("else\n", 9);
      PTRSI("{\n", 9);
      PTRSI("removeNodeMap(morphism);\n", 12);
      PTRSI("matched_nodes[%d] = -1;\n", 12, left_index);
      PTRSI("}\n", 9);
   }
   PTRSI("}\n", 6);
   PTRSI("}\n", 3);
   PTRSI("return false;\n", 3);
   PTRS("}\n\n");
}

/* Matching a node from a matched incident edge always follow an edge match in
 * the searchplan. Thus the generated function takes the host edge matched by  
 * the previous searchplan function as one of its arguments. It gets the
 * appropriate host node (source or target of the host edge) and checks if this
 * node is compatible with left_node. */
void emitNodeFromEdgeMatcher(Node *left_node, char type,
                             ItemList *deleted_nodes, SearchOp *next_op)
{
   int left_index = left_node->index;
   bool dangling_node = queryItemList(deleted_nodes, left_index);

   if(next_op == NULL)
      PTRS("static bool match_n%d(Morphism *morphism, Edge *host_edge, "
           "int *matched_nodes)\n"
           "{\n", left_index);
   else
      PTRS("static bool match_n%d(Morphism *morphism, Edge *host_edge, "
           "int *matched_nodes, int *matched_edges)\n"
           "{\n", left_index);

   if(type == 'i' || type == 'b') 
        PTRSI("Node *host_node = getNode(host, getTarget(host_edge));\n\n", 3);
   else PTRSI("Node *host_node = getNode(host, getSource(host_edge));\n\n", 3);
   PTRS("   /* Set node_matched to true if the node has already been matched. */\n"
        "   bool node_matched = false;\n"
        "   CHECK_MATCHED_NODE\n\n");
   /* Emit code to check the node_matched flag and to test if the candidate 
    * host node is consistent with the left node with respect to label class, 
    * mark, and degrees. If not, the loop will continue without entering the 
    * potentially expensive label matching code. */
   PTRSI(" /* Arguments: label class, mark, indegree, outdegree, bidegree. */\n", 3);
   if(dangling_node)
        PTRSI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d, %d)", 3,
              left_node->label_class, left_node->label->mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree);
   else PTRSI("IF_INVALID_NODE(%d, %d, %d, %d, %d)", 3,
              left_node->label_class, left_node->label->mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree); 

   if(type == 'b')
   {
      PTRS("\n   {\n"); 
      PTRSI("/* Matching from bidirectional edge: check the second incident node. */\n", 6);
      if(type == 'i' || type == 'b') 
           PTRSI("host_node = getNode(host, getSource(host_edge));\n\n", 6);
      else PTRSI("host_node = getNode(host, getTarget(host_edge));\n\n", 6);
      PTRS("      bool node_matched = false;\n"
           "      /* Set node_matched to true if the node has already been matched. */\n"
           "      CHECK_MATCHED_NODE\n\n");
      PTRSI(" /* Arguments: label class, mark, indegree, outdegree, bidegree. */\n", 6);
      if(dangling_node)
           PTRSI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d, %d) return false;\n", 6,
                 left_node->label_class, left_node->label->mark, 
                 left_node->indegree, left_node->outdegree, left_node->bidegree);
      else PTRSI("IF_INVALID_NODE(%d, %d, %d, %d, %d) return false;\n", 6,
                 left_node->label_class, left_node->label->mark, 
                 left_node->indegree, left_node->outdegree, left_node->bidegree); 
      PTRSI("}\n\n", 3);
   }
   else PTRS(" return false;\n\n");
      
   /* TODO: Call to label matcher goes here. */
   PTRSI("bool nodes_match = host_node->label->list_length == 0;\n", 3);
   PTRSI("if(nodes_match)\n", 3);
   PTRSI("{\n", 3);
   PTRSI("addNodeMap(morphism, %d, host_node->index);\n", 6, left_index);
   PTRSI("matched_nodes[%d] = host_node->index;\n", 6, left_index);

   bool total_match = emitNextMatcherCall(next_op, 6); 
   if(!total_match)
   {
      PTRSI("if(result) return true;\n", 6);
      PTRSI("else\n", 6);
      PTRSI("{\n", 6);
      PTRSI("removeNodeMap(morphism);\n", 9);
      PTRSI("matched_nodes[%d] = -1;\n", 9, left_index);
      PTRSI("}\n", 6);
   }
   PTRSI("}\n", 3);
   PTRSI("return false;\n", 3);
   PTRS("}\n\n");
}


void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op)
{
   if(next_op == NULL)
      PTRS("static bool match_e%d(Morphism *morphism, int *matched_edges)\n"
           "{\n", left_edge->index);
   else
      PTRS("static bool match_e%d(Morphism *morphism, int *matched_nodes, "
           "int *matched_edges)\n"
           "{\n", left_edge->index);

   PTRS("   int count;\n"
        "   bool edge_matched = false;\n"
        "   LabelClassTable edges = getEdgesByLabel(host, %d);\n"
        "   for(count = 0; count < edges.index; count++)"
        "   {\n"
        "      Edge *host_edge = getEdge(host, edges.items[count];\n\n",
        left_edge->label_class);
   PTRS("      /* Set edge_matched to true if the edge has already been matched. */\n"
        "      CHECK_MATCHED_EDGE\n\n"
        "      if(edge_matched) continue;\n\n");
   /* Emit code to test the matched_edge flag and if the candidate host edge 
    * is consistent with the left edge with respect to label class, mark, and
    * loopiness. If not, the loop will continue without entering the 
    * potentially expensive label matching code. */
   PTRSI(" /* Arguments: label class, mark. */\n", 6);
   if(left_edge->source == left_edge->target) 
        PTRSI("IF_INVALID_LOOP_EDGE(%d, %d)\n\n", 6,
              left_edge->label_class, left_edge->label->mark);
   else PTRSI("IF_INVALID_EDGE(%d, %d)\n\n", 6, 
              left_edge->label_class, left_edge->label->mark);
   PTRSI("/* If either endpoint has been matched, check that the corresponding\n", 6);
   PTRSI(" * endpoint of the host edge is the image of the node in question. */\n", 6);
   PTRSI("continue;\n\n", 6);
   PTRSI("int source_index = findHostIndex(morphism, %d);\n", left_edge->source, 6);
   PTRSI("if(source_index >= 0 && host_edge->source != source_index) continue;\n", 6);
   PTRSI("int target_index = findHostIndex(morphism, %d);\n", left_edge->target, 6);
   PTRSI("if(target_index >= 0 && host_edge->target != target_index) continue;\n", 6);

   /* TODO: Call to label matcher goes here. */
   PTRSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
   PTRSI("if(edges_match)\n", 6);
   PTRSI("{\n", 6);
   PTRSI("addEdgeMap(morphism, %d, host_edge->index);\n", 9, left_edge->index);
   PTRSI("matched_edges[%d] = host_edge->index;\n", 9, left_edge->index);

   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match)
   {
      PTRSI("if(result) return true;\n", 9);
      PTRSI("else\n", 9);
      PTRSI("{\n", 9);
      PTRSI("removeEdgeMap(morphism);\n", 12);
      PTRSI("matched_edges[%d] = -1;\n", 12, left_edge->index);
      PTRSI("}\n", 9);
   }
   PTRSI("}\n", 6);
   PTRSI("}\n", 3);
   PTRSI("return false;\n", 3);
   PTRS("}\n\n");
}

/* Unlike matching a node from an edge, the LHS-node from which this LHS-edge
 * is matched may not necessarily be the previously matched node in the 
 * searchplan. The generated code uses the index of the source of the LHS-edge 
 * to find the host node to which it has been matched (findHostIndex). Edges 
 * in the outedge list of that host node are the candidate edges to match 
 * left_edge. */
void emitEdgeFromNodeMatcher(Edge *left_edge, bool is_loop, SearchOp *next_op)
{
   if(next_op == NULL)
      PTRS("static bool match_e%d(Morphism *morphism, int *matched_edges)\n"
           "{\n", left_edge->index);
   else
      PTRS("static bool match_e%d(Morphism *morphism, int *matched_nodes, "
           "int *matched_edges)\n"
           "{\n", left_edge->index);

   PTRS("   int source_index = findHostIndex(morphism, %d);\n"
        "   if(source_index < 0) return false;\n"
        "   Node *host_node = getNode(host, source_index);\n"
        "   int target_index = findHostIndex(morphism, %d);\n\n"
        "   int counter;\n"
        "   bool edge_matched = false;\n"
        "   for(counter = host_node->out_index - 1; counter >= 0; counter--)\n",
        left_edge->source, left_edge->target);

   PTRSI("{\n", 3);
   PTRSI("Edge *host_edge = getEdge(host, getOutEdge(host_node, counter));\n", 6);
   PTRSI("if(host_edge == NULL) continue;\n\n", 6);
   PTRS("      edge_matched = false;\n"
        "      /* Set edge_matched to true if the edge has already been matched. */\n"
        "      CHECK_MATCHED_EDGE\n\n");
   PTRSI(" /* Arguments: label class, mark. */\n", 6);
   if(is_loop) 
        PTRSI("IF_INVALID_LOOP_EDGE(%d, %d) continue;\n\n", 6,
              left_edge->label_class, left_edge->label->mark);
   else PTRSI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
              left_edge->label_class, left_edge->label->mark);

   if(left_edge->bidirectional)
   {
      PTRSI("/* If the rule edge's target has been matched, check that either\n", 6);
      PTRSI(" * the source or target of the host edge is the image of the rule\n", 6);
      PTRSI(" * edge's target. */\n", 6);
      PTRSI("if(target_index >= 0 && host_edge->source != target_index &&\n", 6);
      PTRSI("   host_edge->target != target_index) continue;\n", 6);
   }
   else
   {
      PTRSI("/* If the rule edge's target has been matched, check that the\n", 6);
      PTRSI(" * target of the host edge is the image of the rule edge's target. */\n", 6);
      PTRSI("if(target_index >= 0 && host_edge->target != target_index) continue;\n", 6);
   }
 
   /* TODO: Call to label matcher goes here. */
   PTRSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
   PTRSI("if(edges_match)\n", 6);
   PTRSI("{\n", 6);
   PTRSI("addEdgeMap(morphism, %d, host_edge->index);\n", 9, left_edge->index);
   PTRSI("matched_edges[%d] = host_edge->index;\n", 9, left_edge->index);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) 
   {
      PTRSI("if(result) return true;\n", 9);
      PTRSI("else\n", 9);
      PTRSI("{\n", 9);
      PTRSI("removeEdgeMap(morphism);\n", 12);
      PTRSI("matched_edges[%d] = -1;\n", 12, left_edge->index);
      PTRSI("}\n", 9);
   }   
   PTRSI("}\n", 6);
   PTRSI("}\n\n", 3);

   if(left_edge->bidirectional)
   /* Emit code to try and match an edge in the opposite direction. */
   {
      PTRSI("for(counter = host_node->in_index - 1; counter >= 0; counter--)\n", 3);
      PTRSI("{\n", 3);
      PTRSI("Edge *host_edge = getEdge(host, getInEdge(host_node, counter));\n", 6);
      PTRSI("if(host_edge == NULL) continue;\n\n", 6);
      PTRS("      edge_matched = false;\n"
           "      /* Set edge_matched to true if the edge has already been matched. */\n"
           "      CHECK_MATCHED_EDGE\n\n");
      PTRSI(" /* Arguments: label class, mark. */\n", 6);
      if(is_loop)
           PTRSI("IF_INVALID_LOOP_EDGE(%d, %d) continue;\n\n", 6,
                 left_edge->label_class, left_edge->label->mark);
      else PTRSI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
                 left_edge->label_class, left_edge->label->mark);
      if(left_edge->bidirectional)
      {
         PTRSI("/* If the rule edge's source has been matched, check that either\n", 6);
         PTRSI(" * the source or target of the host edge is the image of the rule\n", 6);
         PTRSI(" * edge's source. */\n", 6);
         PTRSI("if(source_index >= 0 && host_edge->source != source_index &&\n", 6);
         PTRSI("   host_edge->target != source_index) continue;\n", 6);
      }
      else
      {
         PTRSI("/* If the rule edge's source has been matched, check that the source\n", 6);
         PTRSI(" * of the host edge is the image of the rule edge's source. */\n", 6);
         PTRSI("if(source_index >= 0 && host_edge->source != source_index) continue;\n", 6);
      }

      /* TODO: Call to label matcher goes here. */
      PTRSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
      PTRSI("if(edges_match)\n", 6);
      PTRSI("{\n", 6);
      PTRSI("addEdgeMap(morphism, %d, host_edge->index);\n", 9, left_edge->index);
      PTRSI("matched_edges[%d] = host_edge->index;\n", 9, left_edge->index);
   
      bool total_match = emitNextMatcherCall(next_op, 9);
      if(!total_match) 
      {
         PTRSI("if(result) return true;\n", 9);
         PTRSI("else\n", 9);
         PTRSI("{\n", 9);
         PTRSI("removeEdgeMap(morphism);\n", 12);
         PTRSI("matched_edges[%d] = -1;\n", 12, left_edge->index);
         PTRSI("}\n", 9);
      }
      PTRSI("}\n", 6);
      PTRSI("}\n", 3);
   }
   PTRSI("return false;\n", 3);
   PTRS("}\n\n");
}

/* Unlike matching a node from an edge, the LHS-node from which this LHS-edge
 * is matched may not necessarily be the previously matched node in the 
 * searchplan. The generated code uses the index of the target of the LHS-edge 
 * to find the host node to which it has been matched (findHostIndex). Edges 
 * in the inedge list of that host node are the candidate edges to match 
 * left_edge. */
void emitEdgeToNodeMatcher(Edge *left_edge, SearchOp *next_op)
{
   if(next_op == NULL)
      PTRS("static bool match_e%d(Morphism *morphism, int *matched_edges)\n"
           "{\n", left_edge->index);
   else
      PTRS("static bool match_e%d(Morphism *morphism, int *matched_nodes, "
           "int *matched_edges)\n"
           "{\n", left_edge->index);

   PTRS("   int target_index = findHostIndex(morphism, %d);\n"
        "   if(target_index < 0) return false;\n"
        "   Node *host_node = getNode(host, target_index);\n\n"
        "   int source_index = findHostIndex(morphism, %d);\n"
        "   int counter;\n\n"
        "   bool edge_matched = false;\n"
        "   for(counter = host_node->in_index - 1; counter >= 0; counter--)\n",
        left_edge->target, left_edge->source);

   PTRSI("{\n", 3);
   PTRSI("Edge *host_edge = getEdge(host, getInEdge(host_node, counter));\n", 6);
   PTRSI("if(host_edge == NULL) continue;\n\n", 6);
   PTRS("      edge_matched = false;\n"
        "      /* Set edge_matched to true if the edge has already been matched. */\n"
        "      CHECK_MATCHED_EDGE\n\n");
   PTRSI(" /* Arguments: label class, mark. */\n", 6);
   PTRSI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
         left_edge->label_class, left_edge->label->mark);
   if(left_edge->bidirectional)
   {
      PTRSI("/* If the rule edge's source has been matched, check that either\n", 6);
      PTRSI(" * the source or target of the host edge is the image of the rule\n", 6);
      PTRSI(" * edge's source. */\n", 6);
      PTRSI("if(source_index >= 0 && host_edge->source != source_index &&\n", 6);
      PTRSI("   host_edge->target != source_index) continue;\n", 6);
   }
   else
   {
      PTRSI("/* If the rule edge's source has been matched, check that the\n", 6);
      PTRSI(" * source of the host edge is the image of the rule edge's source. */\n", 6);
      PTRSI("if(source_index >= 0 && host_edge->source != source_index) continue;\n", 6);
   }

   /* TODO: Call to label matcher goes here. */
   PTRSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
   PTRSI("if(edges_match)\n", 6);
   PTRSI("{\n", 6);
   PTRSI("addEdgeMap(morphism, %d, host_edge->index);\n", 9, left_edge->index);
   PTRSI("matched_edges[%d] = host_edge->index;\n", 9, left_edge->index);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) 
   {
      PTRSI("if(result) return true;\n", 9);
      PTRSI("else\n", 9);
      PTRSI("{\n", 9);
      PTRSI("removeEdgeMap(morphism);\n", 12);
      PTRSI("matched_edges[%d] = -1;\n", 12, left_edge->index);
      PTRSI("}\n", 9);
   }  
   PTRSI("}\n", 6);
   PTRSI("}\n", 3);
   if(left_edge->bidirectional)
   /* Emit code to try and match an edge in the opposite direction. */
   {
      PTRSI("for(counter = host_node->out_index - 1; counter >= 0; counter--)\n", 3);
      PTRSI("{\n", 3);
      PTRSI("Edge *host_edge = getEdge(host, getOutEdge(host_node, counter));\n", 6);
      PTRSI("if(host_edge == NULL) continue;\n\n", 6);
      PTRS("      edge_matched = false;\n"
           "      /* Set edge_matched to true if the edge has already been matched. */\n"
           "      CHECK_MATCHED_EDGE\n\n");
      PTRSI(" /* Arguments: label class, mark. */\n", 6);
      PTRSI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
            left_edge->label_class, left_edge->label->mark);
      if(left_edge->bidirectional)
      {
         PTRSI("/* If the rule edge's target has been matched, check that either\n", 6);
         PTRSI(" * the source or target of the host edge is the image of the rule\n", 6);
         PTRSI(" * edge's target. */\n", 6);
         PTRSI("if(target_index >= 0 && host_edge->source != target_index &&\n", 6);
         PTRSI("   host_edge->target != target_index) continue;\n", 6);
      }
      else
      {
         PTRSI("/* If the rule edge's target has been matched, check that the\n", 6);
         PTRSI(" * target of the host edge is the image of the rule edge's target. */\n", 6);
         PTRSI("if(target_index >= 0 && host_edge->target != target_index) continue;\n", 6);
      }

      /* TODO: Call to label matcher goes here. */
      PTRSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
      PTRSI("if(edges_match)\n", 6);
      PTRSI("{\n", 6);
      PTRSI("addEdgeMap(morphism, %d, host_edge->index);\n", 9, left_edge->index);
      PTRSI("matched_edges[host_edge->index] = true;\n", 9);
   
      bool total_match = emitNextMatcherCall(next_op, 9);
      if(!total_match) 
      {
         PTRSI("if(result) return true;\n", 9);
         PTRSI("else\n", 9);
         PTRSI("{\n", 9);
         PTRSI("removeEdgeMap(morphism);\n", 12);
         PTRSI("matched_edges[%d] = -1;\n", 12, left_edge->index);
         PTRSI("}\n", 9);
      } 
      PTRSI("}\n", 6);
      PTRSI("}\n", 3);
   } 
   PTRSI("return false;\n", 3);
   PTRS("}\n\n");
}

bool emitNextMatcherCall(SearchOp *next_operation, int indent)
{
   if(next_operation == NULL)    
   {
      PTRSI("/* All items matched! */\n", indent);
      PTRSI("return true;\n", indent);
      return true;
   }
   switch(next_operation->type)
   {
      case 'n':
  
      case 'r':

           if(next_operation->next == NULL)
              PTRSI("bool result = match_n%d(morphism, matched_nodes);\n", 
	            indent, next_operation->index);
           else 
              PTRSI("bool result = match_n%d(morphism, matched_nodes, "
                    "matched_edges);\n", indent, next_operation->index);
           break;

      case 'i':

      case 'o':
 
      case 'b':

           if(next_operation->next == NULL)
              PTRSI("bool result = match_n%d(morphism, host_edge, "
                    "matched_nodes);\n", indent, next_operation->index);
           else 
              PTRSI("bool result = match_n%d(morphism, host_edge, "
                    "matched_nodes, matched_edges);\n", indent, 
                    next_operation->index);
           break;
  
      case 'e':

      case 's':
 
      case 't':

      case 'l':

           if(next_operation->next == NULL)
              PTRSI("bool result = match_e%d(morphism, matched_edges);\n",
                    indent, next_operation->index);
           else 
              PTRSI("bool result = match_e%d(morphism, matched_nodes, "
                    "matched_edges);\n", indent, next_operation->index);
           break;

      default:
 
           print_to_log("Error (emitNextMatcherCall): Unexpected "
                           "operation type %c.\n", next_operation->type);
           break;
   
   }
   return false;
}

void generateApplicationCode(Rule *rule, bool empty_lhs, bool empty_rhs)
{
   Graph *lhs = rule->lhs;
   Graph *rhs = rule->rhs;

   PTRH("void apply%s(Morphism *morphism);\n", rule->name);
   PTRS("void apply%s(Morphism *morphism)\n", rule->name);
   PTRS("{\n");

   /* If the LHS is the empty graph, emit code to add the complete RHS graph
    * to the host graph. */
   if(empty_lhs)
   {
      int index;
      PTRS("   /* Array of host node indices indexed by RHS node index. */\n"
           "   Node *map[%d];\n", rhs->number_of_nodes);
      for(index = 0; index < rhs->number_of_nodes; index++)
      {
         Node *rule_node = getNode(rhs, index);
         /* TODO: Evaluate rule_node->label. */
         PTRSI("map[%d] = addNode(host, %d, NULL);\n", 3, rule_node->index,
               rule_node->root);
      }
      NewEdgeList *iterator = rule->added_edges;
      /* TODO: Evaluate rule_edge->label. */
      while(iterator != NULL)
      {
         if(iterator->source_index == iterator->target_index)
            PTRSI("addEdge(host, false, NULL, map[%d], map[%d]);\n", 3,
                  iterator->source_index, iterator->source_index);
         else
            PTRSI("addEdge(host, false, NULL, map[%d], map[%d]);\n", 3,
                  iterator->source_index, iterator->target_index);
         iterator = iterator->next;
      }     
      PTRS("   free(map);\n"
           "   return;\n}\n\n");
      return;
   }

   /* If the RHS is the empty graph, emit code to remove the image of the RHS
    * from the host graph. This code is simple enough to define in a macro. */
   if(empty_rhs)
   {
      PTRS("   int count;\n"
           "   REMOVE_RHS\n" 
           "   freeMorphism(morphism);\n"
           "   return;\n"
           "}\n\n");
      return;
   }
   
   /* Using preserved items lists, populate the maps in the generated code. */
   PTRS("   /* Array of LHS nodes marking their status.\n"
        "    * -1 -> Delete; 0 -> Do nothing; 1 -> Relabel */\n"
        "   int node_map[%d];\n", lhs->number_of_nodes);
   int index;
   for(index = 0; index < lhs->number_of_nodes; index++)
   {
      PreservedItemList *item = queryPItemList(rule->preserved_nodes, index);
      if(item == NULL) 
      {
         PTRSI("node_map[%d] = -1;\n", 3, index);
         continue;
      }
      else
      {
         if(item->label_change)
              PTRSI("node_map[%d] = 1;\n", 3, index);
         else PTRSI("node_map[%d] = 0;\n", 3, index);
      }
   } 

   PTRS("\n   /* Array of LHS edges marking their status.\n"
        "    * -1 -> Delete; 0 -> Do nothing; 1 -> Relabel */\n"
        "   int edge_map[%d];\n", lhs->number_of_edges);
   for(index = 0; index < lhs->number_of_edges; index++)
   {
      PreservedItemList *item = queryPItemList(rule->preserved_edges, index);
      if(item == NULL) 
      {
         PTRSI("edge_map[%d] = -1;\n", 3, index);
         continue;
      }
      else
      {
         if(item->label_change)
              PTRSI("edge_map[%d] = 1;\n", 3, index);
         else PTRSI("edge_map[%d] = 0;\n", 3, index);
      }
   }

   /* TODO: Pass label of RHS items from rule->preserved_edges/nodes */
   PTRS("\n"
        "   int count, left_index, host_index;\n"
        "   /* Pops all edges in the morphism stack and deletes/preserves\n"
        "    * host items according to the entries in edge_map. */\n"
        "   PROCESS_EDGE_MORPHISMS\n" 
        "   /* Pops all node in the morphism stack and deletes/preserves\n"
        "    * host items according to the entries in node_map. */\n"
        /* It is worth nothing here that PROCESS_NODE_MORPHISMS updates the
         * node_map so that, for preserved left_nodes l, node_map[l] is the
         * index of its associated host node. This is used when adding edges
         * to the host graph. */
        "   PROCESS_NODE_MORPHISMS\n\n");

   ItemList *iterator_n = rule->added_nodes;
   if(iterator_n != NULL)
      PTRS("   /* Array of host node pointers indexed by RHS node index. */\n"
           "   Node *map[%d];\n\n", rhs->number_of_nodes);
   while(iterator_n != NULL)
   {   
      Node *rule_node = getNode(rhs, iterator_n->index);
      /* TODO: Evaluate rule_node->label. */
      PTRSI("map[%d] = addNode(host, %d, NULL);\n", 3, rule_node->index, rule_node->root);
      iterator_n = iterator_n->next;
   }   
     
   NewEdgeList *iterator_e = rule->added_edges;
   if(iterator_e != NULL)
      PTRS("int source = 0, target = 0;\n\n");
   while(iterator_e != NULL)
   {
      /* The source and target edges may be nodes preserved by the rule or 
       * nodes added by the rule. Both cases must be handled distinctly as they
       * require the host node pointer to be obtained in different ways.
       *
       * For preserved nodes, the host node index is obtained from node_map
       * (see macro PROCESS_NODE_MORPHISMS) that is used to index into the
       * host graph node pointer array. For added nodes, the pointer is 
       * obtained directly from map. */
      if(iterator_e->source_location == 'l')
           PTRSI("source = node_map[%d];\n", 3, iterator_e->source_index);
      else PTRSI("source = map[%d];\n", 3, iterator_e->source_index);

      if(iterator_e->target_location == 'l')
           PTRSI("target = node_map[%d];\n", 3, iterator_e->target_index);
      else PTRSI("target = map[%d];\n", 3, iterator_e->target_index);
      /* TODO: Evaluate rule_edge->label. */
      PTRSI("addEdge(host, false, NULL, source, target);\n", 3);
   
      iterator_e = iterator_e->next;      
   }
   PTRS("   freeMorphism(morphism);\n"
        "   return;\n"
        "}\n\n");
}
