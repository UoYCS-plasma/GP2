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
                      "bool *matched_nodes);\n", node->index);
              else
                 PTRS("static bool match_n%d(Morphism *morphism, "
                      "bool *matched_nodes, bool *matched_edges);\n", 
                      node->index);
              break;

         case 'i': 

         case 'o': 

         case 'b':

              node = getNode(lhs, operation->index);
              if(operation->next == NULL)
                 PTRS("static bool match_n%d(Morphism *morphism, "
                      "Edge *host_edge, bool *matched_nodes);\n",
                      node->index);
              else
                 PTRS("static bool match_n%d(Morphism *morphism, "
                      "Edge *host_edge, bool *matched_nodes, "
                      "bool *matched_edges);\n", node->index);
              break;

         case 'e': 

         case 's': 

         case 't':

         case 'l':

              edge = getEdge(lhs, operation->index);
              if(operation->next == NULL)
                 PTRS("static bool match_e%d(Morphism *morphism, "
                      "bool *matched_edges);\n", edge->index);
              else
                 PTRS("static bool match_e%d(Morphism *morphism, "
                      "bool *matched_nodes, bool *matched_edges);\n", 
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
   freeSearchplan(searchplan);
}


void emitRuleMatcher(string rule_name, SearchOp *first_op, int left_nodes, 
                     int left_edges, int variables)
{
   char item;
   if(first_op->is_node) item = 'n';
   else item = 'e';

   PTRH("Morphism *match%s(void);\n", rule_name);
   PTRS("\nMorphism *match%s(void)\n"
        "{\n" 
        "   if(%d > host->number_of_nodes || %d > host->number_of_edges)\n"
        "      return false;\n\n"
        "   Morphism *morphism = makeMorphism(%d, %d, %d);\n\n"
        "   MAKE_MATCHED_NODES_ARRAY\n",
        rule_name, left_nodes, left_edges, left_nodes, left_edges, variables);

   /* The matched edges array should not be created when there are 0 host
    * edges. Hence the following macro is only generated if there is at least
    * one left edge. If there are 0 host edges, the generated code will exit
    * before this point is reached because left_edges > host_edges. 
    * If there are no left edges, then there is no purpose for a matched edges
    * array regardless of the number of host edges. */
   if(left_edges > 0)
      PTRS("   MAKE_MATCHED_EDGES_ARRAY\n");

   if(first_op->next == NULL)
      PTRS("\n   bool match_found = match_%c%d(morphism, matched_nodes);\n\n",
           item, first_op->index);
   else
      PTRS("\n   bool match_found = match_%c%d(morphism, matched_nodes, "
           "matched_edges);\n\n", item, first_op->index);
   PTRS("   if(match_found) return morphism;\n"
        "   else\n"
        "   {\n"
        "      freeMorphism(morphism);\n"
        "      return NULL;\n"
        "   }\n"
        "}\n");
}


void emitNodeMatcher(Node *left_node, bool is_root, ItemList *deleted_nodes,
                     SearchOp *next_op)
{
   int left_index = left_node->index;
   bool dangling_node = queryItemList(deleted_nodes, left_index);
   
   if(next_op == NULL)
      PTRS("static bool match_n%d(Morphism *morphism, bool *matched_nodes)\n"
           "{\n", left_index);
   else
      PTRS("static bool match_n%d(Morphism *morphism, bool *matched_nodes, "
           "bool *matched_edges)\n"
           "{\n", left_index);

   /* The candidate list of host nodes is determined by the root status of 
    * left_node. */
   if(is_root) PTRSI("GSList *nodes = getRootNodes(host);\n", 3);
   else PTRSI("GSList *nodes = getNodesByLabel(host, %d);\n", 3,
             left_node->label_class);

   PTRS("   while(nodes != NULL)\n"
        "   {\n"
        "      Node *host_node = (Node *)nodes->data;\n"
        "      int index = host_node->index;\n\n"
        "      CHECK_NODE_MATCHED\n\n");
   
   /* If left_node is not rooted, then the candidate nodes are obtained by 
    * label class. In that case, there is no need to explicitly check the 
    * label class of the host node. */
   if(is_root)
      PTRSI("CHECK_NODE_LABEL_CLASS(%d)\n\n", 6, left_node->label_class);

   PTRSI("CHECK_NODE_MARK(%d)\n\n", 6, left_node->label->mark);

   if(dangling_node)
        PTRSI("CHECK_DANGLING_NODE_DEGREES(%d, %d);\n\n",
             6, left_node->indegree, left_node->outdegree);   
   else PTRSI("CHECK_NODE_DEGREES(%d, %d);\n\n", 
             6, left_node->indegree, left_node->outdegree);

   PTRSI("/* Label matching code does not exist yet. */\n", 6);
   /* TODO: Call to label matcher goes here. */
   PTRSI("bool nodes_match = host_node->label->list_length == 0;\n", 6);
   PTRSI("if(nodes_match)\n", 6);
   PTRSI("{\n", 6);
   PTRSI("addNodeMap(morphism, %d, index);\n", 9, left_index);
   PTRSI("matched_nodes[index] = true;\n", 9);

   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) 
   {
      PTRSI("if(result) return true;\n", 9);
      PTRSI("else\n", 9);
      PTRSI("{\n", 9);
      PTRSI("removeNodeMap(morphism);\n", 12);
      PTRSI("matched_nodes[index] = false;\n", 12);
      PTRSI("}\n", 9);
   }
   PTRSI("}\n", 6);
   PTRSI("nodes = nodes->next;\n", 6);
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
           "bool *matched_nodes)\n"
           "{\n", left_index);
   else
      PTRS("static bool match_n%d(Morphism *morphism, Edge *host_edge, "
           "bool *matched_nodes, bool *matched_edges)\n"
           "{\n", left_index);

   if(type == 'i' || type == 'b') 
        PTRSI("Node *host_node = getTarget(host_edge);\n", 3);
   else PTRSI("Node *host_node = getSource(host_edge);\n", 3);
   PTRSI("int index = host_node->index;\n\n", 3);

   PTRSI("/* This is the only node to check, so perform all the preliminaries "
         "in one step.\n", 3);
   PTRSI(" * Arguments: label class, mark, indegree, outdegree. */\n", 3);
   if(dangling_node)
        PTRSI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d)", 3,
             left_node->label_class, left_node->label->mark, 
             left_node->indegree, left_node->outdegree);
   else PTRSI("IF_INVALID_NODE(%d, %d, %d, %d)", 3,
             left_node->label_class, left_node->label->mark, 
             left_node->indegree, left_node->outdegree); 

   if(type == 'b')
   {
      PTRSI("\n{\n", 3); 
      PTRSI("/* Matching from bidirectional edge: check the second incident node. */\n", 6);
      if(type == 'i' || type == 'b') PTRSI("host_node = getSource(host_edge);\n", 6);
      else PTRSI("host_node = getTarget(host_edge);\n", 6);
      PTRSI("index = host_node->index;\n\n", 6);
      PTRSI("*/ Arguments: label class, mark, indegree, outdegree. */\n", 3);
      if(dangling_node)
           PTRSI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d) return false;", 3,
                left_node->label_class, left_node->label->mark, 
                left_node->indegree, left_node->outdegree);
      else PTRSI("IF_INVALID_NODE(%d, %d, %d, %d) return false;", 3,
               left_node->label_class, left_node->label->mark, 
               left_node->indegree, left_node->outdegree); 
      PTRSI("}\n\n", 3);
   }
   else PTRS(" return false;\n");
      
   /* TODO: Call to label matcher goes here. */
   PTRSI("bool nodes_match = host_node->label->list_length == 0;\n", 3);
   PTRSI("if(nodes_match)\n", 3);
   PTRSI("{\n", 3);
   PTRSI("addNodeMap(morphism, %d, index);\n", 6, left_index);
   PTRSI("matched_nodes[index] = true;\n", 6);

   bool total_match = emitNextMatcherCall(next_op, 6); 
   if(!total_match)
   {
      PTRSI("if(result) return true;\n", 6);
      PTRSI("else\n", 6);
      PTRSI("{\n", 6);
      PTRSI("removeNodeMap(morphism);\n", 9);
      PTRSI("matched_nodes[index] = false;\n", 9);
      PTRSI("}\n", 6);
   }
   PTRSI("}\n", 3);
   PTRSI("return false;\n", 3);
   PTRS("}\n\n");
}


void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op)
{
   if(next_op == NULL)
      PTRS("static bool match_e%d(Morphism *morphism, bool *matched_edges)\n"
           "{\n", left_edge->index);
   else
      PTRS("static bool match_e%d(Morphism *morphism, bool *matched_nodes, "
           "bool *matched_edges)\n"
           "{\n", left_edge->index);

   PTRSI("GSList *edges = getEdgesByLabel(host, %d);\n", 3,
        left_edge->label_class);

   PTRSI("while(edges != NULL)\n", 3);
   PTRSI("{\n", 3);
   PTRSI("Edge *host_edge = (Edge *)edges->data;\n", 6);
   PTRSI("int index = host_edge->index;\n\n", 6);

   PTRSI("CHECK_EDGE_MATCHED\n\n", 6);

   PTRSI("CHECK_EDGE_LABEL_CLASS(%d)\n\n", 6, left_edge->label_class);

   PTRSI("CHECK_EDGE_MARK(%d)\n\n", 6, left_edge->label->mark);

   /* TODO: Call to label matcher goes here. */
   PTRSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
   PTRSI("if(edges_match)\n", 6);
   PTRSI("{\n", 6);
   PTRSI("addEdgeMap(morphism, %d, index);\n", 9, left_edge->index);
   PTRSI("matched_edges[index] = true;\n", 9);

   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match)
   {
      PTRSI("if(result) return true;\n", 9);
      PTRSI("else\n", 9);
      PTRSI("{\n", 9);
      PTRSI("removeEdgeMap(morphism);\n", 12);
      PTRSI("matched_edges[index] = false;\n", 12);
      PTRSI("}\n", 9);
   }
   PTRSI("}\n", 6);
   PTRSI("else edges = edges->next;\n", 6);
   PTRSI("}\n", 3);
   PTRSI("return false;\n", 3);
   PTRS("}\n\n");
}

/* Unlike matching a node from an edge, the LHS-node from which this LHS-edge
 * is matched may not necessarily be the previous matched node in the 
 * searchplan. The operation type is used to find the appropriate left-node.
 * The generated code uses the index of this node to find the host node to
 * which it has been matched (findHostIndex). Edges in the inedge list or 
 * outedge list of the host node are the candidate edges to match left_edge. */

void emitEdgeFromNodeMatcher(Edge *left_edge, char type, SearchOp *next_op)
{
   if(next_op == NULL)
      PTRS("static bool match_e%d(Morphism *morphism, bool *matched_edges)\n"
           "{\n", left_edge->index);
   else
      PTRS("static bool match_e%d(Morphism *morphism, bool *matched_nodes, "
           "bool *matched_edges)\n"
           "{\n", left_edge->index);

   if(type == 's' || type == 'l')
   {
      PTRSI("int host_node_index = findHostIndex(morphism, %d);\n",
            3, left_edge->source->index);
      PTRSI("if(host_node_index < 0) return false;\n", 3);
      PTRSI("Node *host_node = getNode(host, host_node_index);\n\n", 3);
      PTRSI("int counter;\n", 3);
      PTRSI("for(counter = 0; counter < host_node->next_out_edge_index; counter++)\n", 3);
   }
   else 
   {
      PTRSI("int host_node_index = findHostIndex(morphism, %d);\n",
            3, left_edge->target->index);
      PTRSI("if(host_node_index < 0) return false;\n", 3);
      PTRSI("Node *host_node = getNode(host, host_node_index);\n\n", 3);
      PTRSI("int counter;\n", 3);
      PTRSI("for(counter = 0; counter < host_node->next_in_edge_index; counter++)\n", 3);
   }

   PTRSI("{\n", 3);
   if(type == 's' || type == 'l')
        PTRSI("Edge *host_edge = getOutEdge(host_node, counter);\n", 6);
   else PTRSI("Edge *host_edge = getInEdge(host_node, counter);\n", 6);
   PTRSI("if(host_edge->index == -1) continue;\n\n", 6);

   PTRSI("/* Check all the preliminaries in one step.\n", 6);
   PTRSI(" * Arguments: label class, mark. */\n", 6);
   if(type == 'l') 
        PTRSI("IF_INVALID_LOOP_EDGE(%d, %d) continue;\n", 6,
              left_edge->label_class, left_edge->label->mark);
   else PTRSI("IF_INVALID_EDGE(%d, %d) continue;\n", 6, 
              left_edge->label_class, left_edge->label->mark);

   /* TODO: Call to label matcher goes here. */
   PTRSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
   PTRSI("if(edges_match)\n", 6);
   PTRSI("{\n", 6);
   PTRSI("addEdgeMap(morphism, %d, host_edge->index);\n", 9, left_edge->index);
   PTRSI("matched_edges[host_edge->index] = true;\n", 9);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) PTRSI("if(result) return true;\n", 9);

   if(left_edge->bidirectional)
   /* Emit code to try and match an edge in the opposite direction. */
   {
      PTRSI("}\n", 6);
      PTRSI("}\n\n", 3);
      if(type == 's' || type == 'l')
           PTRSI("for(counter = 0; counter < host_node->indegree; counter++)\n", 3);
      else PTRSI("for(counter = 0; counter < host_node->outdegree; counter++)\n", 3);

      PTRSI("{\n", 3);
      if(type == 's' || type == 'l')
           PTRSI("Edge *host_edge = getInEdge(host_node, counter);\n", 6);
      else PTRSI("Edge *host_edge = getOutEdge(host_node, counter);\n", 6);
      PTRSI("if(host_edge->index == -1) continue;\n\n", 6);

      PTRSI("/* Check all the preliminaries in one step.\n", 6);
      PTRSI(" * Arguments: label class, mark. */\n", 6);
      if(type == 'l')
           PTRSI("IF_INVALID_LOOP_EDGE(%d, %d) continue;\n", 6,
                 left_edge->label_class, left_edge->label->mark);
      else PTRSI("IF_INVALID_EDGE(%d, %d) continue;\n", 6, 
                 left_edge->label_class, left_edge->label->mark);

      /* TODO: Call to label matcher goes here. */
      PTRSI("bool edges_match = host_edge->label->list_length == 0;\n", 6);
      PTRSI("if(edges_match)\n", 6);
      PTRSI("{\n", 6);
      PTRSI("addEdgeMap(morphism, %d, host_edge->index);\n", 9, left_edge->index);
      PTRSI("matched_edges[host_edge->index] = true;\n", 9);
   
      bool total_match = emitNextMatcherCall(next_op, 9);
      if(!total_match) PTRSI("if(result) return true;\n", 9);
   }
   if(!total_match)
   {
      PTRSI("else\n", 9);
      PTRSI("{\n", 9);
      PTRSI("removeEdgeMap(morphism);\n", 12);
      PTRSI("matched_edges[host_edge->index] = false;\n", 12);
      PTRSI("}\n", 9);
   }   
   PTRSI("}\n", 6);
   PTRSI("}\n", 3);
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
      PTRS("   Node *host_node = NULL, *source = NULL, *target = NULL;\n\n"
           "   /* Array of host node pointers indexed by RHS node index. */\n"
           "   Node *map[%d];\n", rhs->number_of_nodes);

      for(index = 0; index < rhs->number_of_nodes; index++)
      {
         Node *rule_node = getNode(rhs, index);
         /* TODO: Evaluate rule_node->label. */
         PTRSI("int index = addNode(host, %d, NULL);\n", 3, rule_node->root);
         PTRSI("map[%d] = getNode(host, index);\n\n", 3, rule_node->index);
      }

      NewEdgeList *iterator = rule->added_edges;
      while(iterator != NULL)
      {
         if(iterator->source_index == iterator->target_index)
         {
            PTRSI("source = map[%d];\n", 3, iterator->source_index);
            PTRSI("addEdge(host, false, NULL, source, source);\n", 3);
         }
         else
         {
            PTRSI("source = map[%d];\n", 3, iterator->source_index);
            PTRSI("target = map[%d];\n", 3, iterator->target_index);
            /* TODO: Evaluate rule_edge->label. */
            PTRSI("addEdge(host, false, NULL, source, target);\n", 3);
         }
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
           "   Node *map[%d];\n\n"
           "   Node *host_node = NULL;\n", rhs->number_of_nodes);
   while(iterator_n != NULL)
   {   
      Node *rule_node = getNode(rhs, iterator_n->index);
      /* TODO: Evaluate rule_node->label. */
      PTRSI("int index = addNode(host, %d, NULL);\n", 3, rule_node->root);
      PTRSI("map[%d] = getNode(host, index);\n\n", 3, rule_node->index);
      iterator_n = iterator_n->next;
   }   
     
   NewEdgeList *iterator_e = rule->added_edges;
   if(iterator_e != NULL)
      PTRS("   Node *source = NULL, *target = NULL;\n"
           "   int source_index = 0, target_index = 0;\n\n");
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
      {
         PTRSI("source_index = node_map[%d];\n", 3, iterator_e->source_index);
         PTRSI("source = getNode(host, source_index);\n", 3);
      }
      else PTRSI("source = map[%d];\n", 3, iterator_e->source_index);

      if(iterator_e->target_location == 'l')
      {
         PTRSI("target_index = node_map[%d];\n", 3, iterator_e->target_index);
         PTRSI("target = getNode(host, target_index);\n", 3);
      }
      else PTRSI("target = map[%d];\n", 3, iterator_e->target_index);

      /* TODO: Evaluate rule_edge->label. */
      PTRSI("addEdge(host, false, NULL, source, target);\n", 3);
   
      iterator_e = iterator_e->next;      
   }
   PTRS("   freeMorphism(morphism);\n"
        "   return;\n"
        "}\n\n");
}
