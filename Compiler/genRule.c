/* ////////////////////////////////////////////////////////////////////////////

 * The functions in this file print various macros. macros.h contains their
 * definitions.

/////////////////////////////////////////////////////////////////////////// */

#include "genRule.h"

FILE *header = NULL;
FILE *file = NULL;
 
Searchplan *searchplan = NULL;

void generateRules(List *declarations)
{
   while(declarations != NULL)
   {
      GPDeclaration *decl = declarations->declaration;
     
      switch(decl->decl_type)
      {
         case MAIN_DECLARATION:
              break;

         case PROCEDURE_DECLARATION:

              if(decl->procedure->local_decls != NULL)
                 generateRules(decl->procedure->local_decls);
              break;

         case RULE_DECLARATION:
         {
              Rule *rule = makeRule(decl->rule);
              decl->rule->empty_lhs = rule->lhs == NULL;
              decl->rule->is_predicate = isPredicate(rule);
              generateRuleCode(rule, decl->rule->is_predicate);
              freeRule(rule);
              break;
         }

         default: 
              print_to_log("Error (generateRules): Unexpected declaration type "
                           "%d at AST node %d\n", decl->decl_type, decl->id);
              break;
      }
      declarations = declarations->next;
   }
}

void generateRuleCode(Rule *rule, bool predicate)
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

   string file_name = malloc(length);
   if(file_name == NULL)
   {
      print_to_log("Error: Memory exhausted during file name creation.\n");
      exit(1);
   }
   strcpy(header_name, "runtime/");
   strcpy(file_name, "runtime/");
   strcat(header_name, rule_name);
   strcat(file_name, rule_name);
   strcat(header_name, ".h");
   strcat(file_name, ".c");

   header = fopen(header_name, "w");
   if(header == NULL) { 
     perror(header_name);
     exit(1);
   }  

   file = fopen(file_name, "w");
   if(file == NULL) { 
     perror(file_name);
     exit(1);
   }

   free(header_name);
   free(file_name);

   PTH("#include \"../globals.h\"\n"
        "#include \"../graph.h\"\n"
        "#include \"../graphStacks.h\"\n"
        "#include \"../macros.h\"\n"
        "#include \"host/host.h\"\n"
        "#include \"morphism.h\"\n\n");
   PTF("#include \"%s.h\"\n\n", rule_name);

   if(rule->lhs == NULL) generateApplicationCode(rule, true, false);
   else
   {
      if(rule->rhs == NULL)
      {
         generateMatchingCode(rule_name, rule->lhs, rule->deleted_nodes);
         if(!predicate) generateApplicationCode(rule, false, true);
      }
      else
      {
         generateMatchingCode(rule_name, rule->lhs, rule->deleted_nodes);
         if(!predicate) generateApplicationCode(rule, false, false);
      }
   }
   fclose(header);
   fclose(file);
   return;
}

void generateMatchingCode(string rule_name, Graph *lhs, ItemList *deleted_nodes)
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
   /* The searchplan is iterated over twice. On the first iteration, the
    * prototypes of the matching functions are printed to the generated 
    * source file. 
    * The second iteration prints the definitions of those functions.
    * The type of the searchplan operation determines which emitMatcher
    * function is called and which parameters are passed. */
   while(operation != NULL)
   {
      char type = operation->type;
      switch(type)
      {
         case 'n':
         case 'r':
              node = getNode(lhs, operation->index);
              if(operation->next == NULL)
                 PTF("static bool match_n%d(Morphism *morphism, "
                      "int *matched_nodes);\n", node->index);
              else
                 PTF("static bool match_n%d(Morphism *morphism, "
                      "int *matched_nodes, int *matched_edges);\n", 
                      node->index);
              break;

         case 'i': 
         case 'o': 
         case 'b':
              node = getNode(lhs, operation->index);
              if(operation->next == NULL)
                 PTF("static bool match_n%d(Morphism *morphism, "
                      "Edge *host_edge, int *matched_nodes);\n",
                      node->index);
              else
                 PTF("static bool match_n%d(Morphism *morphism, "
                      "Edge *host_edge, int *matched_nodes, "
                      "int *matched_edges);\n", node->index);
              break;

         case 'e': 
         case 's': 
         case 't':
         case 'l':
              edge = getEdge(lhs, operation->index);
              if(operation->next == NULL)
                 PTF("static bool match_e%d(Morphism *morphism, "
                      "int *matched_edges);\n", edge->index);
              else
                 PTF("static bool match_e%d(Morphism *morphism, "
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
   /* Print the main matching function which sets up the matching environment
    * and calls the first matcher in the searchplan. */
   emitRuleMatcher(rule_name, searchplan->first, lhs->number_of_nodes, 
                   lhs->number_of_edges);
   PTF("\n\n");
   operation = searchplan->first;
   while(operation != NULL)
   {
      switch(operation->type)
      {        
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
              emitNodeFromEdgeMatcher(node, operation->type, deleted_nodes, 
                                      operation->next);
              break;

         case 'e': 
              edge = getEdge(lhs, operation->index);
              emitEdgeMatcher(edge, operation->next);
              break;

         case 'l':
              edge = getEdge(lhs, operation->index);
              emitEdgeFromNodeMatcher(edge, true, operation->next);
              break;

         case 's': 
              edge = getEdge(lhs, operation->index);
              emitEdgeFromNodeMatcher(edge, false, operation->next);
              break;

         case 't':
              edge = getEdge(lhs, operation->index);
              emitEdgeToNodeMatcher(edge, operation->next);
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
                     int left_edges)
{
   char item;
   if(first_op->is_node) item = 'n';
   else item = 'e';

   PTH("bool match%s(Morphism *morphism);\n", rule_name);
   PTF("\nstatic int left_nodes = %d, left_edges = %d;\n\n"
        "bool match%s(Morphism *morphism)\n"
        "{\n" 
        "   if(left_nodes > host->number_of_nodes ||\n"
        "      left_edges > host->number_of_edges) return false;\n\n"
        "   MAKE_MATCHED_NODES_ARRAY\n",
        left_nodes, left_edges, rule_name);

   /* The matched edges array should not be created when there are no host
    * edges: if there are no host edges, the generated code will exit
    * before this point is reached because left_edges > host_edges. */
   if(left_edges > 0)
      PTF("   MAKE_MATCHED_EDGES_ARRAY\n\n");

   if(first_op->next == NULL)
      PTFI("if(match_%c%d(morphism, matched_nodes)) return true;\n",
            3, item, first_op->index);
   else
      PTFI("if(match_%c%d(morphism, matched_nodes, matched_edges)) return true;\n",
            3, item, first_op->index);
   PTF("   else\n"
        "   {\n"
        "      initialiseMorphism(morphism);\n"
        "      return false;\n"
        "   }\n"
        "}\n");
}

void emitNodeMatcher(Node *left_node, bool is_root, ItemList *deleted_nodes,
                     SearchOp *next_op)
{
   int left_index = left_node->index;
   bool dangling_node = queryItemList(deleted_nodes, left_index);
   
   if(next_op == NULL)
      PTF("static bool match_n%d(Morphism *morphism, int *matched_nodes)\n"
           "{\n", left_index);
   else
      PTF("static bool match_n%d(Morphism *morphism, int *matched_nodes, "
           "int *matched_edges)\n"
           "{\n", left_index);
   /* Emit code to initialise the iteration over the candidate nodes. 
    * If the left node is rooted, interrogate the root node list of the host
    * graph, otherwise query the appropriate nodes-by-label-class list. */
    if(is_root) 
    {
       PTFI("bool node_matched = false;\n", 3);
       PTFI("RootNodes *nodes = NULL;\n", 3);   
       PTFI("for(nodes = getRootNodeList(host); nodes != NULL;", 3);
       PTFI("nodes = nodes->next)\n", 1);
       PTFI("{\n", 3);
       PTFI("Node *host_node = getNode(host, nodes->index);\n", 6);
    }
    else
    {
       PTFI("bool node_matched = false;\n", 3);
       PTFI("int count;\n", 3);
       PTFI("LabelClassTable nodes = getNodesByLabel(host, %d);\n", 3,
             left_node->label_class);
       PTFI("for(count = 0; count < nodes.index; count++)\n", 3);
       PTFI("{\n", 3);
       PTFI("Node *host_node = getNode(host, nodes.items[count]);\n", 6);
    }
    PTFI("if(host_node == NULL) continue;\n\n", 6);
    PTFI("node_matched = false;\n", 6);
    PTFI("/* Set node_matched to true if the node has already been matched.*/\n", 6);
    PTFI("int index;\n", 6);
    PTFI("CHECK_MATCHED_NODE\n\n", 6);
   /* Emit code to check the node_matched flag and to test if the candidate 
    * host node is consistent with the left node with respect to label class, 
    * mark, and degrees. If not, the loop will continue without entering the 
    * potentially expensive label matching code. */
   PTFI("/* Arguments: label class, mark, indegree, outdegree, bidegree. */\n", 6);
   if(dangling_node)
        PTFI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d, %d) continue;\n\n", 6,
              left_node->label_class, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree);
   else PTFI("IF_INVALID_NODE(%d, %d, %d, %d, %d) continue;\n\n", 6,
              left_node->label_class, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree); 

   PTFI("Label label = host_node->label;\n\n", 6);
   PTFI("bool match = false;\n", 6);
   if(left_node->label_class == LIST_VAR_L)
      generateVariableListMatchingCode(left_node->label, 6, file);
   else generateFixedListMatchingCode(left_node->label, 6, file);
   PTFI("if(match)\n", 6);
   PTFI("{\n", 6);
   PTFI("addNodeMap(morphism, %d, host_node->index, new_assignments);\n", 9, left_index);
   PTFI("matched_nodes[%d] = host_node->index;\n", 9, left_index);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) PTFI("HANDLE_RESULT(%d)\n", 9, left_index);
   PTFI("}\n", 6);
   PTFI("else removeAssignments(morphism, new_assignments);\n", 6);
   PTFI("}\n", 3);
   PTFI("return false;\n", 3);
   PTF("}\n\n");
}

/* Matching a node from a matched incident edge always follow an edge match in
 * the searchplan. Thus the generated function takes the host edge matched by  
 * the previous searchplan function as one of its arguments. It gets the
 * appropriate host node (source or target of the host edge) and checks if this
 * node is compatible with left_node. */
void emitNodeFromEdgeMatcher(Node *left_node, char type, ItemList *deleted_nodes,
                             SearchOp *next_op)
{
   int left_index = left_node->index;
   bool dangling_node = queryItemList(deleted_nodes, left_index);

   if(next_op == NULL)
      PTF("static bool match_n%d(Morphism *morphism, Edge *host_edge, "
           "int *matched_nodes)\n"
           "{\n", left_index);
   else
      PTF("static bool match_n%d(Morphism *morphism, Edge *host_edge, "
           "int *matched_nodes, int *matched_edges)\n"
           "{\n", left_index);

   if(type == 'i' || type == 'b') 
        PTFI("Node *host_node = getNode(host, getTarget(host_edge));\n\n", 3);
   else PTFI("Node *host_node = getNode(host, getSource(host_edge));\n\n", 3);
   PTFI("/* Set node_matched to true if the node has already been matched. */\n", 3);
   PTFI("bool node_matched = false;\n", 3);
   PTFI("int index;\n", 3);
   PTFI("CHECK_MATCHED_NODE\n\n", 3);
   /* Emit code to check the node_matched flag and to test if the candidate 
    * host node is consistent with the left node with respect to label class, 
    * mark, and degrees. If not, the loop will continue without entering the 
    * potentially expensive label matching code. */
   PTFI(" /* Arguments: label class, mark, indegree, outdegree, bidegree. */\n", 3);
   if(dangling_node)
        PTFI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d, %d)", 3,
              left_node->label_class, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree);
   else PTFI("IF_INVALID_NODE(%d, %d, %d, %d, %d)", 3,
              left_node->label_class, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree); 

   if(type == 'b')
   {
      PTF("\n   {\n"); 
      PTFI("/* Matching from bidirectional edge: check the second incident node. */\n", 6);
      if(type == 'i' || type == 'b') 
           PTFI("host_node = getNode(host, getSource(host_edge));\n\n", 6);
      else PTFI("host_node = getNode(host, getTarget(host_edge));\n\n", 6);
      PTFI("bool node_matched = false;\n", 6);
      PTFI("/* Set node_matched to true if the node has already been matched. */\n", 6);
      PTFI("CHECK_MATCHED_NODE\n\n", 6);
      PTFI(" /* Arguments: label class, mark, indegree, outdegree, bidegree. */\n", 6);
      if(dangling_node)
           PTFI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d, %d) return false;\n", 6,
                 left_node->label_class, left_node->label.mark, 
                 left_node->indegree, left_node->outdegree, left_node->bidegree);
      else PTFI("IF_INVALID_NODE(%d, %d, %d, %d, %d) return false;\n", 6,
                 left_node->label_class, left_node->label.mark, 
                 left_node->indegree, left_node->outdegree, left_node->bidegree); 
      PTFI("}\n\n", 3);
   }
   else PTF(" return false;\n\n");
 
   PTFI("Label label = host_node->label;\n\n", 6);
   PTFI("bool match = false;\n", 6);
   if(left_node->label_class == LIST_VAR_L)
      generateVariableListMatchingCode(left_node->label, 6, file);
   else generateFixedListMatchingCode(left_node->label, 6, file);
   PTFI("if(match)\n", 3);
   PTFI("{\n", 3);
   PTFI("addNodeMap(morphism, %d, host_node->index, new_assignments);\n", 6, left_index);
   PTFI("matched_nodes[%d] = host_node->index;\n", 6, left_index);
   bool total_match = emitNextMatcherCall(next_op, 6); 
   if(!total_match) PTFI("HANDLE_RESULT(%d)\n", 9, left_index);
   PTFI("}\n", 3);
   PTFI("else removeAssignments(morphism, new_assignments);\n", 6);
   PTFI("return false;\n", 3);
   PTF("}\n\n");
}


void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op)
{
   if(next_op == NULL) PTF("static bool match_e%d(Morphism *morphism,"
                            "int *matched_edges)\n{\n", left_edge->index);
   else PTF("static bool match_e%d(Morphism *morphism, int *matched_nodes, "
             "int *matched_edges)\n{\n", left_edge->index);

   PTFI("int count;\n", 3);
   PTFI("bool edge_matched = false;\n", 3);
   PTFI("LabelClassTable edges = getEdgesByLabel(host, %d);\n", 3,
         left_edge->label_class);
   PTFI("for(count = 0; count < edges.index; count++)", 3);
   PTFI("{\n", 3);
   PTFI("Edge *host_edge = getEdge(host, edges.items[count];\n\n", 6);
   PTFI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
   PTFI("int index;\n", 6);
   PTFI("CHECK_MATCHED_EDGE\n\n", 6);
   PTFI("if(edge_matched) continue;\n\n", 6);

   /* Emit code to test the matched_edge flag and if the candidate host edge 
    * is consistent with the left edge with respect to label class, mark, and
    * loopiness. If not, the loop will continue without entering the 
    * potentially expensive label matching code. */
   PTFI(" /* Arguments: label class, mark. */\n", 6);
   if(left_edge->source == left_edge->target) 
        PTFI("IF_INVALID_LOOP_EDGE(%d, %d)\n\n", 6,
              left_edge->label_class, left_edge->label.mark);
   else PTFI("IF_INVALID_EDGE(%d, %d)\n\n", 6, 
              left_edge->label_class, left_edge->label.mark);
   PTFI("/* If either endpoint has been matched, check that the corresponding\n", 6);
   PTFI(" * endpoint of the host edge is the image of the node in question. */\n", 6);
   PTFI("continue;\n\n", 6);
   PTFI("int source_index = lookupNode(morphism, %d);\n", left_edge->source, 6);
   PTFI("if(source_index >= 0 && host_edge->source != source_index) continue;\n", 6);
   PTFI("int target_index = lookupNode(morphism, %d);\n", left_edge->target, 6);
   PTFI("if(target_index >= 0 && host_edge->target != target_index) continue;\n", 6);

   PTFI("Label label = host_edge->label;\n\n", 6);
   PTFI("bool match = false;\n", 6);
   if(left_edge->label_class == LIST_VAR_L)
      generateVariableListMatchingCode(left_edge->label, 6, file);
   else generateFixedListMatchingCode(left_edge->label, 6, file);
   PTFI("if(match)\n", 6);
   PTFI("{\n", 6);
   PTFI("addEdgeMap(morphism, %d, host_edge->index, new_assignments);\n", 9, left_edge->index);
   PTFI("matched_edges[%d] = host_edge->index;\n", 9, left_edge->index);

   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match)
   {
      PTFI("if(result) return true;\n", 9);
      PTFI("else\n", 9);
      PTFI("{\n", 9);
      PTFI("removeEdgeMap(morphism);\n", 12);
      PTFI("matched_edges[%d] = -1;\n", 12, left_edge->index);
      PTFI("}\n", 9);
   }
   PTFI("}\n", 6);
   PTFI("else removeAssignments(morphism, new_assignments);\n", 6);
   PTFI("}\n", 3);
   PTFI("return false;\n", 3);
   PTF("}\n\n");
}

/* Unlike matching a node from an edge, the LHS-node from which this LHS-edge
 * is matched may not necessarily be the previously matched node in the 
 * searchplan. The generated code uses the index of the source of the LHS-edge 
 * to find the host node to which it has been matched (lookupNode). Edges 
 * in the outedge list of that host node are the candidate edges to match 
 * left_edge. */
void emitEdgeFromNodeMatcher(Edge *left_edge, bool is_loop, SearchOp *next_op)
{
   if(next_op == NULL) PTF("static bool match_e%d(Morphism *morphism, "
                            "int *matched_edges)\n{\n", left_edge->index);
   else PTF("static bool match_e%d(Morphism *morphism, int *matched_nodes, "
             "int *matched_edges)\n{\n", left_edge->index);

   PTFI("int source_index = lookupNode(morphism, %d);\n", 3,
         left_edge->source);
   PTFI("if(source_index < 0) return false;\n", 3);
   PTFI("Node *host_node = getNode(host, source_index);\n\n", 3);
   PTFI("int target_index = lookupNode(morphism, %d);\n", 3,
         left_edge->target);
   PTFI("int counter;\n", 3);
   PTFI("bool edge_matched = false;\n", 3);
   PTFI("for(counter = host_node->out_index - 1; counter >= 0; counter--)\n", 3);
   PTFI("{\n", 3);
   PTFI("Edge *host_edge = getEdge(host, getOutEdge(host_node, counter));\n", 6);
   PTFI("if(host_edge == NULL) continue;\n\n", 6);
   PTFI("edge_matched = false;\n", 6);
   PTFI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
   PTFI("int index;\n", 6);
   PTFI("CHECK_MATCHED_EDGE\n\n", 6);
   PTFI("/* Arguments: label class, mark. */\n", 6);
   if(is_loop) 
        PTFI("IF_INVALID_LOOP_EDGE(%d, %d) continue;\n\n", 6,
              left_edge->label_class, left_edge->label.mark);
   else PTFI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
              left_edge->label_class, left_edge->label.mark);

   if(left_edge->bidirectional)
   {
      PTFI("/* If the rule edge's target has been matched, check that either\n", 6);
      PTFI(" * the source or target of the host edge is the image of the rule\n", 6);
      PTFI(" * edge's target. */\n", 6);
      PTFI("if(target_index >= 0 && host_edge->source != target_index &&\n", 6);
      PTFI("   host_edge->target != target_index) continue;\n", 6);
   }
   else
   {
      PTFI("/* If the rule edge's target has been matched, check that the\n", 6);
      PTFI(" * target of the host edge is the image of the rule edge's target. */\n", 6);
      PTFI("if(target_index >= 0 && host_edge->target != target_index) continue;\n", 6);
   }
 
   PTFI("Label label = host_edge->label;\n\n", 6);
   PTFI("bool match = false;\n", 6);
   if(left_edge->label_class == LIST_VAR_L)
      generateVariableListMatchingCode(left_edge->label, 6, file);
   else generateFixedListMatchingCode(left_edge->label, 6, file);
   PTFI("if(match)\n", 6);
   PTFI("{\n", 6);
   PTFI("addEdgeMap(morphism, %d, host_edge->index, new_assignments);\n", 9, left_edge->index);
   PTFI("matched_edges[%d] = host_edge->index;\n", 9, left_edge->index);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) 
   {
      PTFI("if(result) return true;\n", 9);
      PTFI("else\n", 9);
      PTFI("{\n", 9);
      PTFI("removeEdgeMap(morphism);\n", 12);
      PTFI("matched_edges[%d] = -1;\n", 12, left_edge->index);
      PTFI("}\n", 9);
   }   
   PTFI("}\n", 6);
   PTFI("else removeAssignments(morphism, new_assignments);\n", 6);
   PTFI("}\n\n", 3);

   if(left_edge->bidirectional)
   /* Emit code to try and match an edge in the opposite direction. */
   {
      PTFI("for(counter = host_node->in_index - 1; counter >= 0; counter--)\n", 3);
      PTFI("{\n", 3);
      PTFI("Edge *host_edge = getEdge(host, getInEdge(host_node, counter));\n", 6);
      PTFI("if(host_edge == NULL) continue;\n\n", 6);
      PTFI("edge_matched = false;\n", 6);
      PTFI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
      PTFI("CHECK_MATCHED_EDGE\n\n", 6);
      PTFI(" /* Arguments: label class, mark. */\n", 6);
      if(is_loop)
           PTFI("IF_INVALID_LOOP_EDGE(%d, %d) continue;\n\n", 6,
                 left_edge->label_class, left_edge->label.mark);
      else PTFI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
                 left_edge->label_class, left_edge->label.mark);
      if(left_edge->bidirectional)
      {
         PTFI("/* If the rule edge's source has been matched, check that either\n", 6);
         PTFI(" * the source or target of the host edge is the image of the rule\n", 6);
         PTFI(" * edge's source. */\n", 6);
         PTFI("if(source_index >= 0 && host_edge->source != source_index &&\n", 6);
         PTFI("host_edge->target != source_index) continue;\n", 9);
      }
      else
      {
         PTFI("/* If the rule edge's source has been matched, check that the source\n", 6);
         PTFI(" * of the host edge is the image of the rule edge's source. */\n", 6);
         PTFI("if(source_index >= 0 && host_edge->source != source_index) continue;\n", 6);
      }

      /* TODO: Call to label matcher goes here. */
      PTFI("Label label = host_edge->label;\n\n", 6);
      PTFI("bool match = false;\n", 6);
      if(left_edge->label_class == LIST_VAR_L)
         generateVariableListMatchingCode(left_edge->label, 6, file);
      else generateFixedListMatchingCode(left_edge->label, 6, file);
      PTFI("if(match)\n", 6);
      PTFI("{\n", 6);
      PTFI("addEdgeMap(morphism, %d, host_edge->index, new_assignments);\n", 9, left_edge->index);
      PTFI("matched_edges[%d] = host_edge->index;\n", 9, left_edge->index);
   
      bool total_match = emitNextMatcherCall(next_op, 9);
      if(!total_match) 
      {
         PTFI("if(result) return true;\n", 9);
         PTFI("else\n", 9);
         PTFI("{\n", 9);
         PTFI("removeEdgeMap(morphism);\n", 12);
         PTFI("matched_edges[%d] = -1;\n", 12, left_edge->index);
         PTFI("}\n", 9);
      }
      PTFI("}\n", 6);
      PTFI("else removeAssignments(morphism, new_assignments);\n", 6);
      PTFI("}\n", 3);
   }
   PTFI("return false;\n", 3);
   PTF("}\n\n");
}

/* Unlike matching a node from an edge, the LHS-node from which this LHS-edge
 * is matched may not necessarily be the previously matched node in the 
 * searchplan. The generated code uses the index of the target of the LHS-edge 
 * to find the host node to which it has been matched (lookupNode). Edges 
 * in the inedge list of that host node are the candidate edges to match 
 * left_edge. */
void emitEdgeToNodeMatcher(Edge *left_edge, SearchOp *next_op)
{
   if(next_op == NULL) PTF("static bool match_e%d(Morphism *morphism, "
                            "int *matched_edges)\n{\n", left_edge->index);
   else PTF("static bool match_e%d(Morphism *morphism, int *matched_nodes, "
             "int *matched_edges)\n{\n", left_edge->index);

   PTFI("int target_index = lookupNode(morphism, %d);\n", 3,
         left_edge->target);
   PTFI("if(target_index < 0) return false;\n", 3);
   PTFI("Node *host_node = getNode(host, target_index);\n\n", 3);
   PTFI("int source_index = lookupNode(morphism, %d);\n", 3,
         left_edge->source);
   PTFI("int counter;\n\n", 3);
   PTFI("bool edge_matched = false;\n", 3);
   PTFI("for(counter = host_node->in_index - 1; counter >= 0; counter--)\n", 3);
   PTFI("{\n", 3);
   PTFI("Edge *host_edge = getEdge(host, getInEdge(host_node, counter));\n", 6);
   PTFI("if(host_edge == NULL) continue;\n\n", 6);
   PTFI("edge_matched = false;\n", 6);
   PTFI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
   PTFI("int index;\n", 6);
   PTFI("CHECK_MATCHED_EDGE\n\n", 6);
   PTFI(" /* Arguments: label class, mark. */\n", 6);
   PTFI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
         left_edge->label_class, left_edge->label.mark);
   if(left_edge->bidirectional)
   {
      PTFI("/* If the rule edge's source has been matched, check that either\n", 6);
      PTFI(" * the source or target of the host edge is the image of the rule\n", 6);
      PTFI(" * edge's source. */\n", 6);
      PTFI("if(source_index >= 0 && host_edge->source != source_index &&\n", 6);
      PTFI("host_edge->target != source_index) continue;\n", 9);
   }
   else
   {
      PTFI("/* If the rule edge's source has been matched, check that the\n", 6);
      PTFI(" * source of the host edge is the image of the rule edge's source. */\n", 6);
      PTFI("if(source_index >= 0 && host_edge->source != source_index) continue;\n", 6);
   }

   PTFI("Label label = host_edge->label;\n\n", 6);
   PTFI("bool match = false;\n", 6);
   if(left_edge->label_class == LIST_VAR_L)
      generateVariableListMatchingCode(left_edge->label, 6, file);
   else generateFixedListMatchingCode(left_edge->label, 6, file);
   PTFI("if(match)\n", 6);
   PTFI("{\n", 6);
   PTFI("addEdgeMap(morphism, %d, host_edge->index, new_assignments);\n", 9, left_edge->index);
   PTFI("matched_edges[%d] = host_edge->index;\n", 9, left_edge->index);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) 
   {
      PTFI("if(result) return true;\n", 9);
      PTFI("else\n", 9);
      PTFI("{\n", 9);
      PTFI("removeEdgeMap(morphism);\n", 12);
      PTFI("matched_edges[%d] = -1;\n", 12, left_edge->index);
      PTFI("}\n", 9);
   }  
   PTFI("}\n", 6);
   PTFI("else removeAssignments(morphism, new_assignments);\n", 6);
   PTFI("}\n", 3);
   if(left_edge->bidirectional)
   /* Emit code to try and match an edge in the opposite direction. */
   {
      PTFI("for(counter = host_node->out_index - 1; counter >= 0; counter--)\n", 3);
      PTFI("{\n", 3);
      PTFI("Edge *host_edge = getEdge(host, getOutEdge(host_node, counter));\n", 6);
      PTFI("if(host_edge == NULL) continue;\n\n", 6);
      PTFI("edge_matched = false;\n", 6);
      PTFI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
      PTFI("CHECK_MATCHED_EDGE\n\n", 6);
      PTFI(" /* Arguments: label class, mark. */\n", 6);
      PTFI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
            left_edge->label_class, left_edge->label.mark);
      if(left_edge->bidirectional)
      {
         PTFI("/* If the rule edge's target has been matched, check that either\n", 6);
         PTFI(" * the source or target of the host edge is the image of the rule\n", 6);
         PTFI(" * edge's target. */\n", 6);
         PTFI("if(target_index >= 0 && host_edge->source != target_index &&\n", 6);
         PTFI("   host_edge->target != target_index) continue;\n", 6);
      }
      else
      {
         PTFI("/* If the rule edge's target has been matched, check that the\n", 6);
         PTFI(" * target of the host edge is the image of the rule edge's target. */\n", 6);
         PTFI("if(target_index >= 0 && host_edge->target != target_index) continue;\n", 6);
      }

      PTFI("Label label = host_edge->label;\n\n", 6);
      PTFI("bool match = false;\n", 6);
      if(left_edge->label_class == LIST_VAR_L)
         generateVariableListMatchingCode(left_edge->label, 6, file);
      else generateFixedListMatchingCode(left_edge->label, 6, file);
      PTFI("if(match)\n", 6);
      PTFI("{\n", 6);
      PTFI("addEdgeMap(morphism, %d, host_edge->index, new_assignments);\n", 9, left_edge->index);
      PTFI("matched_edges[%d] = host_edge->index;\n", 9, left_edge->index);
   
      bool total_match = emitNextMatcherCall(next_op, 9);
      if(!total_match) 
      {
         PTFI("if(result) return true;\n", 9);
         PTFI("else\n", 9);
         PTFI("{\n", 9);
         PTFI("removeEdgeMap(morphism);\n", 12);
         PTFI("matched_edges[%d] = -1;\n", 12, left_edge->index);
         PTFI("}\n", 9);
      } 
      PTFI("}\n", 6);
      PTFI("else removeAssignments(morphism, new_assignments);\n", 6);
      PTFI("}\n", 3);
   } 
   PTFI("return false;\n", 3);
   PTF("}\n\n");
}

bool emitNextMatcherCall(SearchOp *next_operation, int indent)
{
   if(next_operation == NULL)    
   {
      PTFI("/* All items matched! */\n", indent);
      PTFI("return true;\n", indent);
      return true;
   }
   switch(next_operation->type)
   {
      case 'n':
      case 'r':
           if(next_operation->next == NULL)
              PTFI("bool result = match_n%d(morphism, matched_nodes);\n", 
	            indent, next_operation->index);
           else 
              PTFI("bool result = match_n%d(morphism, matched_nodes, "
                    "matched_edges);\n", indent, next_operation->index);
           break;

      case 'i':
      case 'o':
      case 'b':
           if(next_operation->next == NULL)
              PTFI("bool result = match_n%d(morphism, host_edge, "
                    "matched_nodes);\n", indent, next_operation->index);
           else 
              PTFI("bool result = match_n%d(morphism, host_edge, "
                    "matched_nodes, matched_edges);\n", indent, 
                    next_operation->index);
           break;
  
      case 'e':
      case 's':
      case 't':
      case 'l':
           if(next_operation->next == NULL)
              PTFI("bool result = match_e%d(morphism, matched_edges);\n",
                    indent, next_operation->index);
           else 
              PTFI("bool result = match_e%d(morphism, matched_nodes, "
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
   /* If the LHS is the empty graph, emit code to add the complete RHS graph
    * to the host graph. The morphism is not required for the generated 
    * rule application function. */
   if(empty_lhs)
   {
      PTH("void apply%s(bool record_changes);\n", rule->name);
      PTF("void apply%s(bool record_changes)\n", rule->name);
      PTF("{\n");
      PTFI("int index;\n", 3);
      PTFI("Label label;\n", 3);
      int index;
      if(rule->added_edges != NULL)
      {
         PTFI("/* Array of host node indices indexed by RHS node index. */\n", 3);
         PTFI("int map[%d];\n\n", 3, rhs->number_of_nodes);
      }
      for(index = 0; index < rhs->number_of_nodes; index++)
      {
         /* Add each node to the host graph. If the rule adds edges, extra
          * code is emitted to maintain a rule-to-host index map so that
          * the correct edges are added. */
         Node *rule_node = getNode(rhs, index);
         if(rule_node->label.length == 0 && rule_node->label.mark == NONE)
            PTFI("index = addNode(host, %d, blank_label);\n", 3, rule_node->root);
         else
         {
            generateRHSLabelCode(rule_node->label, index, 3, file);
            PTFI("index = addNode(host, %d, label);\n", 3, rule_node->root);
         }
         if(rule->added_edges != NULL) PTFI("map[%d] = index;\n", 3, rule_node->index);
         PTFI("if(record_changes) pushAddedNode(index);\n", 3);
      }
      NewEdgeList *iterator = rule->added_edges;
      while(iterator != NULL)
      {
         Edge *rule_edge = getEdge(rhs, iterator->edge_index);
         /* The host-source and host-target of added edges are taken from the 
          * map populated in the previous loop. */
         if(rule_edge->label.length == 0 && rule_edge->label.mark == NONE)
            PTFI("index = addEdge(host, false, blank_label, map[%d], map[%d]);\n\n",
                 3, iterator->source_index, iterator->target_index);
         else
         {
            generateRHSLabelCode(rule_edge->label, index, 3, file);
            PTFI("index = addEdge(host, false, label, map[%d], map[%d]);\n\n",
                 3, iterator->source_index, iterator->target_index);
         }
         PTFI("if(record_changes) pushAddedEdge(index);\n", 3);
         iterator = iterator->next;
      }     
      PTFI("\n}\n\n", 3);
      return;
   }

   PTH("void apply%s(Morphism *morphism, bool record_changes);\n", rule->name);
   PTF("void apply%s(Morphism *morphism, bool record_changes)\n", rule->name);
   PTF("{\n");

   /* If the RHS is the empty graph, emit code to remove the image of the RHS
    * from the host graph. This code is simple enough to define in a macro. */
   if(empty_rhs)
   {
      PTFI("int count;\n", 3);
      PTFI("REMOVE_RHS\n", 3);
      PTFI("initialiseMorphism(M_%s);\n", 3, rule->name);
      PTF("}\n\n");
      return;
   }
   
   bool label_declared = false;
   if(lhs->number_of_edges > 0) PTFI("int host_edge_index;\n", 3);
   int index;
   for(index = 0; index < lhs->number_of_edges; index++)
   {
      PreservedItemList *item = queryPItemList(rule->preserved_edges, index);
      if(item == NULL) 
      {
         /* Generate code to remove the edge. */
         PTFI("host_edge_index = lookupEdge(morphism, %d);\n", 3, index);
         PTFI("if(record_changes)\n", 3);
         PTFI("{\n", 3);
         PTFI("Edge *edge = getEdge(host, host_edge_index);\n", 6);
         PTFI("pushRemovedEdge(false, edge->label, edge->source, edge->target);\n", 6);
         PTFI("}\n", 3);
         PTFI("removeEdge(host, host_edge_index);\n", 3);   
      }
      else
      {
         if(item->new_label != NULL)
         {
            /* Generate code to relabel the edge. */
            PTFI("host_edge_index = lookupEdge(morphism, %d);\n", 3, index);
            Label label = *(item->new_label);
            if(label.length == 0 && label.mark == NONE)
            {
               PTFI("relabelEdge(host, host_edge_index, blank_label);\n", 3);
               PTFI("if(record_changes)\n", 3);
               PTFI("pushRelabelledEdge(host_edge_index, false, blank_label);\n", 6);
            }
            else
            {
               if(!label_declared) 
               {
                  PTFI("Label label;\n", 3);
                  label_declared = true;
               }
               generateRHSLabelCode(label, index, 3, file);
               PTFI("relabelEdge(host, host_edge_index, label);\n\n", 3);
               PTFI("if(record_changes)\n", 3);
               PTFI("pushRelabelledEdge(host_edge_index, false, label);\n", 6);
            }
         }
      }
   }
   if(rule->added_edges != NULL) 
   {
      PTFI("/* Array of host node indices indexed by RHS node index. */\n", 3);
      PTFI("int lhs_node_map[%d];\n\n", 3, rhs->number_of_nodes);
   }
   PTFI("int host_node_index;\n", 3);
   bool change_root_declared = false;
   for(index = 0; index < lhs->number_of_nodes; index++)
   {
      PreservedItemList *item = queryPItemList(rule->preserved_nodes, index);
      if(item == NULL) 
      {
         /* Generate code to remove the node. */
         PTFI("host_node_index = lookupNode(morphism, %d);\n", 3, index);
         PTFI("if(record_changes)\n", 3);
         PTFI("{\n", 3);
         PTFI("Node *node = getNode(host, host_node_index);\n", 6);
         PTFI("pushRemovedNode(node->root, node->label);\n", 6);
         PTFI("}\n", 3);
         PTFI("removeNode(host, host_node_index);\n", 3);   
      }
      else
      {
         if(item->new_label != NULL)
         {
            /* Generate code to relabel the node. */
            PTFI("host_node_index = lookupNode(morphism, %d);\n", 3, index);
            Label label = *(item->new_label);
            /* The root is changed in two cases:
             * (1) The LHS node is rooted and the RHS node is non-rooted.
             * (2) The LHS node is non-rooted, the RHS node is rooted, and
             *     the matched host node is non-rooted. 
             * The second case has to be handled at runtime. */
            bool change_root = false;
            Node *lhs_node = getNode(lhs, index);
            /* Case (1) */
            if(lhs_node->root && !item->rhs_root) change_root = true;
            if(label.length == 0 && label.mark == NONE)
               PTFI("relabelNode(host, host_node_index, blank_label);\n", 3);
            else
            {
               if(!label_declared) 
               {
                  PTFI("Label label;\n", 3);
                  label_declared = true;
               }
               generateRHSLabelCode(label, index, 3, file);
               PTFI("relabelNode(host, host_node_index, label);\n\n", 3);
            }
            if(!change_root_declared) 
            {
               PTFI("bool change_root = false;\n", 3);
               change_root_declared = true;
            }
            else PTFI("change_root = false;\n", 3);
            if(change_root) 
            {
               PTFI("changeRoot(host, host_node_index);\n", 3);
               PTFI("change_root = true;\n", 3);
            }
            /* Case (2) */
            if(lhs_node->root && item->rhs_root) 
            {
               PTFI("Node *node%d = getNode(host, host_node_index);\n", 3, index);
               PTFI("if(!(node%d->root))\n", 3, index);
               PTFI("{\n", 3);
               PTFI("changeRoot(host, host_node_index);\n", 6);
               PTFI("change_root = true;\n", 6);
               PTFI("}\n", 3);
            }
            PTFI("if(record_changes) ", 3);
            PTF("pushRelabelledNode(host_node_index, change_root, blank_label);\n");
            if(rule->added_edges != NULL) 
               PTFI("lhs_node_map[%d] = host_node_index;\n", 3, index);  
         }
      }
   }
   if(rule->added_nodes != NULL && rule->added_edges != NULL)
   {
      PTFI("/* Array of host node indices indexed by RHS node index. */\n", 3);
      PTFI("int rhs_node_map[%d];\n\n", 3, rhs->number_of_nodes);
   }
   ItemList *iterator_n = rule->added_nodes;
   while(iterator_n != NULL)
   {   
      Node *rule_node = getNode(rhs, iterator_n->index);
      if(rule_node->label.length == 0 && rule_node->label.mark == NONE)
         PTFI("host_node_index = addNode(host, %d, blank_label);\n", 3, rule_node->root);
      else
      {
         if(!label_declared) 
         {
            PTFI("Label label;\n", 3);
            label_declared = true;
         }
         generateRHSLabelCode(rule_node->label, index, 3, file);
         PTFI("host_node_index = addNode(host, %d, label);\n", 3, rule_node->root);
      }
      if(rule->added_edges != NULL) 
         PTFI("rhs_node_map[%d] = host_node_index;\n", 3, rule_node->index);
      PTFI("if(record_changes) pushAddedNode(host_node_index);\n", 3);
      iterator_n = iterator_n->next;
   }   
   NewEdgeList *iterator_e = rule->added_edges;
   if(iterator_e != NULL)
   {
      /* host_edge_index was not declared previously if the LHS has 0 edges. */
      if(lhs->number_of_edges == 0) PTFI("int host_edge_index;\n", 3);
      PTFI("int source = 0, target = 0;\n\n", 3);
   }
   while(iterator_e != NULL)
   {
      /* The source and target edges are either nodes preserved by the rule or 
       * nodes added by the rule. 
       * The host node indices of preserved nodes are obtained from lhs_node_map.
       * The host node indices of added nodes are obtained from rhs_node_map. */
      if(iterator_e->source_location == 'l')
           PTFI("source = lhs_node_map[%d].host_index;\n", 3, iterator_e->source_index);
      else PTFI("source = rhs_node_map[%d];\n", 3, iterator_e->source_index);

      if(iterator_e->target_location == 'l')
           PTFI("target = lhs_node_map[%d].host_index;\n", 3, iterator_e->target_index);
      else PTFI("target = rhs_node_map[%d];\n", 3, iterator_e->target_index);

      Edge *rule_edge = getEdge(rhs, iterator_e->edge_index);
      if(rule_edge->label.length == 0 && rule_edge->label.mark == NONE)
         PTFI("host_edge_index = addEdge(host, false, blank_label, source, target);\n", 3);
      else
      {
         if(!label_declared) 
         {
            PTFI("Label label;\n", 3);
            label_declared = true;
         }
         generateRHSLabelCode(rule_edge->label, index, 3, file);
         PTFI("host_edge_index = addEdge(host, false, label, source, target);\n", 3);
      }
      PTFI("if(record_changes) pushAddedEdge(host_edge_index);\n", 3);
      iterator_e = iterator_e->next;      
   }
   PTFI("/* Reset the morphism. */\n", 3);
   PTFI("initialiseMorphism(morphism);\n}\n\n", 3);
}
