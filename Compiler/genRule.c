/* ////////////////////////////////////////////////////////////////////////////

 * The functions in this file print various macros. macros.h contains their
 * definitions.

/////////////////////////////////////////////////////////////////////////// */

#include "genRule.h"

FILE *rule_header = NULL;
FILE *rule_source = NULL;
 
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
        "#include \"../graphStacks.h\"\n"
        "#include \"../macros.h\"\n"
        "#include \"match.h\"\n"
        "#include \"host.h\"\n\n");
   PTRS("#include \"%s.h\"\n\n", rule_name);

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
   fclose(rule_header);
   fclose(rule_source);
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
   /* Print the main matching function which sets up the matching environment
    * and calls the first matcher in the searchplan. */
   emitRuleMatcher(rule_name, searchplan->first, lhs->number_of_nodes, 
                   lhs->number_of_edges);
   PTRS("\n\n");
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

   PTRH("bool match%s(Morphism *morphism);\n", rule_name);
   PTRS("\nstatic int left_nodes = %d, left_edges = %d;\n\n"
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
      PTRS("   MAKE_MATCHED_EDGES_ARRAY\n\n");

   if(first_op->next == NULL)
      PTRSI("if(match_%c%d(morphism, matched_nodes)) return true;\n",
            3, item, first_op->index);
   else
      PTRSI("if(match_%c%d(morphism, matched_nodes, matched_edges)) return true;\n",
            3, item, first_op->index);
   PTRS("   else\n"
        "   {\n"
        "      clearMorphism(morphism);\n"
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
      PTRS("static bool match_n%d(Morphism *morphism, int *matched_nodes)\n"
           "{\n", left_index);
   else
      PTRS("static bool match_n%d(Morphism *morphism, int *matched_nodes, "
           "int *matched_edges)\n"
           "{\n", left_index);
   /* Emit code to initialise the iteration over the candidate nodes. 
    * If the left node is rooted, interrogate the root node list of the host
    * graph, otherwise query the appropriate nodes-by-label-class list. */
    if(is_root) 
    {
       PTRSI("bool node_matched = false;\n", 3);
       PTRSI("RootNodes *nodes = NULL;\n", 3);   
       PTRSI("for(nodes = getRootNodeList(host); nodes != NULL;", 3);
       PTRSI("nodes = nodes->next)\n", 1);
       PTRSI("{\n", 3);
       PTRSI("Node *host_node = getNode(host, nodes->index);\n", 6);
    }
    else
    {
       PTRSI("bool node_matched = false;\n", 3);
       PTRSI("int count;\n", 3);
       PTRSI("LabelClassTable nodes = getNodesByLabel(host, %d);\n", 3,
             left_node->label_class);
       PTRSI("for(count = 0; count < nodes.index; count++)\n", 3);
       PTRSI("{\n", 3);
       PTRSI("Node *host_node = getNode(host, nodes.items[count]);\n", 6);
    }
    PTRSI("if(host_node == NULL) continue;\n\n", 6);
    PTRSI("node_matched = false;\n", 6);
    PTRSI("/* Set node_matched to true if the node has already been", 6);
    PTRSI("matched. */\n", 1);
    PTRSI("CHECK_MATCHED_NODE\n\n", 6);
   /* Emit code to check the node_matched flag and to test if the candidate 
    * host node is consistent with the left node with respect to label class, 
    * mark, and degrees. If not, the loop will continue without entering the 
    * potentially expensive label matching code. */
   PTRSI("/* Arguments: label class, mark, indegree, outdegree, bidegree. */\n", 6);
   if(dangling_node)
        PTRSI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d, %d) continue;\n\n", 6,
              left_node->label_class, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree);
   else PTRSI("IF_INVALID_NODE(%d, %d, %d, %d, %d) continue;\n\n", 6,
              left_node->label_class, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree); 

   /* TODO: Call to label matcher goes here. */
   PTRSI("Label *label = makeEmptyList(%d);\n", 6, left_node->label.mark);
   PTRSI("bool nodes_match = labelMatch(label, host_node->label);\n", 6);
   PTRSI("if(!isConstantLabel(label)) freeLabel(label);\n", 6);
   PTRSI("if(nodes_match)\n", 6);
   PTRSI("{\n", 6);
   PTRSI("addNodeMap(morphism, %d, host_node->index);\n", 9, left_index);
   PTRSI("matched_nodes[%d] = host_node->index;\n", 9, left_index);
   bool total_match = emitNextMatcherCall(next_op, 9);
   if(!total_match) PTRSI("HANDLE_RESULT(%d)\n", 9, left_index);
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
              left_node->label_class, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree);
   else PTRSI("IF_INVALID_NODE(%d, %d, %d, %d, %d)", 3,
              left_node->label_class, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree); 

   if(type == 'b')
   {
      PTRS("\n   {\n"); 
      PTRSI("/* Matching from bidirectional edge: check the second incident node. */\n", 6);
      if(type == 'i' || type == 'b') 
           PTRSI("host_node = getNode(host, getSource(host_edge));\n\n", 6);
      else PTRSI("host_node = getNode(host, getTarget(host_edge));\n\n", 6);
      PTRSI("bool node_matched = false;\n", 6);
      PTRSI("/* Set node_matched to true if the node has already been matched. */\n", 6);
      PTRSI("CHECK_MATCHED_NODE\n\n", 6);
      PTRSI(" /* Arguments: label class, mark, indegree, outdegree, bidegree. */\n", 6);
      if(dangling_node)
           PTRSI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d, %d) return false;\n", 6,
                 left_node->label_class, left_node->label.mark, 
                 left_node->indegree, left_node->outdegree, left_node->bidegree);
      else PTRSI("IF_INVALID_NODE(%d, %d, %d, %d, %d) return false;\n", 6,
                 left_node->label_class, left_node->label.mark, 
                 left_node->indegree, left_node->outdegree, left_node->bidegree); 
      PTRSI("}\n\n", 3);
   }
   else PTRS(" return false;\n\n");
 
   PTRSI("Label *label = makeEmptyList(%d);\n", 3, left_node->label.mark);
   PTRSI("bool nodes_match = labelMatch(label, host_node->label);\n", 3);
   PTRSI("if(!isConstantLabel(label)) freeLabel(label);\n", 3);
   PTRSI("if(nodes_match)\n", 3);
   PTRSI("{\n", 3);
   PTRSI("addNodeMap(morphism, %d, host_node->index);\n", 6, left_index);
   PTRSI("matched_nodes[%d] = host_node->index;\n", 6, left_index);

   bool total_match = emitNextMatcherCall(next_op, 6); 
   if(!total_match) PTRSI("HANDLE_RESULT(%d)\n", 9, left_index);
   PTRSI("}\n", 3);
   PTRSI("return false;\n", 3);
   PTRS("}\n\n");
}


void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op)
{
   if(next_op == NULL) PTRS("static bool match_e%d(Morphism *morphism,"
                            "int *matched_edges)\n{\n", left_edge->index);
   else PTRS("static bool match_e%d(Morphism *morphism, int *matched_nodes, "
             "int *matched_edges)\n{\n", left_edge->index);

   PTRSI("int count;\n", 3);
   PTRSI("bool edge_matched = false;\n", 3);
   PTRSI("LabelClassTable edges = getEdgesByLabel(host, %d);\n", 3,
         left_edge->label_class);
   PTRSI("for(count = 0; count < edges.index; count++)", 3);
   PTRSI("{\n", 3);
   PTRSI("Edge *host_edge = getEdge(host, edges.items[count];\n\n", 6);
   PTRSI("/* Set edge_matched to true if the edge has already been", 6);
   PTRSI("matched. */\n", 1);
   PTRSI("CHECK_MATCHED_EDGE\n\n", 6);
   PTRSI("if(edge_matched) continue;\n\n", 6);

   /* Emit code to test the matched_edge flag and if the candidate host edge 
    * is consistent with the left edge with respect to label class, mark, and
    * loopiness. If not, the loop will continue without entering the 
    * potentially expensive label matching code. */
   PTRSI(" /* Arguments: label class, mark. */\n", 6);
   if(left_edge->source == left_edge->target) 
        PTRSI("IF_INVALID_LOOP_EDGE(%d, %d)\n\n", 6,
              left_edge->label_class, left_edge->label.mark);
   else PTRSI("IF_INVALID_EDGE(%d, %d)\n\n", 6, 
              left_edge->label_class, left_edge->label.mark);
   PTRSI("/* If either endpoint has been matched, check that the corresponding\n", 6);
   PTRSI(" * endpoint of the host edge is the image of the node in question. */\n", 6);
   PTRSI("continue;\n\n", 6);
   PTRSI("int source_index = findHostIndex(morphism, %d);\n", left_edge->source, 6);
   PTRSI("if(source_index >= 0 && host_edge->source != source_index) continue;\n", 6);
   PTRSI("int target_index = findHostIndex(morphism, %d);\n", left_edge->target, 6);
   PTRSI("if(target_index >= 0 && host_edge->target != target_index) continue;\n", 6);

   PTRSI("Label *label = makeEmptyList(%d);\n", 6, left_edge->label.mark);
   PTRSI("bool edges_match = labelMatch(label, host_edge->label);\n", 6);
   PTRSI("if(!isConstantLabel(label)) freeLabel(label);\n", 6);
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
   if(next_op == NULL) PTRS("static bool match_e%d(Morphism *morphism, "
                            "int *matched_edges)\n{\n", left_edge->index);
   else PTRS("static bool match_e%d(Morphism *morphism, int *matched_nodes, "
             "int *matched_edges)\n{\n", left_edge->index);

   PTRSI("int source_index = findHostIndex(morphism, %d);\n", 3,
         left_edge->source);
   PTRSI("if(source_index < 0) return false;\n", 3);
   PTRSI("Node *host_node = getNode(host, source_index);\n\n", 3);
   PTRSI("int target_index = findHostIndex(morphism, %d);\n", 3,
         left_edge->target);
   PTRSI("int counter;\n", 3);
   PTRSI("bool edge_matched = false;\n", 3);
   PTRSI("for(counter = host_node->out_index - 1; counter >= 0; counter--)\n", 3);
   PTRSI("{\n", 3);
   PTRSI("Edge *host_edge = getEdge(host, getOutEdge(host_node, counter));\n", 6);
   PTRSI("if(host_edge == NULL) continue;\n\n", 6);
   PTRSI("edge_matched = false;\n", 6);
   PTRSI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
   PTRSI("CHECK_MATCHED_EDGE\n\n", 6);
   PTRSI("/* Arguments: label class, mark. */\n", 6);
   if(is_loop) 
        PTRSI("IF_INVALID_LOOP_EDGE(%d, %d) continue;\n\n", 6,
              left_edge->label_class, left_edge->label.mark);
   else PTRSI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
              left_edge->label_class, left_edge->label.mark);

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
   PTRSI("Label *label = makeEmptyList(%d);\n", 6, left_edge->label.mark);
   PTRSI("bool edges_match = labelMatch(label, host_edge->label);\n", 6);
   PTRSI("if(!isConstantLabel(label)) freeLabel(label);\n", 6);
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
                 left_edge->label_class, left_edge->label.mark);
      else PTRSI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
                 left_edge->label_class, left_edge->label.mark);
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
      PTRSI("Label *label = makeEmptyList(%d);\n", 6, left_edge->label.mark);
      PTRSI("bool edges_match = labelMatch(label, host_edge->label);\n", 6);
      PTRSI("if(!isConstantLabel(label)) freeLabel(label);\n", 6);
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
   if(next_op == NULL) PTRS("static bool match_e%d(Morphism *morphism, "
                            "int *matched_edges)\n{\n", left_edge->index);
   else PTRS("static bool match_e%d(Morphism *morphism, int *matched_nodes, "
             "int *matched_edges)\n{\n", left_edge->index);

   PTRSI("int target_index = findHostIndex(morphism, %d);\n", 3,
         left_edge->target);
   PTRSI("if(target_index < 0) return false;\n", 3);
   PTRSI("Node *host_node = getNode(host, target_index);\n\n", 3);
   PTRSI("int source_index = findHostIndex(morphism, %d);\n", 3,
         left_edge->source);
   PTRSI("int counter;\n\n", 3);
   PTRSI("bool edge_matched = false;\n", 3);
   PTRSI("for(counter = host_node->in_index - 1; counter >= 0; counter--)\n", 3);
   PTRSI("{\n", 3);
   PTRSI("Edge *host_edge = getEdge(host, getInEdge(host_node, counter));\n", 6);
   PTRSI("if(host_edge == NULL) continue;\n\n", 6);
   PTRSI("edge_matched = false;\n", 6);
   PTRSI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
   PTRSI("CHECK_MATCHED_EDGE\n\n", 6);
   PTRSI(" /* Arguments: label class, mark. */\n", 6);
   PTRSI("IF_INVALID_EDGE(%d, %d) continue;\n\n", 6, 
         left_edge->label_class, left_edge->label.mark);
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
   PTRSI("Label *label = makeEmptyList(%d);\n", 6, left_edge->label.mark);
   PTRSI("bool edges_match = labelMatch(label, host_edge->label);\n", 6);
   PTRSI("if(!isConstantLabel(label)) freeLabel(label);\n", 6);
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
            left_edge->label_class, left_edge->label.mark);
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
      PTRSI("Label *label = makeEmptyList(%d);\n", 6, left_edge->label.mark);
      PTRSI("bool edges_match = labelMatch(label, host_edge->label);\n", 6);
      PTRSI("if(!isConstantLabel(label)) freeLabel(label);\n", 6);
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
   /* If the LHS is the empty graph, emit code to add the complete RHS graph
    * to the host graph. The morphism is not required for the generated 
    * rule application function. */
   if(empty_lhs)
   {
      PTRH("void apply%s(bool record_changes);\n", rule->name);
      PTRS("void apply%s(bool record_changes)\n", rule->name);
      PTRS("{\n");
      PTRSI("index = -1;\n", 3);
      int index, label_count = 0;
      if(rule->added_edges != NULL)
      {
         PTRSI("/* Array of host node indices indexed by RHS node index. */\n", 3);
         PTRSI("int map[%d];\n\n", 3, rhs->number_of_nodes);
      }
      for(index = 0; index < rhs->number_of_nodes; index++)
      {
         /* Add each node to the host graph. If the rule adds edges, extra
          * code is emitted to maintain a rule-to-host index map so that
          * the correct edges are added. */
         Node *rule_node = getNode(rhs, index);
         PTRSI("Label *label%d = makeEmptyList(%d);\n", 3,
               label_count, rule_node->label.mark);
         PTRSI("index = addNode(host, %d, label%d);\n\n", 3, 
               rule_node->root, label_count);
         if(rule->added_edges != NULL) 
            PTRSI("map[%d] = index;\n", 3, rule_node->index);
         PTRSI("if(record_changes) pushAddedNode(index);\n", 3);
         label_count++;
      }
      NewEdgeList *iterator = rule->added_edges;
      while(iterator != NULL)
      {
         Edge *rule_edge = getEdge(rhs, iterator->edge_index);
         PTRSI("Label *label%d = makeEmptyList(%d);\n", 3, label_count,
               rule_edge->label.mark);
         /* The host-source and host-target of added edges are taken from the 
          * map populated in the previous loop. */
         PTRSI("index = addEdge(host, false, label%d, map[%d], map[%d]);\n\n",
               3, label_count, iterator->source_index, iterator->target_index);
         PTRSI("if(record_changes) pushAddedEdge(index);\n", 3);
         iterator = iterator->next;
         label_count++;
      }     
      PTRSI("\n}\n\n", 3);
      return;
   }

   PTRH("void apply%s(Morphism *morphism, bool record_changes);\n", rule->name);
   PTRS("void apply%s(Morphism *morphism, bool record_changes)\n", rule->name);
   PTRS("{\n");

   /* If the RHS is the empty graph, emit code to remove the image of the RHS
    * from the host graph. This code is simple enough to define in a macro. */
   if(empty_rhs)
   {
      PTRSI("int count;\n", 3);
      PTRSI("REMOVE_RHS\n", 3);
      PTRSI("clearMorphism(M_%s);\n", 3, rule->name);
      PTRS("}\n\n");
      return;
   }
   
   /* Using the preserved items lists, populate the runtime arrays. */
   PTRSI("RewriteData node_map[%d];\n", 3, lhs->number_of_nodes);
   int index;
   for(index = 0; index < lhs->number_of_nodes; index++)
   {
      PreservedItemList *item = queryPItemList(rule->preserved_nodes, index);
      if(item == NULL) 
      {
         PTRSI("node_map[%d].remove_item = true;\n", 3, index);
         PTRSI("node_map[%d].rhs_root = false;\n", 3, index);
         PTRSI("node_map[%d].new_label = NULL;\n", 3, index);
         continue;
      }
      else
      {
         PTRSI("node_map[%d].remove_item = false;\n", 3, index);
         PTRSI("node_map[%d].rhs_root = %d;\n", 3, index, item->rhs_root);
         if(item->new_label == NULL)
              PTRSI("node_map[%d].new_label = NULL;\n", 3, index);
         else PTRSI("node_map[%d].new_label = makeEmptyList(%d);\n", 3, index,
                    item->new_label->mark);
      }
      PTRSI("node_map[%d].host_index = -1;\n\n", 3, index);
   } 
   if(lhs->number_of_edges > 0)
      PTRSI("RewriteData edge_map[%d];\n", 3, lhs->number_of_edges);
   for(index = 0; index < lhs->number_of_edges; index++)
   {
      PTRSI("edge_map[%d].rhs_root = false;\n", 3, index);
      PreservedItemList *item = queryPItemList(rule->preserved_edges, index);
      if(item == NULL) 
      {
         PTRSI("edge_map[%d].remove_item = true;\n", 3, index);
         PTRSI("edge_map[%d].new_label = NULL;\n", 3, index);
         continue;
      }
      else
      {
         PTRSI("edge_map[%d].remove_item = false;\n", 3, index);
         if(item->new_label == NULL)
              PTRSI("edge_map[%d].new_label = NULL;\n", 3, index);
         else PTRSI("edge_map[%d].new_label = makeEmptyList(%d);\n", 3, index,
                    item->new_label->mark);
      }
      PTRSI("edge_map[%d].host_index = -1;\n\n", 3, index);
   }
   PTRS("   int count, left_index, host_index;\n");
   if(lhs->number_of_edges > 0)
   {
      PTRSI("/* Pops all edges in the morphism stack and deletes/preserves\n", 3);
      PTRSI(" * host items according to the entries in edge_map. */\n", 3);
      PTRSI("PROCESS_EDGE_MORPHISMS\n", 3);
   }
   PTRSI("/* Pops all node in the morphism stack and deletes/preserves\n", 3);
   PTRSI(" * host items according to the entries in node_map. */\n", 3);
        /* It is worth noting here that PROCESS_NODE_MORPHISMS updates the
         * node_map so that, for preserved leftnnode n, node_map[n] is the
         * index of its associated host node. This is used when adding edges
         * to the host graph. */
   PTRSI("PROCESS_NODE_MORPHISMS\n\n", 3);
   if(rule->added_nodes != NULL || rule->added_edges != NULL)
      PTRSI("int index = -1;\n", 3);
   int label_count = 0;
   if(rule->added_nodes != NULL && rule->added_edges != NULL)
      PTRS("   /* Array of host node indices indexed by RHS node index. */\n"
           "   int map[%d];\n\n", rhs->number_of_nodes);
   ItemList *iterator_n = rule->added_nodes;
   while(iterator_n != NULL)
   {   
      Node *rule_node = getNode(rhs, iterator_n->index);
      PTRSI("Label *label%d = makeEmptyList(%d);\n", 3, label_count,
            rule_node->label.mark);
      PTRSI("index = addNode(host, %d, label%d);\n", 3, rule_node->root,
            label_count);
      if(rule->added_edges != NULL) PTRSI("map[%d] = index;\n", 3, rule_node->index);
      PTRSI("if(record_changes) pushAddedNode(index);\n", 3);
      label_count++;
      iterator_n = iterator_n->next;
   }   
   NewEdgeList *iterator_e = rule->added_edges;
   if(iterator_e != NULL) PTRSI("int source = 0, target = 0;\n\n", 3);
   while(iterator_e != NULL)
   {
      /* The source and target edges may be nodes preserved by the rule or 
       * nodes added by the rule. Both cases must be handled distinctly as they
       * require the host node index to be obtained in different ways.
       *
       * For preserved nodes, the host node index is obtained from node_map
       * (see macro PROCESS_NODE_MORPHISMS in macros.h) that is used to index
       * into the host graph node pointer array. For added nodes, the index is 
       * obtained directly from map. */
      if(iterator_e->source_location == 'l')
           PTRSI("source = node_map[%d].host_index;\n", 3, 
                 iterator_e->source_index);
      else PTRSI("source = map[%d];\n", 3, iterator_e->source_index);

      if(iterator_e->target_location == 'l')
           PTRSI("target = node_map[%d].host_index;\n", 3,
                 iterator_e->target_index);
      else PTRSI("target = map[%d];\n", 3, iterator_e->target_index);

      Edge *rule_edge = getEdge(rhs, iterator_e->edge_index);
      PTRSI("Label *label%d = makeEmptyList(%d);\n", 3, label_count,
            rule_edge->label.mark);
      PTRSI("index = addEdge(host, false, label%d, source, target);\n", 3, 
            label_count);
      PTRSI("if(record_changes) pushAddedEdge(index);\n", 3);
      label_count++;
      iterator_e = iterator_e->next;      
   }
   PTRSI("clearMorphism(morphism);\n}\n\n", 3);
}
