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
              Rule *rule = transformRule(decl->rule);
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

   if(rule->condition != NULL)
   {
      PTF("bool evalCond_%s(void)\n{\n", rule_name);
      PTFI("return ", 3);
      generateBoolExpression(rule->condition, false);
      PTF("}\n\n");
   }

   if(rule->lhs == NULL) generateAddRHSCode(rule);
   else
   {
      generateMatchingCode(rule_name, rule->lhs, rule->deleted_nodes);
      if(rule->rhs == NULL && !predicate) generateRemoveLHSCode(rule_name);
      else if(rule->rhs != NULL && !predicate) generateApplicationCode(rule);
   }
   fclose(header);
   fclose(file);
   return;
}

void generateBoolExpression(Condition *condition, bool nested)
{
   static int bool_count = 0;
   switch(condition->type)
   {
      case BOOL_NOT:
           PTF("!b%d", bool_count++);
           break;

      case BOOL_AND:
           if(nested) PTF("(");
           generateBoolExpression(condition->bin_op.left_exp, true);
           PTF(" && ");
           generateBoolExpression(condition->bin_op.right_exp, true);
           if(nested) PTF(")");
           break;
           
      case BOOL_OR:
           if(nested) PTF("(");
           generateBoolExpression(condition->bin_op.left_exp, true);
           PTF(" || ");
           generateBoolExpression(condition->bin_op.right_exp, true);
           if(nested) PTF(")");
           break;

      default:
           PTF("b%d", bool_count++);
           break;
   }
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
              PTF("static bool match_n%d(Morphism *morphism);\n", operation->index);
              break;

         case 'i': 
         case 'o': 
         case 'b':
              PTF("static bool match_n%d(Morphism *morphism, Edge *host_edge);\n",
                  operation->index);
              break;

         case 'e': 
         case 's': 
         case 't':
         case 'l':
              PTF("static bool match_e%d(Morphism *morphism);\n", operation->index);
              break;

         default:
              print_to_log("Error (generateMatchingCode): Unexpected "
                           "operation type %c.\n", operation->type);
              break;
      }
      operation = operation->next;
   }
   /* Print the main matching function which sets up the matching environment. */
   PTH("bool match%s(Morphism *morphism);\n", rule_name);
   PTF("\nstatic int left_nodes = %d, left_edges = %d;\n", 
       lhs->number_of_nodes, lhs->number_of_edges);
   PTF("static int matched_nodes[%d];\n", lhs->number_of_nodes);
   if(lhs->number_of_edges > 0) PTF("static int matched_edges[%d];\n\n", lhs->number_of_edges);
   else PTF("\n");
   PTF("bool match%s(Morphism *morphism)\n{\n", rule_name);
   PTFI("if(left_nodes > host->number_of_nodes ||\n", 3);
   PTFI("left_edges > host->number_of_edges) return false;\n\n", 6);
   PTFI("int count;\n", 3);
   PTFI("for(count = 0; count < left_nodes; count++) matched_nodes[count] = -1;\n", 3);
   if(lhs->number_of_edges > 0) 
      PTFI("for(count = 0; count < left_edges; count++) matched_edges[count] = -1;\n", 3);

   /* Call the first matcher in the searchplan. */
   char item = searchplan->first->is_node ? 'n' : 'e';
   PTFI("if(match_%c%d(morphism)) return true;\n", 3, item, searchplan->first->index);
   PTFI("else\n   {\n", 3);
   PTFI("initialiseMorphism(morphism);\n", 6);
   PTFI("return false;\n   }\n}\n\n", 6);
   operation = searchplan->first;
   while(operation != NULL)
   {
      switch(operation->type)
      {        
         case 'n': 
              node = getNode(lhs, operation->index);
              emitNodeMatcher(node, deleted_nodes, operation->next);
              break;

         case 'r': 
              node = getNode(lhs, operation->index);
              emitRootNodeMatcher(node, deleted_nodes, operation->next);
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
              emitLoopEdgeMatcher(edge, operation->next);
              break;

         case 's': 
              edge = getEdge(lhs, operation->index);
              if(edge->bidirectional) emitBiEdgeFromSourceMatcher(edge, operation->next);
              else emitEdgeFromSourceMatcher(edge, true, true, false, operation->next);
              break;

         case 't':
              edge = getEdge(lhs, operation->index);
              if(edge->bidirectional) emitBiEdgeFromTargetMatcher(edge, operation->next);
              else emitEdgeFromTargetMatcher(edge, true, true, false, operation->next);
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

void emitRootNodeMatcher(Node *left_node, ItemList *deleted_nodes, SearchOp *next_op)
{
   int left_index = left_node->index;
   PTF("static bool match_n%d(Morphism *morphism)\n{\n", left_index);
   PTFI("bool node_matched = false;\n", 3);
   PTFI("RootNodes *nodes = getRootNodeList(host);\n", 3);   
   PTFI("while(nodes != NULL)\n   {\n", 3);
   PTFI("Node *host_node = getNode(host, nodes->index);\n", 6);
   PTFI("if(host_node == NULL) continue;\n\n", 6);
   PTFI("node_matched = false;\n", 6);
   PTFI("int index;\n", 6);
   PTFI("/* Search matched_nodes, set node_matched to true if the node is in the array.*/\n", 6);
   PTFI("CHECK_MATCHED_NODE;\n\n", 6);

   /* Emit code to check the node_matched flag and to test if the candidate 
    * host node is consistent with the left node with respect to mark and
    * degrees. If not, the node can be discarded before label matching. */
   PTFI("/* Arguments: mark, indegree, outdegree, bidegree. */\n", 6);
   /* queryItemList returns true if the passed node is deleted by the rule. */
   if(queryItemList(deleted_nodes, left_index))
        PTFI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d)\n", 6,
             left_node->label.mark, left_node->indegree, left_node->outdegree,
             left_node->bidegree);
   else PTFI("IF_INVALID_NODE(%d, %d, %d, %d)\n", 6,
             left_node->label.mark, left_node->indegree, left_node->outdegree,
             left_node->bidegree); 
   PTFI("{\n", 6);
   PTFI("nodes = nodes->next;\n", 9);
   PTFI("continue;\n", 9);
   PTFI("}\n", 6);

   PTFI("Label label = host_node->label;\n", 6);
   PTFI("bool match = false;\n", 6);
   if(hasListVariable(left_node->label))
      generateVariableListMatchingCode(left_node->label, 6, file);
   else generateFixedListMatchingCode(left_node->label, 6, file);

   generateNodeMatchResultCode(left_index, next_op, 6);
   PTFI("nodes = nodes->next;\n", 6);
   PTFI("}\n", 3);
   PTFI("return false;\n", 3);
   PTF("}\n\n");
}

void emitNodeMatcher(Node *left_node, ItemList *deleted_nodes, SearchOp *next_op)
{
   int left_index = left_node->index;
   PTF("static bool match_n%d(Morphism *morphism)\n{\n", left_index);
   PTFI("bool node_matched = false;\n", 3);
   /* Writes a double for loop. The outer loop iterates over variable mark_index.
    * The inner loop iterates over variable class_index. */
   generateIteratorCode(left_node->label, 3, file);
   PTFI("LabelClassTable *table = getNodeLabelTable(host, mark, label_class);\n", 9);
   PTFI("if(table == NULL) continue;\n", 9);
   PTFI("int items_index;\n", 9);
   PTFI("for(items_index = 0; items_index < table->index; items_index++)\n", 9);
   PTFI("{\n", 9);
   PTFI("int host_index = table->items[items_index];\n", 12);
   PTFI("Node *host_node = getNode(host, host_index);\n", 12);
   PTFI("if(host_node == NULL) continue;\n\n", 12);
   PTFI("node_matched = false;\n", 12);
   PTFI("/* Set node_matched to true if the node has already been matched.*/\n", 12);
   PTFI("int index;\n", 12);
   PTFI("CHECK_MATCHED_NODE;\n\n", 12);
   /* Emit code to check the node_matched flag and to test if the candidate 
    * host node is consistent with the left node with respect to mark and
    * degrees. If not, this node cannot participate in a valid match. */
   PTFI("/* Arguments: mark, indegree, outdegree, bidegree. */\n", 12);
   if(queryItemList(deleted_nodes, left_index))
        PTFI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d) continue;\n\n", 12,
             left_node->label.mark, left_node->indegree, left_node->outdegree,
             left_node->bidegree);
   else PTFI("IF_INVALID_NODE(%d, %d, %d, %d) continue;\n\n", 12,
             left_node->label.mark, left_node->indegree, left_node->outdegree,
             left_node->bidegree); 

   PTFI("Label label = host_node->label;\n", 12);
   PTFI("bool match = false;\n", 12);
   if(hasListVariable(left_node->label))
      generateVariableListMatchingCode(left_node->label, 12, file);
   else generateFixedListMatchingCode(left_node->label, 12, file);

   generateNodeMatchResultCode(left_index, next_op, 12);
   PTFI("}\n", 9);
   PTFI("}\n", 6);
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
   PTF("static bool match_n%d(Morphism *morphism, Edge *host_edge)\n{\n", left_node->index);
   if(type == 'i' || type == 'b') 
        PTFI("Node *host_node = getNode(host, getTarget(host_edge));\n\n", 3);
   else PTFI("Node *host_node = getNode(host, getSource(host_edge));\n\n", 3);
   PTFI("/* Set node_matched to true if the node has already been matched. */\n", 3);
   PTFI("bool node_matched = false;\n", 3);
   PTFI("int index;\n", 3);
   PTFI("CHECK_MATCHED_NODE;\n\n", 3);
   /* Emit code to check the node_matched flag and to test if the candidate 
    * host node is consistent with the left node with respect to mark and
    * degrees. If not, this node cannot participate in a valid match. */
   PTFI(" /* Arguments: mark, indegree, outdegree, bidegree. */\n", 3);
   if(queryItemList(deleted_nodes, left_node->index))
        PTFI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d)", 3, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree);
   else PTFI("IF_INVALID_NODE(%d, %d, %d, %d)", 3, left_node->label.mark, 
              left_node->indegree, left_node->outdegree, left_node->bidegree); 

   /* If the above check fails and the edge is bidirectional, check the other 
    * node incident to the host edge. Otherwise return false. */
   if(type == 'b')
   {
      PTF("\n   {\n"); 
      PTFI("/* Matching from bidirectional edge: check the second incident node. */\n", 6);
      if(type == 'i' || type == 'b') 
           PTFI("host_node = getNode(host, getSource(host_edge));\n\n", 6);
      else PTFI("host_node = getNode(host, getTarget(host_edge));\n\n", 6);
      PTFI("bool node_matched = false;\n", 6);
      PTFI("/* Set node_matched to true if the node has already been matched. */\n", 6);
      PTFI("CHECK_MATCHED_NODE;\n\n", 6);
      PTFI("/* Arguments: mark, indegree, outdegree, bidegree. */\n", 6);
      if(queryItemList(deleted_nodes, left_node->index))
           PTFI("IF_INVALID_DANGLING_NODE(%d, %d, %d, %d) return false;\n", 6,
                left_node->label.mark, left_node->indegree, left_node->outdegree,
                left_node->bidegree);
      else PTFI("IF_INVALID_NODE(%d, %d, %d, %d) return false;\n", 6,
                left_node->label.mark, left_node->indegree, left_node->outdegree,
                left_node->bidegree); 
      PTFI("}\n\n", 3);
   }
   else PTF(" return false;\n\n");
 
   PTFI("Label label = host_node->label;\n", 3);
   PTFI("bool match = false;\n", 3);
   if(hasListVariable(left_node->label))
      generateVariableListMatchingCode(left_node->label, 3, file);
   else generateFixedListMatchingCode(left_node->label, 3, file);

   generateNodeMatchResultCode(left_node->index, next_op, 3);
   PTFI("return false;\n", 3);
   PTF("}\n\n");
}

void generateNodeMatchResultCode(int index, SearchOp *next_op, int indent)
{
   PTFI("if(match)\n", indent);
   PTFI("{\n", indent);
   PTFI("addNodeMap(morphism, %d, host_node->index, new_assignments);\n", indent + 3, index);
   PTFI("matched_nodes[%d] = host_node->index;\n", indent + 3, index);
   if(next_op == NULL)
   {
      PTFI("/* All items matched! */\n", indent);
      PTFI("return true;\n", indent);
   }
   else
   {
      emitNextMatcherCall(next_op, indent + 3);
      PTFI("if(result) return true;\n", indent + 3);           
      PTFI("else\n", indent + 3);
      PTFI("{\n", indent + 3);                              
      PTFI("removeNodeMap(morphism);\n", indent + 6);
      PTFI("matched_nodes[%d] = -1;\n", indent + 6, index);  
      PTFI("}\n", indent + 3);
   } 
   PTFI("}\n", indent);
   PTFI("else removeAssignments(morphism, new_assignments);\n", indent);
}

void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op)
{
   PTF("static bool match_e%d(Morphism *morphism)\n{\n", left_edge->index);
   /* Writes a double for loop. The outer loop iterates over variable mark.
    * The inner loop iterates over variable label_class. */
   generateIteratorCode(left_edge->label, 3, file);
   PTFI("bool edge_matched = false;\n", 3);
   PTFI("LabelClassTable *table = getEdgeLabelTable(host, mark, label_class);\n", 9);
   PTFI("if(table == NULL) continue;\n", 9);
   PTFI("int items_index;\n", 9);
   PTFI("for(items_index = 0; items_index < table->index; items_index++)\n", 9);
   PTFI("{\n", 9);
   PTFI("int host_index = table->items[items_index];\n", 12);
   PTFI("Edge *host_edge = getEdge(host, host_index);\n", 12);
   PTFI("if(host_edge == NULL) continue;\n\n", 12);
   PTFI("edge_matched = false;\n", 12);
   PTFI("/* Set edge_matched to true if the edge has already been matched. */\n", 12);
   PTFI("int index;\n", 12);
   PTFI("CHECK_MATCHED_EDGE;\n\n", 12);
   PTFI("if(edge_matched) continue;\n\n", 12);

   /* Emit code to test the matched_edge flag and if the candidate host edge 
    * is consistent with the left edge with respect to label class, mark, and
    * loopiness. If not, the loop will continue without entering the 
    * potentially expensive label matching code. */
   PTFI(" /* Arguments: mark. */\n", 6);
   if(left_edge->source == left_edge->target) 
        PTFI("if(host_edge->source != host_edge->target) continue;\n\n", 12);
   else PTFI("if(edge_matched || ((%d != 6) && (host_edge->label.mark != %d))) "
             "continue;\n\n", 6, left_edge->label.mark, left_edge->label.mark);

   PTFI("Label label = host_edge->label;\n", 12);
   PTFI("bool match = false;\n", 12);
   if(hasListVariable(left_edge->label))
      generateVariableListMatchingCode(left_edge->label, 12, file);
   else generateFixedListMatchingCode(left_edge->label, 12, file);

   generateEdgeMatchResultCode(left_edge->index, next_op, 12);
   PTFI("}\n", 9);
   PTFI("}\n", 6);
   PTFI("}\n", 3);
   PTFI("return false;\n", 3);
   PTF("}\n\n");
}

void emitLoopEdgeMatcher(Edge *left_edge, SearchOp *next_op)
{
   PTF("static bool match_e%d(Morphism *morphism)\n{\n", left_edge->index);
   PTFI("/* Matching a loop. */\n", 3);
   PTFI("int node_index = lookupNode(morphism, %d);\n", 3, left_edge->source);
   PTFI("if(node_index < 0) return false;\n", 3);
   PTFI("Node *host_node = getNode(host, node_index);\n\n", 3);
   PTFI("int counter;\n\n", 3);
   PTFI("bool edge_matched = false;\n", 3);

   PTFI("for(counter = host_node->out_index - 1; counter >= 0; counter--)\n   {\n", 3);
   PTFI("Edge *host_edge = getEdge(host, getOutEdge(host_node, counter));\n", 6);
   PTFI("if(host_edge == NULL) continue;\n", 6);
   PTFI("if(host_edge->source != host_edge->target) continue;\n\n", 6);
   PTFI("edge_matched = false;\n", 6);
   PTFI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
   PTFI("int index;\n", 6);
   PTFI("CHECK_MATCHED_EDGE;\n\n", 6);
   PTFI("if(edge_matched || ((%d != 6) && (host_edge->label.mark != %d))) "
        "continue;\n\n", 6, left_edge->label.mark, left_edge->label.mark);
   PTFI("Label label = host_edge->label;\n", 6);
   PTFI("bool match = false;\n", 6);
   if(hasListVariable(left_edge->label))
      generateVariableListMatchingCode(left_edge->label, 6, file);
   else generateFixedListMatchingCode(left_edge->label, 6, file);

   generateEdgeMatchResultCode(left_edge->index, next_op, 6);
   PTFI("}\n", 3);
   PTFI("return false;\n}\n\n", 3);
}

/* Unlike matching a node from an edge, the LHS-node from which this LHS-edge
 * is matched may not necessarily be the previously matched node in the 
 * searchplan. The generated code uses the index of the target of the LHS-edge 
 * to find the host node to which it has been matched (lookupNode). Edges 
 * in the inedge list of that host node are the candidate edges to match 
 * left_edge. */
void emitEdgeFromSourceMatcher(Edge *left_edge, bool initialise, bool exit,
                               bool bidirectional, SearchOp *next_op)
{
   if(initialise)
   {
      PTF("static bool match_e%d(Morphism *morphism)\n{\n", left_edge->index);
      PTFI("int source_index = lookupNode(morphism, %d);\n", 3, left_edge->source);
      PTFI("int target_index = lookupNode(morphism, %d);\n", 3, left_edge->target);
      PTFI("if(source_index < 0) return false;\n", 3);
      PTFI("Node *host_node = getNode(host, source_index);\n\n", 3);
      PTFI("int counter;\n\n", 3);
      PTFI("bool edge_matched = false;\n", 3);
   }

   PTFI("for(counter = host_node->out_index - 1; counter >= 0; counter--)\n   {\n", 3);
   PTFI("Edge *host_edge = getEdge(host, getOutEdge(host_node, counter));\n", 6);
   PTFI("if(host_edge == NULL) continue;\n\n", 6);
   PTFI("edge_matched = false;\n", 6);
   PTFI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
   PTFI("int index;\n", 6);
   PTFI("CHECK_MATCHED_EDGE;\n\n", 6);
   PTFI("if(edge_matched || ((%d != 6) && (host_edge->label.mark != %d))) "
        "continue;\n\n", 6, left_edge->label.mark, left_edge->label.mark);
   if(bidirectional)
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
   PTFI("Label label = host_edge->label;\n", 6);
   PTFI("bool match = false;\n", 6);
   if(hasListVariable(left_edge->label))
      generateVariableListMatchingCode(left_edge->label, 6, file);
   else generateFixedListMatchingCode(left_edge->label, 6, file);

   generateEdgeMatchResultCode(left_edge->index, next_op, 6);
   PTFI("}\n", 3);
   if(exit) PTFI("return false;\n}\n\n", 3);
}

void emitEdgeFromTargetMatcher(Edge *left_edge, bool initialise, bool exit,
                               bool bidirectional, SearchOp *next_op)
{
   if(initialise)
   {
      PTF("static bool match_e%d(Morphism *morphism)\n{\n", left_edge->index);
      PTFI("int target_index = lookupNode(morphism, %d);\n", 3, left_edge->target);
      PTFI("int source_index = lookupNode(morphism, %d);\n", 3, left_edge->source);
      PTFI("if(target_index < 0) return false;\n", 3);
      PTFI("Node *host_node = getNode(host, target_index);\n\n", 3);
      PTFI("int counter;\n\n", 3);
      PTFI("bool edge_matched = false;\n", 3);
   }

   PTFI("for(counter = host_node->in_index - 1; counter >= 0; counter--)\n   {\n", 3);
   PTFI("Edge *host_edge = getEdge(host, getInEdge(host_node, counter));\n", 6);
   PTFI("if(host_edge == NULL) continue;\n\n", 6);
   PTFI("edge_matched = false;\n", 6);
   PTFI("/* Set edge_matched to true if the edge has already been matched. */\n", 6);
   PTFI("int index;\n", 6);
   PTFI("CHECK_MATCHED_EDGE;\n\n", 6);
   PTFI("if(edge_matched || ((%d != 6) && (host_edge->label.mark != %d))) "
        "continue;\n\n", 6, left_edge->label.mark, left_edge->label.mark);
   if(bidirectional)
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
   PTFI("Label label = host_edge->label;\n", 6);
   PTFI("bool match = false;\n", 6);
   if(hasListVariable(left_edge->label))
      generateVariableListMatchingCode(left_edge->label, 6, file);
   else generateFixedListMatchingCode(left_edge->label, 6, file);

   generateEdgeMatchResultCode(left_edge->index, next_op, 6);
   PTFI("}\n", 3);
   if(exit) PTFI("return false;\n}\n\n", 3);
}

void emitBiEdgeFromSourceMatcher(Edge *left_edge, SearchOp *next_op)
{
   emitEdgeFromSourceMatcher(left_edge, true, false, true, next_op);
   emitEdgeFromTargetMatcher(left_edge, false, true, true, next_op);
}

void emitBiEdgeFromTargetMatcher(Edge *left_edge, SearchOp *next_op)
{
   emitEdgeFromTargetMatcher(left_edge, true, false, true, next_op);
   emitEdgeFromSourceMatcher(left_edge, false, true, true, next_op);
}

void generateEdgeMatchResultCode(int index, SearchOp *next_op, int indent)
{
   PTFI("if(match)\n", indent);
   PTFI("{\n", indent);
   PTFI("addEdgeMap(morphism, %d, host_edge->index, new_assignments);\n",  indent + 3, index);
   PTFI("matched_edges[%d] = host_edge->index;\n", indent + 3, index);
   if(next_op == NULL)
   {
      PTFI("/* All items matched! */\n", indent);
      PTFI("return true;\n", indent);
   }
   else
   {
      emitNextMatcherCall(next_op, indent + 3);
      PTFI("if(result) return true;\n", indent + 3);           
      PTFI("else\n", indent + 3);
      PTFI("{\n", indent + 3);                              
      PTFI("removeEdgeMap(morphism);\n", indent + 6);
      PTFI("matched_edges[%d] = -1;\n", indent + 6, index);  
      PTFI("}\n", indent + 3);
   } 
   PTFI("}\n", indent);
   PTFI("else removeAssignments(morphism, new_assignments);\n", indent);
}

void emitNextMatcherCall(SearchOp *next_operation, int indent)
{
   switch(next_operation->type)
   {
      case 'n':
      case 'r':
           PTFI("bool result = match_n%d(morphism);\n", indent, next_operation->index);
           break;

      case 'i':
      case 'o':
      case 'b':
           PTFI("bool result = match_n%d(morphism, host_edge);\n", indent,
                next_operation->index);
           break;
  
      case 'e':
      case 's':
      case 't':
      case 'l':
           PTFI("bool result = match_e%d(morphism);\n", indent, next_operation->index);
           break;

      default:
           print_to_log("Error (emitNextMatcherCall): Unexpected "
                           "operation type %c.\n", next_operation->type);
           break;
   }
}

void generateRemoveLHSCode(string rule_name)
{
   PTH("void apply%s(Morphism *morphism, bool record_changes);\n", rule_name);
   PTF("void apply%s(Morphism *morphism, bool record_changes)\n{\n", rule_name);

   PTFI("int count;\n", 3);
   PTFI("for(count = 0; count < morphism->edges; count++)\n", 3);
   PTFI("{\n", 3);                        
   PTFI("if(record_changes)\n", 6);
   PTFI("{\n", 6);
   PTFI("Edge *edge = getEdge(host, morphism->edge_map[count].host_index);\n", 9); 
   PTFI("pushRemovedEdge(false, edge->label, edge->source, edge->target);\n", 9);  
   PTFI("}\n", 6);
   PTFI("removeEdge(host, morphism->edge_map[count].host_index, !record_changes);\n", 6);
   PTFI("}\n", 3);
                                                                           
   PTFI("for(count = 0; count < morphism->nodes; count++)\n", 3);
   PTFI("{\n", 3);                        
   PTFI("if(record_changes)\n", 6);
   PTFI("{\n", 6);
   PTFI("Node *node = getNode(host, morphism->node_map[count].host_index);\n", 9); 
   PTFI("pushRemovedNode(node->root, node->label);\n", 9);  
   PTFI("}\n", 6);
   PTFI("removeNode(host, morphism->node_map[count].host_index, !record_changes);\n", 6);
   PTFI("}\n", 6);
   PTFI("initialiseMorphism(morphism);\n}\n\n", 3);
}

void generateAddRHSCode(Rule *rule)
{
   PTH("void apply%s(bool record_changes);\n", rule->name);
   PTF("void apply%s(bool record_changes)\n{\n", rule->name);
   PTFI("int index;\n", 3);
   PTFI("Label label;\n\n", 3);
   /* Generate code to retrieve the values assigned to the variables in the
    * matching phase. */
   VariableList *variables = rule->variables;
   while(variables != NULL)
   {
      if(variables->used_by_rule) 
         generateVariableCode(variables->variable, variables->type, file);
      variables = variables->next;
   }
   int index;
   Graph *rhs = rule->rhs;
   /* Flag to prevent repeated writing of "label = blank_label" when
    * consecutive blank nodes are added to the graph. */
   bool blank_label = false;
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
      if(rule_node->label.mark == NONE && rule_node->label.length == 0)
      {
         if(!blank_label)
         {
            PTFI("label = blank_label;\n", 3);
            blank_label = true;
         }
      }
      else generateRHSLabelCode(rule_node->label, true, index, 3, file);
      PTFI("index = addNode(host, %d, label);\n", 3, rule_node->root);
      if(rule->added_edges != NULL) PTFI("map[%d] = index;\n", 3, rule_node->index);
      PTFI("if(record_changes) pushAddedNode(index);\n", 3);
   }
   PTF("\n");
   NewEdgeList *iterator = rule->added_edges;
   while(iterator != NULL)
   {
      Edge *rule_edge = getEdge(rhs, iterator->edge_index);
      if(rule_edge->label.mark == NONE && rule_edge->label.length == 0)
      {
         if(!blank_label)
         {
            PTFI("label = blank_label;\n", 3);
            blank_label = true;
         }
      }
      else generateRHSLabelCode(rule_edge->label, false, index, 3, file);
      /* The host-source and host-target of added edges are taken from the 
       * map populated in the previous loop. */
      PTFI("index = addEdge(host, false, label, map[%d], map[%d]);\n",
           3, iterator->source_index, iterator->target_index);
      PTFI("if(record_changes) pushAddedEdge(index);\n", 3);
      iterator = iterator->next;
   }     
   PTF("}\n");
   return;
}

void generateApplicationCode(Rule *rule)
{
   Graph *lhs = rule->lhs;
   Graph *rhs = rule->rhs;
   PTH("void apply%s(Morphism *morphism, bool record_changes);\n", rule->name);
   PTF("void apply%s(Morphism *morphism, bool record_changes)\n{\n", rule->name);
   
   /* Generate code to retrieve the values assigned to the variables in the
    * matching phase. */
   VariableList *variables = rule->variables;
   while(variables != NULL)
   {
      if(variables->used_by_rule) 
         generateVariableCode(variables->variable, variables->type, file);
      variables = variables->next;
   }
   int index;
   bool node_index_declared = false;
   for(index = 0; index < lhs->number_of_nodes; index++)
   {
      PreservedItemList *item = queryPItemList(rule->preserved_nodes, index);
      if(item == NULL) continue;
      if(item->indegree_argument || item->outdegree_argument)
      {
         if(!node_index_declared) 
         {
            PTFI("int node_index = lookupNode(morphism, %d);\n", 3, index);
            node_index_declared = true;
         }
         else PTFI("node_index = lookupNode(morphism, %d);\n", 3, index);
         if(item->indegree_argument) 
            PTFI("int indegree%d = getIndegree(host, node_index);\n", 3, index);
         if(item->outdegree_argument) 
            PTFI("int outdegree%d = getOutdegree(host, node_index);\n", 3, index);
      }
   }
   bool label_declared = false, host_edge_index_declared = false,
        host_node_index_declared = false;
   /* Host graph modifications are performed in the following order: 
    * (1) Delete/relabel edges.
    * (2) Delete/relabel nodes.
    * (3) Add nodes.
    * (4) Add edges.
    *
    * Nodes must be added before edges are added, because the incident nodes of
    * added edges may be added nodes. Edges must be deleted before nodes are
    * deleted because deleting nodes first may leave dangling edges. */

   /* (1) Delete/relabel edges. */
   for(index = 0; index < lhs->number_of_edges; index++)
   {
      PreservedItemList *item = queryPItemList(rule->preserved_edges, index);
      if(item == NULL) 
      {
         if(!host_edge_index_declared)
         {
            PTFI("int host_edge_index;\n", 3);
            host_edge_index_declared = true;
         }
         /* Generate code to remove the edge. */
         PTFI("host_edge_index = lookupEdge(morphism, %d);\n", 3, index);
         PTFI("if(record_changes)\n   {\n", 3);
         PTFI("Edge *edge = getEdge(host, host_edge_index);\n", 6);
         PTFI("pushRemovedEdge(false, edge->label, edge->source, edge->target);\n   }\n", 6);
         PTFI("removeEdge(host, host_edge_index, !record_changes);\n\n", 3);   
      }
      else
      {
         if(item->new_label != NULL)
         {
            if(!host_edge_index_declared)
            {
               PTFI("int host_edge_index;\n", 3);
               host_edge_index_declared = true;
            }
            /* Generate code to relabel the edge. */
            PTFI("host_edge_index = lookupEdge(morphism, %d);\n", 3, index);
            Label label = *(item->new_label);
            PTFI("if(record_changes)\n   {\n", 3);
            PTFI("Edge *edge = getEdge(host, host_edge_index);\n", 6);
            PTFI("pushRelabelledEdge(host_edge_index, edge->label);\n   }\n", 6);
            if(label.length == 0 && label.mark == NONE)
                PTFI("relabelEdge(host, host_edge_index, blank_label, !record_changes);\n", 3);
            else
            {
               if(!label_declared) 
               {
                  PTFI("Label label;\n", 3);
                  label_declared = true;
               }
               generateRHSLabelCode(label, false, index, 3, file);
               PTFI("relabelEdge(host, host_edge_index, label, !record_changes);\n\n", 3);
            }
         }
      }
   }
   bool change_root_declared = false;
   /* (2) Delete/relabel nodes. */
   for(index = 0; index < lhs->number_of_nodes; index++)
   { 
      PreservedItemList *item = queryPItemList(rule->preserved_nodes, index);
      if(item == NULL) 
      {
         if(!host_node_index_declared)
         {
            PTFI("int host_node_index;\n", 3);
            host_node_index_declared = true;
         }
         /* Generate code to remove the node. */
         PTFI("host_node_index = lookupNode(morphism, %d);\n", 3, index);
         PTFI("if(record_changes)\n", 3);
         PTFI("{\n", 3);
         PTFI("Node *node = getNode(host, host_node_index);\n", 6);
         PTFI("pushRemovedNode(node->root, node->label);\n", 6);
         PTFI("}\n", 3);
         PTFI("removeNode(host, host_node_index, !record_changes);\n\n", 3);   
      }
      else
      {
         if(item->new_label != NULL)
         {
            if(!host_node_index_declared)
            {
               PTFI("int host_node_index;\n", 3);
               host_node_index_declared = true;
            }
            /* Generate code to relabel the node. */
            PTFI("host_node_index = lookupNode(morphism, %d);\n", 3, index);
            Label label = *(item->new_label);
            /* The root is changed in two cases:
             * (1) The LHS node is rooted and the RHS node is non-rooted.
             * (2) The LHS node is non-rooted, the RHS node is rooted, and
             *     the matched host node is non-rooted. 
             * The second case is handled at runtime. */
            Node *lhs_node = getNode(lhs, index);
            if(!change_root_declared) 
            {
               PTFI("bool change_root = false;\n", 3);
               change_root_declared = true;
            }
            else PTFI("change_root = false;\n", 3);
            PTFI("Node *node%d = getNode(host, host_node_index);\n", 3, index);
            /* Case (1) */
            if(lhs_node->root && !item->rhs_root) 
            {
               PTFI("if(change_root)\n   {\n", 3);
               PTFI("changeRoot(host, host_node_index);\n", 6);
               PTFI("change_root = true;\n   }\n", 6);
            }
            /* Case (2) */
            if(!lhs_node->root && item->rhs_root) 
            {
               PTFI("if(!node%d->root)\n   {\n", 3, index);
               PTFI("changeRoot(host, host_node_index);\n", 6);
               PTFI("change_root = true;\n   }\n", 6);
            }
            PTFI("if(record_changes) pushRelabelledNode(host_node_index, ", 3);
            PTF("change_root, node%d->label);\n", index);

            if(label.length == 0 && label.mark == NONE)
               PTFI("relabelNode(host, host_node_index, blank_label, !record_changes);\n", 3);
            else
            {
               if(!label_declared) 
               {
                  PTFI("Label label;\n", 3);
                  label_declared = true;
               }
               generateRHSLabelCode(label, true, index + lhs->number_of_edges, 3, file);
               PTFI("relabelNode(host, host_node_index, label, !record_changes);\n\n", 3);
            }
         }
      }
   }
   /* If both nodes and edges are added by the rule, the host indices of the 
    * added nodes need to be recorded in case the added edges require them. */
   if(rule->added_nodes != NULL && rule->added_edges != NULL)
   {
      PTFI("/* Array of host node indices indexed by RHS node index. */\n", 3);
      PTFI("int rhs_node_map[%d];\n\n", 3, rhs->number_of_nodes);
   }
   /* (3) Add nodes. */
   index = lhs->number_of_nodes + lhs->number_of_edges;
   ItemList *iterator_n = rule->added_nodes;
   while(iterator_n != NULL)
   {   
      if(!host_node_index_declared)
      {
         PTFI("int host_node_index;\n", 3);
         host_node_index_declared = true;
      }
      Node *rule_node = getNode(rhs, iterator_n->index);
      if(rule_node->label.length == 0 && rule_node->label.mark == NONE)
         PTFI("host_node_index = addNode(host, %d, blank_label);\n", 3, rule_node->root);
      if(rule->added_edges != NULL) 
         PTFI("rhs_node_map[%d] = host_node_index;\n", 3, rule_node->index);
      else
      {
         if(!label_declared) 
         {
            PTFI("Label label;\n", 3);
            label_declared = true;
         }
         generateRHSLabelCode(rule_node->label, true, index++, 3, file);
         PTFI("host_node_index = addNode(host, %d, label);\n", 3, rule_node->root);
      }
      PTFI("if(record_changes) pushAddedNode(host_node_index);\n", 3);
      iterator_n = iterator_n->next;
   }   
   /* (4) Add edges. */
   NewEdgeList *iterator_e = rule->added_edges;
   if(iterator_e != NULL) PTFI("int source = 0, target = 0;\n\n", 3);
   while(iterator_e != NULL)
   {
      if(!host_edge_index_declared)
      {
         PTFI("int host_edge_index;\n", 3);
         host_edge_index_declared = true;
      }
      /* The source and target edges are either nodes preserved by the rule or 
       * nodes added by the rule. 
       * The host node indices of preserved nodes are obtained from the morphism.
       * The host node indices of added nodes are obtained from rhs_node_map. */
      if(iterator_e->source_location == 'l')
           PTFI("source = lookupNode(morphism, %d);\n", 3, iterator_e->source_index);
      else PTFI("source = rhs_node_map[%d];\n", 3, iterator_e->source_index);

      if(iterator_e->target_location == 'l')
           PTFI("target = lookupNode(morphism, %d);\n", 3, iterator_e->target_index);
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
         generateRHSLabelCode(rule_edge->label, false, index++, 3, file);
         PTFI("host_edge_index = addEdge(host, false, label, source, target);\n", 3);
      }
      PTFI("if(record_changes) pushAddedEdge(host_edge_index);\n", 3);
      iterator_e = iterator_e->next;      
   }
   PTFI("/* Reset the morphism. */\n", 3);
   PTFI("initialiseMorphism(morphism);\n}\n\n", 3);
}

