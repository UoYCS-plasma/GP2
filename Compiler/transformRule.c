#include "transformRule.h"

IndexMap *node_map = NULL;
IndexMap *edge_map = NULL;

Rule *transformRule(GPRule *ast_rule)
{
   int lhs_nodes = countNodes(ast_rule->lhs);
   int lhs_edges = countEdges(ast_rule->lhs);
   int rhs_nodes = countNodes(ast_rule->rhs);
   int rhs_edges = countEdges(ast_rule->rhs);
   Rule *rule = makeRule(ast_rule->variable_count, lhs_nodes, lhs_edges, rhs_nodes, rhs_edges);
   rule->name = ast_rule->name;
   scanVariableList(rule, ast_rule->variables);

   if(lhs_nodes >= 0) scanLHS(rule, ast_rule->lhs);
   if(rhs_nodes >= 0) 
   {
      scanRHS(rule, ast_rule->rhs, ast_rule->interface);
      /* The rule's RHS graph has been populated by the previous function call.
       * Iterate over the labels in this graph to get information about 
       * variables used in the rule and nodes whose degrees are queried in the RHS. */
      int index;
      for(index = 0; index < rule->rhs->node_index; index++)
      { 
         RuleNode *node = getRuleNode(rule->rhs, index);
         int list_index;
         for(list_index = 0; list_index < node->label.length; list_index++) 
            scanRHSAtom(rule, node->relabelled, node->label.list[list_index]);
      }
      for(index = 0; index < rule->rhs->edge_index; index++)
      {
         RuleEdge *edge = getRuleEdge(rule->rhs, index);
         int list_index;
         for(list_index = 0; list_index < edge->label.length; list_index++) 
            scanRHSAtom(rule, edge->relabelled, edge->label.list[list_index]);
      }
   } 
   rule->predicate_count = ast_rule->predicate_count;
   rule->condition = transformCondition(rule, ast_rule->condition, node_map);
   if(node_map != NULL) freeIndexMap(node_map);
   if(edge_map != NULL) freeIndexMap(edge_map);
   node_map = NULL;
   edge_map = NULL;
   return rule;
}

void scanVariableList(Rule *rule, List *declarations)
{
   while(declarations != NULL)
   {
      GPType type = LIST_VAR;
      switch(declarations->list_type)
      {
         case INT_DECLARATIONS:
              type = INTEGER_VAR;
              break;

         case CHAR_DECLARATIONS:
              type = CHARACTER_VAR;
              break;

         case STRING_DECLARATIONS:
              type = STRING_VAR;
              break;

         case ATOM_DECLARATIONS:
              type = ATOM_VAR;
              break;

         case LIST_DECLARATIONS:
              type = LIST_VAR;
              break;

         default:
              print_to_log("Error (scanVariableList): Unexpected type %d\n", 
                           declarations->list_type);
              break;
      }
      List *variables = declarations->variables;
      while(variables != NULL)
      {
         addVariable(rule, variables->variable_name, type);
         variables = variables->next;
      }
      declarations = declarations->next;
   }
}

int countNodes(GPGraph *graph)
{
   int nodes = 0;
   List *iterator;
   for(iterator = graph->nodes; iterator != NULL; iterator = iterator->next) 
       nodes++;
   return nodes;
}

int countEdges(GPGraph *graph)
{
   int edges = 0;
   List *iterator;
   for(iterator = graph->edges; iterator != NULL; iterator = iterator->next) 
       edges++;
   return edges;
}

void scanLHS(Rule *rule, GPGraph *ast_lhs)
{
   List *nodes = ast_lhs->nodes;
   while(nodes != NULL)
   {
      GPNode *ast_node = nodes->node;
      if(ast_node->root) rule->is_rooted = true;
      Label label = transformLabel(ast_node->label, NULL);
      int node_index = addRuleNode(rule->lhs, ast_node->root, label);
      node_map = addIndexMap(node_map, ast_node->name, ast_node->root,
                             node_index, -1, NULL, NULL);
      nodes = nodes->next;   
   }

   List *edges = ast_lhs->edges;
   while(edges != NULL)
   {
      GPEdge *ast_edge = edges->edge;
      /* Use the node map to get the correct indices for the edge's source
       * and target to pass to addEdge. */
      IndexMap *source_map = findMapFromId(node_map, ast_edge->source);
      if(source_map == NULL)
      {
         print_to_log("Error (scanLHS): Edge's source %s not found in the node "
                      "map.\n", ast_edge->source);
         exit(1);
      }
      Label label = transformLabel(ast_edge->label, NULL);
      int edge_index = 0;
      if(!strcmp(ast_edge->source, ast_edge->target))
      {
         RuleNode *source = getRuleNode(rule->lhs, source_map->left_index);
         edge_index = addRuleEdge(rule->lhs, ast_edge->bidirectional, source,
                                  source, label);
      }
      else
      {
         IndexMap *target_map = findMapFromId(node_map, ast_edge->target);
         if(target_map == NULL)
         {
            print_to_log("Error (scanLHS): Edge's target %s not found in the node "
                         "map. \n", ast_edge->target);
            exit(1);
         }
         RuleNode *source = getRuleNode(rule->lhs, source_map->left_index);
         RuleNode *target = getRuleNode(rule->lhs, target_map->left_index);
         edge_index = addRuleEdge(rule->lhs, ast_edge->bidirectional, source,
                                  target, label);
      }
      edge_map = addIndexMap(edge_map, ast_edge->name, false, edge_index, -1,
                             ast_edge->source, ast_edge->target);
      edges = edges->next;   
   }
}

void scanRHS(Rule *rule, GPGraph *ast_rhs, List *interface)
{
   List *ast_nodes = ast_rhs->nodes;
   while(ast_nodes != NULL)
   {
      GPNode *ast_node = ast_nodes->node;
      Label label = transformLabel(ast_node->label, node_map);
      int node_index = addRuleNode(rule->rhs, ast_node->root, label);
      
      IndexMap *map = findMapFromId(node_map, ast_node->name);
      if(map == NULL) 
      {
         /* If the node is not in the map, add a new map for this node with
          * left index -1, and add the node to the added nodes list. */
         node_map = addIndexMap(node_map, ast_node->name, false, -1,
                                node_index, NULL, NULL);
      }
      else
      {
         /* If the map exists, search the interface. If the node is an 
          * interface node, add it to the preserved nodes list, otherwise
          * add it to the added nodes list. */
         List *iterator = interface;
         while(iterator != NULL)
         {
            if(!strcmp(iterator->node_id, ast_node->name))
            {
               RuleNode *left_node = getRuleNode(rule->lhs, map->left_index);
               RuleNode *right_node = getRuleNode(rule->rhs, node_index);
               left_node->interface = right_node;
               right_node->interface = left_node;
               if(equalRuleLabels(left_node->label, right_node->label))
                  right_node->relabelled = false;
               break;
            }
            else iterator = iterator->next;
         }
         map->right_index = node_index;
      }
      ast_nodes = ast_nodes->next;   
   }

   List *ast_edges = ast_rhs->edges;
   while(ast_edges != NULL)
   {
      GPEdge *ast_edge = ast_edges->edge;
      string source_id = ast_edge->source;
      string target_id = ast_edge->target;
      /* Use the node map to get the correct pointers for the edge's source
       * and target to pass to newEdge. */
      IndexMap *source_map = findMapFromId(node_map, source_id);
      if(source_map == NULL)
      {
         print_to_log("Error (scanRHS): Edge's source %s not found in the node "
                      "map.\n", source_id);
         exit(1);
      }
      RuleNode *source = getRuleNode(rule->rhs, source_map->right_index);

      IndexMap *target_map = findMapFromId(node_map, target_id);
      if(target_map == NULL)
      {
         print_to_log("Error (scanRHS): Edge's target %s not found in the node "
                      "map. \n", target_id);
         exit(1);
      }
      RuleNode *target = getRuleNode(rule->rhs, target_map->right_index);

      Label label = transformLabel(ast_edge->label, node_map);
      int edge_index = addRuleEdge(rule->rhs, ast_edge->bidirectional, 
                                   source, target, label);
      /* Flags to signify whether the source and target nodes exist in the
       * interface. */
      RuleEdge *edge = getRuleEdge(rule->rhs, edge_index);
      bool interface_source = edge->source->interface != NULL;
      bool interface_target = edge->target->interface != NULL;
      if(interface_source && interface_target)
      {
         /* Search in the edge map for an edge whose source and target 
          * correspond with that of the current edge. */
         IndexMap *map = findMapFromSrcTgt(edge_map, source_id, target_id);
         if(map == NULL && ast_edge->bidirectional)
            map = findMapFromSrcTgt(edge_map, target_id, source_id);
         if(map != NULL) 
         {
            RuleEdge *left_edge = getRuleEdge(rule->lhs, map->left_index);
            RuleEdge *right_edge = getRuleEdge(rule->rhs, edge_index);
            left_edge->interface = right_edge;
            right_edge->interface = left_edge;
            if(equalRuleLabels(left_edge->label, right_edge->label))
               right_edge->relabelled = false;
            /* The map is removed to ensure that a parallel RHS-edge is not
             * associated with this edge. */
            edge_map = removeMap(edge_map, map);     
         }
      }
      ast_edges = ast_edges->next;   
   }
}

void scanRHSAtom(Rule *rule, bool relabelled, Atom atom)
{
   switch(atom.type)
   {
      case VARIABLE:
      case LENGTH:
      {
           if(atom.type == VARIABLE && !relabelled) break;
           int index;
           for(index = 0; index < rule->variable_index; index++)
           {
              Variable variable = rule->variables[index];
              if(!strcmp(variable.name, atom.variable.name))
              {
                 variable.used_by_rule = true;
                 break;
              }
           }
           break;
      }
      case INDEGREE:
      {
           RuleNode *node = getRuleNode(rule->lhs, atom.node_id);
           if(node->interface == NULL)
              print_to_log("Error (scanRHSLabel): Argument of indegree operator %d"
                           "is not an interface node.\n", atom.node_id);
           else node->indegree_arg = true;
           break;
      }
      case OUTDEGREE:
      {
           RuleNode *node = getRuleNode(rule->lhs, atom.node_id);
           if(node->interface == NULL)
              print_to_log("Error (scanRHSLabel): Argument of outdegree operator %d"
                           "is not an interface node.\n", atom.node_id);
           else node->outdegree_arg = true;
           break;
      }
      case NEG:
           scanRHSAtom(rule, relabelled, *(atom.neg_exp));
           break;

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case CONCAT:
           scanRHSAtom(rule, relabelled, *(atom.bin_op.left_exp));
           scanRHSAtom(rule, relabelled, *(atom.bin_op.right_exp));
           break;

      default: break;
   }
}

Label transformLabel(GPLabel *ast_label, IndexMap *node_map)
{
   Label label;
   label.mark = ast_label->mark;
   label.length = getASTListLength(ast_label->gp_list);
   if(label.length == 0)
   {
      if(label.mark == NONE) return blank_label;
      else label.list = NULL;
   }
   else label.list = transformList(ast_label->gp_list, label.length, node_map);
   return label;
}

int getASTListLength(List *list)
{
   int length = 0;
   while(list != NULL)
   {
      length++;
      list = list->next;
   }
   return length;
}

Atom *transformList(List *ast_list, int length, IndexMap *node_map)
{
   Atom *list = makeList(length);
   int position = 0;
   while(ast_list != NULL)
   {
      Atom atom = transformAtom(ast_list->atom, node_map);
      list[position++] = atom;
      ast_list = ast_list->next;
   }
   return list;
}

Atom transformAtom(GPAtom *ast_atom, IndexMap *node_map)
{
   Atom atom;
   atom.type = ast_atom->type;
   switch(ast_atom->type) 
   {
      case INTEGER_CONSTANT:
           atom.number = ast_atom->number;
           break;

      case STRING_CONSTANT:
           atom.string = strdup(ast_atom->string);
           break;
       
      case VARIABLE:
      case LENGTH:
           atom.variable.name = strdup(ast_atom->variable.name);
           atom.variable.type = ast_atom->variable.type;
           break;

      case INDEGREE:
      case OUTDEGREE:
           atom.node_id = findLeftIndexFromId(node_map, ast_atom->node_id);
           break;

      case NEG:
           if(ast_atom->neg_exp->type == INTEGER_CONSTANT)
           {
              atom.number = -(ast_atom->neg_exp->number);
              break;
           }
           else
           {
              atom.neg_exp = malloc(sizeof(Atom));
              if(atom.neg_exp == NULL)
              {
                 print_to_log("Error (transformAtom): malloc failure.\n");
                 exit(1);
              }
              *(atom.neg_exp) = transformAtom(ast_atom->neg_exp, node_map);
           }
           break;

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case CONCAT:
           atom.bin_op.left_exp = malloc(sizeof(Atom));
           if(atom.bin_op.left_exp == NULL)
           {
              print_to_log("Error (transformAtom): malloc failure.\n");
              exit(1);
           }
           atom.bin_op.right_exp = malloc(sizeof(Atom));
           if(atom.bin_op.right_exp == NULL)
           {
              print_to_log("Error (transformAtom): malloc failure.\n");
              exit(1);
           }
           *(atom.bin_op.left_exp) = transformAtom(ast_atom->bin_op.left_exp, node_map);
           *(atom.bin_op.right_exp) = transformAtom(ast_atom->bin_op.right_exp, node_map);
           break;

      default:
           print_to_log("Error (transformAtom): Unexpected atom type %d.\n", ast_atom->type);
           break;
   }
   return atom;
}

Condition *transformCondition(Rule *rule, GPCondition *ast_condition, IndexMap *node_map)
{
   if(ast_condition == NULL) return NULL;
   Condition *condition = makeCondition();
   static int bool_count = 0;
   Predicate *predicate;
   switch(ast_condition->type)
   {
      case INT_CHECK:
      case CHAR_CHECK:
      case STRING_CHECK:
      case ATOM_CHECK:
           predicate = makeTypeCheck(bool_count++, ast_condition->type, 
                                     ast_condition->var);
           condition->type = 'e';
           condition->predicate = predicate;
           addVariablePredicate(rule, ast_condition->var, predicate);
           break;

      case EDGE_PRED:
      {
           condition->type = 'e';
           int source_index = findLeftIndexFromId(node_map, ast_condition->edge_pred.source);
           int target_index = findLeftIndexFromId(node_map, ast_condition->edge_pred.target);

           if(ast_condition->edge_pred.label == NULL)
              predicate = makeEdgePred(bool_count++, source_index, target_index, NULL);
           else 
           {
              Label label = transformLabel(ast_condition->edge_pred.label, node_map);
              predicate = makeEdgePred(bool_count++, source_index, target_index, &label);
           }
           condition->predicate = predicate;
           RuleNode *source = getRuleNode(rule->lhs, source_index);
           RuleNode *target = getRuleNode(rule->lhs, target_index);
           source->predicates = addPredicate(source->predicates, predicate, 
                                             rule->predicate_count);
           target->predicates = addPredicate(target->predicates, predicate, 
                                             rule->predicate_count);
           break;
      }

      case EQUAL:
      case NOT_EQUAL:
      {
           condition->type = 'e';
           int left_length = getASTListLength(ast_condition->list_cmp.left_list);
           Atom *left_list = transformList(ast_condition->list_cmp.left_list,
                                           left_length, node_map);
           int right_length = getASTListLength(ast_condition->list_cmp.right_list);
           Atom *right_list = transformList(ast_condition->list_cmp.right_list, 
                                            right_length, node_map);
           predicate = makeRelationalCheck(bool_count++, ast_condition->type, left_list,
                                           left_length, right_list, right_length);
           condition->predicate = predicate;
           scanListForPredicates(rule, left_list, left_length, predicate);
           scanListForPredicates(rule, right_list, right_length, predicate);
           break;
      }

      case GREATER:
      case GREATER_EQUAL:
      case LESS:
      case LESS_EQUAL:
      {
           condition->type = 'e';
           Atom *left_list = transformList(ast_condition->list_cmp.left_list, 1, node_map);
           Atom *right_list = transformList(ast_condition->list_cmp.right_list, 1, node_map);
           predicate = makeRelationalCheck(bool_count++, ast_condition->type,
                                           left_list, 1, right_list, 1);
           condition->predicate = predicate;
           scanListForPredicates(rule, left_list, 1, predicate);
           scanListForPredicates(rule, right_list, 1, predicate);
           break;
      }

      case BOOL_NOT:
           condition->type = 'n';
           condition->neg_predicate = 
              transformCondition(rule, ast_condition->not_exp, node_map);
           break;

      case BOOL_OR:
           condition->type = 'o';
           condition->left_predicate = 
              transformCondition(rule, ast_condition->bin_exp.left_exp, node_map);
           condition->right_predicate = 
              transformCondition(rule, ast_condition->bin_exp.right_exp, node_map);
           break;

      case BOOL_AND:
           condition->type = 'a';
           condition->left_predicate = 
              transformCondition(rule, ast_condition->bin_exp.left_exp, node_map);
           condition->right_predicate = 
              transformCondition(rule, ast_condition->bin_exp.right_exp, node_map);
           break;

      default:
           print_to_log("Error (transformCondition): Unexpected type %d.\n", 
                        ast_condition->type);
           break;
   }
   return condition;
}

void scanListForPredicates(Rule *rule, Atom *list, int length, Predicate *predicate)
{
   int index;
   for(index = 0; index < length; index++) 
      scanAtomForPredicates(rule, list[index], predicate);
}

void scanAtomForPredicates(Rule *rule, Atom atom, Predicate *predicate)
{
   switch(atom.type)
   {
      case VARIABLE:
      case LENGTH:
           addVariablePredicate(rule, atom.variable.name, predicate);
           break;

      case INDEGREE:
      case OUTDEGREE:
      {
           RuleNode *node = getRuleNode(rule->lhs, atom.node_id);
           node->predicates = addPredicate(node->predicates, predicate, 
                                           rule->predicate_count);
           break;
      }

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case CONCAT:
           scanAtomForPredicates(rule, *(atom.bin_op.left_exp), predicate);
           scanAtomForPredicates(rule, *(atom.bin_op.right_exp), predicate);
           break;

      default: break;
   }
}

