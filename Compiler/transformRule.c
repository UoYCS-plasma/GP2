#include "transformRule.h"

/* Data structure to facilitate the transformation from the AST-rule to the
 * rule structure defined in rule.h. Principally, the index map keeps records
 * the correspondence between node and edge identifiers from the AST (strings)
 * and their indices in the node and edge arrays of the graph structure. Used
 * to get the source and target indices of edges, and to search for edges
 * that are preserved by the rule. */
typedef struct IndexMap {
   string id;
   bool root;
   int left_index; /* Set to -1 if the item is not in the LHS graph. */
   int right_index; /* Set to -1 if the item is not in the RHS graph. */
   string source_id; 
   string target_id;
   struct IndexMap *next;
} IndexMap;

/* Prepends a new map to the given IndexMap. Returns a pointer to the new
 * head of the list. */
IndexMap *addIndexMap(IndexMap *map, string id, bool root, int left_index,
                      int right_index, string source_id, string target_id)
{
   IndexMap *new_map = malloc(sizeof(IndexMap));
   if(new_map == NULL)
   {
      print_to_log("Error: Memory exhausted during map construction.\n");
      exit(1);
   }
   new_map->id = id;
   new_map->root = root;
   new_map->left_index = left_index;
   new_map->right_index = right_index;
   new_map->source_id = source_id;
   new_map->target_id = target_id;
   new_map->next = map;
   return new_map;
}

int findLeftIndexFromId(IndexMap *map, string id)
{
   while(map != NULL)
   {
      if(!strcmp(map->id, id)) return map->left_index;
      else map = map->next;
   }
   return -1; 
}

IndexMap *findMapFromId(IndexMap *map, string id)
{
   while(map != NULL)
   {
      if(!strcmp(map->id, id)) return map;
      else map = map->next;
   }
   return NULL; 
}

IndexMap *findMapFromSrcTgt(IndexMap *map, string source, string target)
{
   while(map != NULL)
   {
      if(!strcmp(map->source_id, source) && !strcmp(map->target_id, target))
         return map;
      else map = map->next;
   }
   return NULL; 
}

IndexMap *removeMap(IndexMap *map, IndexMap *map_to_remove)
{
   if(map_to_remove == NULL) return map;
   if(map == map_to_remove)
   {
      IndexMap *temp = map;
      map = map->next;
      free(temp);
   }
   else
   {
      IndexMap *iterator = map;
      while(iterator->next != NULL)
      {
         if(iterator->next == map_to_remove)
         {
            /* Relocate the 'next' pointer of the current IndexMap. The current
             * 'next' pointer is stored in a temporary value for freeing, otherwise
             * it would be lost by the relocation. */
            IndexMap *temp = iterator->next;
            iterator->next = iterator->next->next;
            free(temp);
            break;
         }
         else iterator = iterator->next;
      }
   }
   return map;
}

void freeIndexMap(IndexMap *map)
{
   if(map == NULL) return;
   if(map->next) freeIndexMap(map->next);
   free(map);
}

/* Populate the rule's variable list from the variable declaration lists in
 * the AST. */
static void initialiseVariableList(Rule *rule, List *declarations);
/* Sets the used_by_rule flag of variables and the indegree_arg/outdegree_arg
 * flags of nodes. These flags are set if the value of a variable or the
 * degree of a node is needed by label updating in rule application. */
static void scanRHSAtom(Rule *rule, bool relabelled, Atom atom);
static Label transformLabel(GPLabel *ast_label, IndexMap *node_map);
static GPList *transformList(List *ast_list, IndexMap *node_map);
static Atom transformAtom(GPAtom *ast_atom, IndexMap *node_map);
static Condition *transformCondition(Rule *rule, GPCondition *ast_condition, 
                                     bool negated, IndexMap *node_map);
static void scanPredicateAtom(Rule *rule, Atom atom, Predicate *predicate);

IndexMap *node_map = NULL;
IndexMap *edge_map = NULL;

Rule *transformRule(GPRule *ast_rule)
{
   int lhs_nodes = countNodes(ast_rule->lhs);
   int lhs_edges = countEdges(ast_rule->lhs);
   int rhs_nodes = countNodes(ast_rule->rhs);
   int rhs_edges = countEdges(ast_rule->rhs);
   Rule *rule = makeRule(ast_rule->variable_count, lhs_nodes, lhs_edges, rhs_nodes, rhs_edges);
   rule->name = strdup(ast_rule->name);

   initialiseVariableList(rule, ast_rule->variables);

   if(lhs_nodes > 0) scanLHS(rule, ast_rule->lhs);
   if(rhs_nodes > 0) scanRHS(rule, ast_rule->rhs, ast_rule->interface);
   rule->predicate_count = ast_rule->predicate_count;
   rule->condition = transformCondition(rule, ast_rule->condition, false, node_map);
   if(node_map != NULL) freeIndexMap(node_map);
   if(edge_map != NULL) freeIndexMap(edge_map);
   node_map = NULL;
   edge_map = NULL;
   return rule;
}

static void initialiseVariableList(Rule *rule, List *declarations)
{
   while(declarations != NULL)
   {
      GPType type = LIST_VAR;
      /* Set 'type' to the value to pass to addVariable before iterating over
       * the variable declaration list. */
      switch(declarations->type)
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
                           declarations->type);
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

void scanLHS(Rule *rule, GPGraph *ast_lhs)
{
   List *nodes = ast_lhs->nodes;
   while(nodes != NULL)
   {
      GPNode *ast_node = nodes->node;
      if(ast_node->root) rule->is_rooted = true;
      Label label = transformLabel(ast_node->label, NULL);
      int node_index = addRuleNode(rule->lhs, ast_node->root, label);
      /* The right index argument is -1. This changes if the node is an interface
       * node, in which case the right index will become the index of the corresponding
       * node in the RHS. */
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
      int source_index = findLeftIndexFromId(node_map, ast_edge->source);
      if(source_index == -1)
      {
         print_to_log("Error (scanLHS): Edge's source %s not found in the node "
                      "map.\n", ast_edge->source);
         exit(0);
      } 
      int target_index = findLeftIndexFromId(node_map, ast_edge->target);
      if(target_index == -1)
      {
         print_to_log("Error (scanLHS): Edge's target %s not found in the node "
                      "map. \n", ast_edge->target);
         exit(0);
      }
      Label label = transformLabel(ast_edge->label, NULL);
      int edge_index = 0;
      RuleNode *source = getRuleNode(rule->lhs, source_index);
      RuleNode *target = getRuleNode(rule->lhs, target_index);
      edge_index = addRuleEdge(rule->lhs, ast_edge->bidirectional, source,
                               target, label);
      /* The right index argument is -1. This changes if the edge is an interface
       * edge, in which case the right index will become the index of the corresponding
       * edge in the RHS. */
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
         /* If the node ID does not have an entry in the node map, then the RHS
          * node is added by the rule. Create a new entry with left index -1. */
         node_map = addIndexMap(node_map, ast_node->name, false, -1,
                                node_index, NULL, NULL);
         rule->adds_nodes = true;
      }
      else
      {
         /* If the map exists, then there exists a RHS node with the same ID as
          * some LHS node. This is not necessarily an interface (preserved) node:
          * the rule could also delete the left node and add the right one.
          *
          * Thus, search the interface. If the RHS node is an interface node, 
          * set the interface pointers of both rule nodes and set the relabelled
          * flag of the right node by testing label equality. Otherwise it is
          * added by the rule. */
         List *iterator = interface;
         while(iterator != NULL)
         {
            if(!strcmp(iterator->node_id, ast_node->name))
            {
               RuleNode *left_node = getRuleNode(rule->lhs, map->left_index);
               RuleNode *right_node = getRuleNode(rule->rhs, node_index);
               left_node->interface = right_node;
               right_node->interface = left_node;
               if(equalLabels(left_node->label, right_node->label))
                  right_node->relabelled = false;
               if(left_node->root == right_node->root)
                  right_node->root_changed = false;
               break;
            }
            /* If the end of the interface is reached and the loop has not 
             * exited from the break statement above, then the node is not in
             * the interface. */
            if(iterator->next == NULL) rule->adds_nodes = true;
            else iterator = iterator->next;
         }
         /* Update the map's right index. */
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
      RuleEdge *edge = getRuleEdge(rule->rhs, edge_index);

      /* Edges do not have an explicit interface list. Instead check if both
       * incident nodes are in the interface. If so, an LHS -edge between their
       * LHS counterparts is a preserved edge. This is checked by calling 
       * findMapFromSrcTgt. */
      if(edge->source->interface != NULL && edge->target->interface != NULL)
      {
         IndexMap *map = findMapFromSrcTgt(edge_map, source_id, target_id);
         /* If the RHS-edge is bidirectional, search in the other direction. */
         if(map == NULL && ast_edge->bidirectional)
            map = findMapFromSrcTgt(edge_map, target_id, source_id);
         if(map != NULL) 
         {
            RuleEdge *left_edge = getRuleEdge(rule->lhs, map->left_index);
            RuleEdge *right_edge = getRuleEdge(rule->rhs, edge_index);
            left_edge->interface = right_edge;
            right_edge->interface = left_edge;
            if(equalLabels(left_edge->label, right_edge->label))
               right_edge->relabelled = false;
            else right_edge->relabelled = true;
            /* Remove the map for the LHS-edge, otherwise a parallel RHS-edge
             * may be associated with this edge. */
            edge_map = removeMap(edge_map, map);     
         }
         else rule->adds_edges = true;
      }
      else rule->adds_edges = true;
      ast_edges = ast_edges->next;   
   }
   /* Iterate over the labels in this graph to get information about variables
    * used in the rule and nodes whose degrees are queried in the RHS. */
   int index;
   for(index = 0; index < rule->rhs->node_index; index++)
   { 
      RuleNode *node = getRuleNode(rule->rhs, index);
      GPList *list = node->label.first;
      while(list != NULL)
      {
         scanRHSAtom(rule, node->relabelled, list->atom);
         list = list->next;
      }
   }
   for(index = 0; index < rule->rhs->edge_index; index++)
   {
      RuleEdge *edge = getRuleEdge(rule->rhs, index);
      GPList *list = edge->label.first;
      while(list != NULL)
      {
         scanRHSAtom(rule, edge->relabelled, list->atom);
         list = list->next;
      }
   }
}

static void scanRHSAtom(Rule *rule, bool relabelled, Atom atom)
{
   switch(atom.type)
   {
      case VARIABLE:
      case LENGTH:
      {
           /* The value of variable is not required for a RHS-item that is not
            * relabelled. */
           if(!relabelled) break;
           Variable *variable = getVariable(rule, atom.variable.name);
           if(variable == NULL) break;
           else variable->used_by_rule = true;
           break;
      }
      case INDEGREE:
      {
           RuleNode *node = getRuleNode(rule->lhs, atom.node_id);
           /* This should be caught by semantic analysis. */
           assert(node->interface != NULL);
           node->indegree_arg = true;
           break;
      }
      case OUTDEGREE:
      {
           RuleNode *node = getRuleNode(rule->lhs, atom.node_id);
           /* This should be caught by semantic analysis. */
           assert(node->interface != NULL);
           node->outdegree_arg = true;
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

static Label transformLabel(GPLabel *ast_label, IndexMap *node_map)
{
   Label label;
   label.mark = ast_label->mark;
   label.length = getASTListLength(ast_label->gp_list);
   GPList *list = transformList(ast_label->gp_list, node_map);
   label.first = list;
   label.last = getLastElement(list);
   return label;
}

static GPList *transformList(List *ast_list, IndexMap *node_map)
{
   if(ast_list == NULL) return NULL;
   GPList *list = NULL;
   while(ast_list != NULL)
   {
      Atom atom = transformAtom(ast_list->atom, node_map);
      list = appendAtom(list, atom);
      ast_list = ast_list->next;
   }
   return list;
}

/* Generates a Label from the AST representation of a label. The data
 * structures for atoms are extremely similar, admitting a straightforward
 * translation. One key difference is that the target structure stores
 * an integer (RHS node index) for the indegree and outdegree operations
 * in contrast to the string in the AST. The node map is passed to the two
 * transformation functions to get the appropriate index from the string
 * node identifier. */
static Atom transformAtom(GPAtom *ast_atom, IndexMap *node_map)
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
           atom.neg_exp = malloc(sizeof(Atom));
           if(atom.neg_exp == NULL)
           {
              print_to_log("Error (transformAtom): malloc failure.\n");
              exit(1);
           }
           *(atom.neg_exp) = transformAtom(ast_atom->neg_exp, node_map);
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

static Condition *transformCondition(Rule *rule, GPCondition *ast_condition, 
                                     bool negated, IndexMap *node_map)
{
   if(ast_condition == NULL) return NULL;
   Condition *condition = makeCondition();
   /* Used to assign the IDs to each predicate. */
   static int bool_count = 0;
   Predicate *predicate;
   switch(ast_condition->type)
   {
      case INT_CHECK:
      case CHAR_CHECK:
      case STRING_CHECK:
      case ATOM_CHECK:
           predicate = makeTypeCheck(bool_count++, negated, ast_condition->type, 
                                     ast_condition->var);
           condition->type = 'e';
           condition->predicate = predicate;
           Variable *variable = getVariable(rule, ast_condition->var);
           assert(variable != NULL);
           addVariablePredicate(variable, predicate, rule->predicate_count);
           break;

      case EDGE_PRED:
      {
           condition->type = 'e';
           int source_index = findLeftIndexFromId(node_map, ast_condition->edge_pred.source);
           int target_index = findLeftIndexFromId(node_map, ast_condition->edge_pred.target);

           if(ast_condition->edge_pred.label == NULL)
              predicate = makeEdgePred(bool_count++, negated, source_index, target_index, NULL);
           else 
           {
              Label label = transformLabel(ast_condition->edge_pred.label, node_map);
              predicate = makeEdgePred(bool_count++, negated, source_index, target_index, &label);
           }
           condition->predicate = predicate;
           RuleNode *source = getRuleNode(rule->lhs, source_index);
           RuleNode *target = getRuleNode(rule->lhs, target_index);
           addNodePredicate(source, predicate, rule->predicate_count);
           addNodePredicate(target, predicate, rule->predicate_count);
           break;
      }

      case EQUAL:
      case NOT_EQUAL:
           condition->type = 'e';
           /* The arguments are lists whereas the Condition equivalent has Label
            * fields. Hence the Labels are built here to be passed to
            * makeRelationalCheck. */
           int left_length = getASTListLength(ast_condition->list_cmp.left_list);
           int right_length = getASTListLength(ast_condition->list_cmp.right_list);
           GPList *left_list = transformList(ast_condition->list_cmp.left_list, node_map);
           GPList *right_list = transformList(ast_condition->list_cmp.right_list, node_map);

           Label left_label = { .mark = NONE, .length = left_length, .first = left_list,
                                .last = getLastElement(left_list) };
           Label right_label = { .mark = NONE, .length = right_length, .first = right_list,
                                 .last = getLastElement(right_list) };
           predicate = makeListComp(bool_count++, negated, ast_condition->type,
                                    left_label, right_label);
           condition->predicate = predicate;

           while(left_list != NULL)
           {
              scanPredicateAtom(rule, left_list->atom, predicate);
              left_list = left_list->next;
           }
           while(right_list != NULL)
           {
              scanPredicateAtom(rule, right_list->atom, predicate);
              right_list = right_list->next;
           }
           break;

      case GREATER:
      case GREATER_EQUAL:
      case LESS:
      case LESS_EQUAL:
      {
           condition->type = 'e';
           Atom left_atom = transformAtom(ast_condition->atom_cmp.left_exp, node_map);
           Atom right_atom = transformAtom(ast_condition->atom_cmp.right_exp, node_map);
           predicate = makeAtomComp(bool_count++, negated, ast_condition->type,
                                    left_atom, right_atom);
           condition->predicate = predicate;
           scanPredicateAtom(rule, left_atom, predicate);
           scanPredicateAtom(rule, right_atom, predicate);
           break;;
      }
      case BOOL_NOT:
           condition->type = 'n';
           condition->neg_condition = 
              transformCondition(rule, ast_condition->not_exp, !negated, node_map);
           break;

      case BOOL_OR:
           condition->type = 'o';
           condition->left_condition = 
              transformCondition(rule, ast_condition->bin_exp.left_exp, negated, node_map);
           condition->right_condition = 
              transformCondition(rule, ast_condition->bin_exp.right_exp, negated, node_map);
           break;

      case BOOL_AND:
           condition->type = 'a';
           condition->left_condition = 
              transformCondition(rule, ast_condition->bin_exp.left_exp, negated, node_map);
           condition->right_condition = 
              transformCondition(rule, ast_condition->bin_exp.right_exp, negated, node_map);
           break;

      default:
           print_to_log("Error (transformCondition): Unexpected type %d.\n", 
                        ast_condition->type);
           break;
   }
   return condition;
}

/* Searches labels in predicates for variables and degree operators in order to
 * update RuleNode and Variable structures with pointers to predicates in which
 * they participate. */
static void scanPredicateAtom(Rule *rule, Atom atom, Predicate *predicate)
{
   switch(atom.type)
   {
      case VARIABLE:
      case LENGTH:
      {
           Variable *variable = getVariable(rule, atom.variable.name);
           assert(variable != NULL);
           addVariablePredicate(variable, predicate, rule->predicate_count);
           break;
      }
      case INDEGREE:
      case OUTDEGREE:
      {
           RuleNode *node = getRuleNode(rule->lhs, atom.node_id);
           addNodePredicate(node, predicate, rule->predicate_count);
           break;
      }
      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case CONCAT:
           scanPredicateAtom(rule, *(atom.bin_op.left_exp), predicate);
           scanPredicateAtom(rule, *(atom.bin_op.right_exp),  predicate);
           break;

      default:
           print_to_log("Error (scanPredicateAtom): Unexpected type %d.\n", 
                        atom.type);
           break;
   }
}
