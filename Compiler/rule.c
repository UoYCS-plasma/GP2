#include "rule.h"

Rule *makeRule(int variables, int left_nodes, int left_edges,
               int right_nodes, int right_edges)
{
   Rule *rule = malloc(sizeof(Rule));
   if(rule == NULL)
   {
      print_to_log("Error (makeRule): malloc failure.\n");
      exit(1);
   }
   rule->is_rooted = false;
   rule->variables = calloc(variables, sizeof(Variable));
   if(rule->variables == NULL)
   {
      print_to_log("Error (makeRule): malloc failure.\n");
      exit(1);
   }
   rule->variable_index = 0;
   if(left_nodes > 0) rule->lhs = makeRuleGraph(left_nodes, left_edges);
   else rule->lhs = NULL;
   if(right_nodes > 0) rule->rhs = makeRuleGraph(right_nodes, right_edges);
   else rule->rhs = NULL;
   rule->condition = NULL;
   return rule;
}

void addVariable(Rule *rule, string name, GPType type) 
{
   rule->variables[rule->variable_index].name = strdup(name);
   rule->variables[rule->variable_index].type = type;  
   rule->variables[rule->variable_index].predicates = NULL;
   rule->variables[rule->variable_index].used_by_rule = false;   
   rule->variable_index++;
}

/* I assume this called only when a variable is known to be in the list. */
GPType lookupType(Rule *rule, string name) 
{ 
   int index;
   for(index = 0; index < rule->variable_index; index++)   
   {
      if(strcmp(rule->variables[index].name, name) == 0) 
         return rule->variables[index].type;
   }
   print_to_log("Error: lookupType called with variable %s not in the rule.", name);
   exit(0);         
}

RuleGraph *makeRuleGraph(int nodes, int edges)
{
   RuleGraph *graph = malloc(sizeof(RuleGraph));
   if(graph == NULL)
   {
      print_to_log("Error (makeRuleGraph): malloc failure.\n");
      exit(1);
   }
   graph->nodes = calloc(nodes, sizeof(RuleNode));
   if(graph->nodes == NULL)
   {
      print_to_log("Error (makeRuleGraph): malloc failure.\n");
      exit(1);
   }
   graph->edges = calloc(edges, sizeof(RuleEdge));
   if(graph->edges == NULL)
   {
      print_to_log("Error (makeRuleGraph): malloc failure.\n");
      exit(1);
   }
   graph->node_index = 0;
   graph->edge_index = 0;
   return graph;
}

RuleNode *getRuleNode(RuleGraph *graph, int index)
{
   RuleNode *node = &(graph->nodes[index]);
   return node;
}

RuleEdge *getRuleEdge(RuleGraph *graph, int index)
{
   RuleEdge *edge = &(graph->edges[index]);
   return edge;
}

int addRuleNode(RuleGraph *graph, bool root, Label label)
{
   int index = graph->node_index++;
   graph->nodes[index].index = index;
   graph->nodes[index].root = root;
   graph->nodes[index].relabelled = true;
   graph->nodes[index].indegree_arg = false;
   graph->nodes[index].outdegree_arg = false;
   graph->nodes[index].interface = NULL;
   graph->nodes[index].outedges = NULL;
   graph->nodes[index].inedges = NULL;
   graph->nodes[index].label = label;
   graph->nodes[index].predicates = NULL;
   graph->nodes[index].indegree = 0;
   graph->nodes[index].outdegree = 0;
   graph->nodes[index].bidegree = 0;
   return index;
}

int addRuleEdge(RuleGraph *graph, bool bidirectional, RuleNode *source,
                RuleNode *target, Label label)
{
   int index = graph->edge_index++;
   graph->edges[index].index = index;
   graph->edges[index].bidirectional = bidirectional;
   graph->edges[index].relabelled = true;
   graph->edges[index].interface = NULL;
   graph->edges[index].source = source;
   graph->edges[index].target = target;
   graph->edges[index].label = label;
   if(bidirectional) 
   {
      source->biedges = addIncidentEdge(source->biedges, &(graph->edges[index]));
      source->bidegree++;
      target->biedges = addIncidentEdge(target->biedges, &(graph->edges[index]));
      target->bidegree++;
   }
   else
   {
      source->outedges = addIncidentEdge(source->outedges, &(graph->edges[index]));
      source->outdegree++;
      target->inedges = addIncidentEdge(target->inedges, &(graph->edges[index]));
      target->indegree++;
   }
   return index;
}

RuleEdges *addIncidentEdge(RuleEdges *edges, RuleEdge *edge)
{
   RuleEdges *new_edge = malloc(sizeof(RuleEdges));
   if(new_edge == NULL)
   {
      print_to_log("Error (addIncidentEdge): malloc failure.\n");
      exit(1);
   }
   new_edge->edge = edge;
   new_edge->next = edges;
   return new_edge;
}

void addVariablePredicate(Rule *rule, string name, Predicate *predicate)
{
   int index;
   for(index = 0; index < rule->variable_index; index++)   
   {
      Variable variable = rule->variables[index];
      if(strcmp(variable.name, name))
      {
         addPredicate(variable.predicates, predicate, rule->predicate_count);
         break;
      }
   }
}

Predicate **addPredicate(Predicate **predicates, Predicate *predicate, int size)
{
   if(predicates == NULL)
   {
      Predicate **new_predicates = calloc(size, sizeof(Predicate *));
      if(new_predicates == NULL)
      {
         print_to_log("Error (addPredicate): malloc failure.\n");
         exit(1);
      }
      predicates[0] = predicate;
   }
   else
   {
      int index;
      for(index = 0; index < size; index++)
      {
         if(predicates[index] == NULL) 
         {
            predicates[index++] = predicate;
            break;
         }
      }
   }
   return predicates;
}

Predicate *makeTypeCheck(int bool_id, ConditionType type, string variable)
{
   Predicate *predicate = malloc(sizeof(Predicate));
   if(predicate == NULL)
   {
      print_to_log("Error (makePredicate): malloc failure.\n");
      exit(1);
   }
   predicate->bool_id = bool_id;
   predicate->type = type;
   predicate->variable = variable;
   return predicate;
}
   
Predicate *makeEdgePred(int bool_id, int source, int target, Label *label)
{
   Predicate *predicate = malloc(sizeof(Predicate));
   if(predicate == NULL)
   {
      print_to_log("Error (makePredicate): malloc failure.\n");
      exit(1);
   }
   predicate->bool_id = bool_id;
   predicate->type = EDGE_PRED;
   predicate->edge_pred.source = source;
   predicate->edge_pred.target = target;
   if(label == NULL) predicate->edge_pred.label = NULL;
   else
   {
      predicate->edge_pred.label = malloc(sizeof(Label));
      if(predicate->edge_pred.label == NULL)
      {
         print_to_log("Error (makePredicate): malloc failure.\n");
         exit(1);
      }
      copyLabel(label, predicate->edge_pred.label);
   }
   return predicate;
}

Predicate *makeRelationalCheck(int bool_id, ConditionType type, Atom *left_list,
                               int left_length, Atom *right_list, int right_length)
{
   Predicate *predicate = malloc(sizeof(Predicate));
   if(predicate == NULL)
   {
      print_to_log("Error (makePredicate): malloc failure.\n");
      exit(1);
   }
   predicate->bool_id = bool_id;
   predicate->type = type;
   predicate->comparison.left_list = left_list;
   predicate->comparison.left_length = left_length;
   predicate->comparison.right_list = right_list;
   predicate->comparison.right_length = right_length;
   return predicate;
}

Condition *makeCondition(void)
{
   Condition *condition = malloc(sizeof(Condition));
   if(condition == NULL)
   {
      print_to_log("Error (makeCondition): malloc failure.\n");
      exit(1);
   }
   return condition;
}

bool isPredicate(Rule *rule)
{
   int index;
   /* Return false if the rule relabels or adds any items. The rule adds an
    * item if there exists an RHS item that is not in the interface.
    * Relabelling is checked by examining the item's relabelled flag. */
   for(index = 0; index < rule->rhs->node_index; index++)
   {
      RuleNode *node = getRuleNode(rule->rhs, index);
      if(node->relabelled || node->interface == NULL) return false;
   }
   for(index = 0; index < rule->rhs->edge_index; index++)
   {
      RuleEdge *edge = getRuleEdge(rule->rhs, index);
      if(edge->relabelled || edge->interface == NULL) return false;
   }

   /* Return false if the rule deletes any items. The rule deletes an item
    * if there exists a LHS item that is not in the interface. */
   for(index = 0; index < rule->lhs->node_index; index++)
   {
      RuleNode *node = getRuleNode(rule->lhs, index);
      if(node->interface == NULL) return false;
   }
   for(index = 0; index < rule->lhs->edge_index; index++)
   {
      RuleEdge *edge = getRuleEdge(rule->lhs, index);
      if(edge->interface == NULL) return false;
   }
   return true;
}

void printRule(Rule *rule, FILE *file)
{
   if(rule == NULL) 
   {
      print_to_log("Error (printRule): NULL rule pointer.\n\n");
      return;
   }
   PTF("%s\n\n", rule->name);
   printRuleGraph(rule->lhs, file);
   PTF("\n=>\n\n");
   printRuleGraph(rule->rhs, file);
   PTF("\n");

   if(rule->variables == NULL) PTF("No variables.\n\n");
   else 
   {
      PTF("Variables:\n");
      int index;
      for(index = 0; index < rule->variable_index; index++)   
      {
         Variable variable = rule->variables[index];
         PTF("%s, type %d.", variable.name, variable.type);
         if(variable.predicates != NULL) PTF(" In predicates ");
         int i;
         for(i = 0; i < rule->predicate_count; i++)
         {
            PTF("%d", variable.predicates[i]->bool_id);
            if(i == rule->predicate_count - 1) PTF(".\n");
            else PTF(", ");
         }
         PTF("\n");
      }
   }
   PTF("\n");
   PTF("Condition:\n");
   printCondition(rule->condition, file);
}

void printRuleGraph(RuleGraph *graph, FILE *file)
{
   int index, node_count = 0, edge_count = 0;
   if(graph == NULL || graph->node_index == 0) 
   {
      PTF("[ | ]\n");
      return;
   }
   PTF("[ ");
   for(index = 0; index < graph->node_index; index++)
   {
      RuleNode *node = getRuleNode(graph, index);
      /* Five nodes per line */
      if(node_count != 0 && node_count % 5 == 0) PTF("\n  ");
      if(node->root) PTF("(n%d(R), ", index);
      else PTF("(n%d, ", index);
      printLabel(node->label, file);
      PTF(") ");
   }
   if(graph->edge_index == 0)
   {
      PTF("| ]\n\n");
      return;
   }
   PTF("|\n  ");
   for(index = 0; index < graph->edge_index; index++)
   {
      RuleEdge *edge = getRuleEdge(graph, index);
      /* Three edges per line */
      if(edge_count != 0 && edge_count % 3 == 0) PTF("\n  ");
      if(edge->bidirectional) PTF("(e%d(B), ", index);
      else PTF("(e%d, ", index);
      PTF("n%d, n%d, ", edge->source->index, edge->target->index);
      printLabel(edge->label, file);
      PTF(") ");
   }
   PTF("]\n\n");
}
   
void printCondition(Condition *condition, FILE *file)
{
   if(condition->type == 'e')
   {
      Predicate *predicate = condition->predicate;
      PTF("(%d) ", predicate->bool_id);
      switch(predicate->type)
      {
         case INT_CHECK:
              PTF("int(%s)", predicate->variable);
              break;

         case CHAR_CHECK:
              PTF("char(%s)", predicate->variable);
              break;

         case STRING_CHECK:
              PTF("string(%s)", predicate->variable);
              break;

         case ATOM_CHECK:
              PTF("atom(%s)", predicate->variable);
              break;

         case EDGE_PRED:
              PTF("edge(%s)", predicate->variable);
              break;

         case EQUAL:
         case NOT_EQUAL:
         case GREATER:
         case GREATER_EQUAL:
         case LESS:
         case LESS_EQUAL:
              printList(predicate->comparison.left_list, 
                        predicate->comparison.left_length, file);
              if(predicate->type == EQUAL) PTF(" = ");
              if(predicate->type == NOT_EQUAL) PTF(" != ");
              if(predicate->type == GREATER) PTF(" > ");
              if(predicate->type == GREATER_EQUAL) PTF(" >= ");
              if(predicate->type == LESS) PTF(" < ");
              if(predicate->type == LESS_EQUAL) PTF(" <= ");
              printList(predicate->comparison.right_list, 
                        predicate->comparison.right_length, file);
              break;

         default: break;
      }
   }
   else if(condition->type == 'n')
   {
      PTF("not ");
      printCondition(condition->neg_predicate, file);
   }
   else if(condition->type == 'o')
   {
      printCondition(condition->left_predicate, file);
      PTF(" or ");
      printCondition(condition->right_predicate, file);
   }
   else if(condition->type == 'a')
   {
      printCondition(condition->left_predicate, file);
      PTF(" and ");
      printCondition(condition->right_predicate, file);
   }
}

void freeRule(Rule *rule)
{
   if(rule == NULL) return;
   if(rule->name != NULL) free(rule->name);
   if(rule->variables != NULL) 
   {
      int index;
      for(index = 0; index < rule->variable_index; index++)   
      {
         Variable variable = rule->variables[index];
         if(variable.name != NULL) free(variable.name);
         if(variable.predicates != NULL) free(variable.predicates);
      }
      free(rule->variables);
   }
   if(rule->lhs != NULL) freeRuleGraph(rule->lhs);
   if(rule->rhs != NULL) freeRuleGraph(rule->rhs);
   /* Conditions currently ignored. No function exists to free a condition. */
   if(rule->condition) freeCondition(rule->condition);
   free(rule);
}

void freeRuleGraph(RuleGraph *graph)
{
   int index;
   for(index = 0; index < graph->node_index; index++)
   {
      RuleNode *node = getRuleNode(graph, index);
      freeRuleEdges(node->outedges);
      freeRuleEdges(node->inedges);
      freeRuleEdges(node->biedges);
      freeLabel(node->label);
      if(node->predicates) free(node->predicates);
   }
   for(index = 0; index < graph->edge_index; index++)
   {
      RuleEdge *edge = getRuleEdge(graph, index);
      freeLabel(edge->label);
   }
   free(graph->nodes);
   free(graph->edges);
}

void freeRuleEdges(RuleEdges *edges)
{
   if(edges == NULL) return;
   freeRuleEdges(edges->next);
   free(edges);
}

void freeCondition(Condition *condition)
{
   if(condition->type == 'e') freePredicate(condition->predicate);
   else if(condition->type == 'n') freeCondition(condition->neg_predicate);
   else if(condition->type == 'o' || condition->type == 'a')
   {
      freeCondition(condition->left_predicate);
      freeCondition(condition->right_predicate);
   }
   free(condition);
}

void freePredicate(Predicate *predicate)
{
   switch(predicate->type)
   {
      case INT_CHECK:
      case CHAR_CHECK:
      case STRING_CHECK:
      case ATOM_CHECK:
           free(predicate->variable);
           break;

      case EDGE_PRED:
           freeLabel(*(predicate->edge_pred.label));
           free(predicate->edge_pred.label);
           break;

      case EQUAL:
      case NOT_EQUAL:
      case GREATER:
      case GREATER_EQUAL:
      case LESS:
      case LESS_EQUAL:
           freeList(predicate->comparison.left_list, 
                    predicate->comparison.left_length);
           freeList(predicate->comparison.right_list,  
                    predicate->comparison.right_length);
           break;

      default: break;
   }
   free(predicate);
}


IndexMap *addIndexMap(IndexMap *map, string id, bool root, int left_index,
                      int right_index, string source_id, string target_id)
{
   IndexMap *new_map = malloc(sizeof(IndexMap));
   if(new_map == NULL)
   {
      print_to_log("Error: Memory exhausted during map construction.\n");
      exit(1);
   }
   new_map->id = strdup(id);
   new_map->root = root;
   new_map->left_index = left_index;
   new_map->right_index = right_index;
   if(source_id == NULL) new_map->source_id = NULL;
   else new_map->source_id = strdup(source_id);
   if(target_id == NULL) new_map->target_id = NULL;
   else new_map->target_id = strdup(target_id);
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
   if(map == map_to_remove)
   {
      IndexMap *temp = map;
      map = map->next;
      if(temp->id) free(temp->id);
      if(temp->source_id) free(temp->source_id);
      if(temp->target_id) free(temp->target_id);
      free(temp);
   }
   else
   {
      IndexMap *iterator = map;
      while(iterator != NULL)
      {
         if(iterator->next == NULL) break;
         if(iterator->next == map_to_remove)
         {
            IndexMap *temp = iterator->next;
            iterator->next = iterator->next->next;
            if(temp->id) free(temp->id);
            if(temp->source_id) free(temp->source_id);
            if(temp->target_id) free(temp->target_id);
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
   
   if(map->id) free(map->id);
   if(map->source_id) free(map->source_id);
   if(map->target_id) free(map->target_id);
   if(map->next) freeIndexMap(map->next);
   free(map);
}

