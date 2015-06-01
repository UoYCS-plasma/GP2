#include "rule.h"

static RuleGraph *makeRuleGraph(int nodes, int edges)
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
   rule->adds_nodes = false;
   rule->adds_edges = false;
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
   rule->variables[rule->variable_index].predicate_count = 0;
   rule->variables[rule->variable_index].used_by_rule = false;   
   rule->variable_index++;
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
   graph->nodes[index].predicate_count = 0;
   graph->nodes[index].indegree = 0;
   graph->nodes[index].outdegree = 0;
   graph->nodes[index].bidegree = 0;
   return index;
}

static RuleEdges *addIncidentEdge(RuleEdges *edges, RuleEdge *edge)
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
   source->outedges = addIncidentEdge(source->outedges, &(graph->edges[index]));
   target->inedges = addIncidentEdge(target->inedges, &(graph->edges[index]));
   if(bidirectional) 
   {
      source->bidegree++;
      target->bidegree++;
   }
   else
   {
      source->outdegree++;
      target->indegree++;
   }
   return index;
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

Predicate *makeTypeCheck(int bool_id, bool negated, ConditionType type, string variable)
{
   Predicate *predicate = malloc(sizeof(Predicate));
   if(predicate == NULL)
   {
      print_to_log("Error (makePredicate): malloc failure.\n");
      exit(1);
   }
   predicate->bool_id = bool_id;
   predicate->negated = negated;
   predicate->type = type;
   predicate->variable = variable;
   return predicate;
}
   
Predicate *makeEdgePred(int bool_id, bool negated, int source, int target, Label *label)
{
   Predicate *predicate = malloc(sizeof(Predicate));
   if(predicate == NULL)
   {
      print_to_log("Error (makePredicate): malloc failure.\n");
      exit(1);
   }
   predicate->bool_id = bool_id;
   predicate->negated = negated;
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

Predicate *makeListComp(int bool_id, bool negated, ConditionType type,
                        Label left_label, Label right_label)
{
   Predicate *predicate = malloc(sizeof(Predicate));
   if(predicate == NULL)
   {
      print_to_log("Error (makePredicate): malloc failure.\n");
      exit(1);
   }
   predicate->bool_id = bool_id;
   predicate->negated = negated;
   predicate->type = type;
   predicate->list_comp.left_label = left_label;
   predicate->list_comp.right_label = right_label;
   return predicate;
}

Predicate *makeAtomComp(int bool_id, bool negated, ConditionType type,
                        Atom left_atom, Atom right_atom)
{
   Predicate *predicate = malloc(sizeof(Predicate));
   if(predicate == NULL)
   {
      print_to_log("Error (makePredicate): malloc failure.\n");
      exit(1);
   }
   predicate->bool_id = bool_id;
   predicate->negated = negated;
   predicate->type = type;
   predicate->atom_comp.left_atom = left_atom;
   predicate->atom_comp.right_atom = right_atom;
   return predicate;
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

bool isPredicate(Rule *rule)
{
   if(rule->lhs == NULL && rule->rhs == NULL) return true;
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

Variable *getVariable(Rule *rule, string name)
{
   int index;
   for(index = 0; index < rule->variable_index; index++)
   {
      if(strcmp(name, rule->variables[index].name) == 0)
         return &(rule->variables[index]);
   }
   return NULL;
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

static void printRuleGraph(RuleGraph *graph, FILE *file)
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
   
static void printCondition(Condition *condition, bool nested, FILE *file)
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
              printList(predicate->list_comp.left_label.list, 
                        predicate->list_comp.left_label.length, file);
              if(predicate->type == EQUAL) PTF(" = ");
              if(predicate->type == NOT_EQUAL) PTF(" != ");
              printList(predicate->list_comp.right_label.list, 
                        predicate->list_comp.right_label.length, file);

         case GREATER:
         case GREATER_EQUAL:
         case LESS:
         case LESS_EQUAL:
              printAtom(&(predicate->atom_comp.left_atom), false, file);
              if(predicate->type == GREATER) PTF(" > ");
              if(predicate->type == GREATER_EQUAL) PTF(" >= ");
              if(predicate->type == LESS) PTF(" < ");
              if(predicate->type == LESS_EQUAL) PTF(" <= ");
              printAtom(&(predicate->atom_comp.right_atom), false, file);
              break;

         default: break;
      }
   }
   else if(condition->type == 'n')
   {
      PTF("not ");
      printCondition(condition->neg_condition, nested, file);
   }
   else if(condition->type == 'o')
   {
      if(nested) PTF("(");
      printCondition(condition->left_condition, true, file);
      PTF(" or ");
      printCondition(condition->right_condition, true, file);
      if(nested) PTF(")");
   }
   else if(condition->type == 'a')
   {
      if(nested) PTF("(");
      printCondition(condition->left_condition, true, file);
      PTF(" and ");
      printCondition(condition->right_condition, true, file);
      if(nested) PTF(")");
   }
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
   printCondition(rule->condition, false, file);
}

static void freeRuleEdges(RuleEdges *edges)
{
   if(edges == NULL) return;
   freeRuleEdges(edges->next);
   free(edges);
}

static void freeRuleGraph(RuleGraph *graph)
{
   int index;
   for(index = 0; index < graph->node_index; index++)
   {
      RuleNode *node = getRuleNode(graph, index);
      freeRuleEdges(node->outedges);
      freeRuleEdges(node->inedges);
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
   free(graph);
}

static void freePredicate(Predicate *predicate)
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
           freeLabel(predicate->list_comp.left_label);
           freeLabel(predicate->list_comp.right_label);

      case GREATER:
      case GREATER_EQUAL:
      case LESS:
      case LESS_EQUAL:
           freeAtom(&(predicate->atom_comp.left_atom), false);
           freeAtom(&(predicate->atom_comp.right_atom), false);
           break;

      default: break;
   }
   free(predicate);
}

static void freeCondition(Condition *condition)
{
   if(condition->type == 'e') freePredicate(condition->predicate);
   else if(condition->type == 'n') freeCondition(condition->neg_condition);
   else if(condition->type == 'o' || condition->type == 'a')
   {
      freeCondition(condition->left_condition);
      freeCondition(condition->right_condition);
   }
   free(condition);
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
   if(rule->condition != NULL) freeCondition(rule->condition);
   free(rule);
}

