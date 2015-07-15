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
   rule->variable_list = calloc(variables, sizeof(Variable));
   if(rule->variable_list == NULL)
   {
      print_to_log("Error (makeRule): malloc failure.\n");
      exit(1);
   }
   rule->variables = variables;
   if(left_nodes > 0) rule->lhs = makeRuleGraph(left_nodes, left_edges);
   else rule->lhs = NULL;
   if(right_nodes > 0) rule->rhs = makeRuleGraph(right_nodes, right_edges);
   else rule->rhs = NULL;
   rule->condition = NULL;
   return rule;
}

void addVariable(Rule *rule, int index, string name, GPType type) 
{
   rule->variable_list[index].name = strdup(name);
   rule->variable_list[index].type = type;  
   rule->variable_list[index].predicates = NULL;
   rule->variable_list[index].predicate_count = 0;
   rule->variable_list[index].used_by_rule = false;   
}
         
int addRuleNode(RuleGraph *graph, bool root, RuleLabel label)
{
   int index = graph->node_index++;
   graph->nodes[index].index = index;
   graph->nodes[index].root = root;
   graph->nodes[index].remarked = true;
   graph->nodes[index].relabelled = true;
   graph->nodes[index].root_changed = true;
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
                RuleNode *target, RuleLabel label)
{
   int index = graph->edge_index++;
   graph->edges[index].index = index;
   graph->edges[index].bidirectional = bidirectional;
   graph->edges[index].remarked = true;
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

Predicate *makeTypeCheck(int bool_id, bool negated, ConditionType type, int variable_id)
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
   predicate->variable_id = variable_id;
   return predicate;
}
   
Predicate *makeEdgePred(int bool_id, bool negated, int source, int target, RuleLabel label)
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
   predicate->edge_pred.label = label;
   return predicate;
}

Predicate *makeListComp(int bool_id, bool negated, ConditionType type,
                        RuleLabel left_label, RuleLabel right_label)
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
                        RuleAtom *left_atom, RuleAtom *right_atom)
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

void addNodePredicate(RuleNode *node, Predicate *predicate, int size)
{
   if(node->predicates == NULL)
   {
      node->predicates = calloc(size, sizeof(Predicate *));
      if(node->predicates == NULL)
      {
         print_to_log("Error (addPredicate): malloc failure.\n");
         exit(1);
      }
   }
   bool predicate_exists = false;
   int index;
   for(index = 0; index < size; index++)
   {
      /* The passed predicate may already be in the predicates array. */
      if(node->predicates[index] == predicate) 
      {
         predicate_exists = true;
         break;
      }
      if(node->predicates[index] == NULL) 
      {
         node->predicates[index++] = predicate;
         break;
      }
   }
   if(!predicate_exists) node->predicate_count++;
}

void addVariablePredicate(Variable *variable, Predicate *predicate, int size)
{
   if(variable->predicates == NULL)
   {
      variable->predicates = calloc(size, sizeof(Predicate *));
      if(variable->predicates == NULL)
      {
         print_to_log("Error (addPredicate): malloc failure.\n");
         exit(1);
      }
   }
   bool predicate_exists = false;
   int index;
   for(index = 0; index < size; index++)
   {
      /* The passed predicate may already be in the predicates array. */
      if(variable->predicates[index] == predicate) 
      {
         predicate_exists = true;
         break;
      }
      if(variable->predicates[index] == NULL) 
      {
         variable->predicates[index++] = predicate;
         break;
      }
   }
   if(!predicate_exists) variable->predicate_count++;
}

bool isPredicate(Rule *rule)
{
   if(rule->lhs == NULL) return rule->rhs == NULL;
   else if(rule->rhs == NULL) return false;

   int index;
   /* Return false if the rule relabels, remarks, or adds any items. 
    * The rule adds an item if there exists an RHS item that is not in the interface.
    * Relabelling is checked by examining the item's relabelled flag. */
   for(index = 0; index < rule->rhs->node_index; index++)
   {
      RuleNode *node = getRuleNode(rule->rhs, index);
      if(node->relabelled || node->remarked || node->root_changed ||
         node->interface == NULL) return false;
   }
   for(index = 0; index < rule->rhs->edge_index; index++)
   {
      RuleEdge *edge = getRuleEdge(rule->rhs, index);
      if(edge->relabelled || edge->remarked || edge->interface == NULL) return false;
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
   for(index = 0; index < rule->variables; index++)
   {
      if(strcmp(name, rule->variable_list[index].name) == 0)
         return &(rule->variable_list[index]);
   }
   return NULL;
}

int getVariableId(Rule *rule, string name)
{
   int index;
   for(index = 0; index < rule->variables; index++)
   {
      if(strcmp(name, rule->variable_list[index].name) == 0) return index;
   }
   return -1;
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

RuleList *appendRuleAtom(RuleList *list, RuleAtom *atom)
{
   RuleListItem *new_item = malloc(sizeof(RuleListItem));
   if(new_item == NULL)
   {
      print_to_log("Error (appendAtom): malloc failure.\n");
      exit(1);
   }
   new_item->atom = atom;
   new_item->next = NULL;

   if(list == NULL)
   {
      new_item->prev = NULL;
      RuleList *new_list = malloc(sizeof(RuleList));
      if(new_list == NULL)
      {
         print_to_log("Error (appendAtom): malloc failure.\n");
         exit(1);
      }
      new_list->first = new_item;
      new_list->last = new_item;
      return new_list;
   }
   else
   {
      list->last->next = new_item;
      new_item->prev = list->last;
      list->last = new_item;
      return list;
   }
}

static bool equalAtoms(RuleAtom *left_atom, RuleAtom *right_atom)
{
   if(left_atom->type != right_atom->type) return false;
   /* LHS labels are simple expressions; there are only a few cases to consider. */
   switch(left_atom->type)
   {
      case VARIABLE:
           return left_atom->variable.id == right_atom->variable.id;

      case INTEGER_CONSTANT:
           return left_atom->number == right_atom->number;

      case STRING_CONSTANT:
           return !strcmp(left_atom->string, right_atom->string);

      case NEG:
           return !equalAtoms(left_atom->neg_exp, right_atom->neg_exp);

      case CONCAT:
           if(!equalAtoms(left_atom->bin_op.left_exp, 
                          right_atom->bin_op.left_exp)) return false;
           if(!equalAtoms(left_atom->bin_op.right_exp, 
                          right_atom->bin_op.right_exp)) return false;
           return true;

      default: break;
   }
   return false;
}

bool equalRuleLists(RuleLabel left_label, RuleLabel right_label)
{
   if(left_label.length != right_label.length) return false;
   if(left_label.list == NULL && right_label.list == NULL) return true;
   /* If the function gets this far, then the lengths are equal and at least
    * one of lists is non-empty. It follows that both lists are non-empty. */
   assert(left_label.list != NULL && right_label.list != NULL);

   RuleListItem *left_item = left_label.list->first;
   RuleListItem *right_item = right_label.list->first;
  
   while(left_item != NULL)
   {
      if(right_item == NULL) return false;
      if(!equalAtoms(left_item->atom, right_item->atom)) return false;
      left_item = left_item->next;
      right_item = right_item->next;
   }
   return true;
}

bool hasListVariable(RuleLabel label)
{
   if(label.list == NULL) return false;
   RuleListItem *item = label.list->first;
   while(item != NULL)
   {
      if(item->atom->type == VARIABLE && 
         item->atom->variable.type == LIST_VAR) return true;
      item = item->next;
   }
   return false;
}

static void printOperation(RuleAtom *left_exp, RuleAtom *right_exp, 
                           string const operation, bool nested, FILE *file);

static void printRuleAtom(RuleAtom *atom, bool nested, FILE *file)
{
    switch(atom->type) 
    {
        case INTEGER_CONSTANT: 
             fprintf(file, "%d", atom->number);
             break;
              
        case STRING_CONSTANT:
             fprintf(file, "\"%s\"", atom->string);
	     break;

	case VARIABLE: 
	     fprintf(file, "%d", atom->variable.id);
	     break;

	case INDEGREE:
	     fprintf(file, "indeg(%d)", atom->node_id);
	     break;
 
	case OUTDEGREE:
	     fprintf(file, "outdeg(%d)", atom->node_id);
	     break;

	case LENGTH:
	     fprintf(file, "length(%d)", atom->variable.id);
	     break;

	case NEG:
	     fprintf(file, "- ");
	     printRuleAtom(atom->neg_exp, true, file);
	     break;

	case ADD:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp, 
                            "+", nested, file);
	     break;

	case SUBTRACT:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp,
                            "-", nested, file);
	     break;

	case MULTIPLY:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp, 
                            "*", nested, file);
	     break;

	case DIVIDE:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp, 
                            "/", nested, file);
	     break;

	case CONCAT:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp, 
                            ".", nested, file);
	     break;

	default: fprintf(file, "Error (printAtom): Unexpected atom type: %d\n",
		        (int)atom->type); 
		 break;
    }
}

static void printOperation(RuleAtom *left_exp, RuleAtom *right_exp, 
                           string const operation, bool nested, FILE *file)
{
   if(nested) fprintf(file, "(");
   printRuleAtom(left_exp, true, file);
   fprintf(file, " %s ", operation);
   printRuleAtom(right_exp, true, file);
   if(nested) fprintf(file, ")");
}

static void printRuleList(RuleListItem *item, FILE *file)
{
   while(item != NULL)
   {
      printRuleAtom(item->atom, false, file);
      if(item->next != NULL) fprintf(file, " : ");
      item = item->next;
   }
}

static void printRuleLabel(RuleLabel label, FILE *file) 
{
   if(label.length == 0) fprintf(file, "empty");
   else printRuleList(label.list->first, file);
   if(label.mark == RED) fprintf(file, " # red"); 
   if(label.mark == GREEN) fprintf(file, " # green");
   if(label.mark == BLUE) fprintf(file, " # blue");
   if(label.mark == GREY) fprintf(file, " # grey");
   if(label.mark == DASHED) fprintf(file, " # dashed");
   if(label.mark == ANY) fprintf(file, " # any");
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
      printRuleLabel(node->label, file);
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
      printRuleLabel(edge->label, file);
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
              PTF("int(%d)", predicate->variable_id);
              break;

         case CHAR_CHECK:
              PTF("char(%d)", predicate->variable_id);
              break;

         case STRING_CHECK:
              PTF("string(%d)", predicate->variable_id);
              break;

         case ATOM_CHECK:
              PTF("atom(%d)", predicate->variable_id);
              break;

         case EDGE_PRED:
              PTF("edge(%d)", predicate->variable_id);
              break;

         case EQUAL:
         case NOT_EQUAL:
              printRuleLabel(predicate->list_comp.left_label, file);
              if(predicate->type == EQUAL) PTF(" = ");
              if(predicate->type == NOT_EQUAL) PTF(" != ");
              printRuleLabel(predicate->list_comp.right_label, file);

         case GREATER:
         case GREATER_EQUAL:
         case LESS:
         case LESS_EQUAL:
              printRuleAtom(predicate->atom_comp.left_atom, false, file);
              if(predicate->type == GREATER) PTF(" > ");
              if(predicate->type == GREATER_EQUAL) PTF(" >= ");
              if(predicate->type == LESS) PTF(" < ");
              if(predicate->type == LESS_EQUAL) PTF(" <= ");
              printRuleAtom(predicate->atom_comp.right_atom, false, file);
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

   if(rule->variable_list == NULL) PTF("No variables.\n\n");
   else 
   {
      PTF("Variables:\n");
      int index;
      for(index = 0; index < rule->variables; index++)   
      {
         Variable variable = rule->variable_list[index];
         PTF("%d: %s, type %d.", index, variable.name, variable.type);
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

static void freeRuleAtom(RuleAtom *atom)
{
   if(atom == NULL) return;
   switch(atom->type) 
   {
     case VARIABLE:
     case INTEGER_CONSTANT:
          break;

     case STRING_CONSTANT:
          if(atom->string != NULL) free(atom->string);
          break;

     case INDEGREE:
     case OUTDEGREE:
          break;

     case LENGTH:
          break;

     case NEG:
          if(atom->neg_exp != NULL) freeRuleAtom(atom->neg_exp);
          break;

     case ADD:
     case SUBTRACT:
     case MULTIPLY:
     case DIVIDE:
     case CONCAT:
          if(atom->bin_op.left_exp != NULL) freeRuleAtom(atom->bin_op.left_exp);
          if(atom->bin_op.right_exp != NULL) freeRuleAtom(atom->bin_op.right_exp);
          break;

     default: printf("Error (freeAtom): Unexpected atom type: %d\n", 
                     (int)atom->type); 
              break;
   }
   free(atom);
}

static void freeRuleList(RuleListItem *item)
{
   if(item == NULL) return;
   freeRuleAtom(item->atom);
   freeRuleList(item->next);
   free(item);
}

static void freeRuleLabel(RuleLabel label)
{
   if(label.list != NULL) freeRuleList(label.list->first);
   free(label.list);
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
      freeRuleLabel(node->label);
      if(node->predicates) free(node->predicates);
   }
   for(index = 0; index < graph->edge_index; index++)
   {
      RuleEdge *edge = getRuleEdge(graph, index);
      freeRuleLabel(edge->label);
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
           break;

      case EDGE_PRED:
           freeRuleLabel(predicate->edge_pred.label);
           break;

      case EQUAL:
      case NOT_EQUAL:
           freeRuleLabel(predicate->list_comp.left_label);
           freeRuleLabel(predicate->list_comp.right_label);
           break;

      case GREATER:
      case GREATER_EQUAL:
      case LESS:
      case LESS_EQUAL:
           freeRuleAtom(predicate->atom_comp.left_atom);
           freeRuleAtom(predicate->atom_comp.right_atom);
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
   if(rule->variable_list != NULL) 
   {
      int index;
      for(index = 0; index < rule->variables; index++)   
      {
         Variable variable = rule->variable_list[index];
         if(variable.predicates != NULL) free(variable.predicates);
      }
      free(rule->variable_list);
   }
   if(rule->lhs != NULL) freeRuleGraph(rule->lhs);
   if(rule->rhs != NULL) freeRuleGraph(rule->rhs);
   if(rule->condition != NULL) freeCondition(rule->condition);
   free(rule);
}

