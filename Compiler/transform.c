#include "transform.h"

IndexMap *node_map = NULL;
IndexMap *edge_map = NULL;

Rule *makeRule(GPRule *ast_rule)
{
   Rule *rule = malloc(sizeof(Rule));
   if(rule == NULL)
   {
      print_to_log("Error: Memory exhausted during rule construction.\n");
      exit(1);
   }
   
   rule->name = strdup(ast_rule->name);
   rule->number_of_variables = ast_rule->variable_count;
   rule->variables = scanVariableList(NULL, ast_rule->variables);
   rule->lhs = NULL;
   rule->rhs = NULL;
   rule->preserved_nodes = NULL;
   rule->preserved_edges = NULL;
   rule->deleted_nodes = NULL;
   rule->added_nodes = NULL;
   rule->added_edges = NULL;

   if(ast_rule->lhs->nodes != NULL) scanLHS(rule, ast_rule->lhs, ast_rule->interface);
   if(ast_rule->rhs->nodes != NULL) 
   {
      scanRHSNodes(rule, ast_rule->rhs, ast_rule->interface);
      scanRHSEdges(rule, ast_rule->rhs, ast_rule->interface);
      /* The rule's RHS graph has been created by the previous two function calls.
       * Now iterate over the labels in this graph to get information about 
       * variables used in the rule and nodes whose degrees are queried in the RHS. */
      int index;
      for(index = 0; index < rule->rhs->node_index; index++)
      {
         PreservedItemList *list = queryPItemList(rule->preserved_nodes, index);
         bool relabelled = true;
         if(list != NULL) relabelled = (list->new_label != NULL);
         
         Label label = getNodeLabel(rule->rhs, index);
         int list_index;
         for(list_index = 0; list_index < label.length; list_index++) 
            scanRHSAtom(rule, relabelled, label.list[list_index]);
      }
      for(index = 0; index < rule->rhs->edge_index; index++)
      {
         PreservedItemList *list = queryPItemList(rule->preserved_edges, index);
         bool relabelled = true;
         if(list != NULL) relabelled = (list->new_label != NULL);

         Label label = getEdgeLabel(rule->rhs, index);
         int list_index;
         for(list_index = 0; list_index < label.length; list_index++) 
            scanRHSAtom(rule, relabelled, label.list[list_index]);
      }
   } 
   rule->condition = NULL;

   if(node_map != NULL) freeIndexMap(node_map);
   if(edge_map != NULL) freeIndexMap(edge_map);
   node_map = NULL;
   edge_map = NULL;
   return rule;
}

VariableList *scanVariableList(VariableList *list, List *declarations)
{
   while(declarations != NULL)
   {
      List *variables = declarations->variables;
      switch(declarations->list_type)
      {
         case INT_DECLARATIONS:
              while(variables != NULL)
              {
                 list = addVariable(list, variables->variable_name, INTEGER_VAR);
                 variables = variables->next;
              }
              break;

         case CHAR_DECLARATIONS:
              while(variables != NULL)
              {
                 list = addVariable(list, variables->variable_name, CHARACTER_VAR);
                 variables = variables->next;
              }
              break;

         case STRING_DECLARATIONS:
              while(variables != NULL)
              {
                 list = addVariable(list, variables->variable_name, STRING_VAR);
                 variables = variables->next;
              }
              break;

         case ATOM_DECLARATIONS:
              while(variables != NULL)
              {
                 list = addVariable(list, variables->variable_name, ATOM_VAR);
                 variables = variables->next;
              }
              break;

         case LIST_DECLARATIONS:
              while(variables != NULL)
              {
                 list = addVariable(list, variables->variable_name, LIST_VAR);
                 variables = variables->next;
              }
              break;
          
         default:
              print_to_log("Error (scanVariableList): Unexpected type %d\n", 
                           declarations->list_type);
              break;
      }
      declarations = declarations->next;
   }
   return list;
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

int getArraySize(int number_of_items, int minimum_size)
{
   if(number_of_items < minimum_size) return minimum_size;
   if(number_of_items == 0) return 0;
   /* Return the smallest power of 2 greater than number_of_items. */
   number_of_items--;
   number_of_items |= number_of_items >> 1;
   number_of_items |= number_of_items >> 2;
   number_of_items |= number_of_items >> 4;
   number_of_items |= number_of_items >> 8;
   number_of_items |= number_of_items >> 16;
   return number_of_items + 1;
}

void scanLHS(Rule *rule, GPGraph *ast_lhs, List *interface)
{
   int lhs_nodes = getArraySize(countNodes(ast_lhs), 0);
   int lhs_edges = getArraySize(countEdges(ast_lhs), 0);
   rule->lhs = newGraph(lhs_nodes, lhs_edges);
   /* TEMPORARY */
   rule->lhs->classes = false;

   List *nodes = ast_lhs->nodes;
   while(nodes != NULL)
   {
      GPNode *ast_node = nodes->node;
      if(ast_node->root) rule->is_rooted = true;
      Label label = transformLabel(ast_node->label, NULL);
      int node_index = addNode(rule->lhs, ast_node->root, label);
      node_map = addIndexMap(node_map, ast_node->name, ast_node->root,
                             node_index, -1, NULL, NULL, label);
      bool node_in_interface = false;
      List *iterator = interface;
      /* If the node is not an interface node, add it to the deleted nodes
       * list and set the deletes_nodes flag. */
      while(iterator != NULL)
      {
         if(!strcmp(iterator->node_id, ast_node->name))
         {
            node_in_interface = true;
            break;
         }
         else iterator = iterator->next;
      }
      if(!node_in_interface) 
         rule->deleted_nodes = addItem(rule->deleted_nodes, node_index);
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
         printf("Error (scanLHS): Edge's source %s not found in the node map. "
                "\n", ast_edge->source);
         exit(1);
      }
      Label label = transformLabel(ast_edge->label, NULL);
      if(!strcmp(ast_edge->source, ast_edge->target))
      {
         int edge_index = addEdge(rule->lhs, ast_edge->bidirectional, label, 
                                  source_map->left_index, source_map->left_index);
         edge_map = addIndexMap(edge_map, ast_edge->name, false, edge_index, -1,
                                ast_edge->source, ast_edge->target, label);
      }
      else
      {
         IndexMap *target_map = findMapFromId(node_map, ast_edge->target);
         if(target_map == NULL)
         {
            printf("Error (scanLHS): Edge's target %s not found in the node map. "
                  "\n", ast_edge->target);
            exit(1);
         }

         int edge_index = addEdge(rule->lhs, ast_edge->bidirectional, label,
                                  source_map->left_index, target_map->left_index);
         edge_map = addIndexMap(edge_map, ast_edge->name, false, edge_index, -1,
                                ast_edge->source, ast_edge->target, label);
      }
      edges = edges->next;   
   }
}

void scanRHSNodes(Rule *rule, GPGraph *ast_rhs, List *interface)
{
   int rhs_nodes = getArraySize(countNodes(ast_rhs), 0);
   int rhs_edges = getArraySize(countEdges(ast_rhs), 0);
   rule->rhs = newGraph(rhs_nodes, rhs_edges);
   /* TEMPORARY */
   rule->rhs->classes = false;

   List *ast_nodes = ast_rhs->nodes;
   while(ast_nodes != NULL)
   {
      GPNode *ast_node = ast_nodes->node;
      Label label = transformLabel(ast_node->label, node_map);
      int node_index = addNode(rule->rhs, ast_node->root, label);

      IndexMap *map = findMapFromId(node_map, ast_node->name);
      if(map == NULL) 
      {
         /* If the node is not in the map, add a new map for this node with
          * left index -1, and add the node to the added nodes list. */
         node_map = addIndexMap(node_map, ast_node->name, false, -1,
                                node_index, NULL, NULL, label);
         rule->added_nodes = addItem(rule->added_nodes, node_index);
      }
      else
      {
         /* If the map exists, search the interface. If the node is an 
          * interface node, add it to the preserved nodes list, otherwise
          * add it to the added nodes list. */
         bool interface_node = false;
         List *iterator = interface;
         while(iterator != NULL)
         {
            if(!strcmp(iterator->node_id, ast_node->name))
            {
               interface_node = true;
               break;
            }
            else iterator = iterator->next;
         }
         if(interface_node)
         {
            Label *new_label = NULL;
            if(!equalRuleLabels(map->label, label)) 
            {
               new_label = malloc(sizeof(Label));
               if(new_label == NULL)
               {
                  print_to_log("Error (scanRHSNodes): malloc failure.\n");
                  exit(1);
               }
               copyLabel(&label, new_label);
            }
            rule->preserved_nodes = addPreservedItem(rule->preserved_nodes, map->left_index,
                                                     ast_node->root, new_label);
         }
         else rule->added_nodes = addItem(rule->added_nodes, node_index);
         map->right_index = node_index;
      }
      ast_nodes = ast_nodes->next;   
   }
}

void scanRHSEdges(Rule *rule, GPGraph *ast_rhs, List *interface)
{
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
         printf("Error (scanRHS): Edge's source %s not found in the node map."
                "\n", source_id);
         exit(1);
      }
      Node *source = getNode(rule->rhs, source_map->right_index);

      IndexMap *target_map = findMapFromId(node_map, target_id);
      if(target_map == NULL)
      {
         printf("Error (scanRHS): Edge's target %s not found in the node "
                  "map. \n", target_id);
         exit(1);
      }
      Node *target = getNode(rule->rhs, target_map->right_index);

      Label label = transformLabel(ast_edge->label, NULL);
      int edge_index = addEdge(rule->rhs, ast_edge->bidirectional, label, 
                               source->index, target->index);
      /* Flags to signify whether the source and target nodes exist in the
       * interface. This is used to create the NewEdge structure with the
       * correct fields. */
      bool interface_source = false;
      bool interface_target = false;
      List *iterator = interface;

      /* Search the interface for the edge's source and target. */
      while(iterator != NULL)
      {
         if(!strcmp(iterator->node_id, source_id))
         {
            interface_source = true;
            if(interface_target) break;
            if(!strcmp(iterator->node_id, target_id))
            {
               interface_target = true;
               break;
            }         
         }
         if(!strcmp(iterator->node_id, target_id))
         {
            interface_target = true;
            if(interface_source) break;     
         }
         iterator = iterator->next;
      }

      if(interface_source && interface_target)
      {
         /* Search in the edge map for an edge whose source and target 
          * correspond with that of the current edge. */
         IndexMap *map = findMapFromSrcTgt(edge_map, source_id, target_id);
         if(map == NULL && ast_edge->bidirectional)
            map = findMapFromSrcTgt(edge_map, target_id, source_id);
         if(map == NULL)
            /* No such map exists, thus the edge is added by the rule. Both
             * source and target come from the LHS because they are both in
             * the interface. */
            rule->added_edges = addNewEdge(rule->added_edges, edge_index, 'l',
                                           source_map->left_index, 'l', 
                                           target_map->left_index);
         else 
         {
            /* A map has been found, therefore the edge is preserved by the
             * rule. */
            Label *new_label = NULL;
            if(!equalRuleLabels(map->label, label)) 
            {
               new_label = malloc(sizeof(Label));
               if(new_label == NULL)
               {
                  print_to_log("Error (scanRHSNodes): malloc failure.\n");
                  exit(1);
               }
               copyLabel(&label, new_label);
            }
            rule->preserved_edges = addPreservedItem(rule->preserved_edges, map->left_index,
                                                     false, new_label);
            /* The map is removed to ensure that a parallel RHS-edge is not
             * associated with this edge. */
            edge_map = removeMap(edge_map, map);     
         }
      }
      else
      {
         /* At most one of interface_source and interface_target is true. */
         if(interface_source)
            /* The source node is preserved and the target is created. */
            rule->added_edges = addNewEdge(rule->added_edges, edge_index, 'l',
                                           source_map->left_index, 'r', target->index);
         else
         {
            if(interface_target)
            /* The target node is preserved and the source is created. */
               rule->added_edges = addNewEdge(rule->added_edges, edge_index, 'r', 
                                              source->index, 'l', target_map->left_index);
            else 
               /* Both the source and the target are created. */
               rule->added_edges = addNewEdge(rule->added_edges, edge_index, 'r', 
                                              source->index, 'r', target->index);
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
           VariableList *variables = rule->variables;
           while(variables != NULL)
           {
              if(!strcmp(atom.variable.name, variables->variable))
              {
                 variables->used_by_rule = true;
                 break;
              }
              variables = variables->next;
           }
           break;
      }

      case INDEGREE:
      {
           PreservedItemList *list = queryPItemList(rule->preserved_nodes, atom.node_id);
           if(list == NULL)
              print_to_log("Error (scanRHSLabel): Argument of indegree operator %d"
                           "not in preserved nodes list.\n", atom.node_id);
           else list->indegree_argument = true;
           break;
      }

      case OUTDEGREE:
      {
           PreservedItemList *list = queryPItemList(rule->preserved_nodes, atom.node_id);
           if(list == NULL)
              print_to_log("Error (scanRHSLabel): Argument of outdegree operator %d"
                           "not in preserved nodes list.\n", atom.node_id);
           else list->outdegree_argument = true;
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
   label.length = getASTListLength(ast_label);
   if(label.length == 0)
   {
      if(label.mark == NONE) return blank_label;
      else label.list = NULL;
   }
   else
   {
      label.list = makeList(label.length);
      List *list = ast_label->gp_list;
      int position = 0;
      while(list != NULL)
      {
         Atom atom = transformAtom(list->atom, node_map);
         addAtom(atom, label, position++);
         list = list->next;
      }
   }
   return label;
}

int getASTListLength(GPLabel *ast_label)
{
   int length = 0;
   List *list = ast_label->gp_list;
   while(list != NULL)
   {
      length++;
      list = list->next;
   }
   return length;
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
