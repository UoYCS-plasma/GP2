#include "transform.h"

Rule *makeRule(GPRule *ast_rule)
{
   Rule *rule = malloc(sizeof(Rule));
   if(rule == NULL)
   {
      print_to_log("Error: Memory exhausted during rule construction.\n");
      exit(1);
   }
   
   rule->name = strdup(ast_rule->name);
   rule->variables = NULL;
   rule->number_of_variables = 0;

   IndexMap *node_map = NULL;
   IndexMap *edge_map = NULL;

   rule->preserved_nodes = NULL;
   rule->preserved_edges = NULL;
   rule->deleted_nodes = NULL;
   rule->added_nodes = NULL;
   rule->added_edges = NULL;

   if(ast_rule->lhs->nodes == NULL) rule->lhs = NULL;
   else rule->lhs = scanLHS(ast_rule->lhs, ast_rule->interface, &node_map,
                            &edge_map, &(rule->deleted_nodes), &(rule->is_rooted));

   if(ast_rule->rhs->nodes == NULL) rule->rhs = NULL;
   else
   {
      rule->rhs = scanRHSNodes(ast_rule->rhs, ast_rule->interface, &node_map,
                               &(rule->preserved_nodes), &(rule->added_nodes));
      rule->added_edges = scanRHSEdges(ast_rule->rhs, rule->rhs,
                                       ast_rule->interface, node_map, 
                                       &edge_map, &(rule->preserved_edges));
   } 
   if(node_map) freeIndexMap(node_map);
   if(edge_map) freeIndexMap(edge_map);
   rule->condition = NULL;
   return rule;
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

Graph *scanLHS(GPGraph *ast_lhs, List *interface, IndexMap **node_map, 
               IndexMap **edge_map, ItemList **deleted_nodes, bool *is_rooted)
{
   int lhs_nodes = getArraySize(countNodes(ast_lhs), 0);
   int lhs_edges = getArraySize(countEdges(ast_lhs), 0);
   Graph *lhs = newGraph(lhs_nodes, lhs_edges);

   List *nodes = ast_lhs->nodes;
   while(nodes != NULL)
   {
      GPNode *ast_node = nodes->node;
      if(ast_node->root) *is_rooted = 1;
      Label label = transformLabel(ast_node->label, NULL);
      int node_index = addNode(lhs, ast_node->root, label);
      *node_map = addIndexMap(*node_map, ast_node->name, ast_node->root,
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
         *deleted_nodes = addItem(*deleted_nodes, node_index);
      nodes = nodes->next;   
   }

   List *edges = ast_lhs->edges;
   while(edges != NULL)
   {
      GPEdge *ast_edge = edges->edge;
      /* Use the node map to get the correct pointers for the edge's source
       * and target to pass to newEdge. */
      IndexMap *source_map = findMapFromId(*node_map, ast_edge->source);
      if(source_map == NULL)
      {
         printf("Error (scanLHS): Edge's source %s not found in the node map. "
                "\n", ast_edge->source);
         exit(1);
      }
      Label label = transformLabel(ast_edge->label, NULL);
      if(!strcmp(ast_edge->source, ast_edge->target))
      {
         int edge_index = addEdge(lhs, ast_edge->bidirectional, label, 
                                  source_map->left_index, source_map->left_index);
         *edge_map = addIndexMap(*edge_map, ast_edge->name, false, edge_index, -1,
                                 ast_edge->source, ast_edge->target, label);
      }
      else
      {
         IndexMap *target_map = findMapFromId(*node_map, ast_edge->target);
         if(target_map == NULL)
         {
            printf("Error (scanLHS): Edge's target %s not found in the node map. "
                  "\n", ast_edge->target);
            exit(1);
         }

         int edge_index = addEdge(lhs, ast_edge->bidirectional, label,
                                  source_map->left_index, target_map->left_index);
         *edge_map = addIndexMap(*edge_map, ast_edge->name, false, edge_index, -1,
                                 ast_edge->source, ast_edge->target, label);
      }
      edges = edges->next;   
   }
   return lhs;
}

Graph *scanRHSNodes(GPGraph *ast_rhs, List *interface, IndexMap **node_map,
                    PreservedItemList **nodes, ItemList **added_nodes)
{
   int rhs_nodes = getArraySize(countNodes(ast_rhs), 0);
   int rhs_edges = getArraySize(countEdges(ast_rhs), 0);
   Graph *rhs = newGraph(rhs_nodes, rhs_edges);

   List *ast_nodes = ast_rhs->nodes;
   while(ast_nodes != NULL)
   {
      GPNode *ast_node = ast_nodes->node;
      Label label = transformLabel(ast_node->label, *node_map);
      int node_index = addNode(rhs, ast_node->root, label);

      IndexMap *map = findMapFromId(*node_map, ast_node->name);
      if(map == NULL) 
      {
         /* If the node is not in the map, add a new map for this node with
          * left index -1, and add the node to the added nodes list. */
         *node_map = addIndexMap(*node_map, ast_node->name, false, -1,
                                 node_index, NULL, NULL, label);
         *added_nodes = addItem(*added_nodes, node_index);
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
            *nodes = addPreservedItem(*nodes, map->left_index, ast_node->root, new_label);
         }
         else *added_nodes = addItem(*added_nodes, node_index);
         map->right_index = node_index;
      }
      ast_nodes = ast_nodes->next;   
   }
   return rhs;
}

NewEdgeList *scanRHSEdges(GPGraph *ast_rhs, Graph *rhs, List *interface, 
                          IndexMap *node_map, IndexMap **edge_map,
                          PreservedItemList **edges)
{
   NewEdgeList *added_edges = NULL;
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
      Node *source = getNode(rhs, source_map->right_index);

      IndexMap *target_map = findMapFromId(node_map, target_id);
      if(target_map == NULL)
      {
         printf("Error (scanRHS): Edge's target %s not found in the node "
                  "map. \n", target_id);
         exit(1);
      }
      Node *target = getNode(rhs, target_map->right_index);

      Label label = transformLabel(ast_edge->label, NULL);
      int edge_index = addEdge(rhs, ast_edge->bidirectional, label, 
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
         IndexMap *map = findMapFromSrcTgt(*edge_map, source_id, target_id);
         if(map == NULL && ast_edge->bidirectional)
            map = findMapFromSrcTgt(*edge_map, target_id, source_id);
         if(map == NULL)
            /* No such map exists, thus the edge is added by the rule. Both
             * source and target come from the LHS because they are both in
             * the interface. */
            added_edges = 
                addNewEdge(added_edges, edge_index, 'l', source_map->left_index,
                          'l', target_map->left_index);
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
            *edges = addPreservedItem(*edges, map->left_index, false, new_label);
            /* The map is removed to ensure that a parallel RHS-edge ins not
             * associated with this edge. */
            *edge_map = removeMap(*edge_map, map);     
         }
      }
      else
      {
         /* At most one of interface_source and interface_target is true. */
         if(interface_source)
            /* The source node is preserved and the target is created. */
            added_edges = addNewEdge(added_edges, edge_index, 
                                     'l', source_map->left_index,
                                     'r', target->index);
         else
         {
            if(interface_target)
            /* The target node is preserved and the source is created. */
               added_edges = addNewEdge(added_edges, edge_index, 
                                        'r', source->index,
                                        'l', target_map->left_index);
            else 
               /* Both the source and the target are created. */
               added_edges = addNewEdge(added_edges, edge_index, 'r', 
                                        source->index, 'r', target->index);
         }
      }
      ast_edges = ast_edges->next;   
   }
   return added_edges;
}

Label transformLabel(GPLabel *ast_label, IndexMap *node_map)
{
   Label label;
   label.mark = ast_label->mark;
   /* Traverse the AST list to get its length. If it is 0, set the label's list
    * to NULL, otherwise allocate and populate the list. */
   List *list = ast_label->gp_list;
   label.length = 0;
   while(list != NULL)
   {
      label.length++;
      list = list->next;
   }
   if(label.length == 0) label.list = NULL;
   else
   {
      label.list = calloc(label.length, sizeof(Atom));
      if(label.list == NULL)
      {
         print_to_log("Error (transformLabel): malloc failure.\n");
         exit(1);
      }
      list = ast_label->gp_list;
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
           atom.node_id = findRightIndexFromId(node_map, ast_atom->node_id);
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
