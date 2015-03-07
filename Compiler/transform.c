/* ///////////////////////////////////////////////////////////////////////////

  ====================================
  transform.c - Chris Bak (01/12/2014)
  ====================================

/////////////////////////////////////////////////////////////////////////// */

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

   unsigned int is_rooted = 0;

   rule->preserved_nodes = NULL;
   rule->preserved_edges = NULL;
   rule->deleted_nodes = NULL;
   rule->added_nodes = NULL;
   rule->added_edges = NULL;

   if(ast_rule->lhs->nodes == NULL) rule->lhs = NULL;
   else rule->lhs = scanLHS(ast_rule->lhs, ast_rule->interface, &node_map,
                            &edge_map, &(rule->deleted_nodes), &is_rooted);

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
   rule->flags.is_predicate = 0;
   rule->flags.is_rooted = is_rooted;
   return rule;
}

int getNodeSize(GPGraph *graph, int minimum_size)
{
   int nodes = 0;
   List *iterator;
   for(iterator = graph->nodes; iterator != NULL; iterator = iterator->next)
       nodes++;
   if(nodes < minimum_size) return minimum_size;
   if(nodes == 0) return 0;
   /* Return the smallest power of 2 greater than nodes. */
   nodes--;
   nodes |= nodes >> 1;
   nodes |= nodes >> 2;
   nodes |= nodes >> 4;
   nodes |= nodes >> 8;
   nodes |= nodes >> 16;
   return nodes + 1;
}

int getEdgeSize(GPGraph *graph, int minimum_size)
{
   int edges = 0;
   List *iterator;
   for(iterator = graph->edges; iterator != NULL; iterator = iterator->next)
      edges++;
   if(edges < minimum_size) return minimum_size;
   if(edges == 0) return 0;
   /* Return the smallest power of 2 greater than edges. */
   edges--;
   edges |= edges >> 1;
   edges |= edges >> 2;
   edges |= edges >> 4;
   edges |= edges >> 8;
   edges |= edges >> 16;
   return edges + 1;
}

Graph *scanLHS(GPGraph *ast_lhs, List *interface, IndexMap **node_map, 
               IndexMap **edge_map, ItemList **deleted_nodes,
               unsigned int *is_rooted)
{
   int lhs_nodes = getNodeSize(ast_lhs, 0);
   int lhs_edges = getEdgeSize(ast_lhs, 0);
   Graph *lhs = newGraph(lhs_nodes, lhs_edges);

   List *nodes = ast_lhs->nodes;

   while(nodes != NULL)
   {
      GPNode *ast_node = nodes->value.node;
      if(ast_node->root) *is_rooted = 1;
      Label *label = transformLabel(ast_node->label);
      int node_index = addNode(lhs, ast_node->root, label);

      *node_map = addIndexMap(*node_map, ast_node->name, node_index, -1, 
                              NULL, NULL);

      bool node_in_interface = false;
      List *iterator = interface;

      /* If the node is not an interface node, add it to the deleted nodes
       * list and set the deletes_nodes flag. */
      while(iterator != NULL)
      {
         if(!strcmp(iterator->value.node_id, ast_node->name))
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
      GPEdge *ast_edge = edges->value.edge;
 
      /* Use the node map to get the correct pointers for the edge's source
       * and target to pass to newEdge. */
      IndexMap *source_map = findMapFromId(*node_map, ast_edge->source);
      if(source_map == NULL)
      {
         printf("Error (scanLHS): Edge's source %s not found in the node map. "
                "\n", ast_edge->source);
         exit(1);
      }
      Label *label = transformLabel(ast_edge->label);

      if(!strcmp(ast_edge->source, ast_edge->target))
      {
         int edge_index = addEdge(lhs, ast_edge->bidirectional, label, 
                                  source_map->left_index, source_map->left_index);
         *edge_map = addIndexMap(*edge_map, ast_edge->name, edge_index, -1,
                                 ast_edge->source, ast_edge->target);
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
         *edge_map = addIndexMap(*edge_map, ast_edge->name, edge_index, -1,
                                 ast_edge->source, ast_edge->target);
      }
      edges = edges->next;   
   }
   return lhs;
}

Graph *scanRHSNodes(GPGraph *ast_rhs, List *interface, IndexMap **node_map,
                    PreservedItemList **nodes, ItemList **added_nodes)
{
   int rhs_nodes = getNodeSize(ast_rhs, 0);
   int rhs_edges = getEdgeSize(ast_rhs, 0);
   Graph *rhs = newGraph(rhs_nodes, rhs_edges);

   List *ast_nodes = ast_rhs->nodes;

   while(ast_nodes != NULL)
   {
      GPNode *ast_node = ast_nodes->value.node;
      Label *label = transformLabel(ast_node->label);
      int node_index = addNode(rhs, ast_node->root, label);

      IndexMap *map = findMapFromId(*node_map, ast_node->name);
      
      if(map == NULL) 
      {
         /* If the node is not in the map, add a new map for this node with
          * left index -1, and add the node to the added nodes list. */
         *node_map = addIndexMap(*node_map, ast_node->name, -1, node_index,
                                 NULL, NULL);
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
            if(!strcmp(iterator->value.node_id, ast_node->name))
            {
               interface_node = true;
               break;
            }
            else iterator = iterator->next;
         }

         /* TODO: Test label equality here. Pass NULL to addPreservedItem if the
          * labels match. */
         if(interface_node)
            *nodes = addPreservedItem(*nodes, map->left_index, label);
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
      GPEdge *ast_edge = ast_edges->value.edge;
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

      Label *label = transformLabel(ast_edge->label);
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
         if(!strcmp(iterator->value.node_id, source_id))
         {
            interface_source = true;
            if(interface_target) break;
            if(!strcmp(iterator->value.node_id, target_id))
            {
               interface_target = true;
               break;
            }         
         }
         if(!strcmp(iterator->value.node_id, target_id))
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
         if(map == NULL) 
            /* No such map exists, thus the edge is added by the rule. Both
             * source and target come from the LHS because they are both in
             * the interface. */
            /* TODO: Test label equality here. Pass NULL to addPreservedItem if the
             * labels match. */
            added_edges = addNewEdge(added_edges, edge_index,
                                     'l', source_map->left_index, 
                                     'l', target_map->left_index);
         else 
         {
            /* A map has been found, therefore the edge is preserved by the
             * rule. */
            *edges = addPreservedItem(*edges, map->left_index, label);
            /* The map is removed to ensure that a parallel RHS-edge is not
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

Label *transformLabel(GPLabel *ast_label)
{
   if(ast_label->gp_list->list_type == EMPTY_LIST) 
      return makeEmptyList(ast_label->mark);

   /* For now this is the same as makeEmptyList. */
   Label *label = malloc(sizeof(Label));
   if(label == NULL)
   {
      print_to_log("Error: Memory exhausted during label creation.\n");
      exit(1);
   }
   label->mark = ast_label->mark;
   label->list.first = NULL;
   label->list.last = NULL;
   label->list_length = 0;
   label->list_variable = false;
   return label;
}


