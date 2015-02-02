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

   if(ast_rule->lhs->nodes == NULL && ast_rule->lhs->edges == NULL) rule->lhs = NULL;
   else rule->lhs = scanLHS(ast_rule->lhs, ast_rule->interface, &node_map,
                            &edge_map, &(rule->deleted_nodes), &is_rooted);

   if(ast_rule->rhs->nodes == NULL && ast_rule->rhs->edges == NULL) rule->rhs = NULL;
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

Graph *scanLHS(GPGraph *ast_lhs, List *interface, IndexMap **node_map, 
               IndexMap **edge_map, ItemList **deleted_nodes,
               unsigned int *is_rooted)
{
   Graph *lhs = newGraph();

   List *nodes = ast_lhs->nodes;

   while(nodes != NULL)
   {
      GPNode *ast_node = nodes->value.node;
      if(ast_node->root) *is_rooted = 1;
      Label *label = transformLabel(ast_node->label);
      Node *node = newNode(ast_node->root, label);
      addNode(lhs, node);

      *node_map = addIndexMap(*node_map, ast_node->name, node->index, -1, 
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
         *deleted_nodes = addItem(*deleted_nodes, node->index);
      
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
      Node *source = getNode(lhs, source_map->left_index);
      Label *label = transformLabel(ast_edge->label);

      if(!strcmp(ast_edge->source, ast_edge->target))
      {
         Edge *edge = newEdge(ast_edge->bidirectional, label, source, source);
         addEdge(lhs, edge);
         *edge_map = addIndexMap(*edge_map, ast_edge->name, edge->index, -1,
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
         Node *target = getNode(lhs, target_map->left_index);

         Edge *edge = newEdge(ast_edge->bidirectional, label, source, target);
         addEdge(lhs, edge);
         *edge_map = addIndexMap(*edge_map, ast_edge->name, edge->index, -1,
                                 ast_edge->source, ast_edge->target);
      }
      edges = edges->next;   
   }
   return lhs;
}

Graph *scanRHSNodes(GPGraph *ast_rhs, List *interface, IndexMap **node_map,
                    PreservedItemList **nodes, ItemList **added_nodes)
{
   Graph *rhs = newGraph();

   List *ast_nodes = ast_rhs->nodes;

   while(ast_nodes != NULL)
   {
      GPNode *ast_node = ast_nodes->value.node;
      Label *label = transformLabel(ast_node->label);
      Node *node = newNode(ast_node->root, label);
      addNode(rhs, node);

      IndexMap *map = findMapFromId(*node_map, ast_node->name);
      
      if(map == NULL) 
      {
         /* If the node is not in the map, add a new map for this node with
          * left index -1, and add the node to the added nodes list. */
         *node_map = addIndexMap(*node_map, ast_node->name, -1, node->index,
                                 NULL, NULL);
         *added_nodes = addItem(*added_nodes, node->index);
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

         if(interface_node)
            *nodes = addPreservedItem(*nodes, false, map->left_index, 
                                      node->index);
         else *added_nodes = addItem(*added_nodes, node->index);
        
         map->right_index = node->index;
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
      Label *label = transformLabel(ast_edge->label);

      IndexMap *target_map = findMapFromId(node_map, target_id);
      if(target_map == NULL)
      {
         printf("Error (scanRHS): Edge's target %s not found in the node "
                  "map. \n", target_id);
         exit(1);
      }
      Node *target = getNode(rhs, target_map->right_index);

      Edge *edge = newEdge(ast_edge->bidirectional, label, source, target);
      addEdge(rhs, edge);

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
            added_edges = addNewEdge(added_edges, edge->index,
                                     'l', source_map->left_index, 
                                     'l', target_map->left_index);
         else 
         {
            /* A map has been found, therefore the edge is preserved by the
             * rule. */
            *edges = addPreservedItem(*edges, false, map->left_index, 
                                      edge->index);
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
            added_edges = addNewEdge(added_edges, edge->index, 
                                    'l', source_map->left_index,
                                    'r', target->index);
         else
         {
            if(interface_target)
            /* The target node is preserved and the source is created. */
               added_edges = addNewEdge(added_edges, edge->index, 
                                       'r', source->index,
                                       'l', target_map->left_index);
            else 
               /* Both the source and the target are created. */
               added_edges = addNewEdge(added_edges, edge->index, 'r', 
                                        source->index, 'r', target->index);
         }
      }
      ast_edges = ast_edges->next;   
   }
   return added_edges;
}

Label *transformLabel(GPLabel *label)
{
   return NULL;
}


