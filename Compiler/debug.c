#include "debug.h"

/* Invariants on graphs:
 * (1) For 0 <= i <= graph->next_node_index, graph->next_node_index > 0,
 *     node_array[i] is either a pointer to a node or NULL. In the latter case,
 *     i is in the free node slot list.
 * (2) The number of non-NULL pointers in the node array is equal to 
 *     graph->number_of_nodes.
 * (3) A node with label class L is in the nodes_by_label table entry indexed
 *      by the hash key L.
 * (4) The number of non-NULL pointers in the outedges array is equal to 
 *     node->outdegree.
 * (5) The number of non-NULL pointers in the inedges array is equal to 
 *     node->indegree.
 * (6) Edge consistency: For all edges E referred to outside of the graph's
 *     edge array (i.e. in a node's inedge/outedge list), E is the edge pointed
 *     to by the (E.index)th pointer of the edge array.
 * (7) For 0 <= i <= graph->next_edge_index, graph->next_edge_index > 0,
 *     edge_array[i] is either a pointer to a edge or NULL. In the latter case,
 *     i is in the free edge slot list.
 * (8) The number of non-NULL pointers in the edge array is equal to 
 *     graph->number_of_edges.
 * (9) An edge with label class L is in the edges_by_label table entry indexed
 *      by the hash key L.
 * (10) Node consistency: For all nodes N referred to outside of the graph's
 *      node (i.e. the edge's source and target), N is the node pointed to by
 *      the (N.index)th pointer of the node array.
 * (11) For all edges E, if S is E's source and T is E's target, then E is in
 *      S's outedge list and E is in T's inedge list (source/target consistency). 
 */

bool validGraph(Graph *graph)
{
   if(graph == NULL ||
     (graph->number_of_edges == 0 && graph->number_of_nodes == 0)) 
   {
      print_to_console("You asked me to validate the empty graph.\n"
                       "The empty graph trivially satisfies all the invariants.\n"
                       "Have a nice day!\n\n");
      return true;
   }

   bool valid_graph = true, slot_found = false;
   int graph_index, node_index, free_slot, node_count = 0, edge_count = 0;
   StackNode *iterator = NULL;
   
   for(graph_index = 0; graph_index < graph->next_node_index; graph_index++)    
   {
      Node *node = getNode(graph, graph_index);
    
      /* Invariant (1) */
      if(node == NULL) 
      { 
         iterator = graph->free_node_slots->top;

         while(iterator != NULL)
         {
            free_slot = iterator->data->free_slot;
            if(free_slot == graph_index) 
            {
               slot_found = true;
               break;
            }
            iterator = iterator->next;
         }

         if(!slot_found && graph->next_node_index > 0)
         {
            print_to_console("The graph does not satisfy the invariants.\n" 
                             "(1) Pointer at node array index %d is NULL but "
                             "the index is not in the free node slot list.\n", 
                             graph_index);
            valid_graph = false;
         }
         slot_found = false;
      }     
      else
      {
         /* Keep a count of the number of nodes in the array. */
         node_count++;

         for(node_index = 0; node_index < node->next_out_edge_index; node_index++)
         {
            Edge *node_edge = getOutEdge(node, node_index);

            if(node_edge != NULL) 
            {
               /* Keep a count of the number of outedges in the array. */
               edge_count++;

               Edge *graph_edge = getEdge(graph, node_edge->index); 
               /* Invariant (6) */
               if(node_edge != graph_edge) 
               {
                  print_to_console("Graph does not satisfy the invariants.\n"
                                   "(6) Node %d's outedge %d is inconsistent "
                                   "with the graph's edge table.\n",
                                   graph_index, node_edge->index);   
                  valid_graph = false;
               }
            }
         }
         /* Invariant (4) */
         if(node->outdegree != edge_count)
         {
            print_to_console("Graph does not satisfy the invariants.\n"
                             "(4) Node %d's outdegree %d is not equal to the "
                             "number of edges %d in its outedges array.\n",
                             node->index, node->outdegree, edge_count);
            valid_graph = false;
         }
         
         edge_count = 0;

         for(node_index = 0; node_index < node->next_in_edge_index; node_index++)
         {
            Edge *node_edge = getInEdge(node, node_index);

            if(node_edge != NULL) 
            {
               /* Keep a count of the number of inedges in the array. */
               edge_count++;

               Edge *graph_edge = getEdge(graph, node_edge->index); 
               /* Invariant (6) */
               if(node_edge != graph_edge) 
               {
                  print_to_console("The graph does not satisfy the invariants.\n"
                                   "(6) Node %d's outedge %d is inconsistent with "
                                   "the graph's edge table.\n", 
                                   graph_index, node_edge->index);   
                  valid_graph = false;
               }
            }
         }
         /* Invariant (5) */
         if(node->indegree != edge_count)
         {
            print_to_console("The graph does not satisfy the invariants.\n"
                             "(5) Node %d's indegree %d is not equal to the "
                             "number of edges %d in its inedges array.\n",
                             node->index, node->indegree, edge_count);
            valid_graph = false;
         } 

         edge_count = 0;

         /* Invariant (3) */
         GSList *node_list = getNodesByLabel(graph, node->label_class);

         if(g_slist_find(node_list, node) == NULL)
         {
            print_to_console("The graph does not satisfy the invariants.\n"
                             "(3) Node %d does not occcur in the hash list of "
                             "its label class %d.\n", 
                             graph_index, node->label_class);   
            valid_graph = false;
         }   
      }
   }
   /* Invariant (2) */
   if(node_count != graph->number_of_nodes)
   {
      print_to_console("The graph does not satisfy the invariants.\n"
                       "(2) The number of nodes %d is not equal to the "
                       "number of nodes in its node array %d.\n",
                       graph->number_of_nodes, node_count);
      valid_graph = false;
   }   

   for(graph_index = 0; graph_index < graph->next_edge_index; graph_index++)    
   {
      Edge *edge = getEdge(graph, graph_index);

      slot_found = false;

      /* Invariant (7) */
      if(edge == NULL) 
      { 
         StackNode *iterator = graph->free_edge_slots->top;

         while(iterator != NULL)
         {
            free_slot = iterator->data->free_slot;
            if(free_slot == graph_index) 
            {
               slot_found = true;
               break;
            }
            iterator = iterator->next;
         }

         if(!slot_found && graph->next_edge_index > 0)
         {
            print_to_console("The graph does not satisfy the invariants.\n"
                             "(7) Pointer at edge array index %d is NULL but "
                             "the index is not in free edge slot list.\n", 
                             graph_index);
            valid_graph = false;
         }
      }    
      else
      {
         /* Keep a count of the number of edges in the array. */
         edge_count++;

         Node *source = getSource(edge); 
         Node *node = getNode(graph, source->index);
 
         /* Invariant (10) */
         if(source != node) 
         {
            print_to_console("The graph does not satisfy the invariants.\n"
                             "(10) Edge %d's source %d is inconsistent with "
                             "the graph's node array.\n", graph_index, 
                             source->index);   
            valid_graph = false;
         }   
   
         int counter;
         bool edge_found = false;

         /* Search for edge in the out_edges array of its source.  */
         for(counter = 0; counter < source->next_out_edge_index; counter++)
         {
            Edge *out_edge = getOutEdge(source, counter);
            if(out_edge == edge)
            {
               edge_found = true;
               break;
            }
         }
 
         /* Invariant (11) */
         if(!edge_found)
         {
            print_to_console("The graph does not satisfy the invariants.\n"
                             "(11) Edge %d does not occur in node %d's outedge "
                             "array.\n", graph_index, source->index);   
            valid_graph = false;
         }   


         Node *target = getTarget(edge); 
         node = getNode(graph, target->index);

         /* Invariant (10) */
         if(target != node) 
         {
            print_to_console("The graph does not satisfy the invariants.\n"
                             "(10) Edge %d's target %d is inconsistent with "
                             "the graph's node array.\n", graph_index, 
                             target->index);   
            valid_graph = false;
         }   

         edge_found = false;

         /* Search for edge in the in_edges array of its source.  */
         for(counter = 0; counter < target->next_in_edge_index; counter++)
         {
            Edge *in_edge = getInEdge(target, counter);
            if(in_edge == edge)
            {
               edge_found = true;
               break;
            }
         }
 
         /* Invariant (11) */
         if(!edge_found)
         {
            print_to_console("The graph does not satisfy the invariants.\n"
                             "(11) Edge %d does not occur in node %d's inedge "
                             "array.\n", graph_index, target->index);   
            valid_graph = false;
         }   
      
   
         /* Invariant (9) */
         GSList *edge_list = getEdgesByLabel(graph, edge->label_class);

         if(g_slist_find(edge_list, edge) == NULL)
         {
            print_to_console("The graph does not satisfy the invariants.\n"
                             "(9) Edge %d does not occcur in the hash list of "
                             "its label class %d.\n", 
                             graph_index, edge->label_class);   
            valid_graph = false;
         }  
      }
   }
   /* Invariant (8) */
   if(edge_count != graph->number_of_edges)
   {
      print_to_console("The graph does not satisfy the invariants.\n"
                       "(8) The number edges %d is not equal to the number of "
                       "edges in its edge array %d.\n", 
                       graph->number_of_edges, edge_count);
      valid_graph = false;
   }     
    
   if(valid_graph) print_to_console ("Graph satisfies all the data invariants!\n");
   printf("\n");
   return valid_graph;
}


void printVerboseRule(Rule *rule)
{
   printf("Rule %s\n\n", rule->name);
   printf("LHS\n===\n");
   if(rule->lhs) printVerboseGraph(rule->lhs);
   else printf("Empty Graph\n\n");

   printf("RHS\n===\n");
   if(rule->rhs) printVerboseGraph(rule->rhs);
   else printf("Empty Graph\n\n");

   PreservedItemList *item = rule->preserved_nodes;
   printf("Preserved nodes: ");
   while(item != NULL)
   {
      printf("(%d, %d, %d) ", item->left_index, item->right_index, 
            item->label_change);
      item = item->next;
   }

   item = rule->preserved_edges;
   printf("\nPreserved edges: ");
   while(item != NULL)
   {
      printf("(%d, %d, %d) ", item->left_index, item->right_index, 
            item->label_change);
      item = item->next;
   }

   ItemList *iterator = rule->added_nodes;
   printf("\nAdded nodes: ");
   while(iterator != NULL)
   {
      printf("%d ", iterator->index);
      iterator = iterator->next;
   }
   
   iterator = rule->deleted_nodes;
   printf("\nDeleted nodes: ");
   while(iterator != NULL)
   {
      printf("%d ", iterator->index);
      iterator = iterator->next;
   }

   printf("\nAdded edges:\n");

   NewEdgeList *edge = rule->added_edges;
   while(edge != NULL)
   {
      printf("Edge %d. Source %c-%d. Target %c-%d.\n",
            edge->edge_index, edge->source_location, edge->source_index,
            edge->target_location, edge->target_index);
      edge = edge->next;
   }

   if(rule->flags.is_predicate) printf("Rule is a predicate.\n");
   if(rule->flags.is_rooted) printf("Rule is rooted.\n");
   
   printf("\n");
}


void printVerboseGraph(Graph *graph) 
{
    int index;
    printf("Nodes\n=====\n");
    for(index = 0; index < graph->next_node_index; index++)
       if(graph->nodes[index]) printVerboseNode(graph->nodes[index]);
 
    printf("Edges\n=====\n");
    for(index = 0; index < graph->next_edge_index; index++)
       if(graph->edges[index]) printVerboseEdge(graph->edges[index]);
    printf("\n");

    printf("Root Node List\n==============\n");
    GSList *iterator = graph->root_nodes;
    while(iterator != NULL)
    {
       Node *node = (Node *)iterator->data;
       printVerboseNode(node); 
       iterator = iterator->next;
    }
    printf("\n");
}

void printVerboseNode(Node *node)
{
    printf("Index: %d", node->index);
    if(node->root) printf(" (Root)");
    printf("\n");
    printf("Label Class: %d\n", node->label_class);
    printf("Label: ");
    if(node->label->list) 
    {
       printGP2List(node->label->list);
       printf("\n");
    }
    else printf("empty\n");
    printMark(node->label->mark, true);
    printf("Indegree: %d. Outdegree: %d\n\n", node->indegree, node->outdegree);
}

void printVerboseEdge(Edge *edge) 
{
    printf("Index: %d", edge->index);
    if(edge->bidirectional) printf(" (Bidirectional)");
    printf("\n");
    printf("Label Class: %d\n", edge->label_class);
    printf("Label: ");
    if(edge->label->list) 
    {
       printGP2List(edge->label->list);
       printf("\n");
    }
    else printf("empty\n");
    printMark(edge->label->mark, true);
    printf("\n");
    printf("Source: %d. Target: %d\n\n", edge->source->index, edge->target->index);
}

