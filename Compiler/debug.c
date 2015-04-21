#include "debug.h"

/* Invariants on graphs:
 * (1) For 0 <= i <= graph->node_index, graph->node_index > 0,
 *     if node_array[i].index is -1, then i is in the free node slot array.
 * (2) The number of non-dummy nodes in the node array is equal to 
 *     graph->number_of_nodes.
 * (3) The index of a node with label class L is in the nodes_by_label table 
 *     entry indexed by L.
 * (4) The number of valid edge indices in the outedges array is equal to 
 *     node.outdegree.
 * (5) The number of valid edge indices in the inedges array is equal to 
 *     node.indegree.
 * (6) For 0 <= i <= graph->edge_index, graph->edge_index > 0,
 *     if edge_array[i].index is -1, then i is in the free edge slot array.
 * (7) The number of non-dummy edges in the edge array is equal to 
 *     graph->number_of_edges.
 * (8) The index of an edge with label class L is in the edges_by_label table 
 *     entry indexed by L.
 * (9) Source and target consistency: For all edges E, if S is E's source and
 *     T is E's target, then E is in S's outedge list and E is in T's inedge list. 
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

   bool valid_graph = true, slot_found = false, item_found = false;
   int graph_index, node_index, edge_index, node_count = 0, edge_count = 0;
   
   for(graph_index = 0; graph_index < graph->node_index; graph_index++)    
   {
      Node *node = getNode(graph, graph_index);
      /* Invariant (1) */
      if(node->index == -1) 
      {
         int count;
         for(count = 0; count < graph->node_pool_size; count++)
         {
            if(graph->free_node_slots[count] == graph_index) 
            {
               slot_found = true;
               break;
            }
         }
         if(!slot_found)
         {
            print_to_console("(1) Dummy node at array index %d but the index "
                             "is not in the free node slot list.\n", 
                             graph_index);
            valid_graph = false;
         }
         slot_found = false;
      }     
      else
      {
         /* Keep a count of the number of nodes in the array. */
         node_count++;

         for(node_index = 0; node_index < node->out_index; node_index++)
         {
            Edge *node_edge = getEdge(graph, getOutEdge(node, node_index));
            /* Keep a count of the number of outedges in the array. */
            if(node_edge->index >= 0) edge_count++;           
         }
         /* Invariant (4) */
         if(node->outdegree != edge_count)
         {
            print_to_console("(4) Node %d's outdegree (%d) is not equal to the "
                             "number of edges in its outedges array (%d).\n",
                             node->index, node->outdegree, edge_count);
            valid_graph = false;
         }
         edge_count = 0;

         for(node_index = 0; node_index < node->in_index; node_index++)
         {
            Edge *node_edge = getEdge(graph, getInEdge(node, node_index));
            /* Keep a count of the number of inedges in the array. */
            if(node_edge->index >= 0) edge_count++;
         }
         /* Invariant (5) */
         if(node->indegree != edge_count)
         {
            print_to_console("(5) Node %d's indegree (%d) is not equal to the "
                             "number of edges in its inedges array (%d).\n",
                             node->index, node->indegree, edge_count);
            valid_graph = false;
         } 
         edge_count = 0;

         /* Invariant (3) */
         LabelClassTable node_list = getNodesByLabel(graph, node->label_class);
         if(node_list.items != NULL)
         {
            for(node_index = 0; node_index < node_list.index; node_index++)
            {
               if(node_list.items[node_index] == node->index)
               {
                  item_found = true;
                  break;
               }
            }
         }
         if(!item_found)
         {
            print_to_console("(3) Node %d does not occcur in the list of "
                             "its label class (%d).\n", 
                             graph_index, node->label_class);   
            valid_graph = false;
         }   
      }
   }
   /* Invariant (2) */
   if(node_count != graph->number_of_nodes)
   {
      print_to_console("(2) graph->number_of_nodes (%d) is not equal to the "
                       "number of nodes in the node array (%d).\n",
                       graph->number_of_nodes, node_count);
      valid_graph = false;
   }   

   for(graph_index = 0; graph_index < graph->edge_index; graph_index++)    
   {
      Edge *edge = getEdge(graph, graph_index);
      slot_found = false;
      /* Invariant (7) */
      if(edge->index == -1) 
      { 
         int count;
         for(count = 0; count < graph->edge_pool_size; count++)
         {
            if(graph->free_edge_slots[count] == graph_index) 
            {
               slot_found = true;
               break;
            }
         }
         if(!slot_found && graph->edge_index > 0)
         {
            print_to_console("(6) Dummy edge at array index %d but the index "
                             "is not in free edge slot list.\n", 
                             graph_index);
            valid_graph = false;
         }
      }    
      else
      {
         /* Keep a count of the number of edges in the array. */
         edge_count++;
         Node *source = getNode(graph, edge->source); 
         Node *target = getNode(graph, edge->target); 
         int counter;
         bool source_found = false, target_found = false;
         /* Search for the edge in the out_edges array of its source and the
          * in_edges array of its target. */
         for(counter = 0; counter < MAX_INCIDENT_EDGES; counter++)
         {
            if(source->out_edges[counter] == edge->index)
            {
               source_found = true;
               if(target_found) break;
            }
            if(target->in_edges[counter] == edge->index)
            {
               target_found = true;
               if(source_found) break;
            }
         }
         /* Check the extra edge arrays if necessary. */
         if(!source_found && source->extra_out_edges != NULL)
         { 
            for(counter = 0; counter < source->out_index - MAX_INCIDENT_EDGES;
                counter++)
            {
               if(source->extra_out_edges[counter] == edge->index)
               {
                  source_found = true;
                  break;
               }
            }
         }
         if(!target_found && target->extra_in_edges != NULL)
         { 
            for(counter = 0; counter < target->in_index - MAX_INCIDENT_EDGES;
                counter++)
            {
               if(target->extra_in_edges[counter] == edge->index)
               {
                  target_found = true;
                  break;
               }
            }
         }

         /* Invariant (9) */
         if(!source_found)
         {
            print_to_console("(9) Edge %d does not occur in node %d's outedge "
                             "array.\n", graph_index, source->index);   
            valid_graph = false;
         }   
         if(!target_found)
         {
            print_to_console("(9) Edge %d does not occur in node %d's inedge "
                             "array.\n", graph_index, target->index);   
            valid_graph = false;
         }   

         /* Invariant (8) */
         LabelClassTable edge_list = getEdgesByLabel(graph, edge->label_class);
         if(edge_list.items != NULL)
         {
            for(edge_index = 0; edge_index < edge_list.index; edge_index++)
            {
               if(edge_list.items[edge_index] == edge->index)
               {
                  item_found = true;
                  break;
               }
            }
         }
         if(!item_found)
         {
            print_to_console("(8) Edge %d does not occcur in the list of "
                             "its label class (%d).\n", 
                             graph_index, edge->label_class);   
            valid_graph = false;
         }   
      }
   }
   /* Invariant (7) */
   if(edge_count != graph->number_of_edges)
   {
      print_to_console("(7) graph->number_of_edges (%d) is not equal to the "
                       "number of edges in the edge array (%d).\n", 
                       graph->number_of_edges, edge_count);
      valid_graph = false;
   }     
    
   if(valid_graph) print_to_console ("Graph satisfies all the data invariants!\n");
   printf("\n");
   return valid_graph;
}


void printVerboseRule(Rule *rule, FILE *file)
{
   fprintf(file, "Rule %s\n\n", rule->name);
   fprintf(file, "LHS\n===\n");
   if(rule->lhs) printVerboseGraph(rule->lhs, file);
   else fprintf(file, "Empty Graph\n\n");

   fprintf(file, "RHS\n===\n");
   if(rule->rhs) printVerboseGraph(rule->rhs, file);
   else fprintf(file, "Empty Graph\n\n");

   PreservedItemList *item = rule->preserved_nodes;
   fprintf(file, "Preserved nodes: ");
   while(item != NULL)
   {
      fprintf(file, "%d", item->left_index);
      if(item->next != NULL) fprintf(file, ", ");
      item = item->next;
   }

   item = rule->preserved_edges;
   fprintf(file, "\nPreserved edges: ");
   while(item != NULL)
   {
      fprintf(file, "%d", item->left_index);
      if(item->next != NULL) fprintf(file, ", ");
      item = item->next;
   }

   ItemList *iterator = rule->added_nodes;
   fprintf(file, "\nAdded nodes: ");
   while(iterator != NULL)
   {
      fprintf(file, "%d ", iterator->index);
      iterator = iterator->next;
   }
   
   iterator = rule->deleted_nodes;
   fprintf(file, "\nDeleted nodes: ");
   while(iterator != NULL)
   {
      fprintf(file, "%d ", iterator->index);
      iterator = iterator->next;
   }

   fprintf(file, "\nAdded edges:\n");

   NewEdgeList *edge = rule->added_edges;
   while(edge != NULL)
   {
      fprintf(file, "Edge %d. Source %c-%d. Target %c-%d.\n",
            edge->edge_index, edge->source_location, edge->source_index,
            edge->target_location, edge->target_index);
      edge = edge->next;
   }
   fprintf(file, "\n");
}


void printVerboseGraph(Graph *graph, FILE *file) 
{
    int index;
    fprintf(file, "Nodes\n=====\n");
    for(index = 0; index < graph->node_index; index++)
    {
       Node *node = getNode(graph, index);
       if(node->index >= 0) printVerboseNode(node, file);
    }    
 
    fprintf(file, "Edges\n=====\n");
    for(index = 0; index < graph->edge_index; index++)
    {
       Edge *edge = getEdge(graph, index);
       if(edge->index >= 0) printVerboseEdge(edge, file);
    } 
    fprintf(file, "\n");

    fprintf(file, "Root Node List\n==============\n");
    RootNodes *iterator = graph->root_nodes;
    while(iterator != NULL)
    {
       if(iterator->next == NULL) fprintf(file, "%d\n", iterator->index);
       else fprintf(file, "%d, ", iterator->index);
       iterator = iterator->next;
    }
}

void printVerboseNode(Node *node, FILE *file)
{
    fprintf(file, "Index: %d", node->index);
    if(node->root) fprintf(file, " (Root)");
    fprintf(file, "\n");
    fprintf(file, "Label Class: %d\n", node->label_class);
    fprintf(file, "Label: ");
    printLabel(*(node->label), file);
    fprintf(file, "\n");
    fprintf(file, "Indegree: %d. Outdegree: %d. Bidegree: %d\n\n",
           node->indegree, node->outdegree, node->bidegree);
}

void printVerboseEdge(Edge *edge, FILE *file) 
{
    fprintf(file, "Index: %d", edge->index);
    if(edge->bidirectional) fprintf(file, " (Bidirectional)");
    fprintf(file, "\n");
    fprintf(file, "Label Class: %d\n", edge->label_class);
    fprintf(file, "Label: ");
    printLabel(*(edge->label), file);
    fprintf(file, "\n");
    fprintf(file, "Source: %d. Target: %d\n\n", edge->source, edge->target);
}

