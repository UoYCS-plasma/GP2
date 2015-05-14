#include "debug.h"

/* Invariants on graphs:
 * (1) For 0 <= i <= graph->node_index, graph->node_index > 0,
 *     if node_array[i].index is -1, then i is in the free node slot array.
 * (2) The number of non-dummy nodes in the node array is equal to 
 *     graph->number_of_nodes.
 * (3) The index of a node with mark M and label class L is in the (M, L)th
 *     table of the node_classes 2D array. Moreover, the node's index in that
 *     table is node->label_table_index.
 * (4) The number of valid edge indices in the outedges array is equal to 
 *     node.outdegree.
 * (5) The number of valid edge indices in the inedges array is equal to 
 *     node.indegree.
 * (6) For 0 <= i <= graph->edge_index, graph->edge_index > 0,
 *     if edge_array[i].index is -1, then i is in the free edge slot array.
 * (7) The number of non-dummy edges in the edge array is equal to 
 *     graph->number_of_edges.
 * (8) The index of an edge with mark M and label class L is in the (M, L)th
 *     table of the edge_classes 2D array. Moreover, the edge's index in that
 *     table is edge->label_table_index.
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

   bool valid_graph = true, slot_found = false;
   int graph_index, node_index, node_count = 0, edge_count = 0;
   
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
         if(node->label_table_index > -1)
         {
            LabelClassTable *table = getNodesByLabel(graph, node->label);
            LabelClass label_class = getLabelClass(node->label);
            if(table == NULL)
            {
               print_to_console("(3) Node %d's label class table (Mark %d, LC %d) "
                                "is empty!\n", node->index, node->label.mark, label_class);   
               valid_graph = false;
            }
            else
            {
               if(table->items[node->label_table_index] != node->index)
               {
                  print_to_console("(3) Node %d's label table index is inconsistent "
                                   "with its label class table (Mark %d, LC %d).\n",
                                   node->index, node->label.mark, label_class);   
                  valid_graph = false;
               }
            } 
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
         for(counter = 0; counter < source->out_index; counter++)
         {
            if(source->out_edges[counter] == edge->index)
            {
               source_found = true;
               break;
            }
         }
         for(counter = 0; counter < target->in_index; counter++)
         {
            if(target->in_edges[counter] == edge->index)
            {
               target_found = true;
               break;
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
         if(edge->label_table_index > -1)
         {
            LabelClassTable *table = getEdgesByLabel(graph, edge->label);
            LabelClass label_class = getLabelClass(edge->label);
            if(table == NULL)
            {
               print_to_console("(3) Edge %d's label class table (Mark %d, LC %d) "
                                "is empty!\n", edge->index, edge->label.mark, label_class);   
               valid_graph = false;
            }
            else
            {
               if(table->items[edge->label_table_index] != edge->index)
               {
                  print_to_console("(3) Edge %d's label table index is inconsistent "
                                   "with its label class table (Mark %d, LC %d).\n",
                                   edge->index, edge->label.mark, label_class);   
                  valid_graph = false;
               }
            } 
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
   PTF("Rule %s\n\n", rule->name);
   PTF("LHS\n===\n");
   if(rule->lhs) printVerboseGraph(rule->lhs, file);
   else PTF("Empty Graph\n\n");

   PTF("RHS\n===\n");
   if(rule->rhs) printVerboseGraph(rule->rhs, file);
   else PTF("Empty Graph\n\n");

   PreservedItemList *item = rule->preserved_nodes;
   PTF("Preserved nodes: ");
   while(item != NULL)
   {
      PTF("%d", item->left_index);
      if(item->next != NULL) PTF(", ");
      item = item->next;
   }

   item = rule->preserved_edges;
   PTF("\nPreserved edges: ");
   while(item != NULL)
   {
      PTF("%d", item->left_index);
      if(item->next != NULL) PTF(", ");
      item = item->next;
   }

   ItemList *iterator = rule->added_nodes;
   PTF("\nAdded nodes: ");
   while(iterator != NULL)
   {
      PTF("%d ", iterator->index);
      iterator = iterator->next;
   }
   
   iterator = rule->deleted_nodes;
   PTF("\nDeleted nodes: ");
   while(iterator != NULL)
   {
      PTF("%d ", iterator->index);
      iterator = iterator->next;
   }

   PTF("\nAdded edges:\n");

   NewEdgeList *edge = rule->added_edges;
   while(edge != NULL)
   {
      PTF("Edge %d. Source %c-%d. Target %c-%d.\n",
            edge->edge_index, edge->source_location, edge->source_index,
            edge->target_location, edge->target_index);
      edge = edge->next;
   }
   PTF("\n");
}


void printVerboseGraph(Graph *graph, FILE *file) 
{
    int index;
    PTF("Nodes\n=====\n");
    for(index = 0; index < graph->node_index; index++)
    {
       Node *node = getNode(graph, index);
       if(node->index >= 0) printVerboseNode(node, file);
    }    
 
    PTF("Edges\n=====\n");
    for(index = 0; index < graph->edge_index; index++)
    {
       Edge *edge = getEdge(graph, index);
       if(edge->index >= 0) printVerboseEdge(edge, file);
    } 
    PTF("\n");

    PTF("Root Node List\n==============\n");
    RootNodes *iterator = graph->root_nodes;
    while(iterator != NULL)
    {
       if(iterator->next == NULL) PTF("%d\n", iterator->index);
       else PTF("%d, ", iterator->index);
       iterator = iterator->next;
    }
}

void printVerboseNode(Node *node, FILE *file)
{
    PTF("Index: %d", node->index);
    if(node->root) PTF(" (Root)");
    PTF("\n");
    PTF("Label: ");
    printLabel(node->label, file);
    PTF("\n");
    PTF("Indegree: %d. Outdegree: %d. Bidegree: %d\n\n",
        node->indegree, node->outdegree, node->bidegree);
    int index;
    PTF("Inedges: ");
    for(index = 0; index < node->in_pool_size; index++)
    {
       int in_edge = node->in_edges[index];
       if(in_edge >= 0) 
       {
          if(index == node->in_pool_size - 1) PTF("%d\n", in_edge);
          else 
          {
             PTF("%d, ", in_edge);
             /* 20 node indices per line. */
             if(index > 0 && index % 20 == 0) PTF("\n         ");
          }
       }
    }
    PTF("Outedges: ");
    for(index = 0; index < node->out_pool_size; index++)
    {
       int out_edge = node->out_edges[index];
       if(out_edge >= 0) 
       {
          if(index == node->out_pool_size - 1) PTF("%d\n", out_edge);
          else 
          {
             PTF("%d, ", out_edge);
             /* 20 edge indices per line. */
             if(index > 0 && index % 20 == 0) PTF("\n          ");
          }
       }
    }
}

void printVerboseEdge(Edge *edge, FILE *file) 
{
    PTF("Index: %d", edge->index);
    if(edge->bidirectional) PTF(" (Bidirectional)");
    PTF("\n");
    PTF("Label: ");
    printLabel(edge->label, file);
    PTF("\n");
    PTF("Source: %d. Target: %d\n\n", edge->source, edge->target);
}

