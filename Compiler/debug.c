#include "debug.h"

/* Invariants on graphs:
 * (1) For 0 <= i <= graph->nodes.size, if graph->nodes.items[i].index is -1,
 *     then i is in the holes array.
 * (2) The number of non-dummy nodes in the node array is equal to 
 *     graph->number_of_nodes.
 * (3) The index of a node with mark M and label class L is in the appropriate
 *     table of the node_classes list. Moreover, the node's index in that
 *     table's items array is node->label_table_index.
 * (4) The number of edge indices >= 0 stored by a node is equal to its outdegree.
 * (5) The number of edge indices >= 0 stored by a node is equal to its indegree.
 * (6) For 0 <= i <= graph->edges.size, if graph->edges.items[i].index is -1,
 *     then i is in the holes array.
 * (7) The number of non-dummy edges in the edge array is equal to 
 *     graph->number_of_edges.
 * (8) The index of an edge with mark M and label class L is in the appropriate
 *     table of the edge_classes list. Moreover, the edge's index in that
 *     table's items array is edge->label_table_index.
 * (9) Source and target consistency: For all edges E, if S is E's source and
 *     T is E's target, then E is in S's outedge list and E is in T's inedge list. 
 */

bool validGraph(Graph *graph)
{
   if(graph == NULL || (graph->number_of_edges == 0 && graph->number_of_nodes == 0)) 
   {
      print_to_console("You asked me to validate the empty graph.\n"
                       "The empty graph trivially satisfies all the invariants.\n"
                       "Have a nice day!\n\n");
      return true;
   }

   bool valid_graph = true, slot_found = false;
   int node_index, node_count = 0, edge_count = 0;
   
   for(node_index = 0; node_index < graph->nodes.size; node_index++)    
   {
      Node *node = getNode(graph, node_index);
      /* Invariant (1) */
      if(node->index == -1) 
      {
         int i;
         for(i = 0; i < graph->nodes.holes.size; i++)
         {
            if(graph->nodes.holes.items[i] == node_index) 
            {
               slot_found = true;
               break;
            }
         }
         if(!slot_found)
         {
            print_to_console("(1) Dummy node at array index %d but the index "
                             "is not in the free node slot list.\n", node_index);
            valid_graph = false;
         }
         slot_found = false;
      }     
      else
      {
         /* Keep a count of the number of nodes in the array. */
         node_count++;
         int n;
         for(n = 0; n < node->out_edges.size + 2; n++)
         {
            Edge *node_edge = getNthOutEdge(graph, node, n);
            /* Keep a count of the number of outedges in the array. */
            if(node_edge != NULL) edge_count++;           
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

         for(n = 0; n < node->in_edges.size + 2; n++)
         {
            Edge *node_edge = getNthInEdge(graph, node, n);
            /* Keep a count of the number of inedges in the array. */
            if(node_edge != NULL) edge_count++;
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
            LabelClass label_class = getLabelClass(node->label);
            LabelClassTable *table = getNodeLabelTable(graph, node->label.mark, label_class);
            if(table == NULL)
            {
               print_to_console("(3) Node %d's label class table (Mark %d, LC %d) "
                                "does not exist.\n", node->index, node->label.mark, label_class);   
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
  
   int edge_index;
   for(edge_index = 0; edge_index < graph->edges.size; edge_index++)    
   {
      Edge *edge = getEdge(graph, edge_index);
      slot_found = false;
      /* Invariant (7) */
      if(edge->index == -1) 
      { 
         int count;
         for(count = 0; count < graph->edges.holes.size; count++)
         {
            if(graph->edges.holes.items[count] == edge_index) 
            {
               slot_found = true;
               break;
            }
         }
         if(!slot_found)
         {
            print_to_console("(6) Dummy edge at array index %d but the index "
                             "is not in free edge slot list.\n", edge_index);
            valid_graph = false;
         }
      }    
      else
      {
         /* Keep a count of the number of edges in the array. */
         edge_count++;
         Node *source = getNode(graph, edge->source); 
         Node *target = getNode(graph, edge->target);

         bool source_found = false;
         if(source->first_out_edge == edge->index || source->second_out_edge == edge->index)
            source_found = true;
         if(!source_found)
         {
            int counter;
            for(counter = 0; counter < source->out_edges.size; counter++)
            {
               if(source->out_edges.items[counter] == edge->index)
               {
                  source_found = true;
                  break;
               }
            }
         }
         /* Invariant (9) */
         if(!source_found)
         {
            print_to_console("(9) Edge %d does not occur in node %d's outedge "
                             "array.\n", edge_index, source->index);   
            valid_graph = false;
         }   

         bool target_found = false;
         if(target->first_in_edge == edge->index || target->second_in_edge == edge->index)
            target_found = true;
         if(!target_found)
         {
            int counter;
            for(counter = 0; counter < target->in_edges.size; counter++)
            {
               if(target->in_edges.items[counter] == edge->index)
               {
                  target_found = true;
                  break;
               }
            }
         }
         if(!target_found)
         {
            print_to_console("(9) Edge %d does not occur in node %d's inedge "
                             "array.\n", edge_index, target->index);   
            valid_graph = false;
         }   

         /* Invariant (8) */
         if(edge->label_table_index > -1)
         {
            LabelClass label_class = getLabelClass(edge->label);
            LabelClassTable *table = getEdgeLabelTable(graph, edge->label.mark, label_class);
            if(table == NULL)
            {
               print_to_console("(3) Edge %d's label class table (Mark %d, LC %d) "
                                "does not exist.\n", edge->index, edge->label.mark, label_class);   
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
    
   if(valid_graph) print_to_console("Graph satisfies all the data invariants!\n");
   printf("\n");
   return valid_graph;
}

void printVerboseGraph(Graph *graph, FILE *file) 
{
    int index;
    PTF("Nodes\n=====\n");
    for(index = 0; index < graph->nodes.size; index++)
    {
       Node *node = getNode(graph, index);
       if(node->index >= 0) printVerboseNode(node, file);
    }   
    PTF("Root Node List: ");
    RootNodes *iterator = graph->root_nodes;
    while(iterator != NULL)
    {
       if(iterator->next == NULL) PTF("%d\n", iterator->index);
       else PTF("%d, ", iterator->index);
       iterator = iterator->next;
    }
    PTF("\n");
    PTF("Edges\n=====\n");
    for(index = 0; index < graph->edges.size; index++)
    {
       Edge *edge = getEdge(graph, index);
       if(edge->index >= 0) printVerboseEdge(edge, file);
    } 
    PTF("\n");
}

void printVerboseNode(Node *node, FILE *file)
{
    PTF("Index: %d", node->index);
    if(node->root) PTF(" (Root)");
    PTF("\n");
    PTF("Label: ");
    printLabel(node->label, file);
    PTF("\n");
    PTF("Outdegree: %d. Indegree: %d\n", node->outdegree, node->indegree);

    PTF("Outedges: ");
    if(node->first_out_edge >= 0) PTF("%d ", node->first_out_edge);
    if(node->second_out_edge >= 0) PTF("%d ", node->second_out_edge);
    int index;
    for(index = 0; index < node->out_edges.size; index++)
    {
       int out_edge = node->out_edges.items[index];
       if(out_edge >= 0) PTF("%d ", out_edge);
    }

    PTF("\nInedges: ");
    if(node->first_in_edge >= 0) PTF("%d ", node->first_in_edge);
    if(node->second_in_edge >= 0) PTF("%d ", node->second_in_edge);
    for(index = 0; index < node->in_edges.size; index++)
    {
       int in_edge = node->in_edges.items[index];
       if(in_edge >= 0) PTF("%d ", in_edge);
    }
    PTF("\n\n");
}

void printVerboseEdge(Edge *edge, FILE *file) 
{
    PTF("Index: %d", edge->index);
    PTF("\n");
    PTF("Label: ");
    printLabel(edge->label, file);
    PTF("\n");
    PTF("Source: %d. Target: %d\n\n", edge->source, edge->target);
}

