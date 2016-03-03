/* Copyright 2015-2016 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>. */

#include "debug.h"

FILE *log_file = NULL;

void openLogFile(string log_file_name)
{
   log_file = fopen(log_file_name, "w");
   if(log_file == NULL)
   { 
      perror("gp2.log");
      exit(1);
   }
}

void closeLogFile(void)
{
   fclose(log_file);
}

FILE *trace_file = NULL;

void openTraceFile(string trace_file_name)
{
   trace_file = fopen(trace_file_name, "w");
   if(trace_file == NULL)
   { 
      perror("gp2.trace");
      exit(1);
   }
}

void closeTraceFile(void)
{
   fclose(trace_file);
}

/* Invariants on graphs:
 * (1) For 0 <= i <= graph->nodes.size, if graph->nodes.items[i].index is -1,
 *     then i is in the holes array.
 * (2) The number of non-dummy nodes in the node array is equal to 
 *     graph->number_of_nodes.
 * (3) The number of edge indices >= 0 stored by a node is equal to its outdegree.
 * (4) The number of edge indices >= 0 stored by a node is equal to its indegree.
 * (5) For 0 <= i <= graph->edges.size, if graph->edges.items[i].index is -1,
 *     then i is in the holes array.
 * (6) The number of non-dummy edges in the edge array is equal to 
 *     graph->number_of_edges.
 * (7) Source and target consistency: For all edges E, if S is E's source and
 *     T is E's target, then E is in S's outedge list and E is in T's inedge list. 
 */

bool validGraph(Graph *graph)
{
   if(graph == NULL || (graph->number_of_edges == 0 && graph->number_of_nodes == 0)) 
   {
      fprintf(stderr, "You asked me to validate the empty graph.\n"
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
            fprintf(stderr, "(1) Dummy node at array index %d but the index is not "
                    "in the holes array.\n", node_index);
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
         /* Invariant (3) */
         if(node->outdegree != edge_count)
         {
            fprintf(stderr, "(3) Node %d's outdegree (%d) is not equal to the "
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
         /* Invariant (4) */
         if(node->indegree != edge_count)
         {
            fprintf(stderr, "(4) Node %d's indegree (%d) is not equal to the number "
                    "of edges in its inedges array (%d).\n", node->index, 
                    node->indegree, edge_count);
            valid_graph = false;
         } 
         edge_count = 0;
      }
   }
   /* Invariant (2) */
   if(node_count != graph->number_of_nodes)
   {
      fprintf(stderr, "(2) graph->number_of_nodes (%d) is not equal to the number of "
              "nodes in the node array (%d).\n", graph->number_of_nodes, node_count);
      valid_graph = false;
   }   
  
   int edge_index;
   for(edge_index = 0; edge_index < graph->edges.size; edge_index++)    
   {
      Edge *edge = getEdge(graph, edge_index);
      slot_found = false;
      /* Invariant (5) */
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
            fprintf(stderr, "(5) Dummy edge at array index %d but the index "
                    "is not in the holes array.\n", edge_index);
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
         /* Invariant (7) */
         if(!source_found)
         {
            fprintf(stderr, "(7) Edge %d does not occur in node %d's outedge "
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
            fprintf(stderr, "(7) Edge %d does not occur in node %d's inedge "
                    "array.\n", edge_index, target->index);   
            valid_graph = false;
         }   


      }
   }
   /* Invariant (6) */
   if(edge_count != graph->number_of_edges)
   {
      fprintf(stderr, "(6) graph->number_of_edges (%d) is not equal to the number of "
              "edges in the edge array (%d).\n", graph->number_of_edges, edge_count);
      valid_graph = false;
   }     
    
   if(valid_graph) fprintf(stderr, "Graph satisfies all the data invariants!\n");
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
    printHostLabel(node->label, file);
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
    printHostLabel(edge->label, file);
    PTF("\n");
    PTF("Source: %d. Target: %d\n\n", edge->source, edge->target);
}

