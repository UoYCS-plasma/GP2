/* Copyright 2015-2017 Christopher Bak

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
 * - The number of outgoing edges stored by a node is equal to its outdegree.
 * - The number of incoming edges stored by a node is equal to its indegree.
 * - The number of nodes in the node list is equal to graph->number_of_nodes.
 * - The number of edges in the edge list is equal to graph->number_of_edges.
 * - Source and target consistency: For all edges E, if S is E's source and
 *   T is E's target, then E is in S's outedge list and T's inedge list.
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

   NodeList *nlistpos = NULL;
   for(Node *node; (node = yieldNextNode(graph, &nlistpos)) != NULL;
       node_count++)
   {
      int inedge_count = 0, outedge_count = 0;

      EdgeList *elistpos = NULL;
      for(Edge *edge; (edge = yieldNextOutEdge(node, &elistpos)) != NULL;
          outedge_count++)
        ;
      if(node->outdegree != outedge_count)
      {
         fprintf(stderr, "(3) Node %d's outdegree (%d) is not equal to the "
                 "number of edges in its outedges array (%d).\n",
                 node_count, node->outdegree, outedge_count);
         valid_graph = false;
      }

      for(Edge *edge; (edge = yieldNextInEdge(node, &elistpos)) != NULL;
          inedge_count++)
        ;
      /* Invariant (4) */
      if(node->indegree != inedge_count)
      {
         fprintf(stderr, "(4) Node %d's indegree (%d) is not equal to the"
                 "number of edges in its inedges array (%d).\n",
                 node_count, node->indegree, inedge_count);
         valid_graph = false;
      }
   }
   /* Invariant (2) */
   if(node_count != graph->number_of_nodes)
   {
      fprintf(stderr, "(2) graph->number_of_nodes (%d) is not equal to the number of "
              "nodes in the node array (%d).\n", graph->number_of_nodes, node_count);
      valid_graph = false;
   }

   EdgeList *elistpos = NULL;
   for(Edge *edge; (edge = yieldNextEdge(graph, &elistpos)) != NULL;
       edge_count++)
   {
      bool source_found = false;
      EdgeList *olistpos = NULL;
      for(Edge *outedge; (outedge = yieldNextOutEdge(edge->source, &olistpos)) != NULL;)
      {
         if(outedge == edge)
         {
            source_found = true;
            break;
         }
      }
      /* Invariant (7) */
      if(!source_found)
      {
         fprintf(stderr, "(7) Edge %d does not occur in source's outedge "
                 "array.\n", edge_count);
         valid_graph = false;
      }

      bool target_found = false;
      EdgeList *ilistpos = NULL;
      for(Edge *inedge; (inedge = yieldNextInEdge(edge->target, &ilistpos)) != NULL;)
      {
         if(inedge == edge)
         {
            target_found = true;
            break;
         }
      }
      if(!target_found)
      {
         fprintf(stderr, "(7) Edge %d does not occur in target's inedge "
                 "array.\n", edge_count);
         valid_graph = false;
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
    PTF("Nodes\n=====\n");
    NodeList *nlistpos = NULL;
    for(Node *node; (node = yieldNextNode(graph, &nlistpos)) != NULL;)
    {
       printVerboseNode(node, file);
    }
    PTF("Root Node List: ");
    RootNodes *iterator = graph->root_nodes;
    while(iterator != NULL)
    {
       if(iterator->next == NULL) PTF("%p\n", (void *) iterator->node);
       else PTF("%p, ", (void *) iterator->node);
       iterator = iterator->next;
    }
    PTF("\n");
    PTF("Edges\n=====\n");
    EdgeList *elistpos = NULL;
    for(Edge *edge; (edge = yieldNextEdge(graph, &elistpos)) != NULL;)
       printVerboseEdge(edge, file);
    PTF("\n");
}

void printVerboseNode(Node *node, FILE *file)
{
    PTF("ID: %p", (void *) node);
    if(node->root) PTF(" (Root)\n");
    PTF("Label: ");
    printHostLabel(node->label, file);
    PTF("\n");
    PTF("Outdegree: %d. Indegree: %d\n", node->outdegree, node->indegree);

    PTF("Outedges: ");
    EdgeList *elistpos = NULL;
    for(Edge *out_edge; (out_edge = yieldNextOutEdge(node, &elistpos)) != NULL;)
       PTF("%p ", out_edge);

    PTF("\nInedges: ");
    for(Edge *in_edge; (in_edge = yieldNextInEdge(node, &elistpos)) != NULL;)
       PTF("%p ", in_edge);
    PTF("\n\n");
}

void printVerboseEdge(Edge *edge, FILE *file)
{
    PTF("ID: %p", (void *) edge);
    PTF("Label: ");
    printHostLabel(edge->label, file);
    PTF("\n");
    PTF("Source: %p. Target: %p\n\n", (void *) edge->source, (void *) edge->target);
}

