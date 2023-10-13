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

#include "searchplan.h"

static void traverseNode(Searchplan *searchplan, RuleNode *node, char type, 
                         bool *tagged_nodes, bool *tagged_edges);
static void traverseEdge(Searchplan *searchplan, RuleEdge *node, char type, 
                         bool *tagged_nodes, bool *tagged_edges);

static Searchplan *makeSearchplan(void)
{
   Searchplan *plan = malloc(sizeof(Searchplan));
   if(plan == NULL)
   {
      print_to_log("Error (makeSearchplan): malloc failure.\n");
      exit(1);
   }   
   plan->first = NULL;
   plan->last = NULL;
   return plan;
}

static void appendSearchOp(Searchplan *plan, char type, int index)
{
   SearchOp *new_op = malloc(sizeof(SearchOp));
   if(new_op == NULL)
   {
     print_to_log("Error (makeSearchplan): malloc failure.\n");
     exit(1);
   }   
    if(type == 'e' || type == 's' || type == 't' || type == 'l')
        new_op->is_node = false;
   else new_op->is_node = true;
   new_op->type = type;
   new_op->index = index;

   if(plan->last == NULL)
   {
      new_op->next = NULL;
      plan->first = new_op;
      plan->last = new_op;  
   }
   else
   {
      new_op->next = NULL;
      plan->last->next = new_op;
      plan->last = new_op;
   }
}  

Searchplan *generateSearchplan(RuleGraph *lhs)
{
   Searchplan *searchplan = makeSearchplan();
   bool tagged_nodes[lhs->node_index]; 
   bool tagged_edges[lhs->edge_index];  
   int index;
   for(index = 0; index < lhs->node_index; index++) tagged_nodes[index] = false;
   for(index = 0; index < lhs->edge_index; index++) tagged_edges[index] = false;

   /* Perform a depth-first traversal of the graph from its root nodes. */
   for(index = 0; index < lhs->node_index; index++)
   {
      RuleNode *node = getRuleNode(lhs, index);
      if(node->root && !tagged_nodes[node->index]) 
         traverseNode(searchplan, node, 'r', tagged_nodes, tagged_edges);
   }

   /* Search for undiscovered nodes, namely nodes that are not reachable 
    * from a root node. */
   for(index = 0; index < lhs->node_index; index++)
   {
      if(!tagged_nodes[index]) 
      {
         RuleNode *node = getRuleNode(lhs, index);
         traverseNode(searchplan, node, 'n', tagged_nodes, tagged_edges);
      }
   }
   return searchplan;
}

static void traverseNode(Searchplan *searchplan, RuleNode *node, char type, 
                  bool *tagged_nodes, bool *tagged_edges)
{
   tagged_nodes[node->index] = true;
   appendSearchOp(searchplan, type, node->index);
   /* Search the node's incident edges for an untagged edge. Outedges
    * are arbitrarily examined first. If no such edges exist, the function
    * exits and control passes to the caller. */
   RuleEdges *iterator = node->outedges;
   while(iterator != NULL)
   {
      RuleEdge *edge = iterator->edge;
      if(!tagged_edges[edge->index])
      {
         if(edge->source == edge->target) 
              traverseEdge(searchplan, edge, 'l', tagged_nodes, tagged_edges);
         else traverseEdge(searchplan, edge, 's', tagged_nodes, tagged_edges);
      }
      iterator = iterator->next;
   }
   iterator = node->inedges;
   while(iterator != NULL)
   {
      RuleEdge *edge = iterator->edge;
      if(!tagged_edges[edge->index])
      {
         if(edge->source == edge->target)
              traverseEdge(searchplan, edge, 'l', tagged_nodes, tagged_edges);
         else traverseEdge(searchplan, edge, 't', tagged_nodes, tagged_edges);
      }
      iterator = iterator->next;
   }
}

static void traverseEdge(Searchplan *searchplan, RuleEdge *edge, char type,
                  bool *tagged_nodes, bool *tagged_edges)
{
   tagged_edges[edge->index] = true;
   appendSearchOp(searchplan, type, edge->index);

   /* If the edge is a loop, its incident node has already been examined. 
    * Backtrack by returning control to the caller. */
   if(type == 'l') return;

   /* Continue the graph traversal of any untagged nodes incident to the edge. */
   if(type == 's')
   {
      RuleNode *target = edge->target;
      if(!tagged_nodes[target->index])
      {
         if(edge->bidirectional) 
              traverseNode(searchplan, target, 'b', tagged_nodes, tagged_edges);
         else traverseNode(searchplan, target, 'i', tagged_nodes, tagged_edges);
      }
   }      
   else /* type == 't' */ 
   {
      RuleNode *source = edge->source;
      if(!tagged_nodes[source->index])
      {
         if(edge->bidirectional) 
              traverseNode(searchplan, source, 'b', tagged_nodes, tagged_edges);
         else traverseNode(searchplan, source, 'o', tagged_nodes, tagged_edges);  
      }
   }
}

void printSearchplan(Searchplan *plan)
{ 
   if(plan->first == NULL) printf("Empty searchplan.\n");
   else
   {
      SearchOp *iterator = plan->first;
      while(iterator != NULL)
      {
         if(iterator->is_node) printf("Node\n");
         else printf("Edge\n");
         printf("Type: %c\nIndex: %d\n\n", iterator->type, iterator->index);
         iterator = iterator->next;  
      }
   }
}

void freeSearchplan(Searchplan *plan)
{
   if(plan == NULL) return;
   if(plan->first == NULL) free(plan);
   else
   {
      SearchOp *iterator = plan->first;
      while(iterator != NULL)
      {
         SearchOp *temp = iterator;
         iterator = iterator->next;  
         free(temp);
      }
      free(plan);
   }
}

