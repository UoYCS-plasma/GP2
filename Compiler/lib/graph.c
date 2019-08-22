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

#include "graph.h"

Node dummy_node = {false, {NONE, 0, NULL}, 0, 0,
                   NULL, NULL, false, false, false};
Edge dummy_edge = {{NONE, 0, NULL}, NULL, NULL, false, false, false};
//Node dummy_node = {-1, false, {NONE, 0, NULL}, 0, 0, -1, -1, -1, -1, 
//                   {0, 0, NULL}, {0, 0, NULL}, false};
//Edge dummy_edge = {-1, {NONE, 0, NULL}, -1, -1, false};

IntArray makeIntArray(int initial_capacity)
{
   IntArray array;
   array.capacity = initial_capacity;
   array.size = 0;
   if(initial_capacity > 0)
   {
      array.items = calloc(initial_capacity, sizeof(int));
      if(array.items == NULL)
      {
         print_to_log("Error (makeIntArray): malloc failure.\n");
         exit(1);
      }
      int i;
      for(i = 0; i < initial_capacity; i++) array.items[i] = -1;
   }
   else array.items = NULL;
   return array;
}

static void growIntArray(IntArray *array)
{
   int old_capacity = array->capacity;
   /* Node's incident edge arrays have initial capacity of 0. On the first
    * allocation, they are allocated space for 4 integers. In all other cases,
    * the old capacity is doubled. */
   array->capacity = old_capacity == 0 ? 4 : 2*old_capacity;
   array->items = realloc(array->items, array->capacity * sizeof(int));
   if(array->items == NULL)
   {
      print_to_log("Error (doubleCapacity): malloc failure.\n");
      exit(1);
   }
   int i;
   for(i = old_capacity; i < array->capacity; i++) array->items[i] = -1;
}

void addToIntArray(IntArray *array, int item)
{
   if(array->size >= array->capacity) growIntArray(array);
   array->items[array->size++] = item;
}

void removeFromIntArray(IntArray *array, int index)
{
   int i;
   for(i = 0; i < array->size; i++)
   {
      if(array->items[i] == index) 
      {
         array->items[i] = -1;
         /* If the index of the removed item directly precedes the size of the
          * array, decrement the size until it refers to the array element
          * one place to the right of the right-most -1. */
         if(i == array->size - 1) 
         {
            array->size--;
            while(array->size > 0)
            {
               if(array->items[array->size - 1] == -1) array->size--;
               else break;
            }
         }
         break;
      }
   }
}
   
//static NodeArray makeNodeArray(int initial_capacity)
//{
//   NodeArray array;
//   array.capacity = initial_capacity;
//   array.size = 0;
//   array.items = calloc(initial_capacity, sizeof(Node));
//   if(array.items == NULL)
//   {
//      print_to_log("Error (makeNodeArray): malloc failure.\n");
//      exit(1);
//   }
//   array.holes = makeIntArray(16);
//   return array;
//}
//
//static void doubleNodeArray(NodeArray *array)
//{
//   array->capacity *= 2;
//   array->items = realloc(array->items, array->capacity * sizeof(Node));
//   if(array->items == NULL)
//   {
//      print_to_log("Error (doubleCapacity): malloc failure.\n");
//      exit(1);
//   }
//}
//
//static int addToNodeArray(NodeArray *array, Node node)
//{
//   /* If the holes array is empty, the node's index is the current size
//    * of the node array. */
//   if(array->holes.size == 0)
//   {
//      node.index = array->size;
//      if(array->size >= array->capacity) doubleNodeArray(array);
//      array->items[array->size++] = node;
//   }
//   /* If the holes array is non-empty, the node is placed in the hole marked by 
//    * the rightmost element of the holes array. */
//   else 
//   {
//      array->holes.size--;
//      assert(array->holes.items[array->holes.size] >= 0);
//      node.index = array->holes.items[array->holes.size];
//      array->items[node.index] = node;
//      array->holes.items[array->holes.size] = -1;
//   }
//   return node.index;
//}
//
//static void removeFromNodeArray(NodeArray *array, int index)
//{
//   array->items[index] = dummy_node;
//   /* If the index is the last index in the array, no hole is created. */
//   if(index == array->size - 1) array->size--;
//   else addToIntArray(&(array->holes), index);
//}
//   
//static EdgeArray makeEdgeArray(int initial_capacity)
//{
//   EdgeArray array;
//   array.capacity = initial_capacity;
//   array.size = 0;
//   array.items = calloc(initial_capacity, sizeof(Edge));
//   if(array.items == NULL)
//   {
//      print_to_log("Error (makeEdgeArray): malloc failure.\n");
//      exit(1);
//   }
//   array.holes = makeIntArray(16);
//   return array;
//}
//
//
//static void doubleEdgeArray(EdgeArray *array)
//{
//   array->capacity *= 2;
//   array->items = realloc(array->items, array->capacity * sizeof(Edge));
//   if(array->items == NULL)
//   {
//      print_to_log("Error (doubleCapacity): malloc failure.\n");
//      exit(1);
//   }
//}
//
//static int addToEdgeArray(EdgeArray *array, Edge edge)
//{
//   /* If the holes array is empty, the edge's index is the current size
//    * of the edge array. */
//   if(array->holes.size == 0)
//   {
//      /* There are no holes in the node array, so the node's index is the current
//       * size of the node array. */
//      edge.index  = array->size;
//      if(array->size >= array->capacity) doubleEdgeArray(array);
//      array->items[array->size++] = edge;
//   }
//   /* If the holes array is non-empty, the edge is placed in the hole marked by 
//    * the rightmost element of the holes array. */
//   else 
//   {
//      array->holes.size--;
//      assert(array->holes.items[array->holes.size] >= 0);
//      edge.index = array->holes.items[array->holes.size];
//      array->items[edge.index] = edge;
//      array->holes.items[array->holes.size] = -1;
//   }
//   return edge.index;
//}
//
//static void removeFromEdgeArray(EdgeArray *array, int index)
//{
//   array->items[index] = dummy_edge;
//   /* If the index is the last index in the array, no hole is created. */
//   if(index == array->size - 1) array->size--;
//   else addToIntArray(&(array->holes), index);
//}


/* ===============
 * Graph Functions
 * =============== */
Graph *newGraph() 
{
   Graph *graph = malloc(sizeof(Graph));
   if(graph == NULL) 
   {
      print_to_log("Error (newGraph): malloc failure.\n");
      exit(1);
   }
   graph->number_of_nodes = 0;
   graph->number_of_edges = 0;
   graph->root_nodes = NULL;
   return graph;
}

Node *addNode(Graph *graph, bool root, HostLabel label)
{
   NodeList *nlist = malloc(sizeof(NodeList));
   if(nlist == NULL)
   {
      print_to_log("Error (addNode): malloc failure.\n");
      exit(1);
   }
   Node *node = malloc(sizeof(Node));
   if(node == NULL)
   {
      print_to_log("Error (addNode): malloc failure.\n");
      exit(1);
   }
   node->root = root;
   node->label = label;
   node->out_edges = NULL;
   node->in_edges = NULL;
   node->outdegree = 0;
   node->indegree = 0;
   node->matched = false;
   node->deleted = false;
   node->in_stack = false;

   nlist->node = node;
   if (graph->nodes != NULL)
     graph->nodes->prev = nlist;
   nlist->next = graph->nodes;
   graph->nodes = nlist;

   if(root) addRootNode(graph, node);
   graph->number_of_nodes++;
   return node;
}

void addRootNode(Graph *graph, Node *node)
{
   RootNodes *root_node = malloc(sizeof(RootNodes));
   if(root_node == NULL)
   {
      print_to_log("Error (addRootNode): malloc failure.\n");
      exit(1);
   }
   root_node->node = node;
   root_node->next = graph->root_nodes;
   graph->root_nodes = root_node;
}

Edge *addEdge(Graph *graph, HostLabel label, Node *source, Node *target)
{
   EdgeList *elist = malloc(sizeof(EdgeList));
   if(elist == NULL)
   {
      print_to_log("Error (addEdge): malloc failure.\n");
      exit(1);
   }
   Edge *edge = malloc(sizeof(Edge));
   if(edge == NULL)
   {
      print_to_log("Error (addEdge): malloc failure.\n");
      exit(1);
   }
   edge->label = label;
   edge->source = source;
   edge->target = target;
   edge->matched = false;
   edge->deleted = false;
   edge->in_stack = false;

   elist->edge = edge;
   if (graph->edges != NULL)
     graph->edges->prev = elist;
   elist->next = graph->edges;
   graph->edges = elist;

   // add to source edgelist
   EdgeList *srclist = malloc(sizeof(EdgeList));
   if(srclist == NULL)
   {
      print_to_log("Error (addEdge): malloc failure.\n");
      exit(1);
   }
   srclist->edge = edge;
   if (source->out_edges != NULL)
     source->out_edges->prev = srclist;
   srclist->next = source->out_edges;
   source->out_edges = srclist;
   source->outdegree++;

   EdgeList *trglist = malloc(sizeof(EdgeList));
   if(trglist == NULL)
   {
      print_to_log("Error (addEdge): malloc failure.\n");
      exit(1);
   }
   trglist->edge = edge;
   if (target->out_edges != NULL)
     target->out_edges->prev = trglist;
   trglist->next = target->out_edges;
   target->out_edges = trglist;
   target->indegree++;

   graph->number_of_edges++;
   return edge;
}

void removeNode(Graph *graph, Node *node)
{
   node->deleted = true;
   if(node->root) removeRootNode(graph, node);
   graph->number_of_nodes--;
}

void removeRootNode(Graph *graph, Node *node)
{
   RootNodes *current = graph->root_nodes, *previous = NULL;
   while(current != NULL)
   {
      if(current->node == node)
      {
         if(previous == NULL) graph->root_nodes = current->next;
         else previous->next = current->next;
         free(current);
         break;
      }
      previous = current;
      current = current->next;
   }
}

void removeEdge(Graph *graph, Edge *edge)
{
   edge->deleted = true;
   edge->source->outdegree--;
   edge->target->indegree--;

   graph->number_of_edges--;
}

void relabelNode(Node *node, HostLabel new_label)
{
   removeHostList(node->label.list);
   node->label = new_label;
}

void changeNodeMark(Node *node, MarkType new_mark)
{
   node->label.mark = new_mark;
}

void changeRoot(Graph *graph, Node *node)
{
   if(node->root) removeRootNode(graph, node);
   else addRootNode(graph, node);
   node->root = !root;
}

void resetMatchedNodeFlag(Node *node)
{
   node->matched = false;
}

void relabelEdge(Edge *edge, HostLabel new_label)
{
   removeHostList(edge->label.list);
   edge->label = new_label;
}

void changeEdgeMark(Edge *edge, MarkType new_mark)
{
   edge->label.mark = new_mark;
}

void resetMatchedEdgeFlag(Edge *edge)
{
   edge->matched = false; 
}

/* ========================
 * Graph Querying Functions 
 * ======================== */

Node *yieldNextNode(Graph *graph, NodeList **current)
{
   if(*current == NULL) *current = graph->nodes;

   bool deleted_node = true;

   while(deleted_node) {
     if(*current == NULL) return NULL;

     Node *node = (*current)->node;

     deleted_node = node->deleted;
     if(node->deleted)
     {
       if((*current)->prev != NULL)
         (*current)->prev->next = (*current)->next;
       else
         graph->nodes = (*current)->next;
       if((*current)->next != NULL)
         (*current)->next->prev = (*current)->prev;
       *current = (*current)->next;
       free((*current)->prev);
       // If the node is referenced in the stack, just forget about it.
       if(!node->in_stack)
       {
         removeHostList(node->label.list);
         // free out_edges and in_edges
         // not possible to have a dangling edge pointer; when garbage collected, edges clean these out of source/target nodes
         for(EdgeList *curr = node->out_edges; curr != NULL; curr = curr->next)
         {
           curr->edge->in_srclst = false;
           if(curr->prev != NULL) free(curr->prev);
         }
         for(EdgeList *curr = node->in_edges; curr != NULL; curr = curr->next)
         {
           curr->edge->in_trglst = false;
           if(curr->prev != NULL) free(curr->prev);
         }
         free(node);
       }
     }
   }
   return (*current)->node;
}

Edge *yieldNextOutEdge(Node *node, Edgelist **current)
{
   if(*current == NULL) *current = node->out_edges;

   bool deleted_edge = true;

   while(deleted_edge) {
     if(*current == NULL) return NULL;

     Edge *edge = (*current)->edge;

     deleted_edge = edge->deleted;
     if(edge->deleted)
     {
       if((*current)->prev != NULL)
         (*current)->prev->next = (*current)->next;
       else
         node->out_edges = (*current)->next;
       if((*current)->next != NULL)
         (*current)->next->prev = (*current)->prev;
       *current = (*current)->next;
       free((*current)->prev);
       edge->in_srclist = false;
       // Don't try and delete edge here.
     }
   }
   return (*current)->edge;
}

Edge *yieldNextInEdge(Node *node, Edgelist **current)
{
   if(*current == NULL) *current = node->in_edges;

   bool deleted_edge = true;

   while(deleted_edge) {
     if(*current == NULL) return NULL;

     Edge *edge = (*current)->edge;

     deleted_edge = edge->deleted;
     if(edge->deleted)
     {
       if((*current)->prev != NULL)
         (*current)->prev->next = (*current)->next;
       else
         node->in_edges = (*current)->next;
       if((*current)->next != NULL)
         (*current)->next->prev = (*current)->prev;
       *current = (*current)->next;
       free((*current)->prev);
       edge->in_trglst = false;
       // Don't try and delete edge here.
     }
   }
   return (*current)->edge;
}

Edge *yieldNextEdge(Graph *graph, Edgelist **current)
{
   if(*current == NULL) *current = graph->edges;

   bool deleted_edge = true;

   while(deleted_edge) {
     if(*current == NULL) return NULL;

     Edge *edge = (*current)->edge;

     deleted_edge = edge->deleted;
     if(edge->deleted)
     {
       if((*current)->prev != NULL)
         (*current)->prev->next = (*current)->next;
       else
         graph->edges = (*current)->next;
       if((*current)->next != NULL)
         (*current)->next->prev = (*current)->prev;
       *current = (*current)->next;
       free((*current)->prev);
       // If edge is in stack, just forget about it.
       if(!edge->in_stack)
       {
         // Clean out references in src/trg to edge by iterating through them.
         // (If source/target garbage collected, in_srclst/trglst = false.)
         if(edge->in_srclst)
           for(EdgeList *elistpos = NULL, Edge *e;
               (e = yieldNextOutEdge(edge->source, &elistpos)) != NULL;)
             ;
         if(edge->in_trglst)
           for(EdgeList *elistpos = NULL, Edge *e;
               (e = yieldNextInEdge(edge->target, &elistpos)) != NULL;)
             ;
         removeHostList(edge->label.list);
         free(edge);
       }
     }
   }
   return (*current)->edge;
}

RootNodes *getRootNodeList(Graph *graph)
{
   return graph->root_nodes;
}

Node *getSource(Graph *graph, Edge *edge) 
{
   assert(!edge->source->deleted);
   return edge->source;
}

Node *getTarget(Graph *graph, Edge *edge) 
{
   assert(!edge->target->deleted);
   return edge->target;
}

HostLabel getNodeLabel(Node *node) 
{
   return node->label;
}

HostLabel getEdgeLabel(Edge *edge) 
{
   return edge->label;
}

int getIndegree(Node *node) 
{
   return node->indegree;
}

int getOutdegree(Node *node) 
{
   return node->outdegree;
}

void printGraph(Graph *graph, FILE *file) 
{
   /* The node and edge counts are used in the IDs of the printed graph. The item's 
    * index in the graph is not suitable for this purpose because there may be holes
    * in the graph's node array. The counts are also used to control the number of
    * nodes and edges printed per line. */
   int node_count = 0, edge_count = 0;
   if(graph == NULL || graph->number_of_nodes == 0) 
   {
      PTF("[ | ]\n\n");
      return;
   }
   PTF("[ ");
   /* Maps a node's graph-index to the ID it is printed with (node_count). */
   Node *output_indices[graph->nodes.size];
   for(NodeList *nlistpos = NULL, Node *node;
       (node = yieldNextNode(graph, &nlistpos)) != NULL;)
   {
      /* Five nodes per line */
      if(node_count != 0 && node_count % 5 == 0) PTF("\n  ");
      output_indices[node_count] = node;
      if(node->root) PTF("(%d(R), ", node_count++);
      else PTF("(%d, ", node_count++);
      printHostLabel(node->label, file);
      PTF(") ");
   }
   if(graph->number_of_edges == 0)
   {
      PTF("| ]\n\n");
      return;
   }
   PTF("|\n  ");
   for(EdgeList *elistpos = NULL, Edge *edge;
       (edge = yieldNextEdge(graph, &elistpos)) != NULL;)
   {
      /* Three edges per line */
      if(edge_count != 0 && edge_count % 3 == 0) PTF("\n  ");
      PTF("(%d, ", edge_count++);

      int src_index, trg_index;
      for(int i = 0; i < node_count; i++)
      {
        if(edge->source == output_indices[i]) src_index = i;
        if(edge->target == output_indices[i]) trg_index = i;
      }
      PTF("%d, %d, ", src_index, trg_index);
      printHostLabel(edge->label, file);
      PTF(") ");
   }
   PTF("]\n\n");
}

void freeGraph(Graph *graph) 
{
   if(graph == NULL) return;

   for(EdgeList *elistpos = NULL, Edge *edge;
       (edge = yieldNextEdge(graph, &elistpos)) != NULL;)
     removeEdge(edge);

   for(NodeList *nlistpos = NULL, Node *node;
       (node = yieldNextNode(graph, &nlistpos)) != NULL;)
     removeNode(node);

   if(graph->root_nodes != NULL) 
   {
      RootNodes *iterator = graph->root_nodes;
      while(iterator != NULL)
      {
         RootNodes *temp = iterator;
         iterator = iterator->next;
         free(temp);
      }
   }
   free(graph);
}

