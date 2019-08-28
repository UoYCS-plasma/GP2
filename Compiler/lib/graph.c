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
   graph->nodes = NULL;
   graph->edges = NULL;
   graph->_nodearray = makeBigArray(256, sizeof(Node));
   graph->_edgearray = makeBigArray(256, sizeof(Edge));
   graph->_nodelistarray = makeBigArray(256, sizeof(NodeList));
   graph->_edgelistarray = makeBigArray(256, sizeof(EdgeList));
   graph->root_nodes = NULL;
   return graph;
}

Node *addNode(Graph *graph, bool root, HostLabel label)
{
   int nlistind = genFreeBigArrayPos(&(graph->_nodelistarray));
   NodeList *nlist = (NodeList *) getBigArrayValue(
       graph->_nodelistarray, nlistind);
   nlist->index = nlistind;
   int nodeind = genFreeBigArrayPos(&(graph->_nodearray));
   Node *node = (Node *) getBigArrayValue(graph->_nodearray, nodeind);
   node->index = nodeind;
   node->root = root;
   node->label = label;
   node->out_edges = NULL;
   node->in_edges = NULL;
   node->_outedgearray = makeBigArray(16, sizeof(EdgeList));
   node->_inedgearray = makeBigArray(16, sizeof(EdgeList));
   node->outdegree = 0;
   node->indegree = 0;
   node->deleted = false;
   node->in_graph = true;
   node->in_stack = 0;
   node->in_morphism = 0;

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

// Assume node flags are already correct / edges exist.
void insertNode(Graph *graph, Node *node)
{
   int nlistind = genFreeBigArrayPos(&(graph->_nodelistarray));
   NodeList *nlist = (NodeList *) getBigArrayValue(
       graph->_nodelistarray, nlistind);
   nlist->index = nlistind;
   int nodeind = genFreeBigArrayPos(&(graph->_nodearray));
   node->index = nodeind;
   nlist->node = node;
   if (graph->nodes != NULL)
     graph->nodes->prev = nlist;
   nlist->next = graph->nodes;
   graph->nodes = nlist;
   node->in_graph = true;

   if(node->root) addRootNode(graph, node);
   graph->number_of_nodes++;
}

Edge *addEdge(Graph *graph, HostLabel label, Node *source, Node *target)
{
   int elistind = genFreeBigArrayPos(&(graph->_edgelistarray));
   NodeList *elist = (EdgeList *) getBigArrayValue(
       graph->_Edgelistarray, elistind);
   elist->index = elistind;
   int edgeind = genFreeBigArrayPos(&(graph->_edgearray));
   Edge *edge = (Edge *) getBigArrayValue(graph->_edgearray, edgeind);
   edge->index = edgeind;
   edge->label = label;
   edge->source = source;
   edge->target = target;
   edge->deleted = false;
   edge->in_graph = true;
   edge->in_stack = 0;
   edge->in_morphism = 0;

   elist->edge = edge;
   if (graph->edges != NULL)
     graph->edges->prev = elist;
   elist->next = graph->edges;
   graph->edges = elist;

   // add to source edgelist
   int srclstind = genFreeBigArrayPos(&(node->_outedgearray));
   NodeList *srclist = (EdgeList *) getBigArrayValue(
       node->_outedgearray, srclstind);
   srclist->index = srclstind;
   srclist->edge = edge;
   if (source->out_edges != NULL)
     source->out_edges->prev = srclist;
   srclist->next = source->out_edges;
   source->out_edges = srclist;
   source->outdegree++;
   edge->in_srclst = true;

   int trglstind = genFreeBigArrayPos(&(node->_inedgearray));
   NodeList *trglist = (EdgeList *) getBigArrayValue(
       node->_inedgearray, trglstind);
   trglist->index = trglstind;
   trglist->edge = edge;
   if (target->out_edges != NULL)
     target->out_edges->prev = trglist;
   trglist->next = target->out_edges;
   target->out_edges = trglist;
   target->indegree++;
   edge->in_trglst = true;

   graph->number_of_edges++;
   return edge;
}

// Assume edge flags are already correct / src and trg exist.
void insertEdge(Graph *graph, Edge *edge)
{
   int elistind = genFreeBigArrayPos(&(graph->_edgelistarray));
   NodeList *elist = (EdgeList *) getBigArrayValue(
       graph->_Edgelistarray, elistind);
   elist->index = elistind;
   int edgeind = genFreeBigArrayPos(graph->_edgearray);
   edge->index = edgeind;
   edge->in_graph = true;

   elist->edge = edge;
   if (graph->edges != NULL)
     graph->edges->prev = elist;
   elist->next = graph->edges;
   graph->edges = elist;

   int srclstind = genFreeBigArrayPos(&(node->_outedgearray));
   NodeList *srclist = (EdgeList *) getBigArrayValue(
       node->_outedgearray, srclstind);
   srclist->index = srclstind;
   srclist->edge = edge;
   if (source->out_edges != NULL)
     source->out_edges->prev = srclist;
   srclist->next = source->out_edges;
   source->out_edges = srclist;
   source->outdegree++;
   edge->in_srclst = true;

   int trglstind = genFreeBigArrayPos(&(node->_inedgearray));
   NodeList *trglist = (EdgeList *) getBigArrayValue(
       node->_inedgearray, trglstind);
   trglist->index = trglstind;
   trglist->edge = edge;
   if (target->out_edges != NULL)
     target->out_edges->prev = trglist;
   trglist->next = target->out_edges;
   target->out_edges = trglist;
   target->indegree++;
   edge->in_trglst = true;

   graph->number_of_edges++;
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
   node->root = !node->root;
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

void tryGarbageCollectNode(Graph *graph, Node *node)
{
   if(!(node->in_graph || node->in_stack || node->in_morphism)
      && node->deleted)
   {
      removeHostList(node->label.list);
      // free out_edges and in_edges
      // not possible to have a dangling edge pointer;
      // when collected, edges clean these out of source/target nodes
      for(EdgeList *curr = node->out_edges; curr != NULL; curr = curr->next)
        curr->edge->in_srclst = false;
      for(EdgeList *curr = node->in_edges; curr != NULL; curr = curr->next)
        curr->edge->in_trglst = false;
      emptyBigArray(&(node->_outedgearray));
      emptyBigArray(&(node->_inedgearray));
      removeFromBigArray(&(graph->_nodearray), node->index);
   }
}

void tryGarbageCollectEdge(Graph *graph, Edge *edge)
{
   if(!(edge->in_graph || edge->in_stack || edge->in_morphism)
      && edge->deleted)
   {
      // Clean out references in src/trg to edge by iterating through them.
      // (If source/target garbage collected, in_srclst/trglst = false.)
      EdgeList *elpos = NULL;
      if(edge->in_srclst)
        for(Edge *e; (e = yieldNextOutEdge(edge->source, &elpos)) != NULL;)
          ;
      if(edge->in_trglst)
        for(Edge *e; (e = yieldNextInEdge(edge->target, &elpos)) != NULL;)
          ;
      removeHostList(edge->label.list);
      removeFromBigArray(graph->_edgearray, edge->index);
   }
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
       removeFromBigArray(&(graph->_nodelistarray), (*current)->prev->index);
       node->in_graph = false;
       tryGarbageCollectNode(graph, node);
     }
   }
   return (*current)->node;
}

Edge *yieldNextOutEdge(Node *node, EdgeList **current)
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
       removeFromBigArray(&(node->_outedgearray), (*current)->prev->index);
       edge->in_srclst = false;
     }
   }
   return (*current)->edge;
}

Edge *yieldNextInEdge(Node *node, EdgeList **current)
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
       removeFromBigArray(&(node->_inedgearray), (*current)->prev->index);
       edge->in_trglst = false;
     }
   }
   return (*current)->edge;
}

Edge *yieldNextEdge(Graph *graph, EdgeList **current)
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
       removeFromBigArray(&(node->_edgelistarray), (*current)->prev->index);
       edge->in_graph = false;
       tryGarbageCollectEdge(graph, edge);
     }
   }
   return (*current)->edge;
}

RootNodes *getRootNodeList(Graph *graph)
{
   return graph->root_nodes;
}

Node *getSource(Edge *edge) 
{
   assert(!edge->source->deleted);
   return edge->source;
}

Node *getTarget(Edge *edge) 
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
   NodeList *nlistpos = NULL;
   for(Node *node; (node = yieldNextNode(graph, &nlistpos)) != NULL;)
   {
      /* Five nodes per line */
      if(node_count != 0 && node_count % 5 == 0) PTF("\n  ");
      if(node->root) PTF("(%p(R), ", (void *) node);
      else PTF("(%p, ", (void *) node);
      printHostLabel(node->label, file);
      PTF(") ");
   }
   if(graph->number_of_edges == 0)
   {
      PTF("| ]\n\n");
      return;
   }
   PTF("|\n  ");
   EdgeList *elistpos = NULL;
   for(Edge *edge; (edge = yieldNextEdge(graph, &elistpos)) != NULL;)
   {
      /* Three edges per line */
      if(edge_count != 0 && edge_count % 3 == 0) PTF("\n  ");
      PTF("(%p, %p, %p, ", (void *) edge,
          (void *) edge->source, (void *) edge->target);
      printHostLabel(edge->label, file);
      PTF(") ");
   }
   PTF("]\n\n");
}

void freeGraph(Graph *graph)
{
   if(graph == NULL) return;

   EdgeList *elistpos = NULL;
   for(Edge *edge; (edge = yieldNextEdge(graph, &elistpos)) != NULL;)
     removeEdge(graph, edge);

   NodeList *nlistpos = NULL;
   for(Node *node; (node = yieldNextNode(graph, &nlistpos)) != NULL;)
     removeNode(graph, node);

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
   emptyBigArray(graph->_nodearray);
   emptyBigArray(graph->_edgearray);
   emptyBigArray(graph->_nodelistarray);
   emptyBigArray(graph->_edgelistarray);
   free(graph);
}

