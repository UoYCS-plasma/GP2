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
 * Static Graph Functions
 * =============== */

static void addRootNode(Graph *graph, Node *node)
{
   RootNodes *root_node = mallocSafe(sizeof(RootNodes), "addRootNode");
   root_node->node = node;
   root_node->next = graph->root_nodes;
   graph->root_nodes = root_node;
}

static void removeRootNode(Graph *graph, Node *node)
{
   RootNodes *current = graph->root_nodes, *previous = NULL;
   while(current != NULL)
   {
      if(current->node == node)
      {
         clearNodeRoot(node);
         if(previous == NULL) graph->root_nodes = current->next;
         else previous->next = current->next;
         free(current);
         break;
      }
      previous = current;
      current = current->next;
   }
}

/* ===============
 * Graph Functions
 * =============== */

Graph *newGraph() 
{
   Graph *graph = mallocSafe(sizeof(Graph), "newGraph");
   graph->number_of_nodes = 0;
   graph->number_of_edges = 0;
   #ifndef NO_NODE_LIST
   for(int i = 0; i < 5; i++) graph->nodes[i] = NULL;
   #endif
   graph->_nodearray = makeBigArray(sizeof(Node));
   graph->_edgearray = makeBigArray(sizeof(Edge));
   #ifndef NO_NODE_LIST
   graph->_nodelistarray = makeBigArray(sizeof(NodeList));
   #endif
   graph->root_nodes = NULL;
   return graph;
}

Node *addNode(Graph *graph, bool root, HostLabel label)
{
   #ifndef NO_NODE_LIST
   int nlistind = genFreeBigArrayPos(&(graph->_nodelistarray));
   NodeList *nlist = (NodeList *) getBigArrayValue(
       &(graph->_nodelistarray), nlistind);
   nlist->index = nlistind;
   #endif

   int nodeind = genFreeBigArrayPos(&(graph->_nodearray));
   Node *node = (Node *) getBigArrayValue(&(graph->_nodearray), nodeind);
   node->index = nodeind;
   if(root) initializeRootNodeInGraph(node);
   else initializeNodeInGraph(node);

   node->label = label;
   for(int marks = 0; marks < 6; marks++){
      for(int orientations = 0; orientations < 2; orientations++){
         node->edges[marks][orientations][0] = NULL;
         node->edges[marks][orientations][1] = NULL;
      }
   }
   node->outdegree = 0;
   node->indegree = 0;
   node->_edgelistarray = makeBigArray(sizeof(EdgeList));

   #ifndef NO_NODE_LIST
   nlist->node = node;
   nlist->next = graph->nodes[label.mark];
   nlist->prev = NULL;
   if(graph->nodes[label.mark] != NULL) graph->nodes[label.mark]->prev = nlist;
   graph->nodes[label.mark] = nlist;
   node->nodeListAddress = nlist;
   #endif

   if(root) addRootNode(graph, node);
   graph->number_of_nodes++;
   return node;
}

Edge *addEdge(Graph *graph, HostLabel label, Node *source, Node *target)
{
   int edgeind = genFreeBigArrayPos(&(graph->_edgearray));
   Edge *edge = (Edge *) getBigArrayValue(&(graph->_edgearray), edgeind);
   edge->index = edgeind;
   edge->label = label;
   edge->source = source;
   edge->target = target;
   edge->flags = (char) 0;

   int srclstind = genFreeBigArrayPos(&(source->_edgelistarray));
   EdgeList *srclist = (EdgeList *) getBigArrayValue(
       &(source->_edgelistarray), srclstind);
   srclist->index = srclstind;
   srclist->edge = edge;

   srclist->next = source->edges[label.mark][0][edge->source == edge->target];
   srclist->prev = NULL;
   if(source->edges[label.mark][0][edge->source == edge->target] != NULL){
      source->edges[label.mark][0][edge->source == edge->target]->prev = srclist;
   }
   source->edges[label.mark][0][edge->source == edge->target] = srclist;
   edge->edgeSrcListAddress = srclist;

   setEdgeInSrcLst(edge);
   incrementOutDegree(source);

   int trglstind = genFreeBigArrayPos(&(target->_edgelistarray));
   EdgeList *trglist = (EdgeList *) getBigArrayValue(
       &(target->_edgelistarray), trglstind);
   trglist->index = trglstind;
   trglist->edge = edge;

   trglist->next = target->edges[label.mark][1][edge->source == edge->target];
   trglist->prev = NULL;
   if(target->edges[label.mark][1][edge->source == edge->target] != NULL){
      target->edges[label.mark][1][edge->source == edge->target]->prev = trglist;
   }
   target->edges[label.mark][1][edge->source == edge->target] = trglist;
   edge->edgeTrgListAddress = trglist;

   setEdgeInTrgLst(edge);
   incrementInDegree(target);

   graph->number_of_edges++;
   return edge;
}

// Assume node flags are already correct / edges exist.
void recoverNode(Graph *graph, Node *node)
{
   #ifndef NO_NODE_LIST
   int nlistind = genFreeBigArrayPos(&(graph->_nodelistarray));
   NodeList *nlist = (NodeList *) getBigArrayValue(
       &(graph->_nodelistarray), nlistind);
   nlist->index = nlistind;
   nlist->node = node;
   nlist->next = graph->nodes[node->label.mark];
   graph->nodes[node->label.mark] = nlist;
   #endif

   setNodeInGraph(node);
   if(nodeRoot(node)) addRootNode(graph, node);
   graph->number_of_nodes++;
}

void recoverEdge(Graph *graph, Edge *edge)
{
   if(!edgeInSrcLst(edge)){
      Node *source = edge->source;
      int srclstind = genFreeBigArrayPos(&(source->_edgelistarray));
      EdgeList *srclist = (EdgeList *) getBigArrayValue(
         &(source->_edgelistarray), srclstind);
      srclist->index = srclstind;
      srclist->edge = edge;

      srclist->next = source->edges[edge->label.mark][0][edge->source == edge->target];
      srclist->prev = NULL;
      if(source->edges[edge->label.mark][0][edge->source == edge->target] != NULL){
         source->edges[edge->label.mark][0][edge->source == edge->target]->prev = srclist;
      }
      source->edges[edge->label.mark][0][edge->source == edge->target] = srclist;
      edge->edgeSrcListAddress = srclist;
      incrementOutDegree(source);
      setEdgeInSrcLst(edge);
   }
   if(!edgeInTrgLst(edge)){
      Node *target = edge->target;
      int trglstind = genFreeBigArrayPos(&(target->_edgelistarray));
      EdgeList *trglist = (EdgeList *) getBigArrayValue(
         &(target->_edgelistarray), trglstind);
      trglist->index = trglstind;
      trglist->edge = edge;

      trglist->next = target->edges[edge->label.mark][1][edge->source == edge->target];
      trglist->prev = NULL;
      if(target->edges[edge->label.mark][1][edge->source == edge->target] != NULL){
         target->edges[edge->label.mark][1][edge->source == edge->target]->prev = trglist;
      }
      target->edges[edge->label.mark][1][edge->source == edge->target] = trglist;
      edge->edgeTrgListAddress = trglist;
      setEdgeInTrgLst(edge);
      incrementInDegree(target);
   }
   graph->number_of_edges++;
}

void removeNode(Graph *graph, Node *node)
{
   setNodeDeleted(node);
   if(nodeRoot(node)) removeRootNode(graph, node);
   graph->number_of_nodes--;
}

void relistNode(Graph *graph, Node *node, int old_mark)
{
   //setNodeRemarked(node);
   #ifndef NO_NODE_LIST
   int mark = node->label.mark;
   NodeList *nlist = node->nodeListAddress;
   if(nlist->prev != NULL) nlist->prev->next = nlist->next;
   if(nlist->next != NULL) nlist->next->prev = nlist->prev;
   if(nlist->prev == NULL) graph->nodes[old_mark] = nlist->next;
   nlist->prev = NULL;
   nlist->next = graph->nodes[mark];
   if(graph->nodes[mark] != NULL) graph->nodes[mark]->prev = nlist;
   graph->nodes[mark] = nlist;
   #endif
}

void relistEdge(Graph *graph, Edge *edge, int old_mark){
   int mark = edge->label.mark;
   EdgeList *eTrgList = edge->edgeTrgListAddress;
   EdgeList *eSrcList = edge->edgeSrcListAddress;
   Node *src, *trg;
   src = edge->source; trg = edge->target;
   // Source node:
   if(eSrcList->prev != NULL) eSrcList->prev->next = eSrcList->next;
   if(eSrcList->next != NULL) eSrcList->next->prev = eSrcList->prev;
   if(eSrcList->prev == NULL) src->edges[old_mark][0][trg == src] = eSrcList->next;
   eSrcList->prev = NULL;
   eSrcList->next = src->edges[mark][0][trg == src];
   if(src->edges[mark][0][trg == src] != NULL) src->edges[mark][0][trg == src]->prev = eSrcList;
   src->edges[mark][0][trg == src] = eSrcList;
   // Target node:
   if(eTrgList->prev != NULL) eTrgList->prev->next = eTrgList->next;
   if(eTrgList->next != NULL) eTrgList->next->prev = eTrgList->prev;
   if(eTrgList->prev == NULL) trg->edges[old_mark][1][trg == src] = eTrgList->next;
   eTrgList->prev = NULL;
   eTrgList->next = trg->edges[mark][1][trg == src];
   if(trg->edges[mark][1][trg == src] != NULL) trg->edges[mark][1][trg == src]->prev = eTrgList;
   trg->edges[mark][1][trg == src] = eTrgList;
}

void removeEdge(Graph *graph, Edge *edge)
{
   setEdgeDeleted(edge);
   decrementOutDegree(edgeSource(edge));
   decrementInDegree(edgeTarget(edge));
   graph->number_of_edges--;
}

void changeRoot(Graph *graph, Node *node)
{
   if(nodeRoot(node))
   {
     removeRootNode(graph, node);
     clearNodeRoot(node);
   }
   else
   {
     addRootNode(graph, node);
     setNodeRoot(node);
   }
}

#ifndef MINIMAL_GC
void tryGarbageCollectNode(Graph *graph, Node *node)
{
   if(!(nodeInGraph(node) || nodeInStack(node) || nodeMatched(node)) && nodeDeleted(node))
   {
      removeHostList(node->label.list);
      // free all edges of all marks
      for(int i = 0; i < 6; i++){
         for(int j = 0; j < 2; j++){
            for(int k = 0; k < 2; k++){
               for(EdgeList *curr = node->edges[i][j][k]; curr != NULL; curr = curr->next)
               {
                  clearEdgeInSrcLst(curr->edge);
                  if(edgeFree(curr->edge))
                  {
                     removeHostList(curr->edge->label.list);
                     removeFromBigArray(&(graph->_edgearray), curr->edge->index);
                  }
               }
            }
         }
      }
      emptyBigArray(&(node->_edgelistarray));
      removeFromBigArray(&(graph->_nodearray), node->index);
   }
}
#endif

/* ========================
 * Graph Querying Functions 
 * ======================== */

#ifndef NO_NODE_LIST
Node *yieldNextNode(Graph *graph, NodeList **current_prev, int mark)
{
   NodeList *current;
   bool initial = *current_prev == NULL;
   if(initial) *current_prev = current = graph->nodes[mark];
   else current = (*current_prev)->next;

   bool deleted_node = true;

   while(deleted_node) {
     if(current == NULL) return NULL;

     Node *node = current->node;

     deleted_node = nodeDeleted(node);
     if(deleted_node)
     {
       #ifndef MINIMAL_GC
       int index = current->index;
       #endif
       if((*current_prev) != current)
         (*current_prev)->next = current->next;
       if (initial){
         graph->nodes[mark] = current->next;
       }
       current = current->next;
       clearNodeInGraph(node);
       #ifndef MINIMAL_GC
       removeFromBigArray(&(graph->_nodelistarray), index);
       tryGarbageCollectNode(graph, node);
       #endif
     }
   }

   *current_prev = current;
   return current->node;
}

Node *yieldNextNodeFast(Graph *graph, NodeList **current_prev, int mark)
{
   NodeList *current;
   if(*current_prev == NULL) *current_prev = current = graph->nodes[mark];
   else current = (*current_prev)->next;

   bool deleted_node = true;
   while(deleted_node) {
     if(current == NULL) return NULL;
     deleted_node = nodeDeleted(current->node);
     if(deleted_node) current = current->next;
   }

   *current_prev = current;
   return current->node;
}
#endif

Edge *yieldNextOutEdge(Graph *graph, Node *node, EdgeList **current_prev, int mark, bool loop)
{
   EdgeList *current;
   bool initial = *current_prev == NULL;
   if(initial) *current_prev = current = node->edges[mark][0][loop];
   else current = (*current_prev)->next;

   bool deleted_edge = true;

   while(deleted_edge) {
     if(current == NULL) return NULL;

     Edge *edge = current->edge;

     deleted_edge = edgeDeleted(edge);
     if(deleted_edge)
     {
       if((*current_prev) != current)
         (*current_prev)->next = current->next;
       if(initial){
         node->edges[mark][0][loop] = current->next;
       }
       current = current->next;
       clearEdgeInSrcLst(edge);
       #ifndef MINIMAL_GC
       if(edgeFree(edge))
       {
          removeHostList(edge->label.list);
          removeFromBigArray(&(graph->_edgearray), edge->index);
       }
       #endif
     }
   }

   *current_prev = current;
   return current->edge;
}

Edge *yieldNextOutEdgeFast(Graph *graph, Node *node, EdgeList **current_prev, int mark, bool loop)
{
   EdgeList *current;
   if(*current_prev == NULL) *current_prev = current = node->edges[mark][0][loop];
   else current = (*current_prev)->next;

   bool deleted_edge = true;
   while(deleted_edge) {
     if(current == NULL) return NULL;
     deleted_edge = edgeDeleted(current->edge);
     if(deleted_edge) current = current->next;
   }

   *current_prev = current;
   return current->edge;
}

Edge *yieldNextInEdge(Graph *graph, Node *node, EdgeList **current_prev, int mark, bool loop)
{
   EdgeList *current;
   bool initial = *current_prev == NULL;
   if(initial) *current_prev = current = node->edges[mark][1][loop];
   else current = (*current_prev)->next;

   bool deleted_edge = true;
   while(deleted_edge) {
     if(current == NULL) return NULL;

     Edge *edge = current->edge;

     deleted_edge = edgeDeleted(edge);
     if(deleted_edge)
     {
       if((*current_prev) != current)
         (*current_prev)->next = current->next;
       if(initial){
         node->edges[mark][1][loop] = current->next;
       }
       current = current->next;
       clearEdgeInSrcLst(edge);
       #ifndef MINIMAL_GC
       if(edgeFree(edge))
       {
          removeHostList(edge->label.list);
          removeFromBigArray(&(graph->_edgearray), edge->index);
       }
       #endif
     }
   }

   *current_prev = current;
   return current->edge;
}

RootNodes *getRootNodeList(Graph *graph)
{
   return graph->root_nodes;
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
   #ifndef NO_NODE_LIST
   NodeList *nlistpos = NULL;
   for(int i = 0; i < 6; i++){
      if(i == DASHED) continue;
      nlistpos = NULL;
      for(Node *node; (node = yieldNextNode(graph, &nlistpos, i)) != NULL;){
      #else
      Node *node;
      for (int i = 0; i < graph->_nodearray.size; i++)
      {
         node = (Node *) getBigArrayValue(&(graph->_nodearray), i);
         if(nodeDeleted(node))
         {
            clearNodeInGraph(node);
            continue;
         }
      #endif
         /* Five nodes per line */
         if(node_count != 0 && node_count % 5 == 0) PTF("\n  ");
         node_count++;
         if(nodeRoot(node)) PTF("(%d(R), ", node->index);
         else PTF("(%d, ", node->index);
         printHostLabel(node->label, file);
         PTF(") ");
      #ifndef NO_NODE_LIST
      }
   #endif
   }
   if(graph->number_of_edges == 0)
   {
      PTF("| ]\n\n");
      return;
   }
   PTF("|\n  ");
   EdgeList *elistpos = NULL;
   #ifndef NO_NODE_LIST
   for(int n = 0; n < 6; n++){
      if(n == DASHED) continue;
      nlistpos = NULL;
      for(Node *node; (node = yieldNextNode(graph, &nlistpos, n)) != NULL;)
      {
      #else
      for (int i = 0; i < graph->_nodearray.size; i++)
      {
         node = (Node *) getBigArrayValue(&(graph->_nodearray), i);
         if(nodeDeleted(node)) continue;
      #endif
         for(int k = 0; k < 6; k++){
            for(int j = 0; j < 2; j++){
               elistpos = NULL;
               for(Edge *edge; (edge = yieldNextOutEdge(graph, node, &elistpos, k, j)) != NULL;)
               {
                  /* Three edges per line */
                  if(edge_count != 0 && edge_count % 3 == 0) PTF("\n  ");
                  edge_count++;
                  PTF("(%d, %d, %d, ", edge->index, edgeSource(edge)->index, edgeTarget(edge)->index);
                  printHostLabel(edge->label, file);
                  PTF(") ");
               }
            }
         }
      #ifndef NO_NODE_LIST
      }
      #endif
   }
   PTF("]\n\n");
}

void printGraphFast(Graph *graph, FILE *file) 
{
   if(graph == NULL || graph->number_of_nodes == 0) 
   {
      PTF("[ | ]\n\n");
      return;
   }
   PTF("[ ");
   #ifndef NO_NODE_LIST
   NodeList *nlistpos = NULL;
   for(int n = 0; n < 6; n++){
      if(n == DASHED) continue;
      nlistpos = NULL;
      for(Node *node; (node = yieldNextNodeFast(graph, &nlistpos, n)) != NULL;)
      {
         if(nodeRoot(node)) PTF("(%d(R), ", node->index);
         else PTF("(%d, ", node->index);
         printHostLabel(node->label, file);
         PTF(") ");
      }
   }
   #else
   Node *node;
   for (int i = 0; i < graph->_nodearray.size; i++)
   {
      node = (Node *) getBigArrayValue(&(graph->_nodearray), i);
      if(nodeDeleted(node)) continue;
      if(nodeRoot(node)) PTF("(%d(R), ", node->index);
      else PTF("(%d, ", node->index);
      printHostLabel(node->label, file);
      PTF(") ");
   }
   #endif
   if(graph->number_of_edges == 0)
   {
      PTF("| ]\n\n");
      return;
   }
   PTF("|\n  ");
   EdgeList *elistpos = NULL;
   #ifndef NO_NODE_LIST
   for(int n = 0; n < 6; n++){
      if(n == DASHED) continue;
      nlistpos = NULL;
      for(Node *node; (node = yieldNextNodeFast(graph, &nlistpos, n)) != NULL;){
   #else
   for (int i = 0; i < graph->_nodearray.size; i++)
   {
      node = (Node *) getBigArrayValue(&(graph->_nodearray), i);
      if(nodeDeleted(node)) continue;
   #endif
         for(int i = 0; i < 6; i++){
            for(int j = 0; j < 2; j++){
               elistpos = NULL;
               for(Edge *edge; (edge = yieldNextOutEdgeFast(graph, node, &elistpos, i, j)) != NULL;)
               {
                  PTF("(%d, %d, %d, ", edge->index, edgeSource(edge)->index, edgeTarget(edge)->index);
                  printHostLabel(edge->label, file);
                  PTF(") ");
               }
            }
         }
      }
   #ifndef NO_NODE_LIST
   }
   #endif
   PTF("]\n\n");
}

#ifndef MINIMAL_GC
void freeGraph(Graph *graph){
   if(graph == NULL) return;

   #ifndef NO_NODE_LIST
   NodeList *nlistpos = NULL;
   for(int n = 0; n < 6; n++){
      if(n == DASHED) continue;
      nlistpos = NULL;
      for(Node *node; (node = yieldNextNode(graph, &nlistpos, n)) != NULL;)
         removeNode(graph, node);
   }

   for(int n = 0; n < 6; n++){
      if(n == DASHED) continue;
      nlistpos = NULL;
      for(Node *node; (node = yieldNextNode(graph, &nlistpos, n)) != NULL;)
         removeNode(graph, node);
   }
   #else
   Node *node;
   for (int i = 0; i < graph->_nodearray.size; i++)
   {
      node = (Node *) getBigArrayValue(&(graph->_nodearray), i);
      if(nodeDeleted(node))
         clearNodeInGraph(node);
      else
      {
         removeNode(graph, node);
         clearNodeInGraph(node);
      }
      tryGarbageCollectNode(graph, node);
   }
   #endif

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
   emptyBigArray(&(graph->_nodearray));
   emptyBigArray(&(graph->_edgearray));
   #ifndef NO_NODE_LIST
   emptyBigArray(&(graph->_nodelistarray));
   #endif
   free(graph);
}
#endif
