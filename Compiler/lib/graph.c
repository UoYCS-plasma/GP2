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
   graph->_nodearray = makeBigArray(sizeof(Node));
   graph->_edgearray = makeBigArray(sizeof(Edge));
   graph->_nodelistarray = makeBigArray(sizeof(NodeList));
   graph->root_nodes = NULL;
   return graph;
}

Node *addNode(Graph *graph, bool root, HostLabel label)
{
   int nlistind = genFreeBigArrayPos(&(graph->_nodelistarray));
   NodeList *nlist = (NodeList *) getBigArrayValue(
       &(graph->_nodelistarray), nlistind);
   nlist->index = nlistind;
   int nodeind = genFreeBigArrayPos(&(graph->_nodearray));
   Node *node = (Node *) getBigArrayValue(&(graph->_nodearray), nodeind);
   node->index = nodeind;
   if(root) initializeRootNodeInGraph(node);
   else initializeNodeInGraph(node);
   node->label = label;
   node->out_edges = NULL;
   node->in_edges = NULL;
   node->_edgelistarray = makeBigArray(sizeof(EdgeList));

   nlist->node = node;
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
void recoverNode(Graph *graph, Node *node)
{
   int nlistind = genFreeBigArrayPos(&(graph->_nodelistarray));
   NodeList *nlist = (NodeList *) getBigArrayValue(
       &(graph->_nodelistarray), nlistind);
   nlist->index = nlistind;
   nlist->node = node;
   nlist->next = graph->nodes;
   graph->nodes = nlist;
   setNodeInGraph(node);

   if(nodeRoot(node)) addRootNode(graph, node);
   graph->number_of_nodes++;
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
   setEdgeInGraph(edge);

   int srclstind = genFreeBigArrayPos(&(source->_edgelistarray));
   EdgeList *srclist = (EdgeList *) getBigArrayValue(
       &(source->_edgelistarray), srclstind);
   srclist->index = srclstind;
   srclist->edge = edge;
   srclist->next = source->out_edges;
   source->out_edges = srclist;
   setEdgeInSrcLst(edge);
   incrementInDegree(source);

   int trglstind = genFreeBigArrayPos(&(target->_edgelistarray));
   EdgeList *trglist = (EdgeList *) getBigArrayValue(
       &(target->_edgelistarray), trglstind);
   trglist->index = trglstind;
   trglist->edge = edge;
   trglist->next = target->in_edges;
   target->in_edges = trglist;
   setEdgeInTrgLst(edge);
   incrementInDegree(target);

   graph->number_of_edges++;
   return edge;
}

// Assume edge flags are already correct / src and trg exist.
void recoverEdge(Graph *graph, Edge *edge)
{
   setEdgeInGraph(edge);

   int srclstind = genFreeBigArrayPos(&(edgeSource(edge)->_edgelistarray));
   EdgeList *srclist = (EdgeList *) getBigArrayValue(
       &(edgeSource(edge)->_edgelistarray), srclstind);
   srclist->index = srclstind;
   srclist->edge = edge;
   srclist->next = edgeSource(edge)->out_edges;
   edgeSource(edge)->out_edges = srclist;
   setEdgeInSrcLst(edge);
   incrementOutDegree(edgeSource(edge));

   int trglstind = genFreeBigArrayPos(&(edgeTarget(edge)->_edgelistarray));
   EdgeList *trglist = (EdgeList *) getBigArrayValue(
       &(edgeTarget(edge)->_edgelistarray), trglstind);
   trglist->index = trglstind;
   trglist->edge = edge;
   trglist->next = edgeTarget(edge)->in_edges;
   edgeTarget(edge)->in_edges = trglist;
   setEdgeInTrgLst(edge);
   incrementInDegree(edgeTarget(edge));

   graph->number_of_edges++;
}

void removeNode(Graph *graph, Node *node)
{
   setNodeDeleted(node);
   if(nodeRoot(node)) removeRootNode(graph, node);
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
   setEdgeDeleted(edge);
   decrementOutDegree(edgeSource(edge));
   decrementInDegree(edgeTarget(edge));
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
   if(!(nodeInGraph(node) || nodeInStack(node)) && nodeDeleted(node))
   {
      removeHostList(node->label.list);
      // free out_edges and in_edges
      for(EdgeList *curr = node->out_edges; curr != NULL; curr = curr->next)
      {
         clearEdgeInSrcLst(curr->edge);
         if(edgeFree(curr->edge))
         {
            removeHostList(curr->edge->label.list);
            removeFromBigArray(&(graph->_edgearray), curr->edge->index);
         }
      }
      for(EdgeList *curr = node->in_edges; curr != NULL; curr = curr->next)
      {
         clearEdgeInTrgLst(curr->edge);
         if(edgeFree(curr->edge))
         {
            removeHostList(curr->edge->label.list);
            removeFromBigArray(&(graph->_edgearray), curr->edge->index);
         }
      }
      emptyBigArray(&(node->_edgelistarray));
      removeFromBigArray(&(graph->_nodearray), node->index);
   }
}

/* ========================
 * Graph Querying Functions 
 * ======================== */

Node *yieldNextNode(Graph *graph, NodeList **current_prev)
{
   NodeList *current;

   if(*current_prev == NULL) current = graph->nodes;
   else current = (*current_prev)->next;

   if(!nodeDeleted(current->node))
   {
     *current_prev = (*current_prev)->next;
     current = current->next;
   }

   bool deleted_node = true;

   while(deleted_node) {
     if(current == NULL) return NULL;

     Node *node = current->node;

     deleted_node = nodeDeleted(node);
     if(nodeDeleted(node))
     {
       int index = current->index;
       if((*current_prev) != NULL)
         (*current_prev)->next = current->next;
       else
         graph->nodes = current->next;
       *current_prev = current;
       current = current->next;
       removeFromBigArray(&(graph->_nodelistarray), index);
       clearNodeInGraph(node);
       tryGarbageCollectNode(graph, node);
     }

   }
   return current->node;
}

Edge *yieldNextOutEdge(Graph *graph, Node *node, EdgeList **current_prev)
{
   EdgeList *current;

   if(*current_prev == NULL) current = node->out_edges;
   else current = (*current_prev)->next;

   if(!edgeDeleted(current->edge))
   {
     *current_prev = (*current_prev)->next;
     current = current->next;
   }

   bool deleted_edge = true;

   while(deleted_edge) {
     if(current == NULL) return NULL;

     Edge *edge = current->edge;

     deleted_edge = edgeDeleted(edge);
     if(edgeDeleted(edge))
     {
       int index = current->index;
       if((*current_prev) != NULL)
         (*current_prev)->next = current->next;
       else
         node->out_edges = current->next;
       *current_prev = current;
       current = current->next;
       clearEdgeInSrcLst(edge);
       if(edgeFree(edge))
       {
          removeHostList(edge->label.list);
          removeFromBigArray(&(graph->_edgearray), edge->index);
       }
     }
   }
   return current->edge;
}

Edge *yieldNextInEdge(Graph *graph, Node *node, EdgeList **current_prev)
{
   EdgeList *current;

   if(*current_prev == NULL) current = node->in_edges;
   else current = (*current_prev)->next;

   if(!edgeDeleted(current->edge))
   {
     *current_prev = (*current_prev)->next;
     current = current->next;
   }

   bool deleted_edge = true;

   while(deleted_edge) {
     if(current == NULL) return NULL;

     Edge *edge = current->edge;

     deleted_edge = edgeDeleted(edge);
     if(edgeDeleted(edge))
     {
       int index = current->index;
       if((*current_prev) != NULL)
         (*current_prev)->next = current->next;
       else
         node->in_edges = current->next;
       *current_prev = current;
       current = current->next;
       clearEdgeInSrcLst(edge);
       if(edgeFree(edge))
       {
          removeHostList(edge->label.list);
          removeFromBigArray(&(graph->_edgearray), edge->index);
       }
     }
   }
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
   /* Maps a node's graph-index to the ID it is printed with (node_count). */
   NodeList *nlistpos = NULL;
   for(Node *node; (node = yieldNextNode(graph, &nlistpos)) != NULL;)
   {
      /* Five nodes per line */
      if(node_count != 0 && node_count % 5 == 0) PTF("\n  ");
      if(nodeRoot(node)) PTF("(%d(R), ", node->index);
      else PTF("(%d, ", node->index);
      printHostLabel(node->label, file);
      PTF(") ");
   }
   if(graph->number_of_edges == 0)
   {
      PTF("| ]\n\n");
      return;
   }
   PTF("|\n  ");
   for(Node *node; (node = yieldNextNode(graph, &nlistpos)) != NULL;)
   {
      EdgeList *elistpos = NULL;
      for(Edge *edge;
          (edge = yieldNextOutEdge(graph, node, &elistpos)) != NULL;)
      {
         /* Three edges per line */
         if(edge_count != 0 && edge_count % 3 == 0) PTF("\n  ");
         PTF("(%d, %d, %d, ", edge->index,
             edgeSource(edge)->index, edgeTarget(edge)->index);
         printHostLabel(edge->label, file);
         PTF(") ");
      }
   }
   PTF("]\n\n");
}

void freeGraph(Graph *graph)
{
   if(graph == NULL) return;

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
   emptyBigArray(&(graph->_nodearray));
   emptyBigArray(&(graph->_edgearray));
   emptyBigArray(&(graph->_nodelistarray));
   free(graph);
}

