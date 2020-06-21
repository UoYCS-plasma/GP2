/* ///////////////////////////////////////////////////////////////////////////

  Copyright 2015-2017 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>.

  ============
  Graph Module
  ============
                             
  An API for GP2 graphs. Defines structures for graphs, nodes, edges, label
  class tables and functions that operate on these structures.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GRAPH_H
#define INC_GRAPH_H

#include "arrays.h"
#include "common.h"
#include "label.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 

#ifndef NO_NODE_LIST
// 24 bytes
typedef struct NodeList {
  struct Node *node;
  struct NodeList *next;
  int index; // TODO: UNSIGNED
} NodeList;
#endif

// 24 bytes
typedef struct EdgeList {
  struct Edge *edge;
  struct EdgeList *next;
  int index; // TODO: UNSIGNED
} EdgeList;

/* ================================
 * Graph Data Structure + Functions
 * ================================ */

// 96/120 + BIGAR_INIT_SZ * 3 bytes
// currently, 672/696 bytes
typedef struct Graph
{
   #ifndef NO_NODE_LIST
   NodeList *nodes;
   #endif
   struct RootNodes *root_nodes;
   int number_of_nodes, number_of_edges; // TODO: UNSIGNED

   // Internally keep arrays to reduce malloc/free's to O(log n).
   BigArray _nodearray;
   BigArray _edgearray;
   #ifndef NO_NODE_LIST
   BigArray _nodelistarray;
   #endif
} Graph;

/* The arguments nodes and edges are the initial sizes of the node array and the
 * edge array respectively. */
Graph *newGraph();

/* =========================
 * Node and Edge Definitions
 * ========================= */

// 64/72 bytes + BIGAR_INIT_SZ
// currently, 256/264 bytes
typedef struct Node {
   HostLabel label;
#define NFLAG_ROOT 0b1
#define NFLAG_MATCHED 0b10
#define NFLAG_DELETED 0b100
#define NFLAG_INGRAPH 0b1000
#define NFLAG_INSTACK 0b10000
   char flags; // All flags stored here.
   int index; // TODO: UNSIGNED
   EdgeList *out_edges, *in_edges; // Linked list changes nothing complexity-wise.
   int outdegree, indegree; // TODO: UNSIGNED
   BigArray _edgelistarray;
} Node;

// 16 bytes
typedef struct RootNodes {
   Node *node;
   struct RootNodes *next;
} RootNodes;

// 32 bytes
typedef struct Edge {
   HostLabel label;
#define EFLAG_MATCHED 0b10
#define EFLAG_DELETED 0b100
#define EFLAG_INSTACK 0b10000
#define EFLAG_INSRCLST 0b100000
#define EFLAG_INTRGLST 0b1000000
   char flags;
   int index; // TODO: UNSIGNED
   Node *source, *target;
} Edge;

/* Nodes and edges are created and added to the graph with the addNode and addEdge
 * functions. They take the necessary construction data as their arguments and 
 * return their index in the graph. */

Node *addNode(Graph *graph, bool root, HostLabel label);
Edge *addEdge(Graph *graph, HostLabel label, Node *source, Node *target);

// Recover a deleted node that hasn't been garbage collected.
void recoverNode(Graph *graph, Node *node);

void removeNode(Graph *graph, Node *node);
void removeEdge(Graph *graph, Edge *edge);
void changeRoot(Graph *graph, Node *node);


#define relabelNode(node, new_label) (node)->label = new_label
#define changeNodeMark(node, new_mark) (node)->label.mark = new_mark
#define relabelEdge(edge, new_label) (edge)->label = new_label
#define changeEdgeMark(edge, new_mark) (edge)->label.mark = new_mark

#define nodeRoot(node) ((node)->flags & NFLAG_ROOT)
#define nodeMatched(node) ((node)->flags & NFLAG_MATCHED)
#define nodeDeleted(node) ((node)->flags & NFLAG_DELETED)
#define nodeInGraph(node) ((node)->flags & NFLAG_INGRAPH)
#define nodeInStack(node) ((node)->flags & NFLAG_INSTACK)
#define initializeNodeInGraph(node) (node)->flags = NFLAG_INGRAPH
#define initializeRootNodeInGraph(node) (node)->flags = (NFLAG_ROOT | NFLAG_INGRAPH)
#define setNodeRoot(node) (node)->flags |= NFLAG_ROOT
#define setNodeMatched(node) (node)->flags |= NFLAG_MATCHED
#define setNodeDeleted(node) (node)->flags |= NFLAG_DELETED
#define setNodeInGraph(node) (node)->flags |= NFLAG_INGRAPH
#define setNodeInStack(node) (node)->flags |= NFLAG_INSTACK
#define clearNodeRoot(node) (node)->flags &= ~NFLAG_ROOT
#define clearNodeMatched(node) (node)->flags &= ~NFLAG_MATCHED
#define clearNodeDeleted(node) (node)->flags &= ~NFLAG_DELETED
#define clearNodeInGraph(node) (node)->flags &= ~NFLAG_INGRAPH
#define clearNodeInStack(node) (node)->flags &= ~NFLAG_INSTACK

#define nodeLabel(node) (node)->label
#define nodeInDegree(node) (node)->indegree
#define incrementInDegree(node) (node)->indegree++
#define decrementInDegree(node) (node)->indegree--
#define nodeOutDegree(node) (node)->outdegree
#define incrementOutDegree(node) (node)->outdegree++
#define decrementOutDegree(node) (node)->outdegree--

#define edgeMatched(edge) ((edge)->flags & EFLAG_MATCHED)
#define edgeDeleted(edge) ((edge)->flags & EFLAG_DELETED)
#define edgeInStack(edge) ((edge)->flags & EFLAG_INSTACK)
#define edgeInSrcLst(edge) ((edge)->flags & EFLAG_INSRCLST)
#define edgeInTrgLst(edge) ((edge)->flags & EFLAG_INTRGLST)
#define setEdgeMatched(edge) (edge)->flags |= EFLAG_MATCHED
#define setEdgeDeleted(edge) (edge)->flags |= EFLAG_DELETED
#define setEdgeInStack(edge) (edge)->flags |= EFLAG_INSTACK
#define setEdgeInSrcLst(edge) (edge)->flags |= EFLAG_INSRCLST
#define setEdgeInTrgLst(edge) (edge)->flags |= EFLAG_INTRGLST
#define clearEdgeMatched(edge) (edge)->flags &= ~EFLAG_MATCHED
#define clearEdgeDeleted(edge) (edge)->flags &= ~EFLAG_DELETED
#define clearEdgeInStack(edge) (edge)->flags &= ~EFLAG_INSTACK
#define clearEdgeInSrcLst(edge) (edge)->flags &= ~EFLAG_INSRCLST
#define clearEdgeInTrgLst(edge) (edge)->flags &= ~EFLAG_INTRGLST

#define edgeSource(edge) (edge)->source
#define edgeTarget(edge) (edge)->target
#define edgeLabel(edge) (edge)->label

#ifndef MINIMAL_GC
#define edgeFree(edge) \
   !(edgeInStack(edge) || edgeInSrcLst(edge) || edgeInTrgLst(edge))

// Try and free a node/edge's memory, fixing all references.
// If the node/edge is still needed anywhere, do nothing.
void tryGarbageCollectNode(Graph *graph, Node *node);
#endif

/* ========================
 * Graph Querying Functions
 * ======================== */

// Given the current position in the list of nodes/edges,
// yield the next element in the list.
// Done this way so deleted nodes/edges are garbage
// collected when passed by.
#ifndef NO_NODE_LIST
Node *yieldNextNode(Graph *graph, NodeList **current);
#endif
Edge *yieldNextOutEdge(Graph *graph, Node *node, EdgeList **current);
Edge *yieldNextInEdge(Graph *graph, Node *node, EdgeList **current);

RootNodes *getRootNodeList(Graph *graph);

void printGraph(Graph *graph, FILE *file);
void printGraphFast(Graph *graph, FILE *file);

#ifndef MINIMAL_GC
void freeGraph(Graph *graph);
#endif

#endif /* INC_GRAPH_H */
