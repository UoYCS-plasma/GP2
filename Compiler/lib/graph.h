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

#include "common.h"
#include "label.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 

/* There are 7 marks, but the 'any' mark does not occur in host graphs. */
#define NUMBER_OF_MARKS 6 
#define NUMBER_OF_CLASSES 7

typedef struct IntArray {
   int capacity;
   int size;
   int *items;
} IntArray;

typedef struct NodeList {
  int index;
  struct Node *node;
  struct NodeList *next;
  struct NodeList *prev;
} NodeList;

typedef struct EdgeList {
  int index;
  struct Edge *edge;
  struct EdgeList *next;
  struct EdgeList *prev;
} EdgeList;

typedef struct NodeQuery {
  MarkType mark;
} NodeQuery;

extern struct NodeList dummy_nodelist;
extern struct EdgeList dummy_edgelist;


typedef struct BigArrayElem {
  int size;
  void *items;
  struct BigArrayElem *next;
} BigArrayElem;

// Dynamic data struct of arbitrary size, which never moves elements.
// Hence, pointers to its elements are never invalidated.
// This structure is a linked list of arrays repeatedly doubling in size
// and an IntArray of available holes in said array.
// Useful for minimizing the number of malloc's while keeping pointers valid.
typedef struct BigArray {
  int capacity;
  int size;
  size_t elem_sz;
  BigArrayElem *elems;
  IntArray holes;
} BigArray;

BigArray makeBigArray(int initial_capacity, size_t elem_sz);
int genFreeBigArrayPos(BigArray *array);
void *getBigArrayValue(BigArray array, int index);
void removeFromBigArray(BigArray *array, int index);
void emptyBigArray(BigArray *array);

/* ================================
 * Graph Data Structure + Functions
 * ================================ */
typedef struct Graph 
{
   NodeList *nodes;
   EdgeList *edges;

   // Internally keep arrays to reduce malloc/free's to O(log n).
   BigArray _nodearray;
   BigArray _edgearray;
   BigArray _nodelistarray;
   BigArray _edgelistarray;

   int number_of_nodes, number_of_edges;

   /* Root nodes referenced in a linked list for fast access. */
   struct RootNodes *root_nodes;
} Graph;

/* The arguments nodes and edges are the initial sizes of the node array and the
 * edge array respectively. */
Graph *newGraph();

/* =========================
 * Node and Edge Definitions
 * ========================= */
typedef struct Node {
   int index;
   bool root;
   HostLabel label;
   int outdegree, indegree;
   EdgeList *out_edges, *in_edges; // Linked list changes nothing complexity-wise.
   BigArray _outedgearray, _inedgearray;
   bool deleted; // 1 if going to be garbage-collected
   bool in_graph; // 1 if in a graph's nodelist
   int in_stack; // Number of times node appears in stack; dont garbage coll
   int in_morphism; // Number of times node appears in morphisms
} Node;

extern struct Node dummy_node;

typedef struct RootNodes {
   Node *node;
   struct RootNodes *next;
} RootNodes;

typedef struct Edge {
   int index;
   HostLabel label;
   Node *source, *target;
   bool deleted; // 1 if going to be garbage-collected
   bool in_graph; // 1 if in a graph's edgelist
   int in_stack; // Number of times edge appears in stack; dont garbage coll
   int in_morphism; // Number of times edge appears in morphisms
   bool in_srclst;  // Flags for if still in src/trg edge lists
   bool in_trglst;
} Edge;

extern struct Edge dummy_edge;

/* Nodes and edges are created and added to the graph with the addNode and addEdge
 * functions. They take the necessary construction data as their arguments and 
 * return their index in the graph. */

Node *addNode(Graph *graph, bool root, HostLabel label);
void insertNode(Graph *graph, Node *node);
void addRootNode(Graph *graph, Node *node);
Edge *addEdge(Graph *graph, HostLabel label, Node *source, Node *target);
void insertEdge(Graph *graph, Edge *edge);
void removeNode(Graph *graph, Node *node);
void removeRootNode(Graph *graph, Node *node);
void removeEdge(Graph *graph, Edge *edge);
void relabelNode(Node *node, HostLabel new_label);
void changeNodeMark(Node *node, MarkType new_mark);
void changeRoot(Graph *graph, Node *node);
void relabelEdge(Edge *edge, HostLabel new_label);
void changeEdgeMark(Edge *edge, MarkType new_mark);

// Try and free a node/edge's memory, fixing all references.
// If the node/edge is still needed anywhere, do nothing.
void tryGarbageCollectNode(Graph *graph, Node *node);
void tryGarbageCollectEdge(Graph *graph, Edge *edge);

/* ========================
 * Graph Querying Functions
 * ======================== */

// Given the current position in the list of nodes/edges,
// yield the next element in the list.
// Done this way so deleted nodes/edges are garbage
// collected when passed by.
Node *yieldNextNode(Graph *graph, NodeList **current);
Edge *yieldNextEdge(Graph *graph, EdgeList **current);
Edge *yieldNextOutEdge(Node *node, EdgeList **current);
Edge *yieldNextInEdge(Node *node, EdgeList **current);

RootNodes *getRootNodeList(Graph *graph);

Node *getSource(Edge *edge); 
Node *getTarget(Edge *edge);
HostLabel getNodeLabel(Node *node);
HostLabel getEdgeLabel(Edge *edge); 
int getIndegree(Node *node);
int getOutdegree(Node *node);

void printGraph(Graph *graph, FILE *file);
void freeGraph(Graph *graph);

#endif /* INC_GRAPH_H */
