/* ///////////////////////////////////////////////////////////////////////////

  Copyright 2015-2016 Christopher Bak

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

IntArray makeIntArray(int initial_capacity);
void addToIntArray(IntArray *array, int item);
void removeFromIntArray(IntArray *array, int index);

typedef struct NodeArray {
   int capacity;
   int size;
   struct Node *items;
   struct IntArray holes;
} NodeArray;

typedef struct EdgeArray {
   int capacity;
   int size;
   struct Edge *items;
   struct IntArray holes;
} EdgeArray;

/* ================================
 * Graph Data Structure + Functions
 * ================================ */
typedef struct Graph 
{
   NodeArray nodes;
   EdgeArray edges;
   /* The number of non-dummy items in the graph's nodes/edges array.
    * Do NOT use these as an iteration index over the arrays because the items
    * may not be stored contiguously in the array. Instead use nodes.size and
    * edges.size. 
    * The equations below are invariant properties of this data structure.
    * number_of_nodes + node_holes.size = nodes.size. 
    * number_of_edges + edge_holes.size = edges.size.
    * In words, each of the first nodes.size items of the node array is either
    * a dummy node (a hole created by the removal of a node), or a valid node. */
   int number_of_nodes, number_of_edges;
   
   /* Root nodes referenced in a linked list for fast access. */
   struct RootNodes *root_nodes;
} Graph;

/* The arguments nodes and edges are the initial sizes of the node array and the
 * edge array respectively. */
Graph *newGraph(int nodes, int edges);

/* Nodes and edges are created and added to the graph with the addNode and addEdge
 * functions. They take the necessary construction data as their arguments and 
 * return their index in the graph. */
int addNode(Graph *graph, bool root, HostLabel label);
void addRootNode(Graph *graph, int index);
int addEdge(Graph *graph, HostLabel label, int source_index, int target_index);
void removeNode(Graph *graph, int index);
void removeRootNode(Graph *graph, int index);
void removeEdge(Graph *graph, int index);
void relabelNode(Graph *graph, int index, HostLabel new_label);
void changeNodeMark(Graph *graph, int index, MarkType new_mark);
void changeRoot(Graph *graph, int index);
void resetMatchedNodeFlag(Graph *graph, int index);
void relabelEdge(Graph *graph, int index, HostLabel new_label);
void changeEdgeMark(Graph *graph, int index, MarkType new_mark);
void resetMatchedEdgeFlag(Graph *graph, int index);

/* =========================
 * Node and Edge Definitions
 * ========================= */
typedef struct Node {
   int index;
   bool root;
   HostLabel label;
   int outdegree, indegree;
   int first_out_edge, second_out_edge;
   int first_in_edge, second_in_edge;
   /* Dynamic integer arrays for the node's outgoing and incoming edges. */
   IntArray out_edges, in_edges;
   bool matched;
} Node;

extern struct Node dummy_node;

typedef struct RootNodes {
   int index;
   struct RootNodes *next;
} RootNodes;

typedef struct Edge {
   int index;
   HostLabel label;
   int source, target;
   bool matched;
} Edge;

extern struct Edge dummy_edge;

/* ========================
 * Graph Querying Functions
 * ======================== */
Node *getNode(Graph *graph, int index);
Edge *getEdge(Graph *graph, int index);
RootNodes *getRootNodeList(Graph *graph);

/* Called with a positive integer n. The node structures store two outedge indices
 * and two inedge indices. More incident edges are placed in a dynamic array.
 * Pass n = 0 to get the node's first incident edge.
 * Pass n = 1 to get the node's second incident edge.
 * Pass n >= 2 to get the (n-2)th incident edge in the appropriate array. 
 * Designed for iteration e.g. 
 * for(i = 0; i < n->out_edges.size + 2; i++) getNthOutEdge(g, n, i); 
 * I'm sure there's a nicer way to do this... */
Edge *getNthOutEdge(Graph *graph, Node *node, int n);
Edge *getNthInEdge(Graph *graph, Node *node, int n);
Node *getSource(Graph *graph, Edge *edge); 
Node *getTarget(Graph *graph, Edge *edge);
HostLabel getNodeLabel(Graph *graph, int index);
HostLabel getEdgeLabel(Graph *graph, int index); 
int getIndegree(Graph *graph, int index);
int getOutdegree(Graph *graph, int index);

void printGraph(Graph *graph, FILE *file);
void freeGraph(Graph *graph);

#endif /* INC_GRAPH_H */
