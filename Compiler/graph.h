/* ///////////////////////////////////////////////////////////////////////////

  ============
  Graph Module
  ============
                             
  An API for GP2 graphs. Defines structures for graphs, nodes and edges
  and functions that operate on these structures.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GRAPH_H
#define INC_GRAPH_H

#define MAX_INCIDENT_EDGES 16

#include "error.h"
#include "globals.h"
#include "label.h"
#include "labelclass.h"

typedef struct RootNodes {
   int index;
   struct RootNodes *next;
} RootNodes;

typedef struct Graph 
{
   struct Node *nodes;
   int node_pool_size, node_index;

   struct Edge *edges;
   int edge_pool_size, edge_index;

   /* Integer arrays to record the holes in the node and edge arrays caused by
    * node and edge deletion. The first free_node_index elements of the 
    * free node slots array are indices of holes in the graph's node array.
    * When a node is added to the graph, the free node slots array is consulted
    * first, since filling a hole is better than inserting a node at the end
    * of the array in which gaps exist. */
   int *free_node_slots, *free_edge_slots;
   int free_node_index, free_edge_index;

   /* The number of non-dummy items in the graph's nodes/edges array.
    * Do NOT use these as a bound for an iterator over the arrays. Instead use
    * node_index and edge_index. 
    * The equations below are invariant properties of this data structure.
    * number_of_nodes + free_node_index = node_index. 
    * number_of_edges + free_edge_index = edge_index. */
   int number_of_nodes, number_of_edges;
   
   /* Root nodes referenced in a linked list for fast access. */
   struct RootNodes *root_nodes;
} Graph;

void addRootNode(Graph *graph, int index);
void removeRootNode(Graph *graph, int index);

typedef struct Node {
   int index;
   bool root;
   LabelClass label_class;
   Label label;

   /* Fixed-size arrays for the node's outgoing and incoming edges. */
   int out_edges[MAX_INCIDENT_EDGES];
   int in_edges[MAX_INCIDENT_EDGES];
   
   /* Pointers to extra incident edge index storage in case the array's
    * bounds are exceeded. Initially, these are NULL pointers. */
   int *extra_out_edges, *extra_in_edges;
   /* The size of the extra_out_edges and extra_in_edges arrays respectively. */
   int out_pool_size, in_pool_size;

   /* If extra edge arrays have been allocated, you must subtract
    * MAX_INCIDENT_EDGES from this number to get the correct index into the
    * extra edge array. */
   int out_index, in_index;

   /* Bidirectional edges, and hence bidegrees, exist only in rule graphs.
    * A bidirectional edge is internally represented as either a single outedge
    * or a single inedge, but it contributes only to the node's bidegree. In
    * other words, adding a bidirectional edge increments the bidegree but does
    * not change the indegree or the outdegree.
    *
    * For host graphs, and for rule graphs with bidegree 0, the out(in)degree
    * is the number of non-negative indices in the node's out(in)edge arrays.
    * For rule graphs with bidegree > 0, the invariant is less strict, since 
    * a bidirectional edge may lie in either the outedge array or the inedge
    * array. All that can be said for certain is that the sum of the three
    * degrees is the number of non-negative indices in all of the node's edge
    * arrays. */
   int outdegree, indegree, bidegree;

   /* The index of the node in its label class table. Used to quickly remove
    * the entry from the potentially large table. */
   int label_table_index;
} Node;

extern struct Node dummy_node;

typedef struct Edge {
   int index;
   bool bidirectional;
   LabelClass label_class;
   Label label;
   int source, target;
   /* The index of the edge in its label class table. Used to quickly remove
    * the entry from the potentially large table. */
   int label_table_index;
} Edge;

extern struct Edge dummy_edge;

/* The arguments nodes and edges are the initial sizes of the node array and the
 * edge array respectively. */
Graph *newGraph(int nodes, int edges);

/* Nodes and edges are created and added to the graph with the addNode and addEdge
 * functions. They take the necessary construction data as their arguments and 
 * return their index in the graph. 
 *
 * To assign the empty label to a node or edge, pass NULL as the label 
 * argument. This also applies to the relabelling functions. */
int addNode(Graph *graph, bool root, Label label);
int addEdge(Graph *graph, bool bidirectional, Label label, int source_index, 
            int target_index);
void removeNode(Graph *graph, int index);
void removeEdge(Graph *graph, int index);

void relabelNode(Graph *graph, int index, Label new_label);
void changeRoot(Graph *graph, int index);
void relabelEdge(Graph *graph, int index, Label new_label);
void changeBidirectional(Graph *graph, int index);

Node *getNode(Graph *graph, int index);
Edge *getEdge(Graph *graph, int index);
RootNodes *getRootNodeList(Graph *graph);
/* The following four functions return indices. To get pointers to the items,
 * pass the return value to getNode or getEdge with the appropriate graph. */
int getInEdge(Node *node, int index);
int getOutEdge(Node *node, int index);
int getSource(Edge *edge);
int getTarget(Edge *edge);
Label getNodeLabel(Node *node);
Label getEdgeLabel(Edge *edge);
int getIndegree(Node *node);
int getOutdegree(Node *node);

void printGraph(Graph *graph, FILE *file);
void freeGraph(Graph *graph);

#endif /* INC_GRAPH_H */
