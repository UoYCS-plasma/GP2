/* ///////////////////////////////////////////////////////////////////////////

  ============
  Graph Module
  ============
                             
  An API for GP2 graphs. Defines structures for graphs, nodes, edges, label
  class tables and functions that operate on these structures.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GRAPH_H
#define INC_GRAPH_H

#include "error.h"
#include "globals.h"
#include "label.h"

/* There are 7 marks, but the 'any' mark does not occur in host graphs. */
#define NUMBER_OF_MARKS 6 
#define NUMBER_OF_CLASSES 7

/* ================================
 * Graph Data Structure + Functions
 * ================================ */
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

   /* Flag to prevent graph operations from manipulating LabelClassTables in
    * rule graphs. Set to true for host graphs and false for rule graphs.
    * This is temporary - I intend to separate host and rule graphs in the 
    * future. */
   bool classes;
   struct LabelClassTable *node_classes, *edge_classes;
} Graph;

/* The arguments nodes and edges are the initial sizes of the node array and the
 * edge array respectively. */
Graph *newGraph(int nodes, int edges);

/* Nodes and edges are created and added to the graph with the addNode and addEdge
 * functions. They take the necessary construction data as their arguments and 
 * return their index in the graph. */
int addNode(Graph *graph, bool root, Label label);
void addRootNode(Graph *graph, int index);
int addEdge(Graph *graph, bool bidirectional, Label label, int source_index, 
            int target_index);
void removeNode(Graph *graph, int index, bool free_label);
void removeRootNode(Graph *graph, int index);
void removeEdge(Graph *graph, int index, bool free_label);
void relabelNode(Graph *graph, int index, Label new_label, bool free_label);
void changeRoot(Graph *graph, int index);
void relabelEdge(Graph *graph, int index, Label new_label, bool free_label);
void changeBidirectional(Graph *graph, int index);

/* =========================
 * Node and Edge Definitions
 * ========================= */
typedef struct Node {
   int index;
   bool root;
   Label label;

   /* Dynamic integer arrays for the node's outgoing and incoming edges.
    * To iterate over these arrays, use out_index and in_index as the
    * upper bound. */
   int *out_edges, out_index, *in_edges, in_index;
   
   /* The size of the out_edges and in_edges arrays respectively. */
   int out_pool_size, in_pool_size;

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

typedef struct RootNodes {
   int index;
   struct RootNodes *next;
} RootNodes;

typedef struct Edge {
   int index;
   bool bidirectional;
   Label label;
   int source, target;
   /* The index of the edge in its label class table. Used to quickly remove
    * the entry from the potentially large table. */
   int label_table_index;
} Edge;

extern struct Edge dummy_edge;

/* ========================================
 * Label Class Table Definition + Functions
 * ========================================
 * A linked list of structures storing a collection of node/edge indices 
 * with a certain mark and label class.
 * <pool_size> is the number of indices allocated to <items> array. Its
 * initial value is 4, which is doubled on reallocations.
 * <index> is the smallest unassigned index of <items>. 
 * <items> is a dynamic array storing these indices. */
typedef struct LabelClassTable {
   MarkType mark;
   LabelClass label_class;
   int pool_size;
   int index;
   int *items;
   struct LabelClassTable *next;
} LabelClassTable;

/* Argument 1: The graph.
 * Argument 2: Flag to inform the function of which LabelClassTable to access.
 * Argument 3: The label of the item to be added. Used to obtain the mark and
 *             label class.
 * Argument 4: For addLabelClassIndex, the index of the node or edge.
 *             For removeLabelClassIndex, the index of the node or edge in its
 *             table (node/edge->label_table_index). */
void addLabelClassIndex(Graph *graph, bool node, Label label, int index);
void removeLabelClassIndex(Graph *graph, bool node, Label label, int item_index);
LabelClassTable *copyLabelClassTable(LabelClassTable *table);
void freeLabelClassTable(LabelClassTable *table);

/* ========================
 * Graph Querying Functions
 * ======================== */
Node *getNode(Graph *graph, int index);
Edge *getEdge(Graph *graph, int index);
RootNodes *getRootNodeList(Graph *graph);
/* The following four functions return indices. To get pointers to the items,
 * pass the return value to getNode or getEdge with the appropriate graph. */
int getInEdge(Node *node, int index);
int getOutEdge(Node *node, int index);
int getSource(Edge *edge);
int getTarget(Edge *edge);
Label getNodeLabel(Graph *graph, int index);
Label getEdgeLabel(Graph *graph, int index); 
int getIndegree(Graph *graph, int index);
int getOutdegree(Graph *graph, int index);
LabelClassTable *getNodeLabelTable(Graph *graph, MarkType mark, LabelClass label_class); 
LabelClassTable *getEdgeLabelTable(Graph *graph, MarkType mark, LabelClass label_class); 

void printGraph(Graph *graph, FILE *file);
void freeGraph(Graph *graph);

#endif /* INC_GRAPH_H */
