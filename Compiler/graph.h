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

typedef struct IntArray {
   int capacity;
   int size;
   int *items;
} IntArray;

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

   /* Flag to switch the label class indexing on and off. */
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
int addEdge(Graph *graph, Label label, int source_index, int target_index);
void removeNode(Graph *graph, int index, bool free_label);
void removeRootNode(Graph *graph, int index);
void removeEdge(Graph *graph, int index, bool free_label);
void relabelNode(Graph *graph, int index, Label new_label, bool free_label);
void changeRoot(Graph *graph, int index);
void relabelEdge(Graph *graph, int index, Label new_label, bool free_label);

/* =========================
 * Node and Edge Definitions
 * ========================= */
typedef struct Node {
   int index;
   bool root;
   Label label;

   int first_out_edge, second_out_edge;
   int first_in_edge, second_in_edge;

   /* Dynamic integer arrays for the node's outgoing and incoming edges. */
   IntArray out_edges, in_edges;

   int outdegree, indegree;

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
Label getNodeLabel(Graph *graph, int index);
Label getEdgeLabel(Graph *graph, int index); 
int getIndegree(Graph *graph, int index);
int getOutdegree(Graph *graph, int index);
LabelClassTable *getNodeLabelTable(Graph *graph, MarkType mark, LabelClass label_class); 
LabelClassTable *getEdgeLabelTable(Graph *graph, MarkType mark, LabelClass label_class); 

void printGraph(Graph *graph, FILE *file);
void freeGraph(Graph *graph);

#endif /* INC_GRAPH_H */
