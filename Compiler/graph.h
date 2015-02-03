/* ///////////////////////////////////////////////////////////////////////////

  ============
  Graph Module
  ============
                             
  An API for GP2 graphs. Defines structures for graphs, nodes and edges.  

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GRAPH_H
#define INC_GRAPH_H

#define MAX_NODES 64
#define MAX_EDGES 128
#define MAX_INCIDENT_EDGES 16

#include <glib.h>
#include "error.h"
#include "globals.h"
#include "label.h"
#include "stack.h"

typedef struct Graph 
{
   /* The primary reference for a graph's nodes and edges. Indices for each 
    * node and edge remain constant. next_node_index and next_edge_index
    * always store the smallest free index in the array.
    * TODO: bounds checking. */
   struct Node **nodes;
   struct Edge **edges;
   
   /* Keeps track of holes in the arrays when a node or edge is removed. */
   Stack *free_node_slots;
   Stack *free_edge_slots;

   /* These variables refer to the indices one entry beyond the furthest
    * slot containing a live pointer. Items are added to this index if
    * the free slots stack is empty. To be used as the termination index
    * for code that iterates over the arrays. */
   int next_node_index;
   int next_edge_index;

   /* The number of live pointers in the graph's nodes/edges array.
    * Do NOT use these as a bound for an iterator over the arrays. Instead use
    * next_node/edge_index. 
    * number_of_nodes + size(free_node_slots) = next_node_index. 
    * number_of_edges + size(free_edge_slots) = next_edge_index. */
   int number_of_nodes;
   int number_of_edges;

   /* Nodes and edges indexed by label class. Used to quickly obtain candidate
    * items when matching a rule. */
   GHashTable *nodes_by_label;
   GHashTable *edges_by_label;

   /* Root nodes referenced in a linked list for fast access. */
   GSList *root_nodes;
} Graph;

typedef struct Node {
   int index;
   bool root;
   LabelClass label_class;
   Label *label;

   /* TODO: Check for overflow! */
   /* Be careful when iterating over these arrays. No free list is maintained,
    * so the arrays contain NULL pointers, created when an edge is removed that
    * is not at the last index. */
   struct Edge **out_edges;
   struct Edge **in_edges;

   /* These variables refer to the indices one entry beyond the furthest
    * slot containing a live pointer. Items are added to this index if
    * the free slots stack is empty. To be used as the termination index
    * for code that iterates over the arrays. */
   int next_out_edge_index;
   int next_in_edge_index;

   /* The number of live pointers in the node's inedges/outedges array.
    * Do NOT use these as a bound for an iterator over the arrays. Instead use
    * next_out/in_edge_index.
    * indegree + size(free_in_edge_slots) = next_in_edge_index. 
    * outdegree + size(free_out_edge_slots) = next_out_edge_index. */
   int indegree;
   int outdegree;
} Node;


typedef struct Edge {
   int index;
   bool bidirectional;
   LabelClass label_class;
   Label *label;
   struct Node *source;
   struct Node *target;
} Edge;


/* Graph operations
 * ================
 * Nodes and edges are created with the newNode and newEdge functions that take
 * the necessary construction data as their arguments. newNode and newEdge use 
 * getLabelClass to generate label classes. The returned pointers can then be
 * added to a graph with the addNode and addEdge functions.
 * 
 * The relabel functions take boolean arguments to control if the label is 
 * updated and if boolean flag of the item should be changed. For nodes, this 
 * is the root flag. For edges, this is the bidirectional flag.
 *
 * To assign the global Label blank_label to a node or edge, pass the NULL
 * pointer as the Label * argument of newNode, newEdge, relabelNode or 
 * relabelEdge. 
 */

/* Creates an empty graph. */
Graph *newGraph(void);
Node *newNode(bool root, Label *label);
Edge *newEdge(bool bidirectional, Label *label, Node *source, 
              Node *target);
void addNode(Graph *graph, Node *node); 
void addEdge(Graph *graph, Edge *edge);
void removeNode(Graph *graph, int index);
void removeEdge(Graph *graph, int index);
void relabelNode(Graph *graph, Node *node, Label *new_label, bool change_label, 
                 bool change_root); 
void relabelEdge(Graph *graph, Edge *edge, Label *new_label, bool change_label, 
                 bool change_bidirectional);


/* Graph backtracking 
 * ================== */
extern Stack *graph_stack;

/* Creates a complete copy of the passed graph and pushes it to the global
 * graph_stack. */
void copyGraph(Graph *graph);

/* restoreGraph is passed the current graph and frees it before returning
 * the top graph from the graph stack. */
Graph *restoreGraph(Graph *graph);

void freeGraphStack(Stack *graph_stack);

/* Graph querying
 * ============== */
Node *getNode(Graph *graph, int index);
Edge *getEdge(Graph *graph, int index);
GSList *getRootNodes(Graph *graph);
GSList *getNodesByLabel(Graph *graph, LabelClass label_class);
GSList *getEdgesByLabel(Graph *graph, LabelClass label_class);
Edge *getInEdge(Node *node, int index);
Edge *getOutEdge(Node *node, int index);
Node *getSource(Edge *edge);
Node *getTarget(Edge *edge);
Label *getNodeLabel(Node *node);
Label *getEdgeLabel(Edge *edge);
int getIndegree(Node *node);
int getOutdegree(Node *node);

/* Printing and freeing functions 
 * ============================== */
void printGraph(Graph *graph);
void printVerboseGraph(Graph *graph);
void printVerboseNode(Node *node);
void printVerboseEdge(Edge *edge);

/* The node and edge structures are freed in freeGraph when the graph's node 
 * and edge pointer arrays are freed. 
 * It follows that the freeing of the graph's other node/edge accessing 
 * structures (hash tables in the graph, pointer arrays in the nodes, pointers
 * in the edges) should only free any supporting structures and not the
 * node/edge structures themselves.
 */     
void freeGraph(Graph *graph);
void freeNode(Node *node);
void freeEdge(Edge *edge);
/* A wrapper for g_slist_free so that it can be called by g_hash_table_foreach
 * when freeing the hash tables indexed by label class. */
void freeGSList(gpointer key, gpointer value, gpointer data); 

/* structs placed on the graph change stack. It contains sufficient
 * information to undo any of the six types of graph change.
 *
 * Add Node + Add Edge
 * ===================
 * The undo operations are removeNode and removeEdge which only require the 
 * name of the item's since the pointer can be quickly accessed via a hash
 * lookup. Storing the pointer to the item itself is dangerous because the 
 * added item could be deleted before restoreGraph is called.
 *
 * Remove Node + Remove Edge
 * =========================
 * The undo operations are addNode and addEdge. The structs contain the 
 * arguments to these functions. In the case of addEdge, called with pointers
 * to its source and target, these are obtained by hasing the names in
 * the GraphChange removed_edge struct. The label pointers are heap copies
 * since labels are freed when an item is deleted.
 *
 * Relabel Node + Relabel Edge
 * ===========================
 * As above, the structs contain the arguments to the relabelNode and 
 * relabelEdge functions. Labels do not have to be copied to heap in this
 * case as the pointers to the old labels of the items are used.
 */

/* typedef enum { ADD_NODE = 0, ADD_EDGE, REMOVE_NODE, REMOVE_EDGE, 
	       RELABEL_NODE, RELABEL_EDGE } GraphChangeType; 

typedef struct GraphChange 
{
   GraphChangeType type;
   union
   {
      int added_node; 
      int added_edge;

      struct {
         bool root;
         Label *label;
      } removed_node;

      struct {
         Label *label;
         bool bidirectional;
         int source_name;
         int target_name;
      } removed_edge;

      struct {
         int index;
         Label *old_label;
         bool change_root;
      } relabelled_node;
   
      struct {
         int index;
         Label *old_label;
         bool change_bidirectional;
      } relabelled_edge;
   } data;
} GraphChange; 
void freeGraphChange(GraphChange *change); */

#endif /* INC_GRAPH_H */
