/* ///////////////////////////////////////////////////////////////////////////

  ================================
  graph.h - Chris Bak (14/07/2014)
  ================================
                             
  Module for defining the graph data structure and its operations.

  Version 1 (14/07/14) - Initial implementation based on my design notes.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GRAPH_H
#define INC_GRAPH_H

#include "ast.h"

/* Invariants on graphs:
 * (1) A graph with N nodes has assigned node indexes 1,...,N. next_node_index
 *     is N+1.
 * (2) A graph with E edges has assigned edge indexes 1,...,N. next_edge_index
 *     is E+1.
 * (3) All edges in a node's incoming/outgoing edge list are in the graph's
 *     edges_by_label table.
 * (4) An edge's source and target node are in the graph's nodes_by_label table.
 */

typedef struct Graph {
   /* These indices are kept in sync with the GPtrArrays. */
   int next_node_index; 
   int next_edge_index;

   /* Nodes and edges indexed by IDs. Used to quickly access an item after
    * looking it up via the label hashtables. All graph modification operations
    * should be done via these arrays. I think. */
   GPtrArray *nodes;
   GPtrArray *edges;

   /* Nodes and edges indexed by label. Used to quickly obtain candidate
    * items when matching a rule. */
   GHashTable *nodes_by_label;
   GHashTable *edges_by_label;

   /* Singly linked list to quickly look up root nodes. */
   GSList *root_nodes;
} Graph;


typedef struct Label {
   MarkType mark; /* MarkType defined in ast.h */
   GList *list; /* The empty list is represented by a GList with one element with
                 * type EMPTY. */
} Label;

/* Classes of GP 2 labels for querying by label. The first four values refer
 * to singleton lists containing a value/variable of that type; the remaining
 * values refer to lists of a particular length. The limit can be adjusted
 * as necessary.
 */
typedef enum {EMPTY_L=0, INT_L, CHAR_L, STR_L, ATOM_L, LIST2_L, LIST3_L, LIST4_L,
              LIST5_L} LabelClass;

typedef struct Node {
   /* Index in the node array */
   int index;
   string name;
   bool root;
   LabelClass label_class;
   Label label; 
   int indegree;
   int outdegree;
   GSList *in_edges;
   GSList *out_edges;
   GHashTable *in_edges_by_label;
   GHashTable *out_edges_by_label;
} Node;


typedef struct Edge {
   /* Index in the edge array */
   int index;
   string name;
   bool bidirectional;
   LabelClass label_class;
   Label label;
   Node *source;
   Node *target;
} Edge;


typedef struct ListElement {
   AtomExpType type;		  /* From ast.h */
   union {
    string name;		  /* VARIABLE */
    int number; 	 	  /* INTEGER_CONSTANT */
    string string;		  /* CHARACTER_CONSTANT, STRING_CONSTANT */
    string node_id; 		  /* INDEGREE, OUTDEGREE */
    GList *list_arg;	 	  /* LIST_LENGTH */
    struct ListElement *str_arg;  /* STRING_LENGTH */
    struct ListElement *exp; 	  /* NEG */
    struct { 
      struct ListElement *left_exp;
      struct ListElement *right_exp;
    } bin_op; 		   	  /* ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT */
    /* There is also the EMPTY type which has no value. */
  } value;

} ListElement;
   

/* Graph utility functions
 * =======================
 * The add functions take a graph and a pointer to the item to add. Nodes
 * and edges are created with the newNode and newEdge functions which
 * take the item's name, a boolean flag and its label.
 * 
 * The remove and relabel functions take a graph and the index in the graph's 
 * node or edge array of the item to be modified. Each node and edge stores 
 * its own index so that the function can be called when the caller has access
 * to the node/edge pointer.
 */

/* Creates an empty graph and returns a pointer to it. */
Graph *newGraph(void);
Node *newNode(string name, bool root, Label label);
/* Not currently sure how to get the source and target pointers when traversing
 * the AST. */
Edge *newEdge(string name, bool bidirectional, Label label, Node *source, 
             Node *target);
void addNode(Graph *graph, Node *node); 
void addEdge(Graph *graph, Edge *edge);
void removeNode(Graph *graph, Node *node);
void removeEdge(Graph *graph, Edge *edge);
void relabelNode(Graph *graph, Node *node, Label new_label, 
		 LabelClass new_label_class); 
void relabelEdge(Graph *graph, Edge *edge, Label new_label, 
		 LabelClass new_label_class); 

/* Graph querying functions 
 * ========================
 * The functions to get nodes and get edges take a label class.
 * The functions return the list of items with that label class,
 * drawn from the hashtables indexed by label classes.
 */

Node *getNode(Graph *graph, int index);
Edge *getEdge(Graph *graph, int index);
GSList *getRootNodes(Graph *graph);
GSList *getNodes(Graph *graph, LabelClass label_class);
GSList *getEdges(Graph *graph, LabelClass label_class);
GSList *getInEdges(Node *node);
GSList *getOutEdges(Node *node);
GSList *getInEdgesByLabel(Node *node, LabelClass label_class);
GSList *getOutEdgesByLabel(Node *node, LabelClass label_class);
Node *getSource(Edge *edge);
Node *getTarget(Edge *edge);
Label getNodeLabel(Node *node);
Label getEdgeLabel(Edge *edge);
int getIndegree (Node *node);
int getOutdegree (Node *node);


/* Printing and freeing functions */

void printGraph (Graph *graph);
void printNode (gpointer data, gpointer user_data);
void printEdge (gpointer data, gpointer user_data);
void printList(GList *list);
void printListElement(ListElement* elem);
/* printEdgeData is used to print a node's incoming/outgoing edge
 * table. It does not print the entire edge: just its index and name.
 */
void printEdgeData(gpointer data, gpointer user_data);

void freeGraph (Graph *graph);
void freeNode (void *p);
void freeEdge (void *p);
void freeListElement(void *p);
/* A wrapper for g_slist_free so that it can be called by g_hash_table_foreach.
 */
void freeGSList(gpointer key, gpointer value, gpointer data); 

#endif /* INC_GRAPH_H */
