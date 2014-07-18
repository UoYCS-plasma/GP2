/* ///////////////////////////////////////////////////////////////////////////

  ================================
  graph.h - Chris Bak (14/07/2014)
  ================================
                             
  Module for defining the graph data structure and its operations.

  Version 1 (14/07/14) - Initial implementation based on my design notes.

/////////////////////////////////////////////////////////////////////////// */


/* I will use glib structures to map out the initial structure, although I
 * might change to Judy arrays or something else when the proper implementation
 * begins. To start with, I will use Greg Manning's structure for GP1.
 */

#include "ast.h"
#include <glib.h>

/* Classes of GP 2 labels for querying by label. The first four values refer
 * to singleton lists containing a value/variable of that type; the remaining
 * values refer to lists of a particular length. The limit can be adjusted
 * as necessary.
 */
typedef enum {EMPTY=0, CHAR_L, STR_L, INT_L, ATOM_L, LIST2_L, LIST3_L, LIST4_L,
              LIST5_L} LabelClass;

/* Invariants on graphs:
 * (1) A graph with N nodes has assigned node IDs 1,...,N. next_node_id is N+1.
 * (2) A graph with E edges has assigned edge IDs 1,...,N. next_edge_id is E+1.
 * (3) All edges in a node's incoming/outgoing edge list are in the graph's
 *     edges_by_label table.
 * (4) An edge's source and target node are in the graph's nodes_by_label table.
 * Never mind those first two. Won't be ideal for removing nodes.
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


typedef struct Node {
   /* Index in the node array */
   int index;
   string name;
   bool root;
   LabelClass label_class;
   MarkType mark; /* MarkType defined in ast.h */
   GList *list; 
   int indegree;
   int outdegree;
   GHashTable *in_edges_by_label;
   GHashTable *out_edges_by_label;
} Node;


typedef struct Edge {
   /* Index in the edge array */
   int index;
   string name;
   bool bidirectional;
   LabelClass label_class;
   MarkType mark; /* MarkType defined in ast.h */
   GList *list;
   Node *source;
   Node *target;
} Edge;


typedef struct ListElement {
   AtomExpType type;		  /* From ast.h */
   union {
    string name;		  /* VARIABLE */
    int number; 	 	  /* INT_CONSTANT */
    string string;		  /* CHARACTER_CONSTANT, STRING_CONSTANT */
    string node_id; 		  /* INDEGREE, OUTDEGREE */
    GList *list_arg;	 	  /* LIST_LENGTH */
    struct ListElement *str_arg;  /* STRING_LENGTH */
    struct ListElement *exp; 	  /* NEG */
    struct { 
      struct ListElement *left_exp;
      struct ListElement *right_exp;
    } bin_op; 		   	 /* ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT */
  } value;

} ListElement;
   

/* Graph utility functions */

Graph *newGraph(void);
void addNode(Graph *graph, Node *node, int index); 
void addEdge(Graph *graph, Edge *edge, int index);
void removeNode(Graph *graph, int index);
void removeEdge(Graph *graph, int index);
void relabelNode(Graph *graph, int index, GList *new_label); 
void relabelEdge(Graph *graph, int index, GList *new_label); 

void printGraph (Graph *graph);
void printNode (gpointer data, gpointer user_data);
void printEdge (gpointer data, gpointer user_data);
void printListElement(gpointer data, gpointer user_data);
void printEdgeData(gpointer key, gpointer value, gpointer user_data);

void freeGraph (Graph *graph);
void freeNode (void *p);
void freeEdge (void *p);
void freeListElement(void *p);
void freeGSList(gpointer key, gpointer value, gpointer data); 

/* Graph querying functions */

GSList *getNodes(Graph *graph, LabelClass label_class);
GSList *getEdges(Graph *graph, LabelClass label_class);
GSList *getInEdges(Node *node, LabelClass label_class);
GSList *getOutEdges(Node *node, LabelClass label_class);
GSList *getSource(Edge *edge);
GSList *getTarget(Edge *edge);
GList *getNodeLabel(Node *node);
GList *getEdgeLabel(Edge *edge);
int getIndegree (Node *node);
int getOutdegree (Node *node);
