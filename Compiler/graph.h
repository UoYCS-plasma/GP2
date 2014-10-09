/* ///////////////////////////////////////////////////////////////////////////

  ================================
  graph.h - Chris Bak (14/07/2014)
  ================================
                             
  Module for defining the graph data structure and its operations.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GRAPH_H
#define INC_GRAPH_H

#define MAX_NODES 64
#define MAX_EDGES 128
#define MAX_INCIDENT_EDGES 16

#include "globals.h"
#include "stack.h"

typedef struct Graph 
{
   /* The primary reference for a graph's nodes and edges. Indices for each 
    * node and edge remain constant. next_node_index and next_edge_index
    * always store the smallest free index in the array.
    * TODO: bounds checking. */
   struct Node **nodes;
   struct Edge **edges;

   int next_node_index;
   int next_edge_index;

   /* Nodes and edges indexed by label class. Used to quickly obtain candidate
    * items when matching a rule. */
   GHashTable *nodes_by_label;
   GHashTable *edges_by_label;

   /* Root nodes referenced in a linked list for fast access. */
   GSList *root_nodes;
} Graph;

/* Abstract data type for GP2's marks defined in globals.h
 * typedef enum {NONE = 0, RED, GREEN, BLUE, GREY, DASHED, CYAN} MarkType; */

typedef struct Label {
   MarkType mark; 
   GList *list; /* The empty list is represented by a GList with one element with
                 * type EMPTY. It has length 0 in the Label struct. */
   /* Metadata set while the label is being constructed from the AST. */
   int list_length; 
   bool has_list_variable;
} Label;


/* Abstract data type for atomic expressions. From globals.h. 
typedef enum {EMPTY = 0, VARIABLE, INTEGER_CONSTANT, CHARACTER_CONSTANT,
              STRING_CONSTANT, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH,
              NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomExpType; */

typedef struct ListElement {
   AtomExpType type;		  
   /* The EMPTY type has no value. */
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
  } value;
} ListElement;


/* Classes of GP 2 labels for querying by label. This is a partition of the
 * set of all GP 2 labels. 
 * The label classes are the empty list, an integer constant, a string constant,
 * constant, a variable of atomic type (int, string, char or atom), 
 * fixed-length lists of length 2 up to a fixed bound, and any list containing 
 * a list variable. */
typedef enum {EMPTY_L = 0, INT_L, STRING_L, ATOMIC_VAR_L, LIST2_L, LIST3_L,
              LIST4_L, LIST5_L, LISTVAR_L} LabelClass;

typedef struct Node {
   int index;
   bool root;
   LabelClass label_class;
   Label *label; 
   /* Indegree and outdegree act as the next unused index of the
    * in- and out-edge arrays. */
   int indegree;
   int outdegree;
   /* TODO: Check for overflow! */
   struct Edge **out_edges;
   struct Edge **in_edges;
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
 * The relabel functions take a boolean argument to specify if the boolean flag
 * of the node should be changed. For nodes, this is the root flag. For edges,
 * this is the bidirectional flag. To modify the flag and not the label itself,
 * call the function with a NULL third argument.
 */

/* Creates an empty graph. */
Graph *newGraph(void);

/* Tests the passed graph to see if it satisfies the data invariants. */
bool validGraph(Graph *graph);

LabelClass getLabelClass(Label *label);
Node *newNode(bool root, Label *label);
Edge *newEdge(bool bidirectional, Label *label, Node *source, 
              Node *target);
void addNode(Graph *graph, Node *node); 
void addEdge(Graph *graph, Edge *edge);
void removeNode(Graph *graph, int index);
void removeEdge(Graph *graph, int index);
void relabelNode(Graph *graph, Node *node, Label *new_label, bool change_root); 
void relabelEdge(Graph *graph, Edge *edge, Label *new_label, 
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

/* Returns a dynamically-allocated copy of the passed label. */
Label *copyLabel(Label *label);
ListElement *copyListElement(ListElement *atom);


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
void printGraph (Graph *graph);
void printNode(Node *node);
void printEdge(Edge *edge);
void printList(GList *list);
void printListElement(ListElement* elem);

/* The node and edge structures are freed in freeGraph when the graph's node 
 * and edge pointer arrays are freed. 
 * It follows that the freeing of the graph's other node/edge accessing 
 * structures (hash tables in the graph, pointer arrays in the nodes, pointers
 * in the edges) should only free any supporting structures and not the
 * node/edge structures themselves.
 * freeGraph is a function passed to freeStack (via freeGraphStack), so it 
 * takes a void pointer.
 */     
void freeGraph(void *graph_pointer);
void freeNode(Node *node);
void freeEdge(Edge *edge);

/* A wrapper for g_slist_free so that it can be called by g_hash_table_foreach
 * when freeing the hash tables indexed by label class. */
void freeGSList(gpointer key, gpointer value, gpointer data); 

void freeLabel(Label *label);

/* freeListElement is passed to g_list_free_full, so it takes a void pointer. */
void freeListElement(void *atom);


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
