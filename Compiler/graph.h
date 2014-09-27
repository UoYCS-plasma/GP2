/* ///////////////////////////////////////////////////////////////////////////

  ================================
  graph.h - Chris Bak (14/07/2014)
  ================================
                             
  Module for defining the graph data structure and its operations.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GRAPH_H
#define INC_GRAPH_H

#include "globals.h"
#include "stack.h"

typedef struct Graph 
{
   /* The primary reference for a graph's nodes and edges. The hash tables are
    * indexed by their textual identifier according to the graph defined in
    * the graphical editor. This allows items to be uniquely identified and
    * accessed quickly by their ID. These hash tables and its associated
    * functions take sole responsibility for freeing nodes and edges. */
   GHashTable *nodes;
   GHashTable *edges;

   /* Nodes and edges indexed by label class. Used to quickly obtain candidate
    * items when matching a rule. */
   GHashTable *nodes_by_label;
   GHashTable *edges_by_label;

   /* Root nodes references in a linked list for fast access, assuming a
    * small number of root nodes. */
   GSList *root_nodes;

   Stack *graph_change_stack;
} Graph;


typedef struct Label {
   MarkType mark; /* globals.h */
   GList *list; /* The empty list is represented by a GList with one element with
                 * type EMPTY. It has length 0 in the Label struct. */
   /* Metadata set while the label is being constructed from the AST. */
   int list_length; 
   bool has_list_variable;
} Label;


/* Abstract data type for atomic expressions. 
typedef enum {EMPTY = 0, VARIABLE, INTEGER_CONSTANT, CHARACTER_CONSTANT,
              STRING_CONSTANT, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH,
              NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomExpType; */

typedef struct ListElement {
   AtomExpType type;		  /* globals.h */
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


/* Classes of GP 2 labels for querying by label. There is the empty list,
 * an integer constant, a string constant, a variable of atomic type, 
 * fixed-length lists of length 2 up to a fixed bound (here 5), and any list 
 * containing a list variable. */
typedef enum {EMPTY_L = 0, INT_L, STRING_L, ATOMIC_VAR_L, LIST2_L, LIST3_L,
              LIST4_L, LIST5_L, LISTVAR_L} LabelClass;

typedef struct Node {
   string name;
   bool root;
   LabelClass label_class;
   Label *label; 
   int indegree;
   int outdegree;
   GPtrArray *in_edges;
   GPtrArray *out_edges;
} Node;


typedef struct Edge {
   string name;
   bool bidirectional;
   LabelClass label_class;
   Label *label;
   Node *source;
   Node *target;
} Edge;


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

typedef enum { ADD_NODE = 0, ADD_EDGE, REMOVE_NODE, REMOVE_EDGE, 
	       RELABEL_NODE, RELABEL_EDGE } GraphChangeType; 

typedef struct GraphChange 
{
   GraphChangeType type;
   union
   {
      string added_node; 
      string added_edge;

      struct {
         string name;
         bool root;
         Label *label;
      } removed_node;

      struct {
         string name;
         Label *label;
         bool bidirectional;
         string source_name;
         string target_name;
      } removed_edge;

      struct {
         string name;
         Label *old_label;
         bool change_root;
      } relabelled_node;
   
      struct {
         string name;
         Label *old_label;
      } relabelled_edge;
   } data;
} GraphChange;


/* Graph utility functions
 * =======================
 * The add functions take a graph and a pointer to the item to add. Nodes
 * and edges are created with the newNode and newEdge functions which
 * take the necessary data to construct a node or an edge as their
 * arguments. newNode and newEdge use getLabelClass to generate label classes.
 * 
 * The relabelNode function has a boolean argument to specify if the root
 * status of the node should be changed. There are situations where the
 * root status is changed and the label remains the same, in which case
 * the third argument is NULL. In contrast, relabelEdge is only called
 * when the edge's label is modified, so its label argument should never
 * be NULL.
 *
 * The record flag is used if the graph update is to be added to the
 * graph change stack.  
 */

/* Creates an empty graph and returns a pointer to it. */
Graph *newGraph(void);

/* Tests the passed graph to see if it satisfies the data invariants. */
bool validGraph(Graph *graph);

LabelClass getLabelClass(Label *label);
Node *newNode(string name, bool root, Label *label);
Edge *newEdge(string name, bool bidirectional, Label *label, Node *source, 
             Node *target);
void addNode(Graph *graph, Node *node, bool record); 
void addEdge(Graph *graph, Edge *edge, bool record);
void removeNode(Graph *graph, Node *node, bool record);
void removeEdge(Graph *graph, Edge *edge, bool record);
void relabelNode(Graph *graph, Node *node, Label *new_label, bool change_root,
		 bool record); 
void relabelEdge(Graph *graph, Edge *edge, Label *new_label, bool record);

/* Returns a dynamically-allocated copy of the passed label. Used to
 * keep a pointer to old labels for graph backtracking. */
Label *copyLabel(Label *label);
ListElement *copyListElement(ListElement *atom);

/* restoreGraph pops the graph change stack and applies the appropriate undo
 * operation until the stack is empty. */
void restoreGraph (Graph *graph);



/* Graph querying functions 
 * ======================== */

Node *getNode(Graph *graph, string id);
Edge *getEdge(Graph *graph, string id);
GSList *getRootNodes(Graph *graph);
GSList *getNodesByLabel(Graph *graph, LabelClass label_class);
GSList *getEdgesByLabel(Graph *graph, LabelClass label_class);
Edge *getInEdge(Node *node, unsigned int index);
Edge *getOutEdge(Node *node, unsigned int index);
Node *getSource(Edge *edge);
Node *getTarget(Edge *edge);
Label *getNodeLabel(Node *node);
Label *getEdgeLabel(Edge *edge);
int getIndegree(Node *node);
int getOutdegree(Node *node);




/* Printing and freeing functions 
 * ============================== */

void printGraph (Graph *graph);
void printNodeFromHash (gpointer key, gpointer value, gpointer user_data);
void printNodeFromList (gpointer data, gpointer user_data);
void printEdge (gpointer key, gpointer value, gpointer user_data);
void printList(GList *list);
void printListElement(ListElement* elem);
/* printEdgeData is used to print a node's incoming/outgoing edge
 * table. It does not print the entire edge: just its index and name.
 */
void printEdgeData(gpointer data, gpointer user_data);

/* The node and edge structures are freed implicitly in freeGraph when the
 * graph's node and edge pointer arrays are freed. The glib function which 
 * frees the pointer arrays calls freeNode and freeEdge. Both of those 
 * functions use freeListElement to free their labels. 
 *
 * It follows that the freeing of the graph's other node/edge accessing 
 * structures (hash tables in the graph, pointer arrays in the nodes,
 * pointers in the edges) should only free any supporting structure
 * and not the node/edge pointers themselves.
 */     
void freeGraph (Graph *graph);

/* These functions take a void pointer because they are called by glib
 * functions. */
void freeNode (void *p);
void freeEdge (void *p);

void freeLabel(Label *label);

/* freeListElement is passed to g_list_free_full, so it takes a void pointer. */
void freeListElement(void *p);

/* A wrapper for g_slist_free so that it can be called by g_hash_table_foreach.
 */
void freeGSList(gpointer key, gpointer value, gpointer data); 

void freeGraphChange(GraphChange *change);

#endif /* INC_GRAPH_H */
