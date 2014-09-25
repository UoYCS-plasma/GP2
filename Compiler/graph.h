/* ///////////////////////////////////////////////////////////////////////////

  ================================
  graph.h - Chris Bak (14/07/2014)
  ================================
                             
  Module for defining the graph data structure and its operations.

  Version 1 (14/07/14) - Initial implementation based on my design notes.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GRAPH_H
#define INC_GRAPH_H

#include "globals.h"

/* Generic stack implementation. Used to keep track of the host graph changes
 * for try statements, where we may need to roll back to an older graph. 
 * If not used at all for rules, move this to another module. */

typedef struct Stack {
   int top; /* index to the top item in the stack */
   int max_size; /* can be determined statically by examining # nodes, edges in the rule. */
   void **items; /* array of stack items */
} Stack;


Stack *newStack (int maxSize);
bool push (Stack *stack, void *data);
void *pop (Stack *stack);
void freeStack (Stack *stack);



/* Invariants on graphs:
 * (1) For a graph with N nodes, next_node_index is N.
 * (2) For a graph with E edges, next_edge_index is E.
 * (3) Every edge struct E stores its own index E_i. For each reference to an
 *     edge E outside the graph structure (namely a node's inedge/outedge
 *     lists), the pointer in index E_i of the graph's edge array points to E. 
 * (4) Every node struct N stores its own index N_i. For each reference to a
 *     node N outside the graph structure (namely an edge's source and target 
 *     pointers), the pointer in index N_i of the graph's edge array points to N.
 * (5) For all edges E, if S is E's source and T is E's target, then E is in
 *     S's outedge list and in T's inedge list.
 * (6) A node with label class L is in the nodes_by_label table entry indexed
 *     by the hash key L.
 * (7) An edge with label class L is in the edges_by_label table entry indexed
 *     by the hash key L.
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

   Stack *graph_change_stack;
} Graph;

bool validGraph(Graph *graph);


typedef struct Label {
   MarkType mark; /* MarkType defined in ast.h */
   GList *list; /* The empty list is represented by a GList with one element with
                 * type EMPTY. It has length 0 in the Label struct. */
   /* Set while the label is being constructed from the AST. */
   int list_length; 
   bool has_list_variable;
} Label;

Label *copyLabel(Label *label);


/* Classes of GP 2 labels for querying by label. There is the empty list,
 * an integer constant, a string constant, a variable of atomic type, 
 * fixed-length lists of length 2 up to a fixed bound (here 5), and any list 
 * containing a list variable. */
typedef enum {EMPTY_L = 0, INT_L, STRING_L, ATOMIC_VAR_L, LIST2_L, LIST3_L,
              LIST4_L, LIST5_L, LISTVAR_L} LabelClass;

typedef struct Node {
   /* Index in the node array */
   int index;
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
   /* Index in the edge array */
   int index;
   string name;
   bool bidirectional;
   LabelClass label_class;
   Label *label;
   Node *source;
   Node *target;
} Edge;


typedef struct ListElement {
   AtomExpType type;		  /* globals.h */
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

ListElement *copyListElement(ListElement *atom);


typedef enum { ADD_NODE = 0, ADD_EDGE, REMOVE_NODE, REMOVE_EDGE, 
	       RELABEL_NODE, RELABEL_EDGE } GraphChangeType; 

typedef struct GraphChange {
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
         string source;
         string target;
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
 * arguments.
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
LabelClass getLabelClass(Label *label);
Node *newNode(string name, bool root, Label *label);
/* Not currently sure how to get the source and target pointers when traversing
 * the AST. */
Edge *newEdge(string name, bool bidirectional, Label *label, Node *source, 
             Node *target);
void addNode(Graph *graph, Node *node, bool record); 
void addEdge(Graph *graph, Edge *edge, bool record);
void removeNode(Graph *graph, Node *node, bool record);
void removeEdge(Graph *graph, Edge *edge, bool record);
void relabelNode(Graph *graph, Node *node, Label *new_label, bool change_root,
		 bool record); 
void relabelEdge(Graph *graph, Edge *edge, Label *new_label, bool record);
void restoreGraph (Graph *graph);


/* Graph querying functions 
 * ======================== */

Node *getNode(Graph *graph, int index);
Node *getNodeById(Graph *Graph, string id);
Edge *getEdge(Graph *graph, int index);
Edge *getEdgeById(Graph *Graph, string id);
GSList *getRootNodes(Graph *graph);
GSList *getNodesByLabel(Graph *graph, LabelClass label_class);
GSList *getEdgesByLabel(Graph *graph, LabelClass label_class);
GSList *getInEdge(Node *node, int index);
GSList *getOutEdge(Node *node, int index);
Node *getSource(Edge *edge);
Node *getTarget(Edge *edge);
Label *getNodeLabel(Node *node);
Label *getEdgeLabel(Edge *edge);
int getIndegree (Node *node);
int getOutdegree (Node *node);




/* Printing and freeing functions 
 * ============================== */

void printGraph (Graph *graph);
void printNode (gpointer data, gpointer user_data);
void printEdge (gpointer data, gpointer user_data);
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
void freeListElement(void *p);
/* A wrapper for g_slist_free so that it can be called by g_hash_table_foreach.
 */
void freeGSList(gpointer key, gpointer value, gpointer data); 

#endif /* INC_GRAPH_H */
