#ifndef OILRRT_H
#define OILRRT_H

#include <stdbool.h>

#ifdef DEBUG
#include <stdio.h>
#define TEST_INVARIANTS
#define trace(...) do { printf("--> ") ; printf(__VA_ARGS__) ; printf("\n");} while (false);
bool testInvariants();
#else
#define trace(...)
#define dumpTravStack(x)
#define testInvariants()
#endif


#ifdef OILR_STANDALONE

#define MAX_NODES 64
#define MAX_EDGES 1024
#define MAX_INCIDENT_EDGES 128

/* dummy data structures for testing OILR in isolation */
typedef void Label ;
struct Edge;

typedef struct Node {
	bool root;
	int index;
	int outdegree, indegree;
	int loopdegree;
	struct Edge *out_edges[MAX_INCIDENT_EDGES];
} Node;

typedef struct Edge {
	struct Edge *index;
	Node *source, *target;
} Edge;

typedef struct Graph {
	void *dummy;
} Graph;

extern Edge edgePool[];
extern Node nodePool[];


#else

#include "graph.h"

#endif




extern bool success;

struct OilrNode;

#define TOOMANYO 3
#define TOOMANYI 3
#define TOOMANYL 3
#define TOOMANYR 2




typedef struct Link {
	struct OilrNode *prev;
	struct Index *index;
	struct OilrNode *next;
} Link;

typedef struct OilrNode {
	Node *node;
	bool matched;
	Link chain;
} OilrNode;

typedef struct Index {
	int len;
	OilrNode head;
} Index;

typedef struct OilrGraph {
	Graph *graph;
	Index indices[TOOMANYO][TOOMANYI][TOOMANYL][2];
} OilrGraph;

typedef struct OilrEdge {
	Edge *edge;
} OilrEdge;

typedef struct SearchSpace {
	int size, pos;
	struct NodeTraverser *edgeFrom;
	Index **index;
} SearchSpace;


typedef struct NodeTraverser {
	OilrNode *oilrNode;
	int o,i,l;
	int fromo, fromi, froml;
	int capo, capi, capl;
	bool r;
	bool isInterface;
	SearchSpace searchSpace;
} NodeTraverser;

typedef struct EdgeTraverser {
	Edge *edge;
	OilrNode *oilrNode;
	NodeTraverser *src, *tgt;
} EdgeTraverser;

typedef enum {
	InvalidTrav = 0,
	/* Test for node trav with "& 0x1" */
	NodeTrav    = 1,
	/* Test for edge travs with "& 0x2" */
	EdgeTrav    = 2,
	XeTrav      = -2,
} TravType;

#define isNodeTrav(t) ((t->type & 0x1) ? true : false)
#define isEdgeTrav(t) ((t->type & 0x2) ? true : false)
#define isNegated(t) ((t->type < 0) ? true : false)

#define getTravNode(someTrav) (trav->n->oilrNode)
#define getTravEdge(someTrav) (trav->e->edge)

typedef struct Traverser {
	TravType type;
	union {
		NodeTraverser n;
		EdgeTraverser e;
	};
} Traverser;

#define TRAV_STACK_SIZE 100

/*Traverser traverserPool[MAX_NODES+MAX_EDGES]; */

#define GRAPH_STACK_SIZE 10


/* *************************************************** */
/* Graph building and modification functions           */
/* *************************************************** */

/* Compare with operations in graph.h. We combine the newX() and addX()
 * funcs -- all new elems are added to the top graph on the graph stack.
 * TODO: no label args for now -- these will be null if safe! Also check
 * no bidi edges in host graphs */

OilrGraph *newOilrGraph();
OilrNode *addNewOilrNode(bool root);
Edge *addNewEdge(NodeTraverser *src, NodeTraverser *dst);
void delOilrNode(NodeTraverser *nt);
void delOilrEdge(EdgeTraverser *et);
void deleteNonInterfaceNodes();
void deleteEdges();
void setRoot(NodeTraverser *n, bool state);

/* Graph stack management */

/* Clone the top graph on the graph stack */
void dupGraph();

/* Drop the top graph from the graph stack */
void dropGraph();

/* Nip the _second_ graph from the graph stack */
void nipGraph();


/* *************************************************** */
/* Traversal functions                                 */
/* *************************************************** */




/* TODO: does having a zero default for o, i and l have any
   implications for non-interface nodes? */
void newNodeTrav(bool isInterface, int o, int i, int l, bool root);

void newEdgeTrav(NodeTraverser *src, NodeTraverser *tgt);
void newNegatedEdgeTrav(NodeTraverser *src, NodeTraverser *tgt);
		
/* TODO: do we want to also handle root contstraint using this
   interface. probably not
void constrainTrav(int val); */

/* Return the next node from the top traverser. If none then pop
   trav stack */
OilrNode *next();

/* Push current nodes and edges pointed to by traversers on to
   node and edge stacks in reverse and clear the TRAV stack */
void runSearch();

void clearTravs();




#endif /* OILRRT_H */
