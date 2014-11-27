#ifndef INC_OIL_GRAPH_H
#define INC_OIL_GRAPH_H


bool success = false;

struct OilrNode;

#define TOOMANYO 3
#define TOOMANYI 3
#define TOOMANYL 3
#define TOOMANYR 2

typedef struct Link {
	struct OilrNode *prev;
	int val;
	struct OilrNode *next;
} Link;

typedef struct OilrNode {
	Node *node;
	bool matched;
	Link chain;
} OilrNode;

typedef struct Shadow {
	int len;
	OilrNode head;
} Shadow;

typedef struct OilrGraph {
	Graph *graph;
	Shadow shadowTables[TOOMANYO][TOOMANYI][TOOMANYL][2];
} OilrGraph;

typedef struct OilrEdge {
	Edge *edge;
} OilrEdge;


typedef struct NodeTraverser {
	OilrNode *oilrNode;
	int o,i,l;
	int capo, capi, capl;
	bool r;
	bool isInterface;
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

typedef struct Traverser {
	TravType type;
	union {
		NodeTraverser n;
		EdgeTraverser e;
	};
} Traverser;

#define TRAV_STACK_SIZE 10

/* TODO: no bounds checking! stack overflow will happen! */
Traverser travStack[TRAV_STACK_SIZE];
Traverser *tsp = travStack;

/*Traverser traverserPool[MAX_NODES+MAX_EDGES]; */

#define GRAPH_STACK_SIZE 10

/* TODO: no bounds checking! stack overflow will happen! Also we are wasting the first element of the array by pre-incrementing pointer. Find a neater solution! */
OilrGraph oilrGraphStack[GRAPH_STACK_SIZE];
OilrGraph *gsp = oilrGraphStack;

/* MAX_NODES and MAX_EDGES are defined in graph.h */
OilrNode oilrNodePool[MAX_NODES];
OilrNode *onp = oilrNodePool;

/* *************************************************** */
/* Graph building and modification functions           */
/* *************************************************** */

/* Compare with operations in graph.h. We combine the newX() and addX()
 * funcs -- all new elems are added to the top graph on the graph stack.
 * TODO: no label args for now -- these will be null if safe! Also check
 * no bidi edges in host graphs */

OilrGraph *newOilrGraph();
OilrNode *addNewOilrNode(bool root);
OilrEdge *addNewOilrEdge(OilrNode *src, OilrNode *dst);
void delOilrNode(NodeTraverser *nt);
void delOilrEdge(EdgeTraverser *et);

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
void newTrav(bool isInterface, int o, int i, int l, bool root);

/* TODO: do we want to also handle root contstraint using this
   interface. probably not
void constrainTrav(int val); */

/* Return the next node from the top traverser. If none then pop
   trav stack */
OilrNode *next();

/* Push current nodes and edges pointed to by traversers on to
   node and edge stacks in reverse and clear the TRAV stack */
void foundTrav();







#endif /* INC_OIL_GRAPH_H */
