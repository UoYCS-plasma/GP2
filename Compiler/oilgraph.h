#ifndef INC_OIL_GRAPH_H
#define INC_OIL_GRAPH_H

struct OilNode;

#define TOOMANYO 3
#define TOOMANYI 3
#define TOOMANYL 3
#define TOOMANYR 2

typedef struct Link {
	struct OilNode *prev;
	int val;
	struct OilNode *next;
} Link;

typedef struct OilNode {
	Node *node;
	bool matched;
	Link chain;
} OilNode;

typedef struct Shadow {
	int len;
	OilNode head;
} Shadow;

typedef struct OilGraph {
	Graph *graph;
	Shadow shadowTables[TOOMANYO][TOOMANYI][TOOMANYL][2];
} OilGraph;

typedef struct OilEdge {
	Edge *edge;
} OilEdge;


typedef struct Traverser {
	OilNode *oilNode;
	bool isInterface;
} Traverser;

#define TRAV_STACK_SIZE 10

/* TODO: no bounds checking! stack overflow will happen! */
Traverser travStack[TRAV_STACK_SIZE];
Traverser *tsp = travStack;

/*Traverser traverserPool[MAX_NODES+MAX_EDGES]; */

#define GRAPH_STACK_SIZE 10

/* TODO: no bounds checking! stack overflow will happen! Also we are wasting the first element of the array by pre-incrementing pointer. Find a neater solution! */
OilGraph oilGraphStack[GRAPH_STACK_SIZE];
OilGraph *gsp = oilGraphStack;

/* MAX_NODES and MAX_EDGES are defined in graph.h */
OilNode oilNodePool[MAX_NODES];
OilNode *onp = oilNodePool;

OilEdge oilEdgePool[MAX_EDGES];
OilEdge *oep = oilEdgePool;

/* *************************************************** */
/* Graph building and modification functions           */
/* *************************************************** */

/* Compare with operations in graph.h. We combine the newX() and addX()
 * funcs -- all new elems are added to the top graph on the graph stack.
 * TODO: no label args for now -- these will be null if safe! Also check
 * no bidi edges in host graphs */

OilGraph *newOilGraph();
OilNode *addNewOilNode(bool root);
OilEdge *addNewOilEdge(OilNode *src, OilNode *dst);
void remOilNode(OilNode *node);
void remOilEdge(OilEdge *edge);

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
OilNode *next();

/* Push current nodes and edges pointed to by traversers on to
   node and edge stacks in reverse and clear the TRAV stack */
void foundTrav();







#endif /* INC_OIL_GRAPH_H */
