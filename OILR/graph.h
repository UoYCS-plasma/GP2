#ifndef GRAPH_H
#define GRAPH_H

#include "gp2c_out.h"

#define elem(id) (elemPool[(id)])
#define edge(n, pos) (elem((n)->outEdges.elems[pos]))

#define outdeg(n)  ((n)->outEdges.len)
#define indeg(n)   ((n)->inEdges.len)
#define loopdeg(n) ((n)->loop)
#define rooted(n)  ((n)->root)

#define outEdgeList(n) (&((n)->outEdges))
#define inEdgeList(n)  (&((n)->inEdges))
#define outEdge(n, i) ((n)->outEdges[i])
#define inEdge(n, i)  ((n)->inEdges[i])

#define index(g, id) ( &((g)->indices[id]) )
#define indexFor(g, n) ( index((g), (n)->sig) )

#define source(e) ((e)->src)
#define target(e) ((e)->tgt)

/* BEWARE: double-evaluation risk -- no ++! */
#define min(a, b) ((a)<(b)?(a):(b))

#define scaleToIndexSize(o, i, l, r) do { \
	(o) = min((o), O_SZ); \
	(i) = min((i), I_SZ); \
	(l) = min((l), L_SZ); \
	(r) = min((r), R_SZ); \
} while (0);


#ifndef NDEBUG
#define debug(...) do { printf("--> ") ; printf(__VA_ARGS__) ; printf("\n");} while (0);
#else
#define debug(...)
#endif

/*typedef union NodeSignature {
	struct {
		unsigned int o:2;
		unsigned int i:2;
		unsigned int l:2;
		unsigned int r:1;
		unsigned int pad: 1;
	} ;
	struct {
		unsigned int sig:8;
		unsigned int id:NODE_ID_BITS;
	} ;
} NodeSignature;
*/

#define DEF_GRAPH_POOL 4
#define DEF_EDGE_POOL 16
#define DEF_ELEM_POOL 100

/*#ifndef O_BITS

#define O_BITS 2
#define I_BITS 2
#define L_BITS 2
#define R_BITS 1

#endif */

#define OILR_BITS (O_BITS+I_BITS+L_BITS+R_BITS)

#define O_SZ (1<<O_BITS)
#define I_SZ (1<<I_BITS)
#define L_SZ (1<<L_BITS)
#define R_SZ (1<<R_BITS)

#define INDEX_COUNT (1<<OILR_BITS)

typedef struct NodeSignature {
#if O_BITS
	unsigned int o:O_BITS;
#endif
#if I_BITS
	unsigned int i:I_BITS;
#endif
#if L_BITS
	unsigned int l:L_BITS;
#endif
#if R_BITS
	unsigned int r:R_BITS;
#endif
	// unsigned int n:N_BITS;
} NodeSignature;

typedef int ElemId;
typedef int NodeId;
typedef int EdgeId;

typedef struct ElemList {
	int pool;
	int len;
	ElemId *elems;
} ElemList;

typedef union Elem {
	struct { /* edge structure */
		NodeId src, tgt;
	};
	struct { /* node structure */
	/* .out is part of outEdges */
		int loop, root;
		int matched;
		int matchedLoops;
		union {
			NodeSignature oilr;
			int sig;
		};
		ElemList outEdges;
		ElemList inEdges;
	};
	struct { /* free-list */
		ElemId id;
		union Elem *next;
	};
} Elem;
typedef Elem Node;
typedef Elem Edge;

typedef struct Graph {
	ElemList nodes;
	ElemList indices[INDEX_COUNT];
} Graph;

/* Global vars */

extern Elem *elemPool;
extern Graph graphs[];
extern Graph *gsp;

/* API functions */

extern int signature(Node *n);
void printGraph(Graph *g);
Graph *newGraph();
void cloneGraph();
void deleteGraph(Graph *g);

void addNode();
void addEdge(NodeId src, NodeId tgt);

void deleteEdge(EdgeId e);
void deleteNode(NodeId id);

void initGraphEngine();
void destroyGraphEngine();
#endif
