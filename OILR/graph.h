#ifndef GRAPH_H
#define GRAPH_H

#define node(id) (nodePool[(id)])
#define edge(n, id) ((n)->outEdges.nodes[id])

#define outdeg(n)  ((n)->outEdges.len)
#define indeg(n)   ((n)->inEdges.len)
#define loopdeg(n) ((n)->loop)
#define rooted(n)  ((n)->root)

#define outEdgeList(n) (&((n)->outEdges))
#define inEdgeList(n)  (&((n)->inEdges))

#define match(n) do {(n)->matched = 1;} while (0);  
#define unmatch(n) do {(n)->matched = 0;} while (0);

#define index(g, id) ( &((g)->indices[id]) )
#define indexFor(g, n) ( index((g), (n)->sig) )

/* BEWARE: double-evaluation risk -- no ++! */
#define min(a, b) ((a)<(b)?(a):(b))

#define scaleToIndexSize(o, i, l, r) do { \
	(o) = min((o), O_SZ); \
	(i) = min((i), I_SZ); \
	(l) = min((l), L_SZ); \
	(r) = min((r), R_SZ); \
} while (0);

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
#define DEF_EDGE_POOL 4
#define DEF_NODE_POOL 100

#ifndef O_SZ

#define O_BITS 2
#define I_BITS 2
#define L_BITS 2
#define R_BITS 1

#endif

#define OILR_BITS (O_BITS+I_BITS+L_BITS+R_BITS)

#define O_SZ (1<<O_BITS)
#define I_SZ (1<<I_BITS)
#define L_SZ (1<<L_BITS)
#define R_SZ (1<<R_BITS)

#define INDEX_COUNT (1<<OILR_BITS)

typedef struct NodeSignature {
	unsigned int o:O_BITS;
	unsigned int i:I_BITS;
	unsigned int l:L_BITS;
	unsigned int r:R_BITS;
	// unsigned int n:N_BITS;
} NodeSignature;



typedef int NodeId;

typedef struct NodeList {
	unsigned int pool;
	unsigned int len;
	NodeId *nodes;
} NodeList;




typedef union Node {
	struct {
	/* .out is part of outEdges */
		int loop, root;
		int matchedLoops;
		int matched;
		union {
			NodeSignature oilr;
			int sig;
		};
		NodeList outEdges;
		NodeList inEdges;
	};
	struct {
		NodeId id;
		union Node *free;
	};
} Node;

typedef struct Graph {
	NodeList nodes;
	NodeList indices[INDEX_COUNT];
} Graph;

/* Global vars */

Node *nodePool;
Graph graphs[DEF_GRAPH_POOL];
Graph *gsp = graphs;


/* API functions */

void printGraph(Graph *g);
Graph *newGraph();
void cloneGraph();
void deleteGraph(Graph *g);

void addNode();
void addEdge(NodeId src, NodeId tgt);

void deleteEdge(NodeId src, NodeId tgt);
void deleteNode(NodeId id);

#endif
