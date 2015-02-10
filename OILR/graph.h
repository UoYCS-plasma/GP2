#ifndef GRAPH_H
#define GRAPH_H

#define NODE_ID_BITS 24
#define MAX_NODES (1<<NODE_ID_BITS)

#define twoBitInt(i) (~0x3 & (i) ? 0x3 : (i))
#define node(g, id) ((g)->nodes[id])
#define edge(n, id) ((n)->outEdges[id])

typedef union NodeSignature {
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


#define DEF_EDGE_POOL 4
#define DEF_NODE_POOL 100

#define O_SZ 3
#define I_SZ 3
#define L_SZ 3
#define R_SZ 2

typedef struct Link {
	int next;
	int prev;
} Link;

typedef struct Edge {
	unsigned int tgt:NODE_ID_BITS;
	unsigned int matched:1;
} Edge;

typedef struct Node {
	unsigned int out:10;
	unsigned int in:10;
	unsigned int loop:10;
	unsigned int root:1;
	unsigned int matched:1;
	unsigned int edgePoolSize:10;
	unsigned int matchedLoops:10;
	unsigned int pos;
	Edge *outEdges;
	Link chain;
} Node;

typedef struct Index {
	int *index;
	unsigned int len:NODE_ID_BITS;
	unsigned int pool:NODE_ID_BITS;
} Index;

typedef struct Indices {
	Index index[O_SZ][I_SZ][L_SZ][R_SZ];
} Indices;

typedef struct Graph {
	int free;
	int poolSize;
	Node *nodes;
	Indices *indices;
} Graph;


/* API functions */

void printGraph(Graph *g);
Graph *newGraph(int nNodes);
Graph *cloneGraph(Graph *g);
void deleteGraph(Graph *g);

void addNode(Graph *g);
void addEdge(Graph *g, int src, int tgt);

void deleteEdge(Graph *g, int nid, int eid);
void deleteNode(Graph *g, int id);

#endif
