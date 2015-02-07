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
	Edge *outEdges;
} Node;

typedef struct Graph {
	int free;
	int poolSize;
	Node *nodes;
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
