
#define NODE_ID_BITS 24
#define MAX_NODES (1<<NODE_ID_BITS)

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
#define DEF_NODE_POOL MAX_NODES

typedef struct EdgePool {
	NodeSignature tgts[DEF_EDGE_POOL];
	unsigned int next:32; /* index of next block of four edges */
} EdgePool;


typedef struct Edge {
	NodeSignature otherEnd;
} Edge;

typedef struct Node {
	int in, out, loop, root;
	int outEdgeCount;
	Edge *outEdges;
} Node;

typedef struct Graph {
	int free;
	int poolSize;
	Node *nodePool;
	int freeEdgePool;
	EdgePool *edgePools;
} Graph;

