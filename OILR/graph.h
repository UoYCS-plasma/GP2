
#define NODE_ID_BITS 23
#define MAX_NODES (1<<NODE_ID_BITS)

typedef union NodeSignature {
	struct {
		unsigned int o:2;
		unsigned int i:2;
		unsigned int l:2;
		unsigned int r:1;
	} ;
	struct {
		unsigned int sig:7;
		unsigned int blur:1;
		unsigned int id:NODE_ID_BITS;
		unsigned int flag:1; /* flag-bit (for identifying in-edges) */
	} ;
} NodeSignature;


#define DEF_EDGE_POOL 4
#define DEF_NODE_POOL MAX_NODES

typedef struct EdgePool {
	NodeSignature tgts[DEF_EDGE_POOL];
	unsigned int next:32; /* index of next block of four edges */
} EdgePool;

typedef struct Node {
	NodeSignature s;
} Node;

typedef struct Edge {
	unsigned int src:NODE_ID_BITS;
	unsigned int flag:1;
	unsigned int pad:8;
	unsigned int tgt:NODE_ID_BITS;
} Edge;

typedef struct Graph {
	int free;
	int poolSize;
	Node *nodePool;
	int freeEdgePool;
	EdgePool *edgePools;
} Graph;

