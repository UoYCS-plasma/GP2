
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
#define DEF_NODE_POOL 100

typedef struct Edge {
	NodeSignature otherEnd;
	unsigned int matched;
} Edge;

typedef struct Node {
	unsigned int in:10;
	unsigned int out:10;
	unsigned int loop:10;
	unsigned int root:1;
	unsigned int matched:1;
	unsigned int edgePoolSize:10;
	unsigned int outEdgeCount:10;
	unsigned int matchedLoops:10;
	Edge *outEdges;
} Node;

typedef struct Graph {
	int free;
	int poolSize;
	Node *nodes;
} Graph;

