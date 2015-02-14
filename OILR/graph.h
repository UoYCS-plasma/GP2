#ifndef GRAPH_H
#define GRAPH_H

/* we leave the top two bits for flags */
#define NODE_ID_BITS 30
#define MAX_NODES (1<<NODE_ID_BITS)
#define NODE_ID_MASK (~(-1 << NODE_ID_BITS))

#define node(id) (nodePool[(id)&NODE_ID_MASK])
#define edge(n, id) ((n)->outEdges.nodes[id])

#define outdeg(n)  ((n)->outEdges.len)
#define indeg(n)   ((n)->inEdges.len)
#define loopdeg(n) ((n)->loop)
#define rooted(n)  ((n)->root)

#define outEdgeList(n) (&((n)->outEdges))
#define inEdgeList(n)  (&((n)->inEdges))

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

#define O_SZ 3
#define I_SZ 3
#define L_SZ 3
#define R_SZ 2

#endif

#define INDEX_COUNT (O_SZ*I_SZ*L_SZ*R_SZ)


typedef int NodeId;

typedef struct NodeList {
	unsigned int pool;
	unsigned int len:NODE_ID_BITS;
	NodeId *nodes;
} NodeList;


typedef union Node {
	struct {
	/* .out is part of outEdges */
		int loop, root;
		int matchedLoops;
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
	union {
		NodeList flat[INDEX_COUNT];
		NodeList indices[O_SZ][I_SZ][L_SZ][R_SZ];
	};
} Graph;

/* Global vars */


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
