#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "graph.h"

Graph graphs[DEF_GRAPH_POOL];
Graph *gsp = graphs;

Node *nodePool;
int nodePoolSize = 0;
NodeId nextNode = 0;
Node *freeNodes = NULL;

const Node nullNode = {
	.loop=0, .root=0,
	.matchedLoops=0, 
	.outEdges = {
		.pool=0, .len=0, .nodes=NULL
	}
};

void failWith(const char *fmt, ...) {
	va_list argp;
	fprintf(stderr, "error: ");
	va_start(argp, fmt);
	vfprintf(stderr, fmt, argp);
	va_end(argp);
	fprintf(stderr, "\n");
	exit(1);
}

void printGraph(Graph *g) {
	Node *n;
	NodeId tgt;
	int i,j, edgeCount=0;
	printf("[\n");
	for (i=0; i<g->nodes.len; i++)
		printf("\t(n%d, empty)\n", g->nodes.nodes[i]);
	printf("|\n");
	for (i=0; i<g->nodes.len; i++) {
		n = &node(i);
		for (j=0; j<outdeg(n); j++) {
			tgt = edge(n, j);
			printf("\t(e%d, n%d, n%d, empty)\n", edgeCount++, i, tgt);
		}
	}
	printf("]\n");
}

void resizeNodeList(NodeList *nl, int sz) {
	sz = (sz<DEF_EDGE_POOL) ? DEF_EDGE_POOL : sz;
	// fprintf(stderr, " %d ", sz);
	assert(sz > nl->len);
	nl->nodes = realloc(nl->nodes, sz * sizeof(NodeId));
	if (nl->nodes == NULL)
		failWith("Failed to allocate space for a NodeList");
	nl->pool = sz;
	assert(nl->nodes != NULL && nl->pool > 0 && nl->len < nl->pool);
}

#define growNodeList(nl)   resizeNodeList((nl), (nl)->pool * 2)
#define shrinkNodeList(nl) resizeNodeList((nl), (nl)->pool / 2)

void dupGraph() {
	Graph *parent = gsp++;
	int nGraphs = gsp - graphs;
	if (nGraphs >= DEF_GRAPH_POOL)
		failWith("Too many clones: %d", nGraphs);
	*gsp = *parent;
}
void dropGraph() {
	gsp--;
}


int listNode(NodeList *nl, NodeId id) {
	int pos = nl->len++;
	if (pos == nl->pool)
		growNodeList(nl);
	nl->nodes[pos] = id;
	return pos;
}
void unlistNode(NodeList *nl, NodeId id) {
	int pos = nl->len--;
	while (pos-- > 0) {
		if (nl->nodes[pos] == id) {
			nl->nodes[pos] = nl->nodes[nl->len];
			if (nl->len < nl->pool >> 2)
				shrinkNodeList(nl);
			return;
		}
	}
	assert(pos>=0);
	failWith("Node ID not found");
}

#define indexFor(g, n) ( &((g)->indices[outdeg(n)][indeg(n)][loopdeg(n)][rooted(n)]) )

void indexNode(NodeId id) {
	Node *n = &node(id);
	NodeList *idx = indexFor(gsp, n);
	listNode(idx, id);
}
void unindexNode(NodeId id) {
	Node *n = &node(id);
	NodeList *idx = indexFor(gsp, n);
	unlistNode(idx, id);
}


void addNode() {
	int id;
	if (nextNode == nodePoolSize && freeNodes == NULL) {
		nodePoolSize = nodePoolSize * 2;
		nodePool = realloc(nodePool, nodePoolSize *sizeof(Node));
	}
	if (freeNodes != NULL) {
		id = freeNodes->id;
		freeNodes = freeNodes->free;
	} else {
		id = nextNode++;
	}
	nodePool[id] = nullNode;
	memset(&(nodePool[id]), 0, sizeof(Node)); // = nullNode;
	listNode(&(gsp->nodes), id);
	indexNode(id);
}
void deleteNode(NodeId id) {
	Node *n = &(nodePool[id]);
	if (outdeg(n) + indeg(n) + loopdeg(n) > 0)
		failWith("Deleting node %d violates dangling condition: O%d I%d L%d", id, outdeg(n), indeg(n), loopdeg(n));

	unlistNode(&(gsp->nodes), id);
	unindexNode(id);
	if (id == nextNode-1)
		nextNode--;
	else {
		n->id = id;
		n->free = freeNodes;
		freeNodes = n;
	}
}


void addEdge(NodeId src, NodeId tgt) {
	Node *s=&node(src), *t=&node(tgt);
	unindexNode(src);
	unindexNode(tgt);
	listNode(outEdgeList(s), tgt);
	listNode(inEdgeList(t), src);
	indexNode(src);
	indexNode(tgt);
}

void deleteEdge(NodeId src, NodeId tgt) {
	/* TODO: this only works because parallel edges are 
	 * identical. Labels and dashed edge support will break. */
	Node *s=&node(src), *t=&node(tgt);
	unindexNode(src);
	unindexNode(tgt);
	unlistNode(outEdgeList(s), tgt);
	unlistNode(inEdgeList(t), src);
	indexNode(src);
	indexNode(tgt);

}

void initialise() {
	NodeList emptyIndex = {.len=0, .pool=0, .nodes=NULL};
	int i;
	for (i=0; i<INDEX_COUNT; i++)
		gsp->flat[i] = emptyIndex;
	nodePool = malloc(DEF_NODE_POOL * sizeof(Node));
	nodePoolSize = DEF_NODE_POOL;
	if (nodePool == NULL)
		failWith("Failed to allocate a node pool");
}
void finalise() {
	int i;
	Node *n;
	for (i=0; i<nextNode; i++) {
		n = &(nodePool[i]);
		if (outEdgeList(n)->nodes != NULL)
			free(outEdgeList(n)->nodes);
	}
	for (i=0; i<INDEX_COUNT; i++) {
		if (gsp->flat[i].nodes != NULL)
			free(gsp->flat[i].nodes);
	}

	free(nodePool);
}

int main(int argc, char **argv) {
	int i;
	initialise();
	dupGraph();
	dropGraph();
	addNode();
	for (i=1; i<10; i++) {
		addNode();
		addEdge(i-1, i);
	}
	addEdge(9, 0);
	printGraph(gsp);
	deleteEdge(0, 1);
	deleteEdge(1, 2);
	deleteNode(1);
	addNode();
	addNode();
	printGraph(gsp);
	finalise();
	return 0;
}

