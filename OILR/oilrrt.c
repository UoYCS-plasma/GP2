#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>

#include "graph.h"


const Node nullNode = {
	.s.sig = 0,
	.s.id  = 0,
	.outEdges = -1,
};

Node testNode;

#define checkNodeId(i) do { if (MAX_NODES-1 & i) error("Invalid node id"); };
#define twoBitInt(i) (~0x3 & i ? 0x3 : i)

#define isRoot(n) ((n)->r)

void failWith(const char *fmt, ...) {
	va_list argp;
	fprintf(stderr, "error: ");
	va_start(argp, fmt);
	vfprintf(stderr, fmt, argp);
	va_end(argp);
	fprintf(stderr, "\n");
	exit(1);
}

Graph *newGraph(int nNodes) {
	Graph *g;
	Node *np;
	EdgePool *eps;
	g = malloc(sizeof(Graph));
	np = calloc(nNodes, sizeof(Node)); /* nodePool and edgePools */
	eps = calloc(nNodes, sizeof(EdgePool)); /* share a common index   */
	if (g == NULL || np == NULL || eps == NULL )
		failWith("Unable to allocate new graph structures.");

	g->free = 0;
	g->poolSize = nNodes;
	g->nodePool = np;
	g->edgePools = eps;
	assert(g->free == 0 && g->nodePool && g->edgePools);
	return g;
}
void deleteGraph(Graph *g) {
	int i;
	free(g->nodePool);
	free(g->edgePools);

}

void doublePools(Graph *g) {
	failWith("Resizing not yet implemented");
	/* int sz = g->poolSize * 2;
	g->nodePool = realloc(sz, sizeof(Node));
	g->edgePools = */
}

void addNode(Graph *g) {
	int i = g->free;
	if (i == g->poolSize)
		doublePools(g);

	g->free++;
	g->nodePool[i]  = nullNode;
}


int main(int argc, char **argv) {
	Graph *g = newGraph(DEF_NODE_POOL);
	testNode.s.o = twoBitInt(2);
	testNode.s.i = twoBitInt(3);
	testNode.s.l = twoBitInt(4);
	testNode.s.r = 1;

	printf("node-sig: %d, node: %d, edge-pool: %d, graph: %d\n",
			(int) sizeof(NodeSignature),
			(int) sizeof(Node),
			(int) sizeof(EdgePool),
			(int) sizeof(Graph));
	printf("O: %d, I: %d, L: %d, R:%d, sig: %d\n",
			testNode.s.o, testNode.s.i, testNode.s.l, testNode.s.r,
			testNode.s.sig);

	deleteGraph(g);
	return 0;
}
