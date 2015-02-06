#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>

#include "graph.h"


const Node nullNode = {
	.in = 0,
	.out = 0,
	.loop = 0,
	.root = 0,
	.outEdges = NULL,
};

Node testNode;

#define twoBitInt(i) (~0x3 & i ? 0x3 : i)

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
	free(g);
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
	int i;
	Graph *g = newGraph(DEF_NODE_POOL);
	for (i=0; i<MAX_NODES; i++) {
		addNode(g);
		assert(g->free == i+1);
	}

	printf("node-sig: %d, node: %d, edge: %d, edge-pool: %d, graph: %d\n",
			(int) sizeof(NodeSignature),
			(int) sizeof(Node),
			(int) sizeof(Edge),
			(int) sizeof(EdgePool),
			(int) sizeof(Graph));

	deleteGraph(g);
	return 0;
}
