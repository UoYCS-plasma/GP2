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
	.matched = 0,
	.edgePoolSize = 0,
	.outEdgeCount = 0,
	.outEdges = NULL,
};

Node testNode;

#define twoBitInt(i) (~0x3 & (i) ? 0x3 : (i))

void failWith(const char *fmt, ...) {
	va_list argp;
	fprintf(stderr, "error: ");
	va_start(argp, fmt);
	vfprintf(stderr, fmt, argp);
	va_end(argp);
	fprintf(stderr, "\n");
	exit(1);
}


void dumpGraph(Graph *g) {
	Node *n;
	Edge *e;
	int i,j, edgeCount=0;
	printf("[\n");
	for (i=0; i<g->free; i++)
		printf("\t(n%d, empty)\n", i);
	printf("|\n");
	for (i=0; i<g->free; i++) {
		n = &(g->nodes[i]);
		for (j=0; j<n->outEdgeCount; j++) {
			e = &(n->outEdges[j]);
			printf("\t(e%d, %d, %d, empty)\n", edgeCount++, i, e->otherEnd.id);
		}
	}
	
	printf("]\n");
}

NodeSignature signatureFor(Node *n) {
	NodeSignature sig;
	sig.o = twoBitInt(n->out);
	sig.i = twoBitInt(n->in);
	sig.l = twoBitInt(n->loop);
	sig.r = n->root;
	sig.pad = 0;
}


Graph *newGraph(int nNodes) {
	Graph *g;
	Node *np;
	g = malloc(sizeof(Graph));
	np = calloc(nNodes, sizeof(Node));
	if (g == NULL || np == NULL )
		failWith("Unable to allocate new graph structures.");

	g->free = 0;
	g->poolSize = nNodes;
	g->nodes = np;
	assert(g->free == 0 && g->nodes);
	return g;
}
void deleteGraph(Graph *g) {
	int i;
	Node *n;
	for (i=0; i<g->free; i++) {
		n = &(g->nodes[i]);
		if (n->outEdges != NULL)
			free(n->outEdges);
	}

	free(g->nodes);
	free(g);
}

void doublePools(Graph *g) {
	failWith("Resizing not yet implemented");
	int sz = g->poolSize * 2;
	g->nodes = realloc(g->nodes, sz*sizeof(Node));
}

void growEdgePool(Node *n) {
	int sz = n->edgePoolSize * 2;
	if (sz == 0)
		sz = DEF_NODE_POOL;
	n->outEdges = realloc(n->outEdges, sz * sizeof(Edge));
	n->edgePoolSize = sz;
	assert(n->outEdges != NULL);
}

void addNode(Graph *g) {
	int i = g->free;
	if (i == g->poolSize)
		doublePools(g);
	assert(i<g->poolSize);
	g->free++;
	g->nodes[i] = nullNode;
}

void addEdge(Graph *g, int src, int tgt) {
	Node *s=&(g->nodes[src]), *t=&(g->nodes[tgt]);
	int ec = s->outEdgeCount;
	Edge *e;
	if (s->outEdgeCount == s->edgePoolSize)
		growEdgePool(s);
	assert(s->outEdges && s->edgePoolSize > s->outEdgeCount);
	e = &(s->outEdges[s->outEdgeCount++]);
	e->otherEnd = signatureFor(t);
	e->otherEnd.id = tgt;
	e->matched = 0;
	assert(e->matched == 0 && ec+1 == s->outEdgeCount);
}

int main(int argc, char **argv) {
	int i;
	Graph *g = newGraph(DEF_NODE_POOL);
	for (i=0; i<DEF_NODE_POOL; i++) {
		addNode(g);
		assert(g->free == i+1);
	}
	addEdge(g, 0, 1);

	printf("node-sig: %d, node: %d, edge: %d, graph: %d\n",
			(int) sizeof(NodeSignature),
			(int) sizeof(Node),
			(int) sizeof(Edge),
			(int) sizeof(Graph));
	dumpGraph(g);

	deleteGraph(g);
	return 0;
}
