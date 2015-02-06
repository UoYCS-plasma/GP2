#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "graph.h"


const Node nullNode = {
	.in = 0,
	.out = 0,
	.loop = 0,
	.root = 0,
	.matched = 0,
	.edgePoolSize = 0,
	.outEdges = NULL,
};

Node testNode;

#define twoBitInt(i) (~0x3 & (i) ? 0x3 : (i))
#define node(g, id) ((g)->nodes[id])

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
	Edge *e;
	int i,j, edgeCount=0;
	printf("[\n");
	for (i=0; i<g->free; i++)
		printf("\t(n%d, empty)\n", i);
	printf("|\n");
	for (i=0; i<g->free; i++) {
		n = &node(g, i);
		for (j=0; j<n->out; j++) {
			e = &(n->outEdges[j]);
			printf("\t(e%d, n%d, n%d, empty)\n", edgeCount++, i, e->tgt);
		}
	}
	printf("]\n");
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
Graph *cloneGraph(Graph *g) {
	Graph *clone = newGraph(g->poolSize);
	clone->free = g->free;
	memcpy(clone->nodes, g->nodes, clone->free*sizeof(Node));
	assert(g->free == clone->free
			&& g->poolSize == clone->poolSize
			&& g->nodes    != clone->nodes);
	return clone;
}
void deleteGraph(Graph *g) {
	int i;
	Node *n;
	for (i=0; i<g->free; i++) {
		n = &node(g, i);
		if (n->outEdges != NULL)
			free(n->outEdges);
	}
	free(g->nodes);
	free(g);
}

void growNodePool(Graph *g) {
	int sz = g->poolSize * 2;
	g->nodes = realloc(g->nodes, sz*sizeof(Node));
	if (g->nodes == NULL)
		failWith("Failed to allocate space for node pool.");
	g->poolSize = sz;
	assert(g->free < g->poolSize && g->poolSize == sz);
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
		growNodePool(g);
	assert(i<g->poolSize && g->free < MAX_NODES);
	g->free++;
	node(g,i) = nullNode;
}

void addEdge(Graph *g, int src, int tgt) {
	Node *s=&node(g, src), *t=&node(g, tgt);
	int sOut = s->out, tIn = t->in;
	Edge *e;
	if (src == tgt) {
		s->loop++;
	} else {
		if (s->out == s->edgePoolSize)
			growEdgePool(s);
		assert(s->outEdges && s->edgePoolSize > s->out);
		e = &(s->outEdges[s->out++]);
		e->tgt = tgt;
		e->matched = 0;
		t->in++;
	}
	assert(e->matched == 0 && sOut+1 == s->out && tIn+1 == t->in);
}

void deleteEdge(Graph *g, int nid, int eid) {
	Node *n = &node(g, nid);
	int last = n->out-1;
	Node *tgt = &node(g, n->outEdges[eid].tgt);
	if (nid != last) {
		n->outEdges[eid] = n->outEdges[last];
	}
	n->out--;
	assert(n->out == last);
}
void deleteNode(Graph *g, int id) {
	int i, last = g->free-1;
	Node *n = &(g->nodes[id]);
	if (n->outEdges != NULL) {
		for (i=0; i<n->out; i++)
			deleteEdge(g, id, i);
		free(n->outEdges);
	}
	if (id != last) {
		g->nodes[id] = node(g, last);
	}
	g->free--;
	assert(g->free == last);
}

int main(int argc, char **argv) {
	int i;
	Graph *g = newGraph(DEF_NODE_POOL);
	int howMany = DEF_NODE_POOL+1;
	for (i=0; i<howMany; i++) {
		addNode(g);
		assert(g->free == i+1);
	}
	for (i=0; i<howMany; i++) {
		addEdge(g, i, (i+1)%howMany);
	}

	fprintf(stderr,"node-sig: %d, node: %d, edge: %d, graph: %d\n",
			(int) sizeof(NodeSignature),
			(int) sizeof(Node),
			(int) sizeof(Edge),
			(int) sizeof(Graph));
	printGraph(g);

	deleteGraph(g);
	return 0;
}
