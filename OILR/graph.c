#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "graph.h"

int elemPoolSize = 0;
ElemId nextElem = 0;
Elem *freeElems = NULL;

const Node nullElem = {
	.loop=0, .root=0,
	.matched=0,
	.matchedLoops=0, 
	.sig = 0,
	.outEdges = {
		.pool=0, .len=0, .elems=NULL
	},
	.inEdges = {
		.pool=0, .len=0, .elems=NULL
	},
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
	Edge *e;
	NodeId tgt;
	int i,j, edgeCount=0;
	printf("[\n");
	for (i=0; i<g->nodes.len; i++)
		printf("\t(n%d, empty)\n", g->nodes.elems[i]);
	printf("|\n");
	for (i=0; i<g->nodes.len; i++) {
		n = &elem(i);
		for (j=0; j<outdeg(n); j++) {
			e = &edge(n, j);
			tgt = target(e);
			printf("\t(e%d, n%d, n%d, empty)\n", edgeCount++, i, tgt);
		}
	}
	printf("]\n");
}

void resizeElemList(ElemList *nl, int sz) {
	sz = (sz<DEF_EDGE_POOL) ? DEF_EDGE_POOL : sz;
	// fprintf(stderr, " %d ", sz);
	assert(sz > nl->len);
	nl->elems = realloc(nl->elems, sz * sizeof(NodeId));
	if (nl->elems == NULL)
		failWith("Failed to allocate space for a ElemList");
	nl->pool = sz;
	assert(nl->elems != NULL && nl->pool > 0 && nl->len < nl->pool);
}

#define growElemList(nl)   resizeElemList((nl), (nl)->pool * 2)
#define shrinkElemList(nl) resizeElemList((nl), (nl)->pool / 2)

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

int listElem(ElemList *nl, NodeId id) {
	int pos = nl->len++;
	if (pos == nl->pool)
		growElemList(nl);
	nl->elems[pos] = id;
	return pos;
}
void unlistElem(ElemList *nl, NodeId id) {
	int pos = nl->len--;
	while (pos-- > 0) {
		if (nl->elems[pos] == id) {
			nl->elems[pos] = nl->elems[nl->len];
			if (nl->len < nl->pool >> 2)
				shrinkElemList(nl);
			return;
		}
	}
	assert(pos>=0);
	failWith("Node ID not found");
}

void sign(Node *n) {
	int o = outdeg(n);
	int i = indeg(n);
	int l = loopdeg(n);
	int r = rooted(n);
	scaleToIndexSize(o, i, l, r);
	n->oilr.o = o;
	n->oilr.i = i;
	n->oilr.l = l;
	n->oilr.r = r;
}

void indexNode(NodeId id) {
	Node *n = &elem(id);
	ElemList *idx;
	sign(n);
	idx = indexFor(gsp, n);
	listElem(idx, id);
}
void unindexNode(NodeId id) {
	Node *n = &elem(id);
	ElemList *idx = indexFor(gsp, n);
	unlistElem(idx, id);
}

ElemId allocElem() {
	ElemId id;
	if (freeElems != NULL) {
		id = freeElems->id;
		freeElems = freeElems->next;
	} else if (nextElem == elemPoolSize) {
		elemPoolSize = elemPoolSize * 2;
		elemPool = realloc(elemPool, elemPoolSize *sizeof(Elem));
	} else {
		id = nextElem++;
	}
	elemPool[id] = nullElem;
	return id;
}
void freeElem(ElemId id) {
	Elem *e;
	if (id == nextElem-1)
		nextElem--;
	else {
		e = &elem(id);
		e->id = id;
		e->next = freeElems;
		freeElems = e;
	}
}

void addNode() {
	int id = allocElem();
	listElem(&(gsp->nodes), id);
	indexNode(id);
}
void deleteNode(NodeId id) {
	Node *n = &(elemPool[id]);
	if (outdeg(n) + indeg(n) + loopdeg(n) > 0)
		failWith("Deleting node %d violates dangling condition: O%d I%d L%d", id, outdeg(n), indeg(n), loopdeg(n));

	unlistElem(&(gsp->nodes), id);
	unindexNode(id);
	freeElem(id);
}

void addEdge(NodeId src, NodeId tgt) {
	Node *s=&elem(src), *t=&elem(tgt);
	EdgeId eid = allocElem();
	Edge *e = &elem(eid);
	unindexNode(src);
	unindexNode(tgt);
	e->src = src;
	e->tgt = tgt;
	listElem(outEdgeList(s), eid);
	listElem(inEdgeList(t), eid);
	indexNode(src);
	indexNode(tgt);
}

void deleteEdge(EdgeId eid) {
	Edge *e = &elem(eid);
	NodeId src=e->src, tgt=e->tgt;
	Node *s=&elem(src), *t=&elem(tgt);

	unindexNode(src);
	unindexNode(tgt);
	unlistElem(outEdgeList(s), eid);
	unlistElem(inEdgeList(t), eid);
	indexNode(src);
	indexNode(tgt);
	freeElem(eid);
}

void initGraphEngine() {
	ElemList emptyIndex = {.len=0, .pool=0, .elems=NULL};
	int i;
	for (i=0; i<INDEX_COUNT; i++)
		gsp->indices[i] = emptyIndex;
	elemPool = malloc(DEF_ELEM_POOL * sizeof(Node));
	elemPoolSize = DEF_ELEM_POOL;
	if (elemPool == NULL)
		failWith("Failed to allocate a node pool");
}
void destroyGraphEngine() {
	int i;
	Node *n;
	for (i=0; i<nextElem; i++) {
		n = &(elemPool[i]);
		if (outEdgeList(n)->elems != NULL)
			free(outEdgeList(n)->elems);
	}
	for (i=0; i<INDEX_COUNT; i++) {
		if (gsp->indices[i].elems != NULL)
			free(gsp->indices[i].elems);
	}
	free(elemPool);
}

