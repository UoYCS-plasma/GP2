#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "graph.h"

int elemPoolSize = 0;
ElemId nextElem = 0;
Elem *freeElems = NULL;

const Node nullElem = {{0}};

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
	int i,j, edgeCount=nextElem;
	printf("[\n");
	for (i=0; i<g->nodes.len; i++)
		printf("\t(n%d, empty)\n", g->nodes.elems[i]);
	printf("|\n");
	for (i=0; i<g->nodes.len; i++) {
		n = &elem(g->nodes.elems[i]);
		for (j=0; j<outdeg(n); j++) {
			e = &edge(n, j);
			tgt = target(e);
			printf("\t(e%d, n%d, n%d, empty)\n", n->outEdges.elems[j], g->nodes.elems[i], tgt);
		}
		for (j=0; j<loopdeg(n); j++) {
			printf("\t(e%d, n%d, n%d, empty)\n", edgeCount++, i, i);
		}
	}
	printf("]\n");
}

void resizeElemList(ElemList *nl, int sz) {
	sz = (sz<DEF_EDGE_POOL) ? DEF_EDGE_POOL : sz;
	debug("resize: 0x%lx, from %d to %d\n", (unsigned long) nl, nl->pool, sz);
	if (sz == nl->pool)
		return;
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
	assert(pos <= nl->pool && pos >= 0);
	if (pos == nl->pool)
		growElemList(nl);
	assert(pos < nl->pool && nl->elems != NULL);
	nl->elems[pos] = id;
	return pos;
}
void unlistElem(ElemList *el, NodeId id) {
	int pos = el->len--;
	assert(pos > 0 && el->elems != NULL);
	while (pos-- > 0) {
		if (el->elems[pos] == id) {
			el->elems[pos] = el->elems[el->len];
			if (el->len < el->pool >> 3)
				shrinkElemList(el);
			assert(pos < el->pool && pos >= 0);
			return;
		}
	}
	//assert(pos>=0);
	failWith("Node ID not found; %d", id);
}

void sign(Node *n) {
	n->sig = signature(n);
}

void indexNode(NodeId id) {
	Node *n = &elem(id);
	ElemList *idx;
	int count;
	debug("Reindexing node %d from %d to %d", id, n->sig, signature(n));
	sign(n);
	idx = indexFor(gsp, n);
	count = idx->len;
	listElem(idx, id);
	assert(idx->len == count+1);
}
void unindexNode(NodeId id) {
	Node *n = &elem(id);
	ElemList *idx = indexFor(gsp, n);
	int count = idx->len;
	unlistElem(idx, id);
	assert(idx->len == count-1 && count > 0);
}

ElemId allocElem() {
	ElemId id;
	if (freeElems != NULL) {
		id = freeElems->id;
		freeElems = freeElems->next;
	} else {
		if (nextElem == elemPoolSize) {
			elemPoolSize = elemPoolSize << 1;
			elemPool = realloc(elemPool, elemPoolSize *sizeof(Elem));
			assert(elemPool != NULL);
		} 
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

void setRootedness(NodeId id, int r) {
	Node *n = &elem(id);
	unindexNode(id);
	n->root = r;
	indexNode(id);
}
void addNode() {
	int id = allocElem();
	listElem(&(gsp->nodes), id);
	indexNode(id);
	debug("Creating node %d", id);
}
void deleteNode(NodeId id) {
	Node *n = &elem(id);
	if (outdeg(n) + indeg(n) + loopdeg(n) > 0)
		failWith("Deleting node %d violates dangling condition: O%d I%d L%d", id, outdeg(n), indeg(n), loopdeg(n));

	unlistElem(&(gsp->nodes), id);
	unindexNode(id);
	freeElem(id);
}

void addEdge(NodeId src, NodeId tgt) {
	EdgeId eid = allocElem();
	Node *s=&elem(src), *t=&elem(tgt);
	Edge *e = &elem(eid);
	assert(src > 0 && tgt > 0);
	debug("Creating edge %d between %d and %d", eid, src, tgt);
	unindexNode(src);
	unindexNode(tgt);
	e->src = src;
	e->tgt = tgt;
	listElem(outEdgeList(s), eid);
	listElem(inEdgeList(t), eid);
	indexNode(src);
	indexNode(tgt);
}
void addLoop(NodeId id) {
	Node *n = &elem(id);
	unindexNode(id);
	n->loop++;
	indexNode(id);
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
void deleteLoop(NodeId id) {
	Node *n = &elem(id);
	unindexNode(id);
	n->loop--;
	indexNode(id);
}

void initGraphEngine() {
	ElemList emptyIndex = {0};
	int i;
	for (i=0; i<INDEX_COUNT; i++)
		gsp->indices[i] = emptyIndex;
	elemPool = malloc(DEF_ELEM_POOL * sizeof(Elem));
	elemPoolSize = DEF_ELEM_POOL;
	if (elemPool == NULL)
		failWith("Failed to allocate a node pool");
	elemPool[0] = nullElem; // elems[0] is a dummy entry.
	nextElem++;
}
void destroyGraphEngine() {
	int i;
	Node *n;
	for (i=0; i<nextElem; i++) {
		n = &(elemPool[i]);
		if (outEdgeList(n)->elems != NULL)
			free(outEdgeList(n)->elems);
		if (inEdgeList(n)->elems != NULL)
			free(inEdgeList(n)->elems);
	}
	for (i=0; i<INDEX_COUNT; i++) {
		if (gsp->indices[i].elems != NULL)
			free(gsp->indices[i].elems);
	}
	free(elemPool);
}

