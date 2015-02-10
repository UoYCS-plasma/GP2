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


void growIndex(Index *idx) {
	int sz = idx->pool * 2;
	if (sz == 0)
		sz = DEF_EDGE_POOL;
	idx->index = realloc(idx->index, sz * sizeof(int));
	if (idx->index == NULL)
		failWith("Failed to allocate an index");
	idx->pool = sz;
	assert(idx->index != NULL && idx->pool > 0);
}

void initIndices(Graph *g) {
	int o,i,l,r;
	Index *idx;
	for (o=0; o<O_SZ; o++) {
		for (i=0; i<I_SZ; i++) {
			for (l=0; l<L_SZ; l++) {
				for (r=0; r<R_SZ; r++) {
					idx = &(g->indices->index[o][i][l][r]);
					idx->len = 0;
					idx->pool = 0;
					idx->index = NULL;
				}
			}
		}
	}
}


#define indexFor(g, n) ( &((g)->indices->index[(n)->out][(n)->in][(n)->loop][(n)->root]) )

void indexNode(Graph *g, int id) {
	Node *n = &node(g, id);
	Index *idx = indexFor(g, n);
	n->pos = idx->len++;
	if (idx->index == NULL || idx->len == idx->pool)
		growIndex(idx);

	idx->index[idx->len] = id;
	fprintf(stderr, "  %d, %d", idx->pool, idx->len);
	assert(idx->index && idx->len < idx->pool);
}
void unindexNode(Graph *g, int id) {
	Node *n = &node(g, id);
	Index *idx = indexFor(g, n);
	idx->len--;
	assert(idx->len != MAX_NODES-1);
	if (n->pos != idx->len) {
		fprintf(stderr, " (%d) ", idx->len);
		idx->index[n->pos] = idx->index[idx->len];
	}
}

Graph *newGraph(int nNodes) {
	Graph *g;
	Node *np;
	Indices *ind;
	g = malloc(sizeof(Graph));
	np = calloc(nNodes, sizeof(Node));
	ind = malloc(sizeof(Indices));
	if (g == NULL || np == NULL )
		failWith("Unable to allocate new graph structures.");

	g->free = 0;
	g->poolSize = nNodes;
	g->nodes = np;
	g->indices = ind;
	initIndices(g);
	assert(g->free == 0 && g->nodes && g->indices);
	return g;
}
Graph *cloneGraph(Graph *g) {
	Graph *clone = newGraph(g->poolSize);
	Index *ind, *clind;
	Node *n, *cln; int o, i, l, r;
	clone->free = g->free;
	memcpy(clone->nodes, g->nodes, clone->free*sizeof(Node));
	for (i=0; i<g->free; i++) {
		n   = &node(g, i);
		cln = &node(clone, i);
		if (n->outEdges != NULL) {
			cln->outEdges = malloc(n->edgePoolSize*sizeof(Edge));
			memcpy(cln->outEdges, n->outEdges, n->edgePoolSize*sizeof(Edge));
		}
	}
	memcpy(clone->indices, g->indices, sizeof(Indices));
	for (o=0; o<O_SZ; o++) {
		for (i=0; i<I_SZ; i++) {
			for (l=0; l<L_SZ; l++) {
				for (r=0; r<R_SZ; r++) {
					ind   =     &(g->indices->index[o][i][l][r]);
					clind = &(clone->indices->index[o][i][l][r]);
					clind->index = malloc(ind->pool*sizeof(int));
					memcpy(clind->index, ind->index, ind->len);
				}
			}
		}
	}
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
	g = NULL;
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
		sz = DEF_EDGE_POOL;
	n->outEdges = realloc(n->outEdges, sz * sizeof(Edge));
	n->edgePoolSize = sz;
	assert(n->outEdges != NULL);
}

void addNode(Graph *g) {
	int i = g->free;
	if (i == g->poolSize)
		growNodePool(g);
	indexNode(g, i);
	assert(i<g->poolSize && g->free < MAX_NODES);
	g->free++;
	node(g,i) = nullNode;
}

void addEdge(Graph *g, int src, int tgt) {
	Node *s=&node(g, src), *t=&node(g, tgt);
	int sOut = s->out, tIn = t->in;
	Edge *e;
	unindexNode(g, src);
	unindexNode(g, tgt);
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
	indexNode(g, src);
	indexNode(g, tgt);
	assert(e->matched == 0 && sOut+1 == s->out && tIn+1 == t->in);
}

void deleteEdge(Graph *g, int nid, int eid) {
	Node *src = &node(g, nid);
	int last = src->out-1;
	Node *tgt = &node(g, src->outEdges[eid].tgt);
	if (nid != last) {
		src->outEdges[eid] = src->outEdges[last];
	}
	src->out--;
	tgt->in--;
	assert(src->out == last);
}
void deleteNode(Graph *g, int id) {
	int i, last = g->free-1;
	Node *n = &(g->nodes[id]);
	if (n->outEdges != NULL) {
		for (i=0; i<n->out; i++)
			deleteEdge(g, id, i);
		free(n->outEdges);
		n->outEdges = NULL;
	}
	if (id != last) {
		g->nodes[id] = node(g, last);
	}
	g->free--;
	assert(g->free == last);
}

