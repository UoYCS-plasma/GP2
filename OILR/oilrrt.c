#include <assert.h>
#include <stdio.h>

#include "graph.h"
#include "oilrrt.h"

Elem *elemPool;
Graph graphs[DEF_GRAPH_POOL];
Graph *gsp = graphs;

int success = 0; 

#ifndef NDEBUG
extern Tracer tracers[];
void trace(int a, int b, int here) {
	int i;
	printf("  ");

	for (i=a; i<=b; i++) {
		if (i == here)
			printf(">");
		else
			printf(" ");
		tracers[i]();
	}
	printf("\n");
}
#endif


int traverse(ElemList *l, Trav *t, Pred p) {
	Elem *cnd;
	int i, id;
	success = 0;
	//printf("%d\n", t->next);
	for (i=t->next; i<l->len; i++) {
		// printf("%d\n", i);
		id = l->elems[i];
		cnd = &elem(id);
		if (!cnd->matched && p(cnd)) {
			t->next = i+1;
			success = 1;
			return id;
		}
	}
	return -1;
}

void search(Trav *t) {
	ElemList *idx;
	int i;
	ElemId id;

	unmatch(&elem(t->match));
	t->match = 0;
	for (i=t->cur; i<=t->last; i++) {
		idx = index(gsp, searchSpaces[i]);
		// printf("Index: %d, %d\n", i, idx->len);
		id = traverse(idx, t, t->p);
		if (success) {
			match( &elem(id) );
			t->match = id;
			t->cur = i;
			return;
		}
	}
}

void matchNodeAndEdge(Trav *t, EdgeId e, NodeId n) {
	t->edge = e;
	t->match = n;
	match(&elem(e));
	match(&elem(n));
}
void unmatchNodeAndEdge(Trav *t) {
	unmatch(&elem(t->match));
	unmatch(&elem(t->edge));
	t->match = 0;
	t->edge  = 0;
}

void reset(Trav *t) {
	//t->match = -1;
	unmatchNodeAndEdge(t);
	t->cur = t->first;
	t->next = 0;
}


void followOutEdge(Trav *from, Trav *to) {
	Node *n = &elem(from->match);
	ElemList *edges = outEdgeList(n);
	EdgeId e;
	unmatchNodeAndEdge(to);
	e = traverse(edges, to, to->p);
	if (success)
		matchNodeAndEdge(to, e, target(&elem(e)) );
}
void followInEdge(Trav *to, Trav *from) {
	Node *n = &elem(to->match);
	ElemList *edges = inEdgeList(n);
	EdgeId e;
	unmatchNodeAndEdge(from);
	e = traverse(edges, from, from->p);
	if (success)
		matchNodeAndEdge(from, e, source(&elem(e)) );
}

void edgeBetween(Trav *from, Trav *to, int negate) {
	NodeId src = from->match;
	ElemList *es = outEdgeList(&elem(src));
	ElemId e = traverse(es, to, to->p);
	if (success) {
		if (negate)
			success = 0;
		else
			match(&elem(e));
	} else if (negate) {
		success = 1;
	}
	return;
}

void loopExists(Trav *t) {
	Node *n = &elem(t->match);
	success = 0;
	if (availableLoops(n) > 0) {
		matchLoop(n);
		success = 1;
	}
}

int main(int argc, char **argv) {
	argc = argc;
	argv = argv;
	initGraphEngine();
	_HOST();
	GPMAIN();
	printGraph(gsp);
	destroyGraphEngine();
	return 0;
}
