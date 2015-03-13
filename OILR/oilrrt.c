#include <assert.h>
#include <stdio.h>

#include "graph.h"
#include "oilrrt.h"

Elem *elemPool;
Graph graphs[DEF_GRAPH_POOL];
Graph *gsp = graphs;

typedef int (*Pred)(Trav *t, Elem *e);

int testNode(Trav *t, Node *n) {
	return (outdeg(n)  >= t->o &&
			indeg(n)   >= t->i &&
			loopdeg(n) >= t->l && 
			rooted(n)  >= t->r);
}
int testOutEdge(Trav *t, Elem *e) {
	Node *n = &elem(e->tgt);
	return testNode(t, n);
}
int testInEdge(Trav *t, Elem *e) {
	Node *n = &elem(e->src);
	return testNode(t, n);
}

void reset(Trav *t) {
	Node *n = &elem(t->match);
	unmatch(n);
	t->match = -1;
	t->cur = t->first;
	t->next = 0;
}

int traverse(ElemList *l, Trav *t, Pred p) {
	Elem *cnd;
	int i, id;
	success = 0;
	for (i=t->next; i<l->len; i++) {
		id = l->elems[i];
		cnd = &elem(id);
		if (p(t, cnd)) {
			match(cnd);
			t->match = id;
			t->next = i+1;
			success = 1;
			return i;
		}
	}
	return -1;
}

int edgeExistsTo(ElemList *l, NodeId tgt) {
	int i;
	success = 0;
	for (i=0; i<l->len; i++) {
		if (tgt == l->elems[i]) {
			success = 1;
			return i;
		}
	}
	return -1;
}

void search(Trav *t) {
	ElemList *idx;
	int i;

	for (i=t->cur; i<=t->last; i++) {
		idx = index(gsp, searchSpaces[i]);
		traverse(idx, t, testNode);
		if (success) {
			//t->locn = idx;
			t->cur = i;
			return;
		}
	}
}

void followOutEdge(Trav *from, Trav *to) {
	Node *n = &elem(from->match);
	ElemList *edges = outEdgeList(n);
	int e = traverse(edges, to, testOutEdge);
	if (success) {
		matchTarget( &elem(e) );
	}
}
void followInEdge(Trav *to, Trav *from) {
	Node *n = &elem(to->match);
	ElemList *edges = inEdgeList(n);
	int e = traverse(edges, from, testInEdge);
	if (success) {
		matchSource( &elem(e) );
	}
}

void edgeBetween(Trav *from, Trav *to, int negate) {
	NodeId src = from->match;
	ElemList *es = outEdgeList(&elem(src));
	int e = edgeExistsTo(es, to->match);
	if (success) {
		if (negate)
			success = 0;
		else
			match(&elem(e));
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
	initGraphEngine();
	_HOST();
	GPMAIN();
	printGraph(gsp);
	destroyGraphEngine();
	return 0;
}
