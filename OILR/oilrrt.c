#include <assert.h>
#include <stdio.h>

#include "graph.h"
#include "oilrrt.h"

Node *nodePool;
Graph graphs[DEF_GRAPH_POOL];
Graph *gsp = graphs;

#define MATCHED_EDGE (1<<31)

#define available(n) (!(n)->matched)

#define willItMatch(t, n) \
	(  available(n)        && \
	outdeg(n)  == (t)->o   && \
	indeg(n)    == (t)->i  && \
	loopdeg(n)  == (t)->l  && \
	rooted(n)   >= (t)->r )

#define trav(n) (travs[n])

#define matchEdge(idx, e) do { \
	(idx)->nodes[e] |= MATCHED_EDGE; } while (0)
#define unmatchEdge(idx, e) do { \
	(idx)->nodes[e] &= ~MATCHED_EDGE; } while (0)
#define matchLoop(n) do { (n)->matchedLoops++; } while (0)
#define unmatchLoop(n) do { (n)->matchedLoops--; } while (0)
#define availableLoops(n) (loopdeg(n) - (n)->matchedLoops)

void reset(Trav *t) {
	Node *n = &node(t->match);
	unmatch(n);
	t->match = -1;
	t->cur = t->first;
	t->next = 0;
}

int traverse(NodeList *l, Trav *t) {
	Node *cnd;
	int n, nid;
	success = 0;
	for (n=t->next; n<l->len; n++) {
		nid = l->nodes[n];
		if ( (nid & MATCHED_EDGE) )
			continue; /* skip already matched edges */
		cnd = &node(nid);
		if (willItMatch(t, cnd)) {
			match(cnd);
			t->match = nid;
			t->next = n+1;
			success = 1;
			return n;
		}
	}
	return -1;
}

int edgeExistsTo(NodeList *l, NodeId tgt) {
	int i;
	success = 0;
	for (i=0; i<l->len; i++) {
		/* a set MATCHED_EDGE bit prevents matching
		 * an already claimed edge */
		if (tgt == l->nodes[i]) {
			success = 1;
			return i;
		}
	}
	return -1;
}


void search(Trav *t) {
	NodeList *idx;
	int i;

	for (i=t->cur; i<=t->last; i++) {
		idx = index(gsp, searchSpaces[i]);
		traverse(idx, t);
		if (success) {
			//t->locn = idx;
			t->cur = i;
			return;
		}
	}
}

void followOutEdge(TravId from, Trav *to) {
	Node *src = &node(travs[from].match);
	NodeList *edges = &(src->outEdges);
	int e = traverse(edges, to);
	if (success) {
		matchEdge(edges, e);
		return;
	}
}

void edgeBetween(TravId from, TravId to, int negate) {
	Trav *src = &trav(from), *tgt = &trav(to);
	NodeId t = tgt->match;
	NodeList *es = &(node(src->match).outEdges);
	int e = edgeExistsTo(es, t);
	if (success) {
		if (negate)
			success = 0;
		else
			matchEdge(es, e);
	}
	return;
}


void hasLoop(Trav *t) {
	Node *n = &node(t->match);
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
