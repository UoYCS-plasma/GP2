#include <assert.h>
#include <stdio.h>

#include "graph.h"
#include "oilrrt.h"


#define available(n) (!(n)->matched)

#define willItMatch(t, n) \
	(  available(n)        && \
	outdeg(n)  == (t)->o   && \
	indeg(n)    == (t)->i  && \
	loopdeg(n)  == (t)->l  && \
	rooted(n)   >= (t)->r )


void reset(Trav *t) {
	Node *n = &node(t->match);
	unmatch(n);
	t->match = -1;
	t->cur = t->first;
	t->next = 0;
}

int traverseNodes(Trav *t, NodeList *l) {
	Node *cnd;
	int n, nid;
	for (n=t->next; n<l->len; n++) {
		nid = l->nodes[n];
		cnd = &node(nid);
		if (willItMatch(t, cnd)) {
			match(cnd);
			t->match = nid;
			t->next = n+1;
			return 1;
		}
	}
	return 0;
}

void search(Trav *t) {
	NodeList *idx;
	int i;

	for (i=t->cur; i<=t->last; i++) {
		idx = index(gsp, searchSpaces[i]);
		if (traverseNodes(t, idx)) {
			t->cur = i;
			success = 1;
			return;
		}
	}
	success = 0;
}

void followOutEdge(int from, Trav *to) {
	Node *src = &node(travs[from].match);
	NodeList *edges = &(src->outEdges);
	if (traverseNodes(to, edges)) {
		success = 1;
		return;
	}
}


int main(int argc, char **argv) {
	return 0;
}
