#include <assert.h>
#include <stdio.h>

#include "graph.h"
#include "oilrrt.h"


#define resumeTrav(t) do { \
	o=(t)->o; i=(t)->i; l=(t)->l; r=(t)->r; n=(t)->n; \
} while (0);

#define explodeTrav(t) do { \
	o=(t)->o; i=(t)->i; l=(t)->l; r=(t)->r; n=(t)->n; \
	oSz=(t)->oSz; iSz=(t)->iSz; \
	lSz=(t)->lSz; rSz=(t)->rSz; \
} while (0);

#define collapseTrav(t) do { \
	t->o=o; t->i=i; t->l=l; t->r=r; \
} while (0);

void reset(Trav *t) {
	Node *n = &node(t->match);
	unmatch(n);
	t->match = -1;
	t->cur = t->first;
	t->pos = 0;
}

#define available(n) (!(n)->matched)

#define willItMatch(t, n) \
	(  available(n)        && \
	outdeg(n)  == (t)->o   && \
	indeg(n)    == (t)->i  && \
	loopdeg(n)  == (t)->l  && \
	rooted(n)   >= (t)->r )


void search(Trav *t) {
	NodeList *idx;
	Node *cnd;
	int i, n, nid;

	for (i=t->cur; i<=t->last; i++) {
		idx = index(gsp, searchSpaces[i]);
		for (n=t->pos; n<idx->len; n++) {
			nid = idx->nodes[n];
			cnd = &node(nid);
			if (willItMatch(t, cnd)) {
				match(cnd);
				t->match = nid;
				t->cur = i;
				t->pos = n;
				success = 1;
				return;
			}
		}
	}
	success = 0;
}


int main(int argc, char **argv) {
	return 0;
}
