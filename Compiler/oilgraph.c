#include "graph.h"
#include "oilgraph.h"

/* *************************************************** */
/* Graph building and modification functions           */
/* *************************************************** */

OilNode nullNode = {
	.node = NULL,
	.matched = false,
	.chain = { .prev = NULL , .val = 0 , .next = NULL },
};


OilGraph *newOilGraph() {
	(++gsp)->graph = newGraph();
	return gsp;
}

/* When kind == RootTrav val is ignored! */
void insertInChain(OilNode *n, int out, int in, int loop, bool root) {
	Shadow *s;
	out  = out  < TOOMANYO ? out  : TOOMANYO-1;
	in   = in   < TOOMANYI ? in   : TOOMANYI-1;
	loop = loop < TOOMANYL ? loop : TOOMANYL-1;
	root = root ? 1 : 0;

	s = &(gsp->shadowTables[out][in][loop][root]);
	n->chain.next = s->head.chain.next;
	n->chain.prev = &(s->head);
	s->head.chain.next = n;
	s->len++;
}

OilNode *addNewOilNode(bool root) {
	onp->node = newNode(root, NULL);
	onp->matched = false;
	onp->chain.val = 0;
	insertInChain(onp, 0, 0, 0, root);
	return onp++;
}

OilEdge *addNewOilEdge(OilNode *src, OilNode *dst) {
	/* bidirectional param has no meaning for host graph edges, so is always false. TODO: NULL label pointer is not safe! Should be Label structure containing a GList with one element of type EMPTY! */
	oep->edge = newEdge(false, NULL, src->node, dst->node);
	addEdge(gsp->graph, oep->edge);
	return oep++;
}

void remOilNode(OilNode *n) {
	/* TODO: should this automatically remove all in- and out-bound edges? If
	 * so, beware of double-freeing loops! TODO: remove from Shadow Tables too! */
	removeNode(gsp->graph, n->node->index);
	onp--;
}

void remOilEdge(OilEdge *e) {
	removeEdge(gsp->graph, e->edge->index);
	oep--;
}



/* Clone the top graph on the graph stack */
void dupGraph() {

}


/* Drop the top graph from the graph stack */
void dropGraph() {

}


/* Nip the _second_ graph from the graph stack */
void nipGraph() {

}



/* *************************************************** */
/* Traversal functions                                 */
/* *************************************************** */




/* TODO: does having a zero default for o, i and l have any
   implications for non-interface nodes? */
void newTrav(bool isInterface, int o, int i, int l, bool root) {
	tsp++;
	tsp->oilNode = &(gsp->shadowTables[o][i][l][root].head);
	tsp->isInterface = isInterface;
}

/* TODO: do we want to also handle root contstraint using this
   interface. probably not
void constrainTrav(int val) {

} */

/* Return the next node from the top traverser. If none then pop
   trav stack */
OilNode *next() {
	/* TODO: check next list on end-of-list condition for interface nodes */
	tsp->oilNode->matched = false;
	do {
		tsp->oilNode = tsp->oilNode->chain.next;
		/* TODO: not checking for end of list at all! */
	} while (tsp->oilNode->matched);
	tsp->oilNode->matched = true;

	return tsp->oilNode;
}

/* Push current nodes and edges pointed to by traversers on to
   node and edge stacks in reverse and clear the TRAV stack */
void foundTrav() {

}




