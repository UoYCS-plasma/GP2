#include "graph.h"
#include "oilgraph.h"

/* *************************************************** */
/* Graph building and modification functions           */
/* *************************************************** */

OilNode nullNode = {
	.graph = NULL;
	.matched = false;
	.ochain = { .prev = NULL ; .val = 0 ; .next = NULL };
	.ichain = { .prev = NULL ; .val = 0 ; .next = NULL };
	.lchain = { .prev = NULL ; .val = 0 ; .next = NULL };
	.rchain = { .prev = NULL ; .val = 0 ; .next = NULL };
};

OilGraph *newOilGraph() {
	int i;
	(++gsp)->graph = newGraph();
	for (i=0; i<TOOMANY; i++) {
		gsp->chains[i] = nullNode;
	}
	return gsp;
}

/* When kind == RootTrav val is ignored! */
void insertInChain(OilNode *n, Roil kind, int val) {
	int ix = val < TOOMANY ? val : TOOMANY-1;
	switch (kind) {
		case RootTrav:
			gsp->chains[1].rchain.next = n;
			n.rchain.prev = gsp->chains[1];
			break;
		case OutTrav:
			gsp->chains[ix].ochain.next = n;
			n.ochain.prev = gsp->chains[ix];
			break;
		case InTrav:
			gsp->chains[ix].ichain.next = n;
			n.ichain.prev = gsp->chains[ix];
			break;
		case LoopTrav:
			gsp->chains[ix].lchain.next = n;
			n.lchain.prev = gsp->chains[ix];
			break;
	}
}

OilNode *addNewOilNode(bool root) {
	onp->node = newNode(root, NULL);
	onp->matched = false;
	onp->ochain.val = 0;
	insertInChain(onp, OutTrav, 0);
	onp->ichain.val = 0;
	insertInChain(onp, InTrav, 0);
	onp->lchain.val = 0;
	insertInChain(onp, LoopTrav, 0);
	if (root) {
		onp->rchain.val = 1;
		insertInChain(onp, RootTrav, 1);
	}
	return onp;
}

OilEdge *addNewOilEdge(OilNode *src, OilNode *dst) {

}

void remOilNode(OilNode *node) {

}

void remOilEdge(OilEdge *edge) {

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
void newTrav(Roil kind, bool isInterface, int o, int i, int l);

/* TODO: do we want to also handle root contstraint using this
   interface. probably not */
void constrainTrav(Roil kind, int val);

/* Return the next node from the top traverser. If none then pop
   trav stack */
OilNode *next();

/* Push current nodes and edges pointed to by traversers on to
   node and edge stacks in reverse and clear the TRAV stack */
void foundTrav();




