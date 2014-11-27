#include <stdbool.h>

#include "Compiler/graph.h"
#include "oilrgraph.h"

extern void Main();

/* *************************************************** */
/* Graph building and modification functions           */
/* *************************************************** */

OilrNode nullNode = {
	.node = NULL,
	.matched = true, /* always set so dummy nodes won't be returned by a traverser */
	.chain = { .prev = NULL , .val = 0 , .next = NULL },
};


void initShadowTables(OilrGraph *g) {
	int o,i,l;
	for (o=0; o<TOOMANYO; o++) {
		for (i=0; i<TOOMANYI; i++) {
			for (l=0; l<TOOMANYL; l++) {
				g->shadowTables[o][i][l][false].head = nullNode;
				g->shadowTables[o][i][l][true].head  = nullNode;
				/* Non-root list falls through to dummy node of root list */
				g->shadowTables[o][i][l][false].head.chain.next=&(g->shadowTables[o][i][l][true].head);
			}
		}
	}

}

OilrGraph *newOilrGraph() {
	(++gsp)->graph = newGraph();
	initShadowTables(gsp);
	return gsp;
}

/* When kind == RootTrav val is ignored! */
void insertInChain(OilrNode *n, int out, int in, int loop, bool root) {
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

OilrNode *addNewOilrNode(bool root) {
	onp->node = newNode(root, NULL);
	onp->matched = false;
	onp->chain.val = 0;
	insertInChain(onp, 0, 0, 0, root);
	return onp++;
}

Edge *addNewEdge(OilrNode *src, OilrNode *dst) {
	/* bidirectional param has no meaning for host graph edges, so is always false. TODO: NULL label pointer is not safe! Should be Label structure containing a GList with one element of type EMPTY! */
	Edge *edge = newEdge(false, NULL, src->node, dst->node);
	addEdge(gsp->graph, edge);
	return edge;
}

void remOilrNode(NodeTraverser *nt) {
	/* TODO: should this automatically remove all in- and out-bound edges? If
	 * so, beware of double-freeing loops! TODO: remove from Shadow Tables too! */
	removeNode(gsp->graph, nt->oilrNode->node->index);
	nt->oilrNode->node = NULL; /* mark for garbage collection */
	while (onp->node == NULL && onp > oilrNodePool) {
		/* maybe free some nodes */
		onp--;
	}
}


void deleteEdges() {
	/* Delete all edges on the TRAV stack */
	int i;
	Traverser *t;
	for (i=0; i<TRAV_STACK_SIZE; i++) {
		t = &(travStack[i]);
		if (t > tsp)
			break;
		if (isEdgeTrav(t)) {
			removeEdge(gsp->graph, t->e.edge->index);
			t->type = InvalidTrav;
			t->e.edge = NULL;
		}
	}
}

void deleteNonInterfaceNodes() {
	/* Delete all non-interface nodes in TRAV registers */

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
void newNodeTrav(bool isInterface, int o, int i, int l, bool root) {
	tsp++;
	tsp->type = NodeTrav;
	tsp->n.o = o;
	tsp->n.i = i;
	tsp->n.l = l;
	tsp->n.r = root;
	tsp->n.capo = TOOMANYO;
	tsp->n.capi = TOOMANYI;
	tsp->n.capl = TOOMANYL;
	tsp->n.oilrNode = &(gsp->shadowTables[o][i][l][root].head);
	tsp->n.isInterface = isInterface;
}


void newEdgeTrav(NodeTraverser *src, NodeTraverser *tgt) {
	tsp++;
	tsp->type = EdgeTrav;
	tsp->e.edge = NULL;
	tsp->e.src = src;
	tsp->e.tgt = tgt;
}

void newNegatedEdgeTrav(NodeTraverser *src, NodeTraverser *tgt) {
	newEdgeTrav(src, tgt);
	tsp->type = XeTrav;
}

/* Return the next node from the top traverser. If none then pop
   trav stack */
OilrNode *nextNode(NodeTraverser *nt) {
	int o,i,l;
	OilrNode *n = nt->oilrNode;

	/* Only unset the matched flag if not a dummy node! */
	if (n->node != NULL)
		n->matched = false;

	if (nt->isInterface) {
		/* TODO: naive traversal strategy! */
		/* TODO: only works for fixing in- and out-degrees at zero */
		for (o=nt->o; o<nt->capo; o++) {
			for (i=nt->i; i<nt->capi; i++) {
				for (l=nt->l; l<nt->capl; l++) {
					do {
						n = n->chain.next;
					} while (n != NULL && n->matched);
					if (n != NULL) {
						n->matched = true;
						nt->o = o;
						nt->i = i;
						nt->l = l;
						return n;
					}
				}
			}
		}

		/* update state so that we don't repeat last traversal if the spent
		 * traverser incorrectly gets called again */
		nt->o = o;
		nt->i = i;
		nt->l = l;
	} else {
		do {
			n = n->chain.next;
		} while (n != NULL && n->matched);

		if (n != NULL) {
			n->matched = true;
			return n;
		}
	}
	return NULL;
}

Edge *findEdgeBetween(OilrNode *src, OilrNode *tgt) {
	/* TODO: not marking edge as matched! */
	int i;
	int nedges = src->node->outdegree;
	Edge **es = src->node->out_edges;

	for (i=0; i<nedges; i++) {
		if (es[i] != NULL && es[i]->target == tgt->node) {
			return es[i];
		}
	}
	return NULL;
}

Edge *nextEdge(EdgeTraverser *et) {
	Edge *e;
	if (et->edge) {
		/* This et has been called before. For structural matching there is never any benefit to finding a second edge between the same two nodes, so we just clear edge and fail. */
		et->edge = NULL;
		return NULL;
	}
	e = findEdgeBetween(et->src->oilrNode, et->tgt->oilrNode);

	return e;
}

void constrainO(NodeTraverser *nt) {
	nt->capo = 1;
}
void constrainI(NodeTraverser *nt) {
	nt->capi = 1;
}
void constrainL(NodeTraverser *nt) {
	nt->capl = 1;
}

void runSearch() {
	Traverser *txp = travStack;
	OilrNode *n;
	Edge *e;
	while (txp <= tsp) {
		if (isNodeTrav(txp)) {
			n = nextNode(&(txp->n));
			if (n == NULL) {
				/* backtrack */
				success = false;
				if (txp == travStack)
					return;
				txp--;
			}
		} else if (isEdgeTrav(txp)) {
			e = nextEdge(&(txp->e));
			if (e == NULL) {
				/* TODO: backtrack */
				success = false;
				while (--txp >= travStack && isNodeTrav(txp)) {} /* finding a different edge won't help with structural match */
				continue;
			}
		}
		txp++;	
	}
	success = true;
}


int main(int argc, char **argv) {
	Main();
	return 0;
}
