#include <stdio.h>
#include <stdbool.h>

#include "oilrrt.h"

extern void Main();
extern void _HOST();

bool success = false;


#define fail(...) do { printf (__VA_ARGS__) ; passed = false; } while (false);

#define nextInChain(on) (on->chain.next)
#define prevInChain(on) (on->chain.prev)

#define hasRealNode(on) (on->node != NULL)

#define getNodeId(n)      ( n==NULL ? -1 : (int)(n-nodePool))
#define getEdgeId(e)      ( e==NULL ? -1 : (int)(e-edgePool))
#define getOilrNodeId(on) (on==NULL ? -1 \
							: ((void*)on > (void*)&gsp[0] && (void*)on < (void*)&gsp[2]) ? -2 \
							: (int)(on-oilrNodePool))
#define getTravId(t)      ( t==NULL ? -1 : (int)(t-travStack))

#define isDeletedEdge(e)  ( (e)->source == NULL )

/* TODO: no bounds checking! stack overflow will happen! */
Traverser travStack[TRAV_STACK_SIZE];
Traverser *tsp = travStack-1;

/* TODO: no bounds checking! stack overflow will happen! Also we are wasting the first element of the array by pre-incrementing pointer. Find a neater solution! */
OilrGraph oilrGraphStack[GRAPH_STACK_SIZE];
OilrGraph *gsp = oilrGraphStack;

/* MAX_NODES and MAX_EDGES are defined in graph.h */
OilrNode oilrNodePool[MAX_NODES];
OilrNode *freeOilrNode = oilrNodePool;

/* *************************************************** */
/* Graph building and modification functions           */
/* *************************************************** */

OilrNode nullNode = {
	.node = NULL,
	.matched = true, /* always set so dummy nodes won't be returned by a traverser */
	.chain = { .prev = NULL , .shadow = NULL , .next = NULL },
};



#ifdef OILR_STANDALONE

Graph dummyGraph = { NULL };

Node nodePool[MAX_NODES];
Edge edgePool[MAX_EDGES];

Node *freeNode = nodePool;
Edge *freeEdge = edgePool;

Graph *newGraph() {
	return &dummyGraph;
}

Node *newNode(bool root, Label *label) {
	freeNode->root = root;
	freeNode->index = freeNode; /* not an index! just to preserve API compat */
	freeNode->indegree = 0;
	freeNode->outdegree = 0;
	freeNode->loopdegree = 0;

	return freeNode++;
}

Edge *newEdge(bool bidirectional, Label *label, Node *src, Node *tgt) {
	freeEdge->source = src;
	freeEdge->target = tgt;
	freeEdge->index = freeEdge;
	src->out_edges[src->outdegree] = freeEdge;
	src->outdegree++;
	tgt->indegree++;
	/* todo track in_edges as well as out_edges */
	if (src == tgt)
		src->loopdegree++;
	return freeEdge++;
}

void addEdge(Graph *g, Edge *e) {
	/* Adding is handled by newEdge, as we don't have a graph structure! */
	return;
}

void removeEdge(Graph *g, Edge *e) {
	Edge *free = freeEdge;
	Node *src = e->source, *tgt = e->target;
	int i;
	e->source = NULL;
	e->target = NULL;
	e->index = NULL;
	for (i=0; i<src->outdegree; i++) {
		if (src->out_edges[i] == e) {
			tgt->indegree--;
			src->outdegree--;
			if (src == tgt)
				src->loopdegree--;
			/* keep out_edges contiguous by moving the final element to the freed slot */
			if (i == src->outdegree)
				src->out_edges[i] = NULL;
			else
				src->out_edges[i] = src->out_edges[src->outdegree];
			break;
		}
	}

	while (free > edgePool) {
		/* compaction */
		--free;
		if (free->index != NULL) {
			free++;
			break;
		}
	}
	freeEdge = free;
}

void removeNode(Graph *g, Node *n) {
	Node *free = freeNode;
	n->index = NULL;
	while (free > nodePool) {
		/* compaction */
		--free;
		if (free->index != NULL) {
			free++;
			break;
		}
	}
}

#else

FILE *log_file;

#endif




void initShadowTables(OilrGraph *g) {
	int o,i,l;
	Shadow *s, *sr;
	for (o=0; o<TOOMANYO; o++) {
		for (i=0; i<TOOMANYI; i++) {
			for (l=0; l<TOOMANYL; l++) {
				s = &(g->shadowTables[o][i][l][false]);
				sr = &(g->shadowTables[o][i][l][true]);

				s->head = nullNode;
				sr->head = nullNode;
				s->len = 0;

				/* Non-root list falls through to dummy node of root list */
				s->head.chain.next = &(sr->head);
				sr->head.chain.prev = &(s->head);
			}
		}
	}

}

OilrGraph *newOilrGraph() {
	(++gsp)->graph = newGraph();
	initShadowTables(gsp);
	return gsp;
}

void disconnect(OilrNode *n) {
	/* TODO: not updating chain-length count! */
	OilrNode *prev = prevInChain(n), *next = nextInChain(n);
	n->chain.shadow->len--;
	nextInChain(prev) = next;
	prevInChain(next) = prev;
	n->chain.shadow = NULL;
	nextInChain(n) = NULL;
	prevInChain(n) = NULL;
}

/* When kind == RootTrav val is ignored! */
void connect(OilrNode *n) {
	OilrNode *next, *head;
	Shadow *s;
	int loop = n->node->loopdegree;
	int out  = n->node->outdegree - loop;
	int in   = n->node->indegree - loop;
	bool root = n->node->root;
	out  = out  < TOOMANYO ? out  : TOOMANYO-1;
	in   = in   < TOOMANYI ? in   : TOOMANYI-1;
	loop = loop < TOOMANYL ? loop : TOOMANYL-1;

	s = &(gsp->shadowTables[out][in][loop][root]);
	head = &(s->head);
	next = nextInChain(head);
	nextInChain(head) = n;
	prevInChain(n) = head;
	nextInChain(n) = next;
	n->chain.shadow = s;
	if (next != NULL)
		next->chain.prev = n;
	s->len++;
}

OilrNode *addNewOilrNode(bool root) {
	newNodeTrav(true, 0, 0, 0, root);
	tsp->type = NodeTrav;
	freeOilrNode->node = newNode(root, NULL);
	freeOilrNode->matched = false;
	freeOilrNode->chain.shadow = NULL;
	tsp->n.oilrNode =  freeOilrNode;
	connect(freeOilrNode);
	return freeOilrNode++;
}

Edge *addNewEdge(NodeTraverser *src, NodeTraverser *tgt) {
	/* bidirectional param has no meaning for host graph edges, so is always
	 * false. TODO: NULL label pointer is not safe! Should be Label structure
	 * containing a GList with one element of type EMPTY! */
	Edge *edge = newEdge(false, NULL, src->oilrNode->node, tgt->oilrNode->node);
	addEdge(gsp->graph, edge);
	disconnect(src->oilrNode);
	connect(src->oilrNode);
	disconnect(tgt->oilrNode);
	connect(tgt->oilrNode);
	return edge;
}

void remOilrNode(NodeTraverser *nt) {
	/* TODO: should this automatically remove all in- and out-bound edges? If
	 * so, beware of double-freeing loops! TODO: remove from Shadow Tables too! */
	removeNode(gsp->graph, nt->oilrNode->node->index);
	nt->oilrNode->node = NULL; /* mark for garbage collection */
	disconnect(nt->oilrNode);
	while (freeOilrNode->node == NULL && freeOilrNode > oilrNodePool) {
		/* maybe free some nodes */
		freeOilrNode--;
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
		if ( isEdgeTrav(t) && !isNegated(t) ) {
			removeEdge(gsp->graph, t->e.edge->index);
			t->type = InvalidTrav;
			t->e.edge = NULL;
		}
	}
}

void deleteNonInterfaceNodes() {
	/* Delete all non-interface nodes in TRAV registers */
	int i;
	Traverser *t;
	for (i=0; i<TRAV_STACK_SIZE; i++) {
		t = &(travStack[i]);
		if (t > tsp)
			break;
		if (isNodeTrav(t) && !t->n.isInterface && hasRealNode(t->n.oilrNode)) {
			remOilrNode(&(t->n));
			t->type = InvalidTrav;
		}
	}
}





/* *************************************************** */
/* Traversal functions                                 */
/* *************************************************** */




/* TODO: does having a zero default for o, i and l have any
   implications for non-interface nodes? */
void newNodeTrav(bool isInterface, int o, int i, int l, bool root) {
	tsp++;
	tsp->type = NodeTrav;
	tsp->n.o = tsp->n.fromo = o;
	tsp->n.i = tsp->n.fromi = i;
	tsp->n.l = tsp->n.froml = l;
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

void clearTravs() {
	trace("clearTravs(): success=%s\n", success ? "true" : "false");
	while (tsp >= travStack) {
		if ( isNodeTrav(tsp) && hasRealNode(tsp->n.oilrNode) ) {
			tsp->n.oilrNode->matched = false;
			tsp->type = InvalidTrav;
		}
		tsp--;
	}
}

/* Return the next node from a traverser. */
bool nextNode(NodeTraverser *nt) {
	/* this requires some thought:
	 *
	 * nt may point to either:
	 *  - dummy node at head of chain (fresh trav only)
	 *  - a real node (used trav)
	 *
	 * we must never see an nt with a NULL oilrNode here.
	 */

	int o,i,l;
	OilrNode *on = nt->oilrNode;
	Shadow *s;

	/* if we've got a non-dummy node, clear the matched flag */
	if ( hasRealNode(on) ) {
		on->matched = false;
		on = nextInChain(on);
	}
	trace("\tnextNode():");

	if (nt->isInterface) {
		/* TODO: naive traversal strategy! */
		/* TODO: only works for fixing in- and out-degrees at zero */
		for (o=nt->o; o<nt->capo; o++) {
			for (i=nt->i; i<nt->capi; i++) {
				for (l=nt->l; l<nt->capl; l++) {
					s = &(gsp->shadowTables[o][i][l][nt->r]);
					/* no point descending empty chains */
					if (s->len == 0)
						continue;

					trace("\n\to=%d i=%d l=%d   count=%d", o, i, l, s->len);

					/* if we reached the end of a previous chain
					 * and wrapped to a new one... */
					if (on == NULL)
						on = &(s->head);

					/* skip over dummy and matched nodes */
					while (on && on->matched) {
						on = nextInChain(on);
					} 

					/* if we get to here, we've either... */
					if (on == NULL) /* ...reached the end of the chain... */ 
						continue;
					else {         /* ...or found a candidate */
						on->matched = true;
						nt->oilrNode = on;
						nt->o = o;
						nt->i = i;
						nt->l = l;
						return true;
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
		while (on && on->matched) { /* skip dummy and already-matched nodes */
			on = nextInChain(on);
		}
		if (on != NULL) {
			on->matched = true;
			nt->oilrNode = on;
			return true;
		}
	}
	return false;
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

bool nextEdge(EdgeTraverser *et) {
	if (et->edge) {
		/* This et has been called before. For structural matching there is never any benefit to finding a second edge between the same two nodes, so we just clear edge and fail. */
		return NULL;
	}
	return (et->edge = findEdgeBetween(et->src->oilrNode, et->tgt->oilrNode));
	
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

void resetTrav(Traverser *t) {
	NodeTraverser *nt = &(t->n);
	EdgeTraverser *et = &(t->e);
	if ( isNodeTrav(t) ) {
		nt->o = nt->fromo;
		nt->i = nt->fromi;
		nt->l = nt->froml;
	} else if (isEdgeTrav(t) ) {
		et->edge = NULL;
	}
}

void runSearch() {
	Traverser *txp = travStack;
	bool found;
	while (txp <= tsp) {
		trace("runSearch(): (type: %d) txp=%d \n", txp->type, getTravId(txp));
		if (isNodeTrav(txp)) {
			if (! nextNode(&(txp->n)) ) {
				/* backtrack */
				trace("\n\t[33m-- failure[0m\n");
				success = false;
				resetTrav(txp);
				txp--;
				if (txp<travStack) {
					trace("\texit runSearch()\n");
					return;
				}
				continue;
			}
			trace(" [32m-- success[0m oNode=%d\n", getOilrNodeId(txp->n.oilrNode));
		} else if (isEdgeTrav(txp)) {
			found = nextEdge(&(txp->e));
			if (isNegated(txp))
				found = !found;
			if (!found) {
				trace("\n\t[33m-- failure[0m\n");
				success = false;
				while (--txp >= travStack && isEdgeTrav(txp)) {} /* finding a different edge won't help with structural match */
				continue;
			}
			trace("\t[32m-- success[0m edge=%d\n", getEdgeId(txp->e.edge));
		}
		txp++;	
	}
	success = true;
}

void setRoot(NodeTraverser *nt, bool state) {
	OilrNode *on = nt->oilrNode;
	on->node->root = state;
	disconnect(on);
	connect(on);
}


void dumpGraph() {
	Node *n = nodePool;
	Edge *e = edgePool;
	int src_id, tgt_id, i = 0;

	printf("[\n");
	while (n < freeNode) {
		printf("\t(n%d%s, empty)\n", i++, n->root ? "(R)" : "");
		n++;
	}
	printf("|\n");
	i=0;
	while (e < freeEdge) {
		if (!isDeletedEdge(e)) {
			src_id = getNodeId(e->source);
			tgt_id = getNodeId(e->target);
			printf("\t(e%d, n%d, n%d, empty)\n", i++, src_id, tgt_id);
		}
		e++;
	}
	printf("]\n");
}

bool testInvariants() {
	bool passed = true;
	int o, i, l, sum=0, steps=0, skips=0, loops=0;
	/* TODO: will these still hold we start deleting nodes?! */
	int nodeCount = getNodeId(freeNode);
	int edgeCount = getEdgeId(freeEdge);
	int oilrNodeCount = getOilrNodeId(freeOilrNode);
	int deletedEdgeCount = 0;
	Shadow *s;
	Node *n;
	Edge *e;
	OilrNode *on;

	for (i=0; i<edgeCount; i++) {
		if ( isDeletedEdge(&(edgePool[i])) )
			deletedEdgeCount++;
	}

	printf("Testing invariants...\n");
	/* sum of lens in shadowTables
	 * 	== nodeCount
	 * 	== oilrNodeCount */
	if (nodeCount != oilrNodeCount) {
		fail("** Number of Nodes is not the same as number of OilrNodes\n");
	}
	for (o=0; o<TOOMANYO; o++) {
		for (i=0; i<TOOMANYI; i++) {
			for (l=0; l<TOOMANYL; l++) {
				steps = 0;
				skips = 0;
				s = &(gsp->shadowTables[o][i][l][false]);
				/* root chain -> prev -> next shoudl be root chain. */
				if (&(s->head) != s->head.chain.next->chain.prev)
					fail("Root chaining incorrectly initialised");

				if (!s->head.matched)
					fail("Found a dummy node with unset matched flag");

				sum += s->len;

				/* every real node n in chain ch points back to Shadow s, at the
				 * head of ch */
				on = &(s->head);
				while ( on->chain.next != NULL && steps <= MAX_NODES) {
					/* on->next is never equal to on->prev or on! */
					if (on->chain.next == on)
						fail("Link with self-referential next pointer in chain [%d][%d][%d]\n",
								o, i, l);
					if (on->chain.prev == on)
						fail("Link with self-referential prev pointer\n");
					if (on->chain.next == on->chain.prev)
						fail("Link with identical next and prev\n");
					on = on->chain.next;
					steps++;
					if (on->node == NULL) /* skip dummy nodes */
						skips++;
					if (on->node != NULL && on->chain.shadow != s) {
						fail("** Found node with incorrect shadow pointer\n");
					}
					if (!passed)
						break;
				}
				if (steps-skips != s->len)
					fail("** Found an incorrect chain length. %d-%d != s->len: %d\n",
							steps, skips, s->len);

				while ( on->chain.prev != NULL && steps >= 0 ) {
					on = on->chain.prev;
					steps--;
				}

				/* every chain must be the same length in both directions, and 
				 * prevs must end at the head where nexts started. */
				if (on != &(s->head) )
					fail("** Found an inconsistency in a chain.\n");
				if (steps != 0)
					fail("** asymmetric chain\n");

				s = &(gsp->shadowTables[o][i][l][true]);
				if (!s->head.matched)
					fail("Found a dummy node on root chain with unset matched flag");

			}
		}
	}
	if (sum != nodeCount) {
		fail("** nodeCount (%d) differs from number of shadow table entries (%d)\n",
				nodeCount, sum);
	}

	/* total number of live edges in graph
	 *   == sum of outdegrees of all nodes
	 *   == sum or indegrees of all nodes */
	o = 0;
	i = 0;
	for (l=0; l<nodeCount; l++) {
		o += nodePool[l].outdegree;
		i += nodePool[l].indegree;
	}
	if (edgeCount-deletedEdgeCount != o) {
		fail("** Number of Edges (%d) differs from sum of outdegrees (%d)\n",
				edgeCount, o);
	}
	if (edgeCount-deletedEdgeCount != i) {
		fail("** Number of Edges (%d) differs from sum of indegrees (%d)\n",
				edgeCount, o);
	}

	/* for every node the number of used out_edges must be the node's outdegree */
	o = 0;
	i = 0;
	for (steps=0; steps<nodeCount; steps++) {
		i=0; o=0; loops = 0;
		n = &(nodePool[steps]);
		while ( (e = n->out_edges[i++]) != NULL) {
			if (e->source == e->target)
				loops++;
			o++;
		}
		if (o != n->outdegree) {
			fail("** Found a node with an incorrect outdegree\n");
		}
		/* every node, n with a loopdegree of l has exactly 
		*   l out-edges with target=n */
		if (loops != n->loopdegree) {
			fail("** Found a node with an incorrect loopdegree\n");
		}
	}
		

	/* for every edge e, e is an element of s->source->out_edges */
	if (passed)
		printf("[32mOK[0m\n");
	else
		printf("[31mFailed[0m\n");
	return passed;
}

int main(int argc, char **argv) {
#ifndef OILR_STANDALONE
	log_file = stderr;
#endif
	newOilrGraph();
	if (! testInvariants() )
		return 1;
	_HOST();
	dumpGraph();
	if (! testInvariants() )
		return 1;
	Main();
	if (! testInvariants() )
		return 1;
	dumpGraph();
	return 0;
}
