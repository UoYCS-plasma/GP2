#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <error.h>

#include "oilrrt.h"

extern void Main();
extern void _HOST();

bool success = false;


#define fail(...) do { printf ("\n ** ") ; printf (__VA_ARGS__) ; passed = false; } while (false);

#define nextInChain(on) ((on)->chain.next)
#define prevInChain(on) ((on)->chain.prev)

#define hasRealNode(on) ((on) && (on)->node != NULL)

#define getNodeId(n)      ( (n)==NULL ? -1 : (int)((n)-nodePool))
#define getEdgeId(e)      ( (e)==NULL ? -1 : (int)((e)-edgePool))
#define getOilrNodeId(on) \
	((on)==NULL ? -1 \
		: ((void*)(on) > (void*)&gsp[0] && (void*)(on) < (void*)&gsp[2]) ? -2 \
		: (int)((on)-oilrNodePool))
#define getTravId(t)      ( (t)==NULL ? -1 : (int)((t)-travStack))

#define isDeletedEdge(e)  ( (e)->source == NULL )

#define match(on) (on)->matched = true
#define unmatch(on) (on)->matched = false
#define isMatched(on) ((on)->matched)

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
	.chain = { .prev = NULL , .index = NULL , .next = NULL },
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



void dumpTravStack(Traverser *txp) {
	int id;
	char *col;
	Traverser *t = travStack;
	printf("  ");

	while (t <= tsp) {
		if (t == txp)
			printf("[0;1m>");
		switch (t->type) {
			case NodeTrav:
				id = getOilrNodeId(t->n.oilrNode);
				col = id<0 ? "[31m" : "[32m";
				if (id>=0)
					printf("%sn%d ", col, id);
				else if (id == -1)
					printf("%sn_ ", col);
				else
					printf("[31mn?? ");
				break;
			case EdgeTrav:
				id = getEdgeId(t->e.edge);
				col = id<0 ? "[31m" : "[32m";
				if (id>=0)
					printf("%se%d ", col, id);
				else
					printf("%se_ ", col);
				break;
			case XeTrav:
				col = "[33m";
				printf("%sxe ", col);
				break;
			default:
				col = "[31m";
				printf("%s__ ", col);
		}
		if (t == txp)
			printf("[0m");
		t++;
	}
	printf("[0m\n");
}


void initIndices(OilrGraph *g) {
	int o,i,l;
	Index *s, *sr;
	for (o=0; o<TOOMANYO; o++) {
		for (i=0; i<TOOMANYI; i++) {
			for (l=0; l<TOOMANYL; l++) {
				s = &(g->indices[o][i][l][false]);
				sr = &(g->indices[o][i][l][true]);

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
	initIndices(gsp);
	return gsp;
}

void makeSearchSpace(Traverser *t) {
	int o, i, l, pos=0;
	Index *ind;
	NodeTraverser *nt = &(t->n);
	SearchSpace *spc;
	/* only node-traversers need search spaces */
	if (!isNodeTrav(t))
		return;

	if ( !(spc = malloc(sizeof(SearchSpace))) ) {
		error(1, 0, "Couldn't allocate a new search space");
	}
	spc->pos = 0;
	spc->size = 0;

	if (nt->isInterface) {
		for (o=nt->o; o<nt->capo; o++) {
			for (i=nt->i; i<nt->capo; i++) {
				for (l=nt->l; l<nt->capl; l++) {
					/* add all indices containing candidates to search space */
					ind = &(gsp->indices[o][i][l][nt->r]);
					if (ind->len == 0)
						continue;
					spc->index[pos++] = ind;
					spc->size += ind->len;
					/* TODO: special cases for spc->size == 1 and spc->size == 0 */
				}
			}
		}
		trace("Search space %d: O:%d-%d  I:%d-%d  L:%d-%d  R:%d", getTravId(t),
				nt->o, nt->capo,
				nt->i, nt->capi,
				nt->l, nt->capl,
				nt->r);
		trace("Contains %d nodes across %d live indices", spc->size, pos);
	} else {
		/* TODO */
		spc->index[pos++] = &(gsp->indices[nt->o][nt->i][nt->l][nt->r]);
	}
	spc->index[pos+1] = NULL;
	t->n.searchSpace = spc;
}

void disconnect(OilrNode *n) {
	/* TODO: not updating chain-length count! */
	OilrNode *prev = prevInChain(n), *next = nextInChain(n);
	n->chain.index->len--;
	nextInChain(prev) = next;
	prevInChain(next) = prev;
	n->chain.index = NULL;
	nextInChain(n) = NULL;
	prevInChain(n) = NULL;
}

/* When kind == RootTrav val is ignored! */
void connect(OilrNode *n) {
	OilrNode *next, *head;
	Index *s;
	int loop = n->node->loopdegree;
	int out  = n->node->outdegree - loop;
	int in   = n->node->indegree - loop;
	bool root = n->node->root;
	out  = out  < TOOMANYO ? out  : TOOMANYO-1;
	in   = in   < TOOMANYI ? in   : TOOMANYI-1;
	loop = loop < TOOMANYL ? loop : TOOMANYL-1;

	s = &(gsp->indices[out][in][loop][root]);
	head = &(s->head);
	next = nextInChain(head);
	nextInChain(head) = n;
	prevInChain(n) = head;
	nextInChain(n) = next;
	n->chain.index = s;
	if (next != NULL)
		next->chain.prev = n;
	s->len++;
}

OilrNode *addNewOilrNode(bool root) {
	newNodeTrav(true, 0, 0, 0, root);
	tsp->type = NodeTrav;
	freeOilrNode->node = newNode(root, NULL);
	unmatch(freeOilrNode);
	freeOilrNode->chain.index = NULL;
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
	 * so, beware of double-freeing loops! TODO: remove from index Tables too! */
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



void newNodeTrav(bool isInterface, int o, int i, int l, bool root) {
	/* TODO: o, i, and l should be capped at TOOMANYx too */
	tsp++;
	tsp->type = NodeTrav;
	tsp->n.o = tsp->n.fromo = o;
	tsp->n.i = tsp->n.fromi = i;
	tsp->n.l = tsp->n.froml = l;
	tsp->n.r = root;
	tsp->n.capo = TOOMANYO;
	tsp->n.capi = TOOMANYI;
	tsp->n.capl = TOOMANYL;
	tsp->n.oilrNode = NULL; //  &(gsp->indices[o][i][l][root].head);
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
	trace("clearTravs() (success=%s):", success ? "true" : "false");
	while (tsp >= travStack) {
		if (isNodeTrav(tsp)) {
			if (tsp->n.searchSpace)
				free(tsp->n.searchSpace);
			if (hasRealNode(tsp->n.oilrNode) ) {
				unmatch(tsp->n.oilrNode);
				tsp->type = InvalidTrav;
			}
		}
		tsp--;
	}
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
	int o, i, l;
	NodeTraverser *nt = &(t->n);
	EdgeTraverser *et = &(t->e);
	if ( isNodeTrav(t) ) {
		/* reset the search space tracker */
		nt->searchSpace->pos = 0;
		o = nt->fromo;
		i = nt->fromi;
		l = nt->froml;
		if (nt->oilrNode && hasRealNode(nt->oilrNode)) {
			unmatch(nt->oilrNode);
			nt->oilrNode = NULL;
		}
		nt->o = o;
		nt->i = i;
		nt->l = l;
	} else if (isEdgeTrav(t) ) {
		et->edge = NULL;
	}
}

OilrNode *nextUnmatchedNodeInChain(OilrNode *on) {
	/* requires a valid OilrNode! */
	if (hasRealNode(on))
		unmatch(on);
	do {
		on = on->chain.next;
		if (!on)
			return NULL;
	} while (isMatched(on));
	match(on);
	return on;
}

bool findCandidateNode(NodeTraverser *nt) {
	/* TODO: nt must have a valid, non-exhausted search space! */
	SearchSpace *spc = nt->searchSpace;
	Index *ind = spc->index[spc->pos];
	OilrNode *on = nt->oilrNode == NULL ? &(ind->head) : nt->oilrNode;
	
	do {
		if (!on)
			on = &(ind->head);
		while ( (on = nextUnmatchedNodeInChain(on)) ) {
			nt->oilrNode = on;
			return true;
		} 
		spc->pos++;
	} while ( (ind = spc->index[spc->pos]) );
	return false;
}

void runSearch() {
	Traverser *txp = travStack;
	SearchSpace *spc;
	bool found;
	dumpTravStack(txp);

	/* set-up search spaces */
	while (txp <= tsp) {
		if (isNodeTrav(txp)) {
			makeSearchSpace(txp); /* init the search space */
			spc = txp->n.searchSpace;
			if (spc->size == 0) {
				/* can't ever match this traverser with this host graph! */
				trace("Shortcut exit due to unmatchable rule node.");
				success = false;
				return;
			}
		}
		/* TODO: fix traverser on single matching node */
		txp++;
	}

	txp = travStack;
	while (txp <= tsp) {
		if ( isNodeTrav(txp) && !findCandidateNode(&(txp->n)) ) {
			success = false;
			resetTrav(txp);
			txp--;
			dumpTravStack(txp);
			if (txp < travStack)
				return;
			continue;
		} else if (isEdgeTrav(txp)) {
			found = nextEdge(&(txp->e));
			if (isNegated(txp))
				found = !found;
			if (!found) {
				//trace("\t[33m-- failure[0m\n");
				success = false;
				while (txp >= travStack && isEdgeTrav(txp)) {
					/* finding a different edge won't help with structural match */
					txp->e.edge = NULL;
					txp--;
				}
				dumpTravStack(txp);
				continue;
			}
			//trace("\t[32m-- success[0m edge=%d\n", getEdgeId(txp->e.edge));
		}
		txp++;	
		dumpTravStack(txp);
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
	Index *s;
	Node *n;
	Edge *e;
	OilrNode *on;

	for (i=0; i<edgeCount; i++) {
		if ( isDeletedEdge(&(edgePool[i])) )
			deletedEdgeCount++;
	}

	printf("Testing invariants... ");
	/* sum of lens in indices
	 * 	== nodeCount
	 * 	== oilrNodeCount */
	if (nodeCount != oilrNodeCount) {
		fail("Number of Nodes is not the same as number of OilrNodes");
	}
	for (o=0; o<TOOMANYO; o++) {
		for (i=0; i<TOOMANYI; i++) {
			for (l=0; l<TOOMANYL; l++) {
				steps = 0;
				skips = 0;
				s = &(gsp->indices[o][i][l][false]);
				/* root chain -> prev -> next shoudl be root chain. */
				if (&(s->head) != s->head.chain.next->chain.prev)
					fail("Root chaining incorrectly initialised");

				if (!isMatched(&(s->head)))
					fail("dummy node with unset matched flag: [%d][%d][%d][false]", o, i, l);

				sum += s->len;

				/* every real node n in chain ch points back to index s, at the
				 * head of ch */
				on = &(s->head);
				while ( on->chain.next != NULL && steps <= MAX_NODES) {
					/* on->next is never equal to on->prev or on! */
					if (on->chain.next == on)
						fail("Link with self-referential next pointer in chain [%d][%d][%d]",
								o, i, l);
					if (on->chain.prev == on)
						fail("Link with self-referential prev pointer");
					if (on->chain.next == on->chain.prev)
						fail("Link with identical next and prev");
					on = on->chain.next;
					steps++;
					if (on->node == NULL) /* skip dummy nodes */
						skips++;
					if (on->node != NULL && on->chain.index != s) {
						fail("Found node with incorrect index pointer");
					}
					if (hasRealNode(on) && isMatched(on))
						fail("Found a spurious matched node: %d", getOilrNodeId(on));
					/*if (!passed)
						break;*/
				}
				if (steps-skips != s->len)
					fail("Found an incorrect chain length. %d-%d != s->len: %d",
							steps, skips, s->len);

				while ( on->chain.prev != NULL && steps >= 0 ) {
					on = on->chain.prev;
					steps--;
				}

				/* every chain must be the same length in both directions, and 
				 * prevs must end at the head where nexts started. */
				if (on != &(s->head) )
					fail("Found an inconsistency in a chain.");
				if (steps != 0)
					fail("asymmetric chain");

				s = &(gsp->indices[o][i][l][true]);
				if (!isMatched(&(s->head)))
					fail("Found a dummy node on root chain with unset matched flag");

			}
		}
	}
	if (sum != nodeCount) {
		fail("nodeCount (%d) differs from number of index table entries (%d)",
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
		fail("Number of Edges (%d) differs from sum of outdegrees (%d)",
				edgeCount, o);
	}
	if (edgeCount-deletedEdgeCount != i) {
		fail("Number of Edges (%d) differs from sum of indegrees (%d)",
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
			fail("Found a node with an incorrect outdegree");
		}
		/* every node, n with a loopdegree of l has exactly 
		*   l out-edges with target=n */
		if (loops != n->loopdegree) {
			fail("Found a node with an incorrect loopdegree");
		}
	}
		

	/* for every edge e, e is an element of s->source->out_edges */
	if (passed)
		printf("[32mOK[0m\n");
	else
		printf("\n[31mFailed[0m\n");
	return passed;
}

int main(int argc, char **argv) {
#ifndef OILR_STANDALONE
	log_file = stderr;
#endif
	newOilrGraph();
	_HOST();
	dumpGraph();
	Main();
	dumpGraph();
	return 0;
}
