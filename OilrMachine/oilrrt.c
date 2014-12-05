#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <error.h>

#include "oilrrt.h"

extern void Main();
extern void _HOST();

bool success = false;

#define DYNAMIC_OPTS

#define nextInChain(on) ((on)->chain.next)
#define prevInChain(on) ((on)->chain.prev)

#define hasRealNode(on) ((on) && (on)->node != NULL)

#define getOilrNodeId(on) \
	((on)==NULL ? -1 \
		: ((void*)(on) > (void*)&gsp[0] && (void*)(on) < (void*)&gsp[2]) ? -2 \
		: (int)((on)-oilrNodePool))
#define getTravId(t)      ( (t)==NULL ? -1 : (int)((t)-travStack))

#define isDeletedEdge(e)  ( (e)->source == NULL )
#define isDeletedOilrNode(n)  ( (n)->node == NULL )

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

/* *************************************************** */
/* Graph building and modification functions           */
/* *************************************************** */

OilrNode nullNode = {
	.node = NULL,
	.matched = true, /* always set so dummy nodes won't be returned by a traverser */
	.chain = { .prev = NULL , .index = NULL , .next = NULL },
};



#ifdef OILR_STANDALONE

#define getNodeId(n)      ( (n)==NULL ? -1 : (int)((n)-nodePool))
#define getEdgeId(e)      ( (e)==NULL ? -1 : (int)((e)-edgePool))

#define getO(n) ((n)->outdegree - (n)->loopdegree)
#define getI(n) ((n)->indegree  - (n)->loopdegree)
#define getL(n) ((n)->loopdegree)
#define getR(n) ((n)->root)

#define copyLabel(...) NULL

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
	freeNode->index = getNodeId(freeNode);
	freeNode->indegree = 0;
	freeNode->outdegree = 0;
	freeNode->loopdegree = 0;

	return freeNode++;
}

Edge *newEdge(bool bidirectional, Label *label, Node *src, Node *tgt) {
	trace("newEdge() src=%d tgt=%d", getNodeId(src), getNodeId(tgt));
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

/* we don't need either of these functions */
#define addNode(...)
#define addEdge(...)


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
		if (free->index >= 0) {
			free++;
			break;
		}
	}
	freeEdge = free;
}

void removeNode(Graph *g, int index) {
	Node *free = freeNode;
	Node *n = &(nodePool[index]);
	n->index = -1;
	while (free > nodePool) {
		/* compaction */
		--free;
		if (free->index >= 0) {
			free++;
			break;
		}
	}
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

#else

FILE *log_file;

#define getNodeId(n)      ( (n)==NULL ? -1 : (n)->index )
#define getEdgeId(e)      ( (e)==NULL ? -1 : (e)->index )

#define dumpGraph(...)
#define getO(n) ((n)->outdegree - getL(n))
#define getI(n) ((n)->indegree  - getL(n))
#define getR(n) ((n)->root)


int getL(Node *n) {
	int i, loopcount = 0;
	for (i=0; i<n->outdegree; i++) {
		if (n->out_edges[i]->source == n->out_edges[i]->target)
			loopcount++;
	}
	return loopcount;
}

Label emptyLabel = { .mark=0, .list = NULL, .list_length = 0, .has_list_variable = false };

#endif


#ifdef DEBUG
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
				else {
					printf("[31mn?? ");
					trace("Suspicious OilrNode id: %d", id);
				}
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
#endif


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
	SearchSpace *spc = &(nt->searchSpace);
	Index **buf;
	/* only node-traversers need search spaces */
	if (!isNodeTrav(t))
		return;

	if ( !(buf = malloc(sizeof(Index *) * TOOMANYO * TOOMANYL * TOOMANYI + 1)) ) { 
		error(1, 0, "Couldn't allocate a new search space");
	}
	spc->pos = 0;
	spc->size = 0;
	spc->index = buf;
	spc->edgeFrom = NULL;

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
	int out  = getO(n->node);
	int in   = getI(n->node);
	int loop = getL(n->node);
	bool root = getR(n->node);
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
	OilrNode *on;
	Node *n = newNode(root, copyLabel(&emptyLabel));
	addNode(gsp->graph, n);
	on = &(oilrNodePool[n->index]);
	on->node = n;
	unmatch(on);
	on->chain.index = NULL;
	connect(on);

	newNodeTrav(true, 0, 0, 0, root);
	tsp->type = NodeTrav;
	tsp->n.oilrNode = on;

	return on;
}

Edge *addNewEdge(NodeTraverser *src, NodeTraverser *tgt) {
	/* bidirectional param has no meaning for host graph edges, so is always
	 * false. TODO: NULL label pointer is not safe! Should be Label structure
	 * containing a GList with one element of type EMPTY! */
	trace("Edge from %d to %d", src->oilrNode->node->index, tgt->oilrNode->node->index);
	Edge *edge = newEdge(false, copyLabel(&emptyLabel), src->oilrNode->node, tgt->oilrNode->node);
	//trace("Edge %d --> old outdegree: %d", getEdgeId(edge), getO(src->oilrNode->node));
	addEdge(gsp->graph, edge);
	//trace("Edge %d --> new outdegree: %d", getEdgeId(edge), getO(src->oilrNode->node));
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
	tsp->n.searchSpace.pos = 0;
	tsp->n.searchSpace.edgeFrom = 0;
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
			if (tsp->n.searchSpace.index)
				free(tsp->n.searchSpace.index);
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
		nt->searchSpace.pos = 0;
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

OilrNode *nextUnmatchedNodeFromEdge(NodeTraverser *nt) {
	int pos, in, out, loop;
	bool root;
	OilrNode *on, *origin = nt->searchSpace.edgeFrom->oilrNode;
	Node *n = NULL;
	if (origin == NULL)
		error(1, 0, "NULL node as edge origin! Something went wrong!");

	if (hasRealNode(nt->oilrNode))
		unmatch(nt->oilrNode);
			
	trace("%d", nt->searchSpace.pos);
	for (pos=nt->searchSpace.pos; pos<origin->node->outdegree; pos++) {
		n = origin->node->out_edges[pos]->target;
		out  = getO(n);
		in   = getI(n);
		loop = getL(n);
		root = getR(n);

		if (root == nt->r) {
			if (nt->isInterface
					/* TODO: completely disregards limited range (eg from indeg() directive) */
					&& out  >= nt->o
					&& in   >= nt->i
					&& loop >= nt->l ) {
				on = &(oilrNodePool[n->index]);
				match(on); /* Nodes and OilrNodes must have the same index */
				break;
			} else if ( out == nt->o
					&&   in == nt->i
					&& loop == nt->l) {
				on = &(oilrNodePool[n->index]);
				match(on); /* Nodes and OilrNodes must have the same index */
				break;
			}
		}
		on = NULL;
	}

	nt->searchSpace.pos = pos+1;
	return on;
}

bool findCandidateNode(NodeTraverser *nt) {
	/* TODO: nt must have a valid, non-exhausted search space! */
	SearchSpace *spc = &(nt->searchSpace);
	Index *ind;
	OilrNode *on;
	
	if (spc->edgeFrom) {
		while ( (on = nextUnmatchedNodeFromEdge(nt)) ) {
			nt->oilrNode = on;
			return true;
		}
	} else {
		ind = spc->index[spc->pos];
		on = nt->oilrNode == NULL ? &(ind->head) : nt->oilrNode;
		do {
			if (!on)
				on = &(ind->head);
			while ( (on = nextUnmatchedNodeInChain(on)) ) {
				nt->oilrNode = on;
				return true;
			} 
			spc->pos++;
		} while ( (ind = spc->index[spc->pos]) );
	}
	return false;
}

void runSearch() {
	Traverser *txp = tsp;
	SearchSpace *spc;
	bool found;

	/* set-up search spaces */
	while (txp >= travStack) {
		if (isNodeTrav(txp) && !txp->n.searchSpace.edgeFrom) {
			/* TODO: fix traverser result on single matching node */
			makeSearchSpace(txp); /* init the search space */
			spc = &(txp->n.searchSpace);
			if (spc->size == 0) {
				/* can't ever match this traverser with this host graph! */
				trace("Shortcut exit due to unmatchable rule node.");
				success = false;
				return;
			}
		}
#ifdef DYNAMIC_OPTS
		else if (isEdgeTrav(txp) && txp->e.src <= txp->e.tgt) {
			/* TODO: only works for out-edges currently. Needs to consider in-edges too */
			txp->e.tgt->searchSpace.edgeFrom = txp->e.src;
			
		} 
#endif
		txp--;
	}

	txp = travStack;
	dumpTravStack(txp);
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


#ifdef TEST_INVARIANTS
/* yes, I know this is very bad practice... */
#include "invariants.c"
#endif

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
