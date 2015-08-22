#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#define OILR_INDEX_SIZE (1<<(OILR_O_BITS+OILR_I_BITS+OILR_L_BITS+OILR_R_BITS))
#define DEFAULT_POOL_SIZE (1000000)
#define ABORT return

void _HOST();
void _GPMAIN();
long bindCount   = 0;
long unbindCount = 0;


/////////////////////////////////////////////////////////
// graph structure

struct Node;
struct Edge;
struct Element;

typedef struct DList {
	long count;
	struct Element *el;
	struct DList *head;
	struct DList *next;
	struct DList *prev;
} DList;

typedef struct Node {
	DList index;
	DList outEdges;
	DList inEdges;
	DList loops;
	long root;
	long sig;
} Node;

typedef struct Edge {
	struct Element *src;
	struct Element *tgt;
	DList outList;
	DList inList;
} Edge;

typedef struct Element {
	union {
		Node n;
		Edge e;
		struct Element *free;
	};
	long bound;
} Element;

typedef struct Graph {
	long freeId;
	long poolSize;
	long nodeCount;
	long edgeCount;
	Element *pool;
	Element *freeList;
	DList idx[OILR_INDEX_SIZE];
} Graph;

Graph g;

#define asNode(el) (&(el)->n)
#define asEdge(el) (&(el)->e)

#ifdef NDEBUG
#define debug(...)
#define debugCode(...)
#define oilrStatus(...)
#else
#define debug(...) do { fprintf(stderr, __VA_ARGS__); } while (0)
#define debugCode(c) do { c ; } while (0)
#define oilrStatus(node) do { Element *mEl = (node); Node *mN = asNode(mEl); debug("\tNode %ld has OILR %ld: (%ld, %ld, %ld, %ld)\n", elementId(mEl), signature(mN), outdeg(mN), indeg(mN), loopdeg(mN), isroot(mN)); } while (0)
#endif

/////////////////////////////////////////////////////////
// stack-machine

#define DS_SIZE 16
long ds[DS_SIZE];
long *dsp = ds;
long boolFlag = 1;

#define DEF(id) void (id)() {
#define END     }
#define POP     (*(dsp--))
#define TOS     (*(dsp))
#define LIT(n)  do { *(++dsp) = (n); } while (0);
#define ADD     do { long sum = POP + TOS ; TOS = sum; } while (0);
#define SUB     do { long val = POP ; TOS = TOS - val; } while (0);
#define SHL     do { long bits = POP ; TOS = TOS << bits } while (0);
#define SHR     do { long bits = POP ; TOS = TOS >> bits } while (0);
#define LT      do { long n = POP; boolFlag = ( n >= POP ); } while (0);
#define GT      do { long n = POP; boolFlag = ( n <= POP ); } while (0);
#define EMIT    do { printf("%ld\n", POP); } while (0);



/////////////////////////////////////////////////////////
// doubly-linked list support

#define nextElem(dl) ((dl)->next)
#define prevElem(dl) ((dl)->prev)
#define elementOfListItem(dl) ((dl)->el)
#define setElementOfListItem(dl, elem) do { (dl)->el = (elem); } while (0)
#define listLength(dl) ((dl)->count)
#define incListLength(dl) ((dl)->count++)
#define decListLength(dl) ((dl)->count--)

void prependElem(DList *dl, DList *elem) {
#ifndef NDEBUG
	long len = listLength(dl);
#endif
	DList *nx = dl->next;
	elem->head = dl;
	elem->prev = NULL ;
	elem->next = nx;
	if (nx)
		dl->next->prev = elem;
	dl->next = elem;
	incListLength(dl);
	assert(listLength(dl) == len+1);
	assert(listLength(dl) >= 0 && listLength(dl) < g.poolSize);
	debug("dl %p has length %ld after insert\n", dl, dl->count);
}
// void appendElem(DList *dl, DList *elem) {
// 	elem->head = dl;
// 	elem->prev = dl->prev;
// 	elem->next = NULL;
// 	dl->prev->next = elem;
// 	dl->prev = elem;
// 	dl->data.count++;
// }

void removeElem(DList *elem) {
	DList *dl = elem->head;
	DList *nx = elem->next;
	DList *pv = elem->prev;
#ifndef NDEBUG
	long len = listLength(dl);
#endif
	
	if (nx)
		nx->prev = pv;
	else
		dl->prev = pv;

	if (pv)
		pv->next = nx;
	else
		dl->next = nx;

	elem->next = NULL;
	elem->prev = NULL;
	decListLength(dl);
	assert(listLength(dl) == len-1);
	assert(dl->count >= 0 && dl->count < g.poolSize);
	debug("dl %p has length %ld after remove\n", dl, dl->count);
}

/////////////////////////////////////////////////////////
// graph traversal

#define source(e)   (((e)->src))
#define target(e)   (((e)->tgt))
#define outChain(e) (&(e)->outList)
#define inChain(e)  (&(e)->inList)

#define chainFor(n)        (&(n)->index)
#define outListFor(n)      (&(n)->outEdges)
#define inListFor(n)       (&(n)->inEdges)
#define loopListFor(n)     (&(n)->loops)

#define indeg(n)    (listLength(inListFor(n)))
#define outdeg(n)   (listLength(outListFor(n)))
#define loopdeg(n)  (listLength(loopListFor(n)))
#define isroot(n)   ((n)->root)

#define index(sig) &(g.idx[sig])

#define getElementById(id) &(g.pool[(id)])
#define elementId(el) ((el)-g.pool)

long min(long x, long y) {
	return (x<=y ? x : y);
}

long signature(Node *n) {
	long o = min( (1<<OILR_O_BITS)-1 , outdeg(n) ) << (OILR_I_BITS+OILR_L_BITS+OILR_R_BITS),
		 i = min( (1<<OILR_I_BITS)-1 , indeg(n)  ) << (OILR_L_BITS+OILR_R_BITS),
		 l = min( (1<<OILR_L_BITS)-1 , loopdeg(n)) << OILR_R_BITS,
		 r = min( (1<<OILR_R_BITS)-1 , isroot(n) );
	long sig = (o+i+l+r);
	assert(sig >= 0 && sig < OILR_INDEX_SIZE);
	return sig;
}

#if defined(OILR_PARANOID_CHECKS) && !defined(NDEBUG)
long walkChain(DList *dl) {
	long len = 0;
	while ( (dl = nextElem(dl)) )
		len++;
	return len;
}
void checkGraph() {
	long i, iLen, nodes=0, inEdges=0, outEdges=0, loops=0;
	DList *ind;
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		ind = index(i);
		iLen = listLength(ind);
		nodes += iLen;
		while ( (ind = nextElem(ind)) ) {
			Node *n = asNode(elementOfListItem(ind));
			oilrStatus(elementOfListItem(ind));
			debug("\t\t -> o: %ld\n", walkChain(outListFor(n)));
			iLen--;
			outEdges += outdeg(n);
			inEdges  += indeg(n);
			loops    += loopdeg(n);
			assert( walkChain(outListFor(n) ) == outdeg(n)  );
			assert( walkChain(inListFor(n)  ) == indeg(n)   );
			assert( walkChain(loopListFor(n)) == loopdeg(n) );
		}
		assert(iLen == 0);
	}
	assert(nodes == g.nodeCount);
	assert(inEdges == outEdges);
	assert(outEdges + loops == g.edgeCount);
}
#else
#define checkGraph()
#endif

/////////////////////////////////////////////////////////
// graph manipulation

#define setRoot(n)   do { (n)->root = 1 ; } while (0)
#define unsetRoot(n) do { (n)->root = 0 ; } while (0)
#define setRootById(n) setRoot( asNode(getElementById(n)) )

void freeElement(Element *ne) {
	ne->free = g.freeList;
	g.freeList = ne;
}


void indexNode(Node *n) {
	long sig = signature(n);
	n->sig = sig;
	prependElem(index(sig), chainFor(n));
}
void unindexNode(Node *n) {
	removeElem(chainFor(n));
	n->sig = -1;
}

void checkMemory(long neededElems) {
	Element *newPool;
	if (g.freeId + neededElems >= g.poolSize) {
		newPool = realloc(g.pool, sizeof(Element) * g.poolSize);
		if (!newPool) {
			printf("Ran out of memory. :( Sadface\n");
		}
		assert(newPool == g.pool);
		debug("Expanded memory to %ld elements\n", g.poolSize);
	}
	debug("g.freeId: %ld\n", g.freeId);
}
Element *allocElement() {
	Element *ne = g.freeList;

	if (ne == NULL) {
		if (g.freeId == g.poolSize) {
			g.poolSize = g.poolSize * 2;
			g.pool = realloc(g.pool, sizeof(Element) * g.poolSize);
			if (!g.pool) {
				printf("Ran out of memory. :( Sadface\n");
				exit(1);
			}
			debug("Memory grew to %ld elements\n", g.poolSize);
		}
		assert(g.freeId < g.poolSize);
		ne = &(g.pool[g.freeId++]);
	} else {
		g.freeList = ne->free;
	}
	memset(ne, '\0', sizeof(Element));
	return ne;
}

Element *addNode() {
	Element *el = allocElement();
	Node *n = asNode(el);
	setElementOfListItem( chainFor(n)   , el );
	setElementOfListItem( outListFor(n) , el );
	setElementOfListItem( inListFor(n)  , el );
	setElementOfListItem( loopListFor(n), el );
	indexNode(n);
	g.nodeCount++;
	assert(indeg(n) == 0 && outdeg(n) == 0 && loopdeg(n) == 0);
	debug("( ) Created node %ld\n", elementId(el));
	return el;
}
Element *addLoop(Element *node) {
	Element *el = allocElement();
	Edge *e     = asEdge(el);
	Node *n     = asNode(node);
#ifndef NDEBUG
	long lcLen=loopdeg(n);
#endif
	unindexNode(n);
	prependElem(loopListFor(n), outChain(e) );
	e->src = node;
	e->tgt = node;
	setElementOfListItem( outChain(e), el );
	indexNode(n);
	g.edgeCount++;
	assert( lcLen+1 == listLength(loopListFor(n)) );
	assert( listLength(loopListFor(n)) >= 0 );
	debug("C₇O Created loop %ld on node %ld\n", elementId(el), elementId(node) );
	oilrStatus(node);
	return el;
}
Element *addEdge(Element *src, Element *tgt) {
	Element *el = allocElement();
	Edge *e = asEdge(el);
	Node *s = asNode(src), *t = asNode(tgt);
#ifndef NDEBUG
	long icLen=listLength(inListFor(t)), ocLen=listLength(outListFor(s));
#endif
	unindexNode(s);
	unindexNode(t);
	prependElem( outListFor(s), outChain(e) );
	prependElem( inListFor(t), inChain(e) );
	e->src = src;
	e->tgt = tgt;
	setElementOfListItem( outChain(e), el );
	setElementOfListItem( inChain(e),  el );
	indexNode(s);
	indexNode(t);
	g.edgeCount++;
	assert( icLen+1 == listLength(inListFor(t)) && ocLen+1 == listLength(outListFor(s)) );
	assert( listLength(inListFor(t)) >= 0 && listLength(outListFor(s)) >= 0 );
	debug("--> Created edge %ld as %ld-->%ld\n", elementId(el), elementId(src), elementId(tgt) );
	oilrStatus(src); oilrStatus(tgt);
	return el;
}
void addEdgeById(long sid, long tid) {
	if (sid == tid) {
		addLoop(getElementById(sid));
	} else {
		addEdge(getElementById(sid), getElementById(tid));
	}
}

void deleteNode(Element *el) {
	Node *n = asNode(el);
#ifndef NDEBUG
	debug("(X) deleted node %ld\n", elementId(el));
	if (indeg(n) + outdeg(n) + loopdeg(n)) {
		printf("Dangling condition violated\n");
		exit(1);
	}
#endif
	unindexNode(n);
	freeElement(el);
	g.nodeCount--;
}
void deleteEdge(Element *el) {
	Edge *e = asEdge(el);
	Element *s = source(e), *t = target(e);
	Node *src = asNode(s), *tgt = asNode(t);
	if (src == tgt) {
		debug("CxO deleted loop %ld\n", elementId(el));
		unindexNode(src);
		removeElem(outChain(e));
		freeElement(el);
		indexNode(src);
		oilrStatus(s);
	} else {
		debug("-X> deleted edge %ld\n", elementId(el));
		unindexNode(src);
		unindexNode(tgt);
		removeElem(outChain(e));
		removeElem(inChain(e));
		freeElement(el);
		indexNode(src);
		indexNode(tgt);
		oilrStatus(s); oilrStatus(t);
	}
	g.edgeCount--;
}

/////////////////////////////////////////////////////////
// graph search

#define unbound(el) ( !(el)->bound )
#define bound(el)   ( (el)->bound )

#ifdef NDEBUG
#define bindEdge(el) bind(el)
#define bindNode(el) bind(el)
#define bindLoop(el) bind(el)
#else
#define bindEdge(el)  do { Element *macroE = (el); bind(macroE) ; debug("\tBound edge %ld (%ld-->%ld)\n", elementId(macroE), elementId(source(asEdge(macroE))), elementId(target(asEdge(macroE)))); } while (0)
#define bindNode(el)  do { Element *macroN = (el); bind(macroN) ; debug("\tBound node %ld\n", elementId(macroN)); } while (0)
#define bindLoop(el)  do { Element *macroL = (el); bind(macroL) ; debug("\tBound loop %ld (on %ld)\n", elementId(macroL), elementId(source(asEdge(macroL)))); } while (0)
#endif

void bind(Element *el) {
	assert(el);
	assert(unbound(el));
	el->bound = 1;
	bindCount++;
}
void unbind(Element *el) {
	if (el) {
		// assert(bound(el));
		el->bound = 0;
		unbindCount++;
		debug("\tUnbound %ld\n", elementId(el));
	}
	//debug("\tUnbound %ld\n", ((el)==NULL ? 0 : elementId(el)));
}
void unbindAll(Element **travs, long n) {
	long i;
	for (i=0; i<n; i++)
		unbind(travs[i]);
}

Element *searchList(DList **dlp) {
	// iterate over the unbound nodes in doubly-linked list dlp
	Element *e;
	while (*dlp) {
		*dlp = nextElem(*dlp);
		if (*dlp && ( e = elementOfListItem(*dlp) ) && unbound(e) )
			return e;
	}
	return NULL;
}
typedef enum {
	OutEdge, InEdge
} Direction;

void lookupNode(DList **dlp, Element **node) {
	// iterate over nodes in dlp, binding them to *node.
	unbind(*node);
	if ( (*node = searchList(dlp)) ) {
		bindNode(*node);
		boolFlag = 1;
		return;
	}
	boolFlag = 0;
}
void followEdges(DList **dlp, Element **edge, Element **node, Direction dirn) {
	// iterate over the edges in dlp, which are in the direction specified by dirn,
	// bind the edge and node on the other end to *edge and *node respectively.
	unbind(*edge);
	unbind(*node);
	while ( (*edge = searchList(dlp)) ) {
		// we know edge is unbound, because searchList() checks.
		// We still need to check that node is available.
		*node = (dirn==OutEdge) ? target(asEdge(*edge)) : source(asEdge(*edge));
		if (unbound(*node)) {
			bindEdge(*edge);
			bindNode(*node);
			boolFlag = 1;
			return;
		}
	}
	*node = NULL;
	boolFlag = 0;
}
void edgeBetween(Element **edge, Element *src, Element *tgt) {
	// Find an outgoing edge from src to tgt
	// TODO: possible optimisation to be had in picking the shortest list to traverse
	unbind(*edge);
	DList *dl = outListFor(asNode(src));
	assert(src != NULL); assert(tgt != NULL);
	debug("out list for %ld contains %ld entries\n", elementId(src), outdeg(asNode(src)));
	debug("\tSearching for edge between %ld and %ld... ", elementId(src), elementId(tgt) ); 
	while ( (*edge = searchList(&dl)) ) {
		// searchList() only returns unbound edges, so no check is necessary
		if ( target(asEdge(*edge)) == tgt ) {
			debug("found edge %ld\n", elementId(*edge));
			bindEdge(*edge);
			boolFlag = 1;
			return;
		}
	}
	debug("not found\n");
	*edge = NULL;
	boolFlag = 0;
}
void loopOnNode(Element *node, Element **edge) {
	unbind(*edge);
	DList *dl = loopListFor(asNode(node));
	while ( (*edge = searchList(&dl)) ) {
		// found an unbound loop
		debug("found loop %ld\n", elementId(*edge));
		bindLoop(*edge);
		boolFlag = 1;
		return;
	}
	debug("loop not found\n");
	*edge = NULL;
	boolFlag = 0;
}

// A simple Trav only searches a single OILR index
#define makeSimpleTrav(dest, oilrInd)  \
do { \
	DList *dl = (state[dest]) ? state[dest] : (oilrInd); \
	lookupNode(&dl, &matches[(dest)]); \
	state[dest] = dl; \
} while (0)

// a full Trav searches a list of OILR indices
#define makeTrav(dest, ...) \
do { \
	static DList *searchSpace[] = { __VA_ARGS__ , NULL}; \
	static long pos = 0; \
	DList *dl = state[dest]; \
 \
	if (!dl) { \
		pos = 0; \
		dl = searchSpace[0]; \
	} \
 \
	do { \
		lookupNode(&dl, &matches[(dest)]); \
		if (boolFlag) { \
			break ; \
		} \
	} while ( (dl = searchSpace[++pos]) ); \
	state[dest] = dl; \
} while (0)

#define makeExtendOutTrav(fromTrav, eDest, nDest, predCode) \
do { \
	Element *src=matches[fromTrav]; \
	DList *dl = (state[eDest]) ? state[eDest] : outListFor(asNode(src));    \
	oilrStatus(src); \
 	assert(eDest != nDest && fromTrav != eDest && fromTrav != nDest);     \
	assert(src);                           \
	do { \
		followEdges(&dl, &matches[eDest], &matches[nDest], OutEdge); \
	} while (boolFlag && ! predCode); \
	state[eDest] = dl ; \
} while (0)

#define makeExtendInTrav(toTrav, eDest, nDest, predCode) \
do { \
	Element *src=matches[toTrav]; \
	DList *dl = (state[eDest]) ? state[eDest] : inListFor(asNode(src));    \
	oilrStatus(src); \
 	assert(eDest != nDest && toTrav != eDest && toTrav != nDest);     \
	assert(src);                           \
	do { \
		followEdges(&dl, &matches[eDest], &matches[nDest], InEdge); \
	} while (boolFlag && ! predCode); \
	state[eDest] = dl ; \
} while (0)

#define makeEdgeTrav(srcTrav, edgeTrav, tgtTrav) \
do { \
	edgeBetween(&matches[edgeTrav], matches[srcTrav], matches[tgtTrav]); \
} while (0)

#define makeLoopTrav(nTrav, edgeTrav) \
do { \
	loopOnNode(matches[nTrav], &matches[edgeTrav]); \
} while (0)

#define makeAntiEdgeTrav(srcTrav, tgtTrav) \
do { \
	Element *antiEdge = NULL; \
	edgeBetween(&antiEdge, matches[srcTrav], matches[tgtTrav]); \
	unbind(antiEdge); \
	boolFlag = 1-boolFlag; \
} while (0)


/////////////////////////////////////////////////////////
// utilities

#define getId(ne) (((Element *) (ne)) - g.pool)
void dumpGraph() {
	long i;
	DList *index, *out;
	Node *n, *src, *tgt;
	Edge *e;
#ifndef NDEBUG
	long nodeCount = 0, nodeIndexCount = 0;
	long edgeCount = 0, edgeIndexCount = 0;
#endif
	printf("[\n");
	// Dump nodes
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		debugCode( nodeIndexCount += listLength(index) );
		while ( (index = nextElem(index)) ) {
			char *rootStatus;
			debugCode( nodeCount++ );
			assert(unbound(elementOfListItem(index)));
			n = asNode(elementOfListItem(index));
			rootStatus = isroot(n) ? " (R)" : "";
			printf("\t( n%ld%s, empty)\n", getId(n), rootStatus );
		}
	}
	debug("%ld %ld\n", nodeIndexCount, nodeCount);
	assert(nodeIndexCount == nodeCount);
	assert(nodeCount == g.nodeCount);

	printf("|\n");
	// Dump edges and loops
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		while ( (index = nextElem(index)) ) {
			n = asNode(elementOfListItem(index));
			out = outListFor(n);
			debugCode( edgeIndexCount += listLength(out) );
			while ( (out = nextElem(out)) ) {
				debugCode( edgeCount++ );
				assert(unbound(elementOfListItem(out)));
				e = asEdge(elementOfListItem(out));
				src = asNode(source(e));
				tgt = asNode(target(e));
				printf("\t( e%ld, n%ld, n%ld, empty)\n", getId(e), getId(src), getId(tgt) );
			}
			out = loopListFor(n);
			debugCode( edgeIndexCount += listLength(out) );
			while ( (out = nextElem(out)) ) {
				debugCode( edgeCount++ );
				assert(unbound(elementOfListItem(out)));
				e = asEdge(elementOfListItem(out));
				src = asNode(source(e));
				printf("\t( e%ld, n%ld, n%ld, empty)\n", getId(e), getId(src), getId(src) );
			}
		}
	}
	printf("]\n");
	debug("%ld %ld\n\n", edgeIndexCount, edgeCount);
	assert(edgeIndexCount == edgeCount);
}


/////////////////////////////////////////////////////////
// main

int main(int argc, char **argv) {
	long i;
	g.pool = malloc(sizeof(Element) * DEFAULT_POOL_SIZE);
	if (!g.pool)
		exit(1);
	g.poolSize = DEFAULT_POOL_SIZE;
	g.nodeCount = 0;
	g.edgeCount = 0;
	g.freeId = 1;  // pool[0] is not used so zero can function as a NULL value
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		DList *ind = index(i);
		ind->count = 0;
		ind->head  = NULL;
		ind->next  = NULL;
		ind->prev  = NULL;
	}

	checkGraph();
	_HOST();
	checkGraph();
	_GPMAIN();
#ifndef NDEBUG
	debug("Program completed in %ld bind and %ld unbind operations.\n", bindCount, unbindCount);
#else
	fprintf(stderr, "Program completed in %ld bind operations.\n", bindCount);
#endif
	//assert(bindCount == unbindCount);
	dumpGraph();
	free(g.pool);
	return 0;
}

/////////////////////////////////////////////////////////
// generated code goes here....


