#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define OILR_INDEX_SIZE (1<<(OILR_O_BITS+OILR_I_BITS+OILR_L_BITS+OILR_R_BITS))
#define DEFAULT_POOL_SIZE (10000)

void _HOST();
void _GPMAIN();
long bindCount = 0;


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
	union {
		long root:1;
		long loops:15;
		long matchedLoops:15;
	};
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
	Element *pool;
	Element *freeList;
	DList idx[OILR_INDEX_SIZE];
} Graph;

Graph g;

#define asNode(el) (&(el)->n)
#define asEdge(el) (&(el)->e)

#ifdef NDEBUG
#define debug(...)
#define oilrStatus(...)
#else
#define debug(...) do { fprintf(stderr, __VA_ARGS__); } while (0)
#define oilrStatus(node) do { debug("\tNode %ld has OILR: %ld, %ld, %d, %d\n", elementId(node), outdeg(asNode(node)), indeg(asNode(node)), loopdeg(asNode(node)), isroot(asNode(node))); } while (0)
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
	DList *nx = dl->next;
	elem->head = dl;
	elem->prev = NULL ;
	elem->next = nx;
	if (nx)
		dl->next->prev = elem;
	dl->next = elem;
	incListLength(dl);
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

#define indeg(n)    (listLength(inListFor(n)))
#define outdeg(n)   (listLength(outListFor(n)))
#define loopdeg(n)  ((n)->loops)
#define isroot(n)   ((n)->root)

#define index(sig) &(g.idx[sig])

#define getElementById(id) &(g.pool[(id)])
#define elementId(el) ((el)-g.pool)

#define space(id) (index( searchSpaces[*(id)] ))

/////////////////////////////////////////////////////////
// graph manipulation

#define setRoot(n)   do { (n)->root = 1 } while (0)
#define unsetRoot(n) do { (n)->root = 0 } while (0)

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
	return ne;
}

Element *addNode() {
	Element *el = allocElement();
	Node *n = asNode(el);
	setElementOfListItem( chainFor(n), el );
	setElementOfListItem( outListFor(n), el );
	setElementOfListItem( inListFor(n), el );
	indexNode(n);
	assert(indeg(n) == 0 && outdeg(n) == 0 && loopdeg(n) == 0);
	debug("(x) Created node %ld\n", elementId(el));
	return el;
}
void addLoop(Node *n) {
	n->loops++;
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
#ifndef NDEBUG
	assert( icLen+1 == listLength(inListFor(t)) && ocLen+1 == listLength(outListFor(s)) );
	assert( listLength(inListFor(t)) >= 0 && listLength(outListFor(s)) >= 0 );
#endif
	debug("--> Created edge %ld as %ld-->%ld\n", elementId(el), elementId(src), elementId(tgt) );
	oilrStatus(src); oilrStatus(tgt);
	return el;
}
void addEdgeById(long sid, long tid) {
	addEdge(getElementById(sid), getElementById(tid));
}

void deleteNode(Element *el) {
	Node *n = asNode(el);
	if (indeg(n) + outdeg(n) + loopdeg(n)) {
		printf("Dangling condition violated\n");
		exit(1);
	}
	unindexNode(n);
	freeElement(el);
}
void deleteLoop(Element *n) {
	asNode(n)->loops--;
}
void deleteEdge(Element *el) {
	Edge *e = asEdge(el);
	Node *src = asNode(source(e));
	Node *tgt = asNode(target(e));
	unindexNode(src);
	unindexNode(tgt);
	removeElem(outChain(e));
	removeElem(inChain(e));
	freeElement(el);
	indexNode(src);
	indexNode(tgt);
}

/////////////////////////////////////////////////////////
// graph search

#define unbound(el) ( !(el)->bound )
#define bound(el)   ( (el)->bound )

#ifdef NDEBUG
#define bindEdge(el) bind(el)
#define bindNode(el) bind(el)
#else
#define bindEdge(el)  do { Element *macroE = (el); bind(macroE) ; debug("\tBound edge %ld (%ld-->%ld)\n", elementId(macroE), elementId(source(asEdge(macroE))), elementId(target(asEdge(macroE)))); } while (0)
#define bindNode(el)  do { Element *macroN = (el); bind(macroN) ; debug("\tBound node %ld\n", elementId(macroN)); } while (0)
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
	}
	debug("\tUnbound %ld\n", ((el)==NULL ? 0 : elementId(el)));
}
void unbindAll(Element **travs, long n) {
	long i;
	for (i=0; i<n; i++)
		unbind(travs[i]);
}

Element *searchList(DList **dlp) {
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
	unbind(*node);
	while ( (*node = searchList(dlp)) ) {
		bindNode(*node);
		boolFlag = 1;
		return;
	}
	boolFlag = 0;
}
void followEdges(DList **dlp, Element **edge, Element **node, Direction dirn) {
	unbind(*edge);
	unbind(*node);
	while ( (*edge = searchList(dlp)) ) {
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
	unbind(*edge);
	DList *dl = outListFor(asNode(src));
	debug("out list for %ld contains %ld entries\n", elementId(src), outdeg(asNode(src)));
	debug("\tSearching for edge between %ld and %ld... ", elementId(src), elementId(tgt) ); 
	while ( (*edge = searchList(&dl)) && unbound(*edge) ) {
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

#define makeSimpleTrav(travName, dest, list)  \
do { \
	DList *dl = (state[dest]) ? state[dest] : (list); \
	lookupNode(&dl, &travs[(dest)]); \
	state[dest] = dl; \
} while (0);

#define makeTrav(travName, dest, ...) \
void travName(Element **travs) { \
	static DList *searchSpace[] = { __VA_ARGS__ , NULL}; \
	static long pos = 0; \
	static DList *dl = NULL; \
	Element *e = NULL; \
 \
	if (!dl) \
		dl = searchSpace[0]; \
 \
	do { \
		lookupNode(&dl, &travs[(dest)]); \
		if (boolFlag) \
			return ; \
		dl = searchSpace[++pos]; \
	} while (dl); \
	dl = NULL; \
	pos = 0; \
}

#define makeExtendOutTrav(travName, fromTrav, eDest, nDest, predCode) \
do { \
	Element *src=travs[fromTrav]; \
	DList *dl = (state[eDest]) ? state[eDest] : outListFor(asNode(src));    \
	oilrStatus(src); \
 	assert(eDest != nDest && fromTrav != eDest && fromTrav != nDest);     \
	assert(src);                           \
	do { \
		followEdges(&dl, &travs[eDest], &travs[nDest], OutEdge); \
	} while (boolFlag && ! predCode); \
	state[eDest] = dl ; \
} while (0);

#define makeEdgeTrav(travName, destTrav, srcTrav, tgtTrav) \
do { \
	edgeBetween(&travs[destTrav], travs[srcTrav], travs[tgtTrav]); \
} while (0);

#define makeAntiEdgeTrav(travName, destTrav, srcTrav, tgtTrav) \
do { \
	Element *antiEdge = NULL; \
	edgeBetween(&antiEdge, travs[srcTrav], travs[tgtTrav]); \
	unbind(antiEdge); \
	boolFlag = 1-boolFlag; \
} while (0);


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
#ifndef NDEBUG
		nodeIndexCount += listLength(index);
#endif
		while ( (index = nextElem(index)) ) {
#ifndef NDEBUG
			nodeCount++;
			assert(unbound(elementOfListItem(index)));
#endif
			n = asNode(elementOfListItem(index));
			//printf("%lx %lx\n", (long) n, (long) g.pool);
			printf("\t( n%ld, empty)\n", getId(n) );
		}
	}
	assert(nodeIndexCount == nodeCount);
	printf("|\n");
	// Dump edges
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		while ( (index = nextElem(index)) ) {
			n = asNode(elementOfListItem(index));
			out = outListFor(n);
#ifndef NDEBUG
			edgeIndexCount += listLength(out);
#endif
			while ( (out = nextElem(out)) ) {
#ifndef NDEBUG
				edgeCount++;
				assert(unbound(elementOfListItem(out)));
#endif
				e = asEdge(elementOfListItem(out));
				src = asNode(source(e));
				tgt = asNode(target(e));
				printf("\t( e%ld, n%ld, n%ld, empty)\n", getId(e), getId(src), getId(tgt) );
			}
		}
	}
	printf("]\n");
	debug("%ld %ld\n\n", edgeIndexCount, edgeCount);
	assert(edgeIndexCount == edgeCount);
	fprintf(stderr, "Program completed in %ld bind operations.\n", bindCount);
}


/////////////////////////////////////////////////////////
// main

int main(int argc, char **argv) {
	g.pool = malloc(sizeof(Element) * DEFAULT_POOL_SIZE);
	g.poolSize = DEFAULT_POOL_SIZE;
	g.freeId = 1;  // pool[0] is not used so zero can function as a NULL value
	if (!g.pool)
		exit(1);

	_HOST();
	_GPMAIN();
	dumpGraph();
	free(g.pool);
	return 0;
}

/////////////////////////////////////////////////////////
// generated code goes here....


