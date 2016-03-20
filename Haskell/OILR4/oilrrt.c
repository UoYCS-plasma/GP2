#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>


#define OILR_BIND_BITS 1
#define OILR_INDEX_SIZE (1<<(OILR_C_BITS+OILR_O_BITS+OILR_I_BITS+OILR_L_BITS+OILR_R_BITS))
#define DEFAULT_POOL_SIZE (100000000)
#define DONE return
#define RECURSE

void _HOST();
void _GPMAIN();
long bindCount   = 0;
long unbindCount = 0;

char *colourNames[] = { "", "# red", "# blue", "# green", "# grey" };

/////////////////////////////////////////////////////////
// graph structure

struct Element;

typedef struct DList {
	union {
		long count;
		struct DList *head;
	};
	struct DList *next;
	struct DList *prev;
#ifndef OILR_COMPACT_LISTS
	// OILR_COMPACT_LISTS forces Element to be 128 byte alignmened, which lets
	// us simply mask the address of any list to find the Element.
	struct Element *el;
#endif
} DList;

// Flags field bit packing (starting with low-order bits):
#define flags(el) ((el)->flags)
#define setFlags(el, f) do { (el)->flags = (f); } while (0)
#define mask(bits, offs) (((1<<(bits))-1)<<(offs))

// Root-flag "rBits"
#define ROOT_OFFS 0
#define ROOT_MASK mask(OILR_R_BITS, ROOT_OFFS)
#define rBits(el) (flags(el) & ROOT_MASK)
#define isRoot(el) (rBits(el))

// Loop-degree "lBits"
#define LOOP_OFFS (ROOT_OFFS+OILR_R_BITS)
#define LOOP_MASK mask(OILR_L_BITS, LOOP_OFFS)
#define lBits(el) (flags(el) & LOOP_MASK)

// In-degree "iBits"
#define IDEG_OFFS (LOOP_OFFS+OILR_L_BITS)
#define IDEG_MASK mask(OILR_I_BITS, IDEG_OFFS)
#define iBits(el) (flags(el) & IDEG_MASK)

// Out-degree "oBits"
#define ODEG_OFFS (IDEG_OFFS+OILR_I_BITS)
#define ODEG_MASK mask(OILR_O_BITS, ODEG_OFFS)
#define oBits(el) (flags(el) & ODEG_MASK)

// Colour "cBits"
#define COLR_OFFS (ODEG_OFFS+OILR_O_BITS)
#define COLR_MASK mask(OILR_C_BITS, COLR_OFFS)
#define cBits(el) (flags(el) & COLR_MASK)
#define colour(el) (cBits(el)>>COLR_OFFS)

// laBel "bBits"
#define LABL_OFFS (COLR_OFFS+OILR_C_BITS)
#define LABL_MASK mask(OILR_B_BITS, LABL_OFFS)
#define bBits(el) (flags(el) & LABL_MASK)
#define isLabelled(el) (bBits(el))
#define getLabel(el) ((el)->label)

// bound
#define BIND_OFFS (LABL_OFFS+OILR_B_BITS)
#define BIND_MASK mask(OILR_BIND_BITS, BIND_OFFS)
#define bound(el)   (flags(el) & BIND_MASK)
#define unbound(el) (!bound(el))

// Type (node, edge or free). Only used for assertions when debugging is enabled
#define TYPE_OFFS (BIND_OFFS+OILR_BIND_BITS)
#define TYPE_MASK mask(OILR_T_BITS, TYPE_OFFS)
#define eType(el)  (((el)->flags)&TYPE_MASK)
#define isNode(el) (eType(el) == NODE_TYPE)
#define isEdge(el) (eType(el) == EDGE_TYPE)
#define isFree(el) (eType(el) == FREE_TYPE)

typedef struct Element {
	unsigned int flags;
	int label;
	union {
		struct {
			DList index;
			DList outEdges;
			DList inEdges;
			DList loops;
		};
		struct {
			struct Element *src;
			struct Element *tgt;
			DList outList;
			DList inList;
		};
		struct Element *free;
#ifdef OILR_COMPACT_LISTS
		// WARNING: with OILR_COMPACT_LISTS defined Element must be _exactly_
		// OILR_ELEM_ALIGN bytes in size!
		char pad[OILR_ELEM_ALIGN-2*sizeof(int)]
#endif
	};
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

typedef struct SearchSpaceComponent {
	long weight;
	DList *data;
} SearchSpaceComponent;


#ifdef NDEBUG
#define debug(...)
#define debugCode(...)
#define oilrStatus(...)
#else
#define debug(...) do { fprintf(stderr, __VA_ARGS__); } while (0)
#define debugCode(c) do { c ; } while (0)
#define oilrStatus(node) do { Element *mN = (node); debug("\tNode %ld has OILR %ld: (%ld, %ld, %ld, %d)\n", elementId(mN), signature(mN), outdeg(mN), indeg(mN), loopdeg(mN), isRoot(mN)); } while (0)
#endif

/////////////////////////////////////////////////////////
// stack-machine (removed)

long boolFlag = 1;


/////////////////////////////////////////////////////////
// doubly-linked list support

#define OILR_ELEM_MASK (~(OILR_ELEM_ALIGN-1))

#define nextElem(dl) ((dl)->next)
#define prevElem(dl) ((dl)->prev)
#ifdef OILR_COMPACT_LISTS
#define OILR_ELEM_ALIGN 128
	// TODO: introduce a union to prevent this casting evil
#	define elementOfListItem(dl) ((Element *)((long)(dl)&(OILR_ELEM_MASK)))
#	define setElementOfListItem(dl, elem)
#else
#	define elementOfListItem(dl) ((dl)->el)
#	define setElementOfListItem(dl, elem) do { (dl)->el = (elem); } while (0)
#endif
#define listLength(dl) ((dl)->count)
#define incListLength(dl) ((dl)->count++)
#define decListLength(dl) ((dl)->count--)

#if defined(OILR_PARANOID_CHECKS) && !defined(NDEBUG)
long walkChain(DList *dl) {
	DList *l = dl;
	long len=0, blen=0;
	while ( (l = nextElem(l)) )
		len++;
	l = dl;
 	while ( (l = prevElem(l)) )
		blen++;
	assert(blen == len);
	return len;
}
#else
#define walkChain(dl)
#endif

void prependElem(DList *dl, DList *elem) {
#ifndef NDEBUG
	long len = listLength(dl);
#endif
	DList *nx = dl->next;
	elem->head = dl;
	elem->prev = NULL;
	elem->next = nx;
	if (nx)
		dl->next->prev = elem;
	if (!listLength(dl))
		dl->prev = elem;
	dl->next = elem;
	incListLength(dl);
	walkChain(dl);
	assert(listLength(dl) == len+1);
	assert(listLength(dl) >= 0 && listLength(dl) < g.poolSize);
	debug("dl %p has length %ld after insert\n", dl, dl->count);
}
void appendElem(DList *dl, DList *elem) {
#ifndef NDEBUG
	long len = listLength(dl);
#endif
	DList *pv = dl->prev;
	elem->head = dl;
	elem->prev = pv;
	elem->next = NULL;
	if (pv)
		dl->prev->next = elem;
	if (!listLength(dl))
		dl->next = elem;
	dl->prev = elem;
	incListLength(dl);
	walkChain(dl);
	assert(listLength(dl) == len+1);
	assert(listLength(dl) >= 0 && listLength(dl) < g.poolSize);
	debug("dl %p has length %ld after insert\n", dl, dl->count);
}
void sliceBefore(DList *dl, DList *here) {
	here->prev = NULL;
	dl->next = here;
}
void spliceAfter(DList *dl, DList *first, DList *last) {
	dl->prev->next = first;
	first->prev = dl->prev;
	last->next = NULL;
	dl->prev = last;
}
void moveToFront(DList *dl, DList *this) {
	DList *first=dl->next, *last=this->prev;
	sliceBefore(dl, this);
	spliceAfter(dl, first, last);
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

#define source(e)   ((e)->src)
#define target(e)   ((e)->tgt)
#define outChain(e) (&(e)->outList)
#define inChain(e)  (&(e)->inList)

#define chainFor(n)        (&(n)->index)
#define outListFor(n)      (&(n)->outEdges)
#define inListFor(n)       (&(n)->inEdges)
#define loopListFor(n)     (&(n)->loops)

#define outdeg(n)   (listLength(outListFor(n)))
#define indeg(n)    (listLength(inListFor(n)))
#define loopdeg(n)  (listLength(loopListFor(n)))

// Warning: beware of double-evaluation of expressions in these macros!
#define realOutdeg(n) (outdeg(n) + loopdeg(n))
#define realIndeg(n)  (indeg(n)  + loopdeg(n))

#define index(sig) &(g.idx[sig])

#define getElementById(id) &(g.pool[(id)])
#define elementId(el) ((el)-g.pool)

long min(long x, long y) {
	return (x<=y ? x : y);
}

#define signature(n) flags(n)

#if defined(OILR_PARANOID_CHECKS) && !defined(NDEBUG)
void checkGraph() {
	long i, iLen, nodes=0, inEdges=0, outEdges=0, loops=0;
	DList *ind;
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		ind = index(i);
		iLen = listLength(ind);
		nodes += iLen;
		debug("\tind: %ld has len: %ld\n", i, iLen);
		while ( (ind = nextElem(ind)) ) {
			Element *n = elementOfListItem(ind);
			oilrStatus(elementOfListItem(ind));
			debug("\t\t -> o: %ld\n", walkChain(outListFor(n)));
			debug("\t\t -> i: %ld\n", walkChain(inListFor(n)));
			debug("\t\t -> l: %ld\n", walkChain(loopListFor(n)));
			iLen--;
			outEdges += outdeg(n);
			inEdges  += indeg(n);
			loops    += loopdeg(n);
			assert( walkChain(outListFor(n) ) == outdeg(n)  );
			assert( walkChain(inListFor(n)  ) == indeg(n)   );
			assert( walkChain(loopListFor(n)) == loopdeg(n) );
		}
		// debug("//%ld\n", iLen);
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

void indexNode(Element *n) {
	long sig = signature(n);
	prependElem(index(sig), chainFor(n));
}
void unindexNode(Element *n) {
	removeElem(chainFor(n));
}

void setRoot(Element *n) {
	unindexNode(n);
	setFlags(n, flags(n) | ROOT_MASK);
	indexNode(n);
}
void unsetRoot(Element *n) {
	unindexNode(n);
	setFlags(n, flags(n) & ~ROOT_MASK);	
	indexNode(n);
}
void setColour(Element *n, long c) {
	long flags = (flags(n) & ~COLR_MASK) | (c<<COLR_OFFS);

	unindexNode(n);
	setFlags(n, flags);
	indexNode(n);
}
#define setRootById(n) setRoot( getElementById(n) )
#define setColourById(n, c) setColour( getElementById(n), (c) )

void freeElement(Element *ne) {
	ne->free = g.freeList;
	g.freeList = ne;
}


Element *allocElement() {
	Element *ne = g.freeList;
	if (ne == NULL) {
		if (g.freeId == g.poolSize) {
			printf("Ran out of memory. :( Sadface\n");
			exit(1);
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
	Element *n = el;
	setElementOfListItem( chainFor(n)   , el );
	setElementOfListItem( outListFor(n) , el );
	setElementOfListItem( inListFor(n)  , el );
	setElementOfListItem( loopListFor(n), el );
	indexNode(n);
	g.nodeCount++;
	assert(indeg(n) == 0 && outdeg(n) == 0 && loopdeg(n) == 0);
	debug("( ) Created node %ld\n", elementId(n));
	return n;
}
Element *addLoop(Element *node) {
	Element *e = allocElement();
	Element *n     = node;
#ifndef NDEBUG
	long lcLen=loopdeg(n);
#endif
	unindexNode(n);
	prependElem(loopListFor(n), outChain(e) );
	e->src = node;
	e->tgt = node;
	setElementOfListItem( outChain(e), e );
	indexNode(n);
	g.edgeCount++;
	assert( lcLen+1 == listLength(loopListFor(n)) );
	assert( listLength(loopListFor(n)) >= 0 );
	debug("C₇O Created loop %ld on node %ld\n", elementId(e), elementId(node) );
	oilrStatus(n);
	return e;
}
Element *addEdge(Element *s, Element *t) {
	Element *e = allocElement();
#ifndef NDEBUG
	long icLen=listLength(inListFor(t)), ocLen=listLength(outListFor(s));
#endif
	unindexNode(s);
	unindexNode(t);
	prependElem( outListFor(s), outChain(e) );
	prependElem( inListFor(t), inChain(e) );
	e->src = s;
	e->tgt = t;
	setElementOfListItem( outChain(e), e );
	setElementOfListItem( inChain(e),  e );
	indexNode(s);
	indexNode(t);
	g.edgeCount++;
	assert( icLen+1 == listLength(inListFor(t)) && ocLen+1 == listLength(outListFor(s)) );
	assert( listLength(inListFor(t)) >= 0 && listLength(outListFor(s)) >= 0 );
	debug("--> Created edge %ld as %ld-->%ld\n", elementId(e), elementId(s), elementId(t) );
	oilrStatus(s); oilrStatus(t);
	return e;
}
void addEdgeById(long sid, long tid) {
	if (sid == tid) {
		addLoop(getElementById(sid));
	} else {
		addEdge(getElementById(sid), getElementById(tid));
	}
}

void deleteNode(Element *n) {
#ifndef NDEBUG
	debug("(X) deleted node %ld\n", elementId(n));
	if (indeg(n) + outdeg(n) + loopdeg(n)) {
		printf("Dangling condition violated\n");
		exit(1);
	}
#endif
	unindexNode(n);
	freeElement(n);
	g.nodeCount--;
}
void deleteEdge(Element *e) {
	Element *src = source(e), *tgt = target(e);
	if (src == tgt) {
		debug("CxO deleted loop %ld\n", elementId(e));
		unindexNode(src);
		removeElem(outChain(e));
		freeElement(e);
		indexNode(src);
		oilrStatus(src);
	} else {
		debug("-X> deleted edge %ld\n", elementId(e));
		unindexNode(src);
		unindexNode(tgt);
		removeElem(outChain(e));
		removeElem(inChain(e));
		freeElement(e);
		indexNode(src);
		indexNode(tgt);
		oilrStatus(src); oilrStatus(tgt);
	}
	g.edgeCount--;
}

/////////////////////////////////////////////////////////
// graph search


#ifdef OILR_EXECUTION_TRACE
FILE *oilrTraceFile;
long oilrTraceId=0;
char *oilrCurrentRule="";
void trace(char c) {
	fprintf(oilrTraceFile, "%c", c);
}
void oilrTrace(Element *el) {
	DList *ndl, *edl;
	Element *node, *edge;
	Element *n; Element *e;
	char format;
	long i;
	fprintf(oilrTraceFile, "%ld %s :", oilrTraceId, oilrCurrentRule);
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		ndl = index(i);
		while ( (ndl = nextElem(ndl)) ) {
			n = elementOfListItem(ndl);
			edl = outListFor(n);
			format = n == el ? '?' : bound(n) ? '!' : ' ';
			fprintf(oilrTraceFile, " %c%ld", format, elementId(n));
			while ( (edl = nextElem(edl)) ) {
				e = elementOfListItem(edl);
				format = e == el ? '?' : bound(e) ? '!' : ' ';
				fprintf(oilrTraceFile, " %c%ld->%ld", format, elementId(source(e)), elementId(target(e)));
			}
			edl = loopListFor(n);
			while ( (edl = nextElem(edl)) ) {
				e = elementOfListItem(edl);
				format = e == el ? '?' : bound(e) ? '!' : ' ';
				fprintf(oilrTraceFile, " %c%ld->%ld", format, elementId(source(e)), elementId(target(e)));
			}
		}
	}
	fprintf(oilrTraceFile, "\n");
	oilrTraceId++;
}
#else
#define trace(c)
#define oilrTrace(e)
#endif

#ifdef NDEBUG
#define bindEdge(el) bind(el)
#define bindNode(el) bind(el)
#define bindLoop(el) bind(el)
#else
#define bindEdge(el)  do { Element *macroE = (el); bind(macroE) ; debug("\tBound edge %ld (%ld-->%ld)\n", elementId(macroE), elementId(source(macroE)), elementId(target(macroE))); } while (0)
#define bindNode(el)  do { Element *macroN = (el); bind(macroN) ; debug("\tBound node %ld\n", elementId(macroN)); } while (0)
#define bindLoop(el)  do { Element *macroL = (el); bind(macroL) ; debug("\tBound loop %ld (on %ld)\n", elementId(macroL), elementId(source(macroL))); } while (0)
#endif

// Explanation of binding rules:
// - An exclusive bind sets all bits of el->bound
// - A shared bind sets el->boundHere
// - When a rule hands off to a recursive call to itself it should call 
//     packBinds() on all its matches. packBinds() sets el->boundElsewhere
//     and unsets el->boundHere

void bind(Element *el) {
	// global bind operation
	long flags;
	assert(el);
	assert(unbound(el));
	flags = flags(el);
	setFlags( el, (flags | BIND_MASK) );
	bindCount++;
	trace('B');
	oilrTrace(el);
}
void unbind(Element *el) {
	long flags = flags(el);
	if (el) {
		setFlags( el, flags & ~BIND_MASK );
		unbindCount++;
		debug("\tUnbound %ld\n", elementId(el));
	}
	//debug("\tUnbound %ld\n", ((el)==NULL ? 0 : elementId(el)));
}
void unbindAll(Element **regs, long n) {
	long i;
	for (i=0; i<n; i++) {
		unbind(travs[i]);
	}
	oilrTrace(NULL);
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
		*node = (dirn==OutEdge) ? target(*edge) : source(*edge);
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
	DList *dl = outListFor(src);
	assert(src != NULL); assert(tgt != NULL);
	debug("out list for %ld contains %ld entries\n", elementId(src), outdeg(src));
	debug("\tSearching for edge between %ld and %ld... ", elementId(src), elementId(tgt) ); 
	while ( (*edge = searchList(&dl)) ) {
		// searchList() only returns unbound edges, so no check is necessary
		if ( target(*edge) == tgt ) {
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
	DList *dl = loopListFor(node);
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

void adjustWeighting(SearchSpaceComponent *searchSpace, long count) {
	SearchSpaceComponent spc1, spc2;
	DList *ind1, *ind2;
	long i, s1, s2, w1, w2;
	for (i=count-1; i>0; i--) {
		spc1 = searchSpace[i];
		spc2 = searchSpace[i-1];
		ind1 = spc1.data;
		ind2 = spc2.data;
		s1 = listLength( ind1 );
		if (s1 > 0) {
			s2 = listLength( ind2 );
			w1 = spc1.weight;
			w2 = spc2.weight;
			if (s2 == 0 || w1 > w2) {
				searchSpace[i] = spc2;
				searchSpace[i-1] = spc1;
			}
		}
	}
}
#define heavier(ssc) ((ssc).weight++)
#define lighter(ssc) ((ssc).weight--)



#define reg(n) (regs[(n)])

// OILR instructions

#define REGS(n)  Element *regs[n]; regs[0]=NULL

#define ABN(r) addNode();
#define DBE(r) deleteEdge(reg(r))
		
#define BND(dst, spc)      matchANode(reg(dst), reg(spc))
#define BOE(dst, src, tgt) matchAnEdge(reg(dst), reg(src), reg(tgt))
#define BED(dst, r1, r2)   matchBidi(reg(dst), reg(r1), reg(r2))


#define BNZ(tgt) if (boolFlag) goto tgt;

#define TRU() do { boolFlag = 1; } while (0);
#define FLS() do { boolFlag = 0; } while (0);

#define UBN(n)  unbindAll(&regs, (n))



// A simple Trav only searches a single OILR index
#define makeSimpleTrav(dest, oilrInd)  \
do { \
	DList *dl = (state[dest]) ? state[dest] : (oilrInd); \
	lookupNode(&dl, &matches[(dest)]); \
	state[dest] = dl; \
} while (0)

// a full Trav searches a list of OILR indices
#define makeTrav(dest, spcSize, ...) \
do { \
	static SearchSpaceComponent searchSpace[] = { __VA_ARGS__ , {0, NULL} }; \
	static long pos = 0; \
	DList *dl = state[dest]; \
	adjustWeighting(searchSpace, (spcSize)); \
 \
	if (!dl) { \
		pos = 0; \
		dl = searchSpace[0].data; \
	} else { \
		lighter(searchSpace[pos]); \
	}\
 \
	do { \
		heavier(searchSpace[pos]); \
		lookupNode(&dl, &matches[(dest)]); \
		if (boolFlag) { \
			break ; \
		} \
		lighter(searchSpace[pos]); \
	} while ( (dl = searchSpace[++pos].data) ); \
	state[dest] = dl; \
} while (0)

#define makeExtendOutTrav(fromTrav, eDest, nDest, predCode) \
do { \
	Element *src=matches[fromTrav]; \
	DList *dl = (state[eDest]) \
					? state[eDest] \
					: outListFor(src);    \
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
	DList *dl = (state[eDest]) ? state[eDest] : inListFor(src);    \
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

#define makeBidiEdgeTrav(n1Trav, edgeTrav, n2Trav) \
do { \
	edgeBetween(&matches[edgeTrav], matches[n1Trav], matches[n2Trav]); \
	if (!boolFlag) { \
		boolFlag = 1; \
		edgeBetween(&matches[edgeTrav], matches[n2Trav], matches[n1Trav]); \
	} \
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


#ifndef NDEBUG
void oilrReport() {
	long i;
	DList *index;
	// OILR index population report
	debug("OILR index stats:\n");
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		debug("\t[%03ld]: %ld", i, listLength(index) );
		if ( (i+1) % 4  == 0 )
			debug("\n");
	}
	debug("\n");
}
#else
#define oilrReport()
#endif

#define getId(ne) (((Element *) (ne)) - g.pool)
void dumpGraph(FILE *file) {
	long i;
	DList *index, *out;
	Element *n, *src, *tgt;
	Element *e;
	char *rootStatus, *boundStatus;
#ifndef NDEBUG
	long nodeCount = 0, nodeIndexCount = 0;
	long edgeCount = 0, edgeIndexCount = 0;
#endif
	fprintf(file, "[\n");
	// Dump nodes
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		debugCode( nodeIndexCount += listLength(index) );
		while ( (index = nextElem(index)) ) {
			debugCode( nodeCount++ );
			assert(unbound(elementOfListItem(index)));
			n = elementOfListItem(index);
			rootStatus = isRoot(n) ? " (R)" : "";
			boundStatus = bound(elementOfListItem(index)) ? " +" : "";
			fprintf(file, "\t( n%ld%s%s, empty %s)\n", getId(n), boundStatus, rootStatus, colourNames[colour(n)] );
		}
	}
	debug("%ld %ld\n", nodeIndexCount, nodeCount);
	assert(nodeIndexCount == nodeCount);
	assert(nodeCount == g.nodeCount);

	fprintf(file, "|\n");
	// Dump edges and loops
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		while ( (index = nextElem(index)) ) {
			n = elementOfListItem(index);
			out = outListFor(n);
			debugCode( edgeIndexCount += listLength(out) );
			while ( (out = nextElem(out)) ) {
				debugCode( edgeCount++ );
				assert(unbound(elementOfListItem(out)));
				e = elementOfListItem(out);
				src = source(e);
				tgt = target(e);
				boundStatus = bound(elementOfListItem(out)) ? " +" : "";
				fprintf(file, "\t( e%ld%s, n%ld, n%ld, empty)\n", getId(e), boundStatus, getId(src), getId(tgt) );
			}
			out = loopListFor(n);
			debugCode( edgeIndexCount += listLength(out) );
			while ( (out = nextElem(out)) ) {
				debugCode( edgeCount++ );
				assert(unbound(elementOfListItem(out)));
				e = elementOfListItem(out);
				src = source(e);
				fprintf(file, "\t( e%ld, n%ld, n%ld, empty)\n", getId(e), getId(src), getId(src) );
			}
		}
	}
	fprintf(file, "]\n");
	debug("%ld %ld\n\n", edgeIndexCount, edgeCount);
	assert(edgeIndexCount == edgeCount);
	oilrReport();
}

/////////////////////////////////////////////////////////
// main

int main(int argc, char **argv) {
	long i;
#ifdef OILR_COMPACT_LISTS
	int failure = posix_memalign((void*)(&g.pool), OILR_ELEM_ALIGN, sizeof(Element) * DEFAULT_POOL_SIZE);
	assert(sizeof(Element) == OILR_ELEM_ALIGN);
	if (failure) {
#else
	g.pool = malloc(sizeof(Element) * DEFAULT_POOL_SIZE);
	if (!g.pool) {
#endif
		printf("Couldn't couldn't allocate %ld MiB\n", DEFAULT_POOL_SIZE*sizeof(Element)/(1024*1024));
		exit(1);
	}
	fprintf(stderr, "Allocated %ld MiB\n", DEFAULT_POOL_SIZE*sizeof(Element)/(1024*1024));
	g.poolSize = DEFAULT_POOL_SIZE;
	g.nodeCount = 0;
	g.edgeCount = 0;
	g.freeId = 1;  // pool[0] is not used so zero can function as a NULL value
#ifdef OILR_EXECUTION_TRACE
	oilrTraceFile = stderr;
#endif
	// printf("sizeof(Element) = %ld\n", sizeof(Element));
	// printf("elem[0]=%p elem[1]=%p", &g.pool[0], &g.pool[1]);
	// exit(0);
	assert(DEFAULT_POOL_SIZE * sizeof(Element) > 0);
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
#elif ! defined(OILR_EXECUTION_TRACE)
	fprintf(stderr, "Program completed in %ld bind operations.\n", bindCount);
#endif
	//assert(bindCount == unbindCount);
	dumpGraph(stdout);
	free(g.pool);
	return 0;
}


#define ALAP(rule, recursive, ...) do { \
	DList *state[] = { __VA_ARGS__ }; \
	oilrReport(); \
	(rule)((recursive), state); \
} while (boolFlag); \
boolFlag=1;

#define CALL(rule, ...) do { \
	DList *state[] = { __VA_ARGS__ }; \
	(rule)(0, state); \
	if (!boolFlag) DONE ; \
} while (0);

/////////////////////////////////////////////////////////
// generated code goes here....


