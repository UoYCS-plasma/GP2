#if !( defined(OILR_PARANOID_CHECKS) || defined(OILR_DEBUGGING) )
#define NDEBUG
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>

#define OILR_BIND_BITS 1
#define OILR_T_BITS 2
#define OILR_INDEX_BITS (OILR_B_BITS+OILR_C_BITS+OILR_O_BITS+OILR_I_BITS+OILR_L_BITS+OILR_R_BITS)

#define OILR_INDEX_SIZE (1<<(OILR_INDEX_BITS-1))
#define DEFAULT_POOL_SIZE (100000000)
#define DONE return
#define MIN_ALLOC_INCREMENT (1024*1024*4)  // 4 meg

#define OILR_ELEM_ALIGN 128
#define OILR_ELEM_MASK  (~(OILR_ELEM_ALIGN-1))

#ifndef MAX_RECURSE
#	define MAX_RECURSE 128
#endif


void OILR_Main();
long bindCount   = 0;
long unbindCount = 0;
void *currentBrk;
long lastAlloc = 0;
long recursionDepth = 0;
void (*self)();

char *colourNames[]   = { "", " # red", " # blue", " # green", " # grey" };
char *edgeMarkNames[] = { "", " # dashed" };

/////////////////////////////////////////////////////////
// accessor macros


#define nextElem(dl) ((dl)->next)
#define prevElem(dl) ((dl)->prev)
#define headOfList(dl) ((dl)->head)
// TODO: introduce a union to prevent this casting evil
#define elementOfListItem(dl) ((Element *)((long)(dl)&(OILR_ELEM_MASK)))

#define listLength(dl) ((dl)->count)
#define incListLength(dl) ((dl)->count++)
#define decListLength(dl) ((dl)->count--)


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
#define indexIdFor(addr) ((addr)-g.idx)

#define getElementById(id) &(g.pool[(id)])
#define elementId(el) ((el)-g.pool)


/////////////////////////////////////////////////////////
// graph structure

typedef struct DList {
	union {
		long count;
		struct DList *head;
	};
	struct DList *next;
	struct DList *prev;
	// Elements MUST be aligned to OILR_ELEM_ALIGN, which must in turn
	// be a power of 2. That way we can simply mask-off a DList address
	// to get to the containing Element.
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

#define OIL_MASK (ODEG_MASK | IDEG_MASK | LOOP_MASK)

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
#define elType(el)  (((el)->flags)&TYPE_MASK)
#define isNode(el) (elType(el) == NODE_TYPE)
#define isEdge(el) (elType(el) == EDGE_TYPE)
#define isFree(el) (elType(el) == FREE_TYPE)

#define SIG_MASK (mask(OILR_INDEX_BITS, 0))
#define signature(n) (flags(n) & SIG_MASK)

#define FREE_TYPE (0)
#define NODE_TYPE (1 << TYPE_OFFS)
#define EDGE_TYPE (2 << TYPE_OFFS)


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
		// WARNING: Element must be _exactly_
		// OILR_ELEM_ALIGN bytes in size!
		char pad[OILR_ELEM_ALIGN-2*sizeof(int)];
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

#ifdef NDEBUG
#define debug(...)
#define debugCode(...)
#define oilrStatus(...)
#else
#define debug(...) do { fprintf(stderr, __VA_ARGS__); } while (0)
#define debugCode(c) do { c ; } while (0)
#define oilrStatus(node) do { Element *mN = (node); debug("\tNode %ld has OILR %ld: (%ld, %ld, %ld, %d)\n", elementId(mN), signature(mN), outdeg(mN), indeg(mN), loopdeg(mN), isRoot(mN)); } while (0)
#endif

#define failwith(...)  do { fprintf(stderr, __VA_ARGS__); exit(1); } while (0)


long max(long a, long b) {
	return (a>b) ? a : b;
}
long min(long a, long b) {
	return (a>b) ? b : a;
}
void setElType(Element *el, long type) {
	long flags = flags(el);
	assert(flags == NODE_TYPE || flags == EDGE_TYPE || flags == FREE_TYPE);
	flags &= (~TYPE_MASK);
	flags |= type;
	setFlags(el, flags);
}



/////////////////////////////////////////////////////////
// stack-machine (removed)

long boolFlag = 1;

/////////////////////////////////////////////////////////
// doubly-linked list support

#define OILR_ELEM_MASK (~(OILR_ELEM_ALIGN-1))

#ifndef NDEBUG
long identList(DList *dl, char **str) {
	// dl must point to a list-head!
	Element *e = elementOfListItem(dl);
	long elemId  = elementId( e );
	long indexId = indexIdFor(dl);
	if (elemId > 0 && elemId < g.freeId) {
		switch ( elType(getElementById(elemId)) ) {
			case (NODE_TYPE):
				if ( dl == outListFor(e) )
					*str = "outEdges for node";
				else if ( dl == inListFor(e) )
					*str = "inEdges for node";
				else if ( dl == loopListFor(e) )
					*str = "loops for node";
				else
					failwith("Chain for element %ld is not in, out or loops!", elemId);
				break;
			default:
				failwith("dl is not part of a node!");
		}
		return elemId;
	} else if (indexId >= 0 && indexId < OILR_INDEX_SIZE) {
		*str = "index";
		return indexId;
	}
	failwith("This should never happen.");
	return -1;  // suppress compiler warning. Unreachable
}
#else
#define identList(dl, str)
#endif

#if defined(OILR_PARANOID_CHECKS) && !defined(NDEBUG)
long walkChain(DList *dl) {
	DList *p, *l = dl;
	long len=0, blen=0;
	while ( (l = nextElem(l)) ) {
		len++;
		if (p==l)
			failwith("Cycle detected in chain at %p!\n", dl);
		p=l;
	}
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
	char *type;
	long id = identList(dl, &type);
#endif
	DList *nx = nextElem(dl);
	assert(headOfList(elem) == NULL);
	elem->head = dl;
	elem->prev = NULL;
	elem->next = nx;
	if (nx)
		nx->prev = elem;
	if (listLength(dl) == 0)
		dl->prev = elem;
	dl->next = elem;
	incListLength(dl);
	walkChain(dl);
	assert(listLength(dl) == len+1);
	assert(listLength(dl) >= 0 && listLength(dl) < g.poolSize);
	debug("dl %s[%ld] has length %ld after insert\n", type, id, dl->count);
}
void removeElem(DList *elem) {
	DList *dl = headOfList(elem);
	DList *nx = nextElem(elem);
	DList *pv = prevElem(elem);
	if (dl == NULL) // Not in a list!
		return;
#ifndef NDEBUG
	long len = listLength(dl);
	char *type;
	long id = identList(dl, &type);
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
	elem->head = NULL;
	decListLength(dl);
	assert(listLength(dl) == len-1);
	assert(dl->count >= 0 && dl->count < g.poolSize);
	debug("dl %s[%ld] has length %ld after remove\n", type, id, dl->count);
}

/////////////////////////////////////////////////////////
// graph traversal

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

void sign(Element *n) {
	int f = flags(n) & ~OIL_MASK;  // clear current values
	int o=min(outdeg(n), (1<<OILR_O_BITS)-1),
		i=min(indeg(n) , (1<<OILR_I_BITS)-1),
		l=min(loopdeg(n),(1<<OILR_L_BITS)-1);
	f = f | (o<<ODEG_OFFS) | (i<<IDEG_OFFS) | (l<<LOOP_OFFS);
	assert( (f & SIG_MASK) < OILR_INDEX_SIZE );
	setFlags(n, f);
}
void indexNode(Element *n) {
	sign(n);
	prependElem(index(signature(n)), chainFor(n));
}
void unindexNode(Element *n) {
	removeElem(chainFor(n));
}
void reindexNode(Element *n) {
	unindexNode(n);
	indexNode(n);
}

void setRoot(Element *n) {
	setFlags(n, flags(n) | ROOT_MASK);
	reindexNode(n);
	debug("(R) Set root on node %ld\n", elementId(n));
	oilrStatus(n);
}
void unsetRoot(Element *n) {
	setFlags(n, flags(n) & ~ROOT_MASK);	
	reindexNode(n);
	debug("(-) Unset root on node %ld\n", elementId(n));
	oilrStatus(n);
}
void setColour(Element *n, long c) {
	long flags = (flags(n) & ~COLR_MASK) | (c<<COLR_OFFS);
	setFlags(n, flags);
	reindexNode(n);
	debug("(#) Set colour on element %ld to %ld\n", elementId(n), c);
	oilrStatus(n);
}
#define setRootById(n) setRoot( getElementById(n) )
#define setColourById(n, c) setColour( getElementById(n), (c) )

void freeElement(Element *ne) {
	ne->free = g.freeList;
	setFlags(ne, FREE_TYPE);  // clobbering flags is fine here.
	g.freeList = ne;
}


void checkSpace(long n) {
	// Ensure there is space to allocate n elements
	if (g.freeId + n >= g.poolSize) {
		assert( sbrk(0) == currentBrk );  // check malloc hasn't moved the brk!
		if (lastAlloc)
			lastAlloc += lastAlloc;
		else  // our first allocation.
			lastAlloc = max(MIN_ALLOC_INCREMENT, sizeof(Element) * n * 3);
		currentBrk += lastAlloc;
		if ( brk( currentBrk ) < 0 )
			failwith("Couldn't move brk address to 0x%x\n", currentBrk);
		debug("Allocated space for %d Elements\n", lastAlloc/sizeof(Element));
		g.poolSize = lastAlloc/sizeof(Element);
	}
}

Element *allocElement() {
	Element *ne = g.freeList;
	if (ne == NULL) {
		assert(g.freeId < g.poolSize);
		ne = &(g.pool[g.freeId++]);
	} else {
		g.freeList = ne->free;
	}
	memset(ne, '\0', sizeof(Element));
	// setElType(type);
	return ne;
}

Element *unsafeAddNode() {
	Element *el = allocElement();
	Element *n = el;
	setElType(n, NODE_TYPE);
	indexNode(n);
	g.nodeCount++;
	assert(indeg(n) == 0 && outdeg(n) == 0 && loopdeg(n) == 0);
	debug("( ) Created node %ld\n", elementId(n));
	oilrStatus(n);
	return n;
}
Element *addNode() {
	checkSpace(1);
	return unsafeAddNode();
}
Element *addLoop(Element *node) {
	Element *e = allocElement();
	Element *n     = node;
	setElType(e, EDGE_TYPE);
#ifndef NDEBUG
	long lcLen=loopdeg(n);
#endif
	prependElem(loopListFor(n), outChain(e) );
	e->src = node;
	e->tgt = node;
	reindexNode(n);
	g.edgeCount++;
	assert( lcLen+1 == listLength(loopListFor(n)) );
	assert( listLength(loopListFor(n)) >= 0 );
	debug("C₇O Created loop %ld on node %ld\n", elementId(e), elementId(node) );
	oilrStatus(n);
	return e;
}
Element *addEdge(Element *s, Element *t) {
	Element *e = allocElement();
	setElType(e, EDGE_TYPE);
	assert(s != t);
#ifndef NDEBUG
	long icLen=listLength(inListFor(t)), ocLen=listLength(outListFor(s));
#endif
	prependElem( outListFor(s), outChain(e) );
	prependElem( inListFor(t), inChain(e) );
	e->src = s;
	e->tgt = t;
	reindexNode(s);
	reindexNode(t);
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
void deleteLoop(Element *e) {
	Element *n = source(e);
	debug("CxO deleted loop %ld\n", elementId(e));
	removeElem(outChain(e));
	freeElement(e);
	reindexNode(n);
	oilrStatus(n);
	g.edgeCount--;
}
void deleteEdge(Element *e) {
	Element *src = source(e), *tgt = target(e);
	debug("-X> deleted edge %ld\n", elementId(e));
	removeElem(outChain(e));
	removeElem(inChain(e));
	freeElement(e);
	reindexNode(src);
	reindexNode(tgt);
	oilrStatus(src); oilrStatus(tgt);
	g.edgeCount--;
}

/////////////////////////////////////////////////////////
// Host-graph DSL support

// host  => <nodes> <spc> <labl>* <spc> <path>*
// nodes => <int> <spc> ( "nodes" | "node" )
// labl  => <nod> <spc> "labelled" <spc> <int>
// path  => <nod> <spc> <edge>+ <spc> "path"
// edge  => <ed> | <led>
// ed    => "-->" <spc> <nod>
// led   => "--(" <spc> <int> <spc> ")-->" <spc> <nod>
// nod   => <int>

#define DS_SIZE 16
long ds[DS_SIZE];
long *dsp=(ds-1);
#define push(n) do { *(++dsp) = n; } while (0)
#define peek()  (*dsp)
#define pop()   (*(dsp--))

#define MAX_TOK       16
#define TOK_MATCH   "%15s"

#define KW_NODES     537
#define KW_NODE      422
#define KW_EDGE      152
#define KW_LABELLED  821
#define KW_PATH      429
#define KW_LEDGE_A   130
#define KW_LEDGE_B   193

long hash(char *str) {
	char c;
	long h=0;
	while ( (c=*str) )
		h += c;
	return h;
}

int nextTok(FILE *f, char *tok) {
	return fscanf(f, TOK_MATCH, tok);
}
void parseHost(FILE *host) {
	char tok[MAX_TOK];
	char *rem;
	long n, h;
	
	while ( nextTok(host, tok) > 0 ) {
		n = strtol(tok, &rem, 10);
		h = hash(tok);
		if (*rem == '\0') {
			// numeric value
			push(n);
		} else {
			switch (h) {
				case KW_NODES:
				case KW_NODE:
					checkSpace(peek());

					break;
				case KW_EDGE:  // n -- n'

					break;
				case KW_LEDGE_A:
					break;
				case KW_LEDGE_B:  // n l -- n'
					break;
				default:
					failwith("Couldn't parse host graph. Problem token is '%s'\n", tok);
			}
		}
	}
	
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
	long fl;
	if (el && bound(el)) {
		fl = flags(el);
		setFlags( el, fl & ~BIND_MASK );
		unbindCount++;
		debug("\tUnbound %ld\n", elementId(el));
	}
	//debug("\tUnbound %ld\n", ((el)==NULL ? 0 : elementId(el)));
}
void unbindAll(Element **regs, long n) {
	long i;
	for (i=0; i<n; i++) {
		unbind(regs[i]);
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
	DList *dl = (*edge==NULL) ? outListFor(src) : outChain(*edge);
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
void loopOnNode(Element **edge, Element *node) {
	assert(node != NULL);
	unbind(*edge);
	DList *dl = (*edge == NULL) ? loopListFor(node) : outChain(*edge);
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


#define reg(r) (regs[(r)])

// The local jump-stack code uses 
#define setReg(r, val) do { regs[(r)] = (val); } while (0)
#define setFailTo(id)  do { failStack[++fsi] = (id) ; debug("fsi: %d\n", fsi); } while (0)
#define fail()         goto *failStack[fsi--];

// OILR instructions

#define REGS(n) \
	int i; \
	static void *failStack[(n)]; \
	long fsi = 0; \
	Element *regs[n]; \
	failStack[0] = &&l_exit; \
	for (i=0; i<(n); i++) \
		regs[i]=NULL

#define ABN(dst)            do { reg(dst) = addNode(); } while (0)
#define ABE(dst, src, tgt)  do { reg(dst) = addEdge(reg(src), reg(tgt)); } while (0)
#define ABL(dst, src)       do { reg(dst) = addLoop(reg(src)); } while (0)
#define DBE(r) deleteEdge(reg(r))
#define DBL(r) deleteLoop(reg(r))

void bnd(Element **dst, DList **spc, DList **dl, long *pos) {
	*pos = *dl ? *pos : 0; 
	*dl  = *dl ? *dl  : spc[0];
	do {
		while ( listLength(spc[*pos]) == 0 ) {
			// skip empty indices
			if (spc[++(*pos)]==NULL) {
				boolFlag=0;
				*dl = NULL;
				return;
			}
			*dl = spc[*pos];
		}
		lookupNode(dl, dst);
	} while ( !boolFlag && (*dl=spc[++(*pos)]) );
	assert(boolFlag && *dst != NULL);
}

#define BND(dstR, spc) \
	l_ ## dstR : \
	do { \
		bnd( &reg(dstR), (spc), &spc ## _dl, &spc ## _pos ); \
		if (boolFlag) \
			setFailTo(&&l_ ## dstR); \
		else \
			fail(); \
	} while (0)

#define BOE(dst, src, tgt) \
	l_ ## dst : \
	do { \
		debug("--> %p   ", reg(src)); \
		edgeBetween(&reg(dst), reg(src), reg(tgt)); \
		if (boolFlag) \
			setFailTo(&&l_ ## dst); \
		else \
			fail(); \
	} while (0)

#define BED(dst, r1, r2) \
	l_ ## dst : \
	do { \
		edgeBetween(&reg(dst), reg(r1), reg(r2)); \
		if (!boolFlag) { \
			boolFlag=1; \
			edgeBetween(&reg(dst), reg(r2), reg(r1)); \
		} \
		if (boolFlag) \
			setFailTo(&&l_ ## dst); \
		else \
			fail(); \
	} while (0)

#define BLO(dst, r) \
	l_ ## dst : \
	do { \
		debug("--> %p   ", reg(dst)); \
		loopOnNode(&reg(dst), reg(r)); \
		if (boolFlag) \
			setFailTo(&&l_ ## dst); \
		else \
			fail(); \
	} while (0)
	

#define NEC(src, tgt) \
	do { \
		Element *antiEdge = NULL; \
		edgeBetween(&antiEdge, reg(src), reg(tgt)); \
		unbind(antiEdge); \
		boolFlag = !boolFlag; \
		if (!boolFlag) fail(); \
	} while (0)

#define SUC() if (recursionDepth>0) do { recursionDepth--; (*self)(); boolFlag=1; } while (0)

#define BNZ(tgt) if (boolFlag) goto tgt
#define BRZ(tgt) if (!boolFlag) goto tgt

#define TRU() do { boolFlag = 1; } while (0)
#define FLS() do { boolFlag = 0; } while (0)

#define UBN(n)  unbindAll(regs, (n))
#define RST(spc) do { spc ## _dl = (spc)[0] ; spc ## _pos = 0; } while (0)



/////////////////////////////////////////////////////////
// utilities


#ifndef NDEBUG
void oilrReport() {
	long i;
	DList *index;
	// OILR index population report
	debug("OILR index stats (%d indices):\n", OILR_INDEX_SIZE);
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

void dumpNode(FILE *file, Element *n) {
	long id = elementId(n);
	char label[16]; // label is only 32 bit = max 12 digits (incl minus & \0)
	char *rt = isRoot(n)   ? " (R)"      : "";
	char *lb = isLabelled(n) ? label : "empty";
	char *cl = colourNames[colour(n)];
	sprintf(label, "%d", getLabel(n));
	assert(unbound(n));
	fprintf(file, "\t( n%ld%s, %s%s )\n", id, rt, lb, cl);
}
void dumpEdge(FILE *file, Element *e) {
	long id = elementId(e);
	char label[16];
	long sr=elementId(source(e)), tg=elementId(target(e));
	char *lb = isLabelled(e) ? label : "empty";
	char *cl = edgeMarkNames[colour(e)];
	sprintf(label, "%d", getLabel(e));
	assert(unbound(e));
	fprintf(file, "\t( e%ld, n%ld, n%ld, %s%s )\n", id, sr, tg, lb, cl);
}
void dumpGraph(FILE *file) {
	long i;
	Element *el;
#ifndef NDEBUG
	DList *index;
	long nodeCount = 0, nodeIndexCount = 0;
	long edgeCount = 0, edgeIndexCount = 0;
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		debugCode( nodeIndexCount += listLength(index) );
	}
#endif
	fprintf(file, "[\n");
	// Dump nodes
	for (i=1; i<g.freeId; i++) {  // element[0] is a dummy element!
		el = getElementById(i);
		if (elType(el) == NODE_TYPE) {
			debugCode( edgeIndexCount += listLength(outListFor(el) ) );
			debugCode( edgeIndexCount += listLength(loopListFor(el)) );
			debugCode( nodeCount++ );
			dumpNode(file, el);
		}
	}
	fprintf(file, "|\n");
	// Dump edges and loops
	for (i=1; i<g.freeId; i++) {  // element[0] is a dummy element!
		el = getElementById(i);
		if (elType(el) == EDGE_TYPE) {
			debugCode( edgeCount++ );
			dumpEdge(file, el);
		}
	}
	fprintf(file, "]\n");
	assert(nodeCount == g.nodeCount);
	assert(nodeIndexCount == nodeCount);
	assert(edgeIndexCount == edgeCount);
	oilrReport();
}

/////////////////////////////////////////////////////////
// main

int main(int argc, char **argv) {
	// C doesn't provide a way to get an extensible memory area
	// that is guaranteed not to move, so we have to do it ourselves with brk().
	// Screw C. Added bonus: brk always starts out aligned to a memory page, so we
	// get aligned memory for free.
	currentBrk = sbrk(0);  // get current brk address -- C doesn't give us a way to 
	debug("Brk: 0x%x, sizeof(Element): %d, num inds: %d\n", currentBrk, sizeof(Element), OILR_INDEX_SIZE);
	g.pool = currentBrk;
	g.poolSize  = 0;
	g.nodeCount = 0;
	g.edgeCount = 0;
	g.freeId    = 1;  // we don't use g.pool->[0]

	(void)argc; (void)argv;  // silence unused param warning for this function only
#ifdef OILR_EXECUTION_TRACE
	oilrTraceFile = stderr;
#endif

/*	for (i=0; i<OILR_INDEX_SIZE; i++) {
		DList *ind = index(i);
		ind->count = 0;
		ind->head  = NULL;
		ind->next  = NULL;
		ind->prev  = NULL;
	} */

	// checkGraph();
	// _HOST();

	addNode();
	addEdgeById(1, 1);
	addEdgeById(1, 1);
	addEdgeById(1, 1);
	addEdgeById(1, 1);
	addEdgeById(1, 1);

	addEdgeById(1, 1);
	addEdgeById(1, 1);
	addEdgeById(1, 1);
	addEdgeById(1, 1);
	addEdgeById(1, 1);

	setRootById(1);
	checkGraph();
//	dumpGraph(stdout);

	OILR_Main();
#ifndef NDEBUG
	debug("Program completed in %ld bind and %ld unbind operations.\n", bindCount, unbindCount);
#elif ! defined(OILR_EXECUTION_TRACE)
	fprintf(stderr, "Program completed in %ld bind operations.\n", bindCount);
#endif
	//assert(bindCount == unbindCount);

	if (!boolFlag) {
		debug("* GP2 program failed.\n");
		return 1;
	}
	dumpGraph(stdout);
	return 0;
}

#define CAL(fun) do { self=(fun); (fun)(); } while (0)
#define ALAP() do { recursionDepth=MAX_RECURSE; } while (0)
#define ONCE() do { recursionDepth=0; } while (0)
	
/* #define ALAP(rule, recursive, ...) do { \
	DList *state[] = { __VA_ARGS__ }; \
	oilrReport(); \
	(rule)((recursive), state); \
} while (boolFlag); \
boolFlag=1 */

/* #define CALL(rule, ...) do { \
	self = (rule); \
	DList *state[] = { __VA_ARGS__ }; \
	(rule)(0, state); \
	if (!boolFlag) DONE ; \
} while (0) */

/////////////////////////////////////////////////////////
// generated code goes here....


