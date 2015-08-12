#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define OILR_INDEX_SIZE (1<<(OILR_O_BITS+OILR_I_BITS+OILR_L_BITS+OILR_R_BITS))
#define DEFAULT_POOL_SIZE (1024)

void _HOST();
void _GPMAIN();


/////////////////////////////////////////////////////////
// graph structure

struct Node;
struct Edge;
struct Element;

typedef union ListPayload {
		struct Element *e;
		long count;
} ListPayload ;

typedef struct DList {
	union ListPayload data;
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
	Element *pool;
	Element *freeList;
	DList idx[OILR_INDEX_SIZE];
} Graph;

Graph g;

#define asNode(el) (&(el)->n)
#define asEdge(el) (&(el)->e)

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
#define elementOfListItem(dl) ((dl)->data.e)

void prependElem(DList *dl, DList *elem) {
	DList *nx = dl->next;
	elem->head = dl;
	elem->prev = NULL ;
	elem->next = nx;
	if (nx)
		dl->next->prev = elem;
	dl->next = elem;
	dl->data.count++;
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
	dl->data.count--;
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

#define indeg(n)    (inListFor(n)->data.count)
#define outdeg(n)   (outListFor(n)->data.count)
#define loopdeg(n)  ((n)->loops)
#define isroot(n)   ((n)->root)

#define index(sig) &(g.idx[sig])

#define getElementById(id) &(g.pool[(id)])

#define space(id) (index( searchSpaces[*(id)] ))

/////////////////////////////////////////////////////////
// graph manipulation

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

Element *allocElement() {
	Element *ne = g.freeList;
	if (ne == NULL) {
		ne = &(g.pool[g.freeId++]);
	} else {
		g.freeList = ne->free;
	}
	return ne;
}

Element *addNode() {
	Element *el = allocElement();
	Node *n = asNode(el);
	chainFor(n)->data.e = el;
	indexNode(n);
	return el;
}
void addLoop(Node *n) {
	n->loops++;
}
Element *addEdge(Element *src, Element *tgt) {
	Element *el = allocElement();
	Edge *e = asEdge(el);
	Node *s = asNode(src), *t = asNode(tgt);
	unindexNode(s);
	unindexNode(t);
	prependElem( outListFor(s), outChain(e) );
	prependElem( inListFor(t), inChain(e) );
	e->src = src;
	e->tgt = tgt;
	outChain(e)->data.e = el;
	inChain(e)->data.e = el;
	indexNode(s);
	indexNode(t);
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

#define bind(el)   do { (el)->bound = 1; } while(0)
#define unbind(el) do { (el)->bound = 0; } while(0)
#define unbound(el) ( !(el)->bound )

#define makeSimpleTrav(travName, destination, list)  \
void travName(Element **travs) { \
	const long dest = (destination); \
	static DList *dl = (list); \
	Element *e; \
 \
	if ( (e = travs[dest]) ) \
		unbind(e); \
 \
	while (dl) { \
		dl = nextElem(dl); \
		if (dl) { \
			e = elementOfListItem(dl); \
			bind(e); \
			travs[dest] = e; \
			boolFlag = 1; \
			return ; \
		} \
	} \
	boolFlag = 0; \
}

#define makeTrav(travName, destination, ...) \
void travName(Element **travs) { \
	const long dest = (destination); \
	static DList *searchSpace[] = { __VA_ARGS__ , NULL}; \
	static long pos = 0; \
	static DList *dl = NULL; \
	Element *e = NULL; \
 \
	if (!dl) \
		dl = searchSpace[0]; \
 \
	while (dl) { \
		if ((e = travs[dest])) \
			unbind(e); \
		while (dl) { \
			dl = nextElem(dl); \
			if ( dl && unbound(dl) ) { \
				e = elementOfListItem(dl); \
				bind(e); \
				travs[dest] = e; \
				boolFlag = 1; \
				return ; \
			} \
		} \
		dl = searchSpace[++pos]; \
	} \
	boolFlag = 0; \
}

#define makeExtendOutTrav(travName, fromTrav, edgeDestination, nodeDestination, predCode) \
void travName(Element **travs) { \
	const long eDest = (edgeDestination), nDest = (nodeDestination); \
	Element *src = travs[fromTrav]; \
	Element *edge = travs[edgeDestination], *tgt = travs[nodeDestination]; \
	static DList *dl; \
 	assert(edgeDestination != nodeDestination \
			&& fromTrav != edgeDestination    \
			&& fromTrav != nodeDestination);  \
	assert(src); \
	if (!dl) \
		dl = outListFor(asNode(src)); \
	 \
	if (edge) \
		unbind(edge); \
	if (tgt) \
		unbind(tgt); \
 \
	while (dl) { \
		dl = nextElem(dl); \
		if (dl) { \
			edge = elementOfListItem(dl); \
			tgt = target(asEdge(edge)); \
			assert(edge && tgt) ; \
			assert(unbound(tgt)); \
			if ( unbound(edge) && unbound(tgt) && (predCode) ) { \
				bind(edge); \
				bind(tgt); \
				travs[eDest] = edge; \
				travs[nDest] = tgt; \
				boolFlag = 1; \
				assert(!unbound(edge) && !unbound(tgt)); \
				return; \
			} \
		} \
	} \
	boolFlag = 0; \
}

// no edge predicates can simply use this and then negate boolFlag
#define makeEdgeTrav(travName, destTrav, srcTrav, tgtTrav) \
void travName(Element **travs) { \
	const long dest = (destTrav); \
	Element *src = travs[srcTrav]; \
	Element *tgt = travs[tgtTrav]; \
	DList *dl = outListFor(asNode(src)); \
 \
	while (dl) { \
		dl = nextElem(dl); \
		if (dl) { \
			Element *edge = elementOfListItem(dl); \
			if ( unbound(edge) && target(asEdge(edge)) == tgt ) { \
				bind(edge); \
				travs[dest] = edge; \
				boolFlag = 1; \
				return; \
			} \
		} \
	} \
	boolFlag = 0; \
}



/////////////////////////////////////////////////////////
// utilities

#define getId(ne) (((Element *) (ne)) - g.pool)
void dumpGraph() {
	long i;
	DList *index, *out;
	Node *n, *src, *tgt;
	Edge *e;
	printf("[\n");
	// Dump nodes
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		while ( (index = nextElem(index)) ) {
			n = asNode(index->data.e);
			//printf("%lx %lx\n", (long) n, (long) g.pool);
			printf("\t( n%ld, empty)\n", getId(n) );
		}
	}
	printf("|\n");
	// Dump edges
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		while ( (index = nextElem(index)) ) {
			n = asNode(index->data.e);
			out = outListFor(n);
			while ( (out = nextElem(out)) ) {
				e = asEdge(out->data.e);
				src = asNode(source(e));
				tgt = asNode(target(e));
				printf("\t( e%ld, n%ld, n%ld, empty)\n", getId(e), getId(src), getId(tgt) );
			}
		}
	}
	printf("]\n");
}



/////////////////////////////////////////////////////////
// main

int main(int argc, char **argv) {
	g.pool = malloc(sizeof(Element) * DEFAULT_POOL_SIZE);
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


