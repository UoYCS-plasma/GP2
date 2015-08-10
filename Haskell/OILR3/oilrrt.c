#include <stdio.h>
#include <stdlib.h>

//#define OILR_INDEX_SIZE (1<<3)
#define DEFAULT_POOL_SIZE (1024)

void _HOST();
void _GPMAIN();

/////////////////////////////////////////////////////////
// graph structure

#define signature(n) 0

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

typedef struct Trav {
	Element *match;
	long *curSpc;
	long *spc;
} Trav;

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

#define source(e)   (asNode((e)->src))
#define target(e)   (asNode((e)->tgt))
#define outChain(e) (&(e)->outList)
#define inChain(e)  (&(e)->inList)

#define chainFor(n)        (&(n)->index)
#define outListFor(n)      (&(n)->outEdges)
#define inListFor(n)       (&(n)->inEdges)

#define indeg(n)    (inListFor(n)->data.count)
#define outdeg(n)   (outListFor(n)->data.count)
#define loopdeg(n)  ((n)->loops)

#define index(sig) &(g.idx[sig])

#define getElementById(id) &(g.pool[(id)])

/////////////////////////////////////////////////////////
// graph manipulation

void freeElement(Element *ne) {
	ne->free = g.freeList;
	g.freeList = ne;
}


void indexNode(Node *n) {
	long sig = signature(n);
	prependElem(index(sig), chainFor(n));
}
void unindexNode(Node *n) {
	removeElem(chainFor(n));
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
	Node *src = source(e);
	Node *tgt = target(e);
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

Trav travs[TRAV_COUNT];

#define getTrav(id) (&travs[(id)])
#define boundElement(tid) ((getTrav(tid))->match)

void bindElement(long tid, Element *el) {
	boundElement(tid) = el;
	el->bound = 1;
}
void unbindElement(long tid) {
	Element *el = boundElement(tid);
	if (el)
		el->bound = 0;
	getTrav(tid)->match = NULL;
}


void resetTrav(long tid) {
	Trav *t = getTrav(tid);
	t->curSpc = t->spc;
	unbindElement(tid);
}
void nextSpace(long tid) {
	Trav *t = getTrav(tid);
	++t->spc;
}

// TODO: Move spc and sz initialisation to compile time!
void findNode(long tid, long *spc, long sz) {
	Trav *t = getTrav(tid);
	if (!t->spc)
		t->spc = spc;
	unbindElement(tid);
	while (! t->match)  {
		if (t->spc - spc > sz) {
			boolFlag = 0;
		} 
	}

	if (! t->match) {
		
	} 
}
void findEdge(long a, long b, long c) {

}

void followOutEdge(long tid, long src) {
	Node *s = asNode( boundElement(src) );
	DList *outEdges = outListFor(s);
	bindElement(tid, elementOfListItem( nextElem(outEdges) ) );
	boolFlag = boundElement(tid) == NULL ? 0 : 1;
}
void followInEdge(long tid, long tgt) {
	Node *t = asNode( boundElement(tgt) );
	DList *inEdges = outListFor(t);
	bindElement(tid, elementOfListItem( nextElem(inEdges) ) );
	boolFlag = boundElement(tid) == NULL ? 0 : 1;
}

/////////////////////////////////////////////////////////
// Graph manipulation via travs

void deleteEdgeByTrav(long tid) {
	Element *el = boundElement(tid);
	unbindElement(tid);
	deleteEdge(el);
}
void deleteNodeByTrav(long tid) {
	Element *el = boundElement(tid);
	unbindElement(tid);
	deleteNode(el);
}

void addNodeByTrav(long tid) {
	Element *el = addNode();
	bindElement(tid, el);
}
void addEdgeByTrav(long tid, long src, long tgt) {
	Element *s = boundElement(src), *t = boundElement(tgt);
	Element *e = addEdge(s,t);
	bindElement(tid, e);
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
				src = source(e);
				tgt = target(e);
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


