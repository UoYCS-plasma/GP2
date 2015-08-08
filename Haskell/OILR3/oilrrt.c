#include <stdio.h>
#include <stdlib.h>

//#define OILR_INDEX_SIZE (1<<3)
#define DEFAULT_POOL_SIZE (1024)
#define TRAV_COUNT (100)

void _HOST();
void _GPMAIN();

/////////////////////////////////////////////////////////
// graph structure

#define signature(n) 0

/////////////////////////////////////////////////////////
// graph structure

struct Node;
struct Edge;

typedef union ListPayload {
		struct Node *node;
		struct Edge *edge;
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
		long bound:1;
		long root:1;
		long loops:15;
		long matchedLoops:15;
	};
} Node;

typedef struct Edge {
	long bound;
	Node *src;
	Node *tgt;
	DList outList;
	DList inList;
} Edge;

typedef union Nodge {
	Node n;
	Edge e;
	union Nodge *free;
} Nodge;

typedef struct Graph {
	long freeId;
	Nodge *pool;
	Nodge *freeList;
	DList idx[OILR_INDEX_SIZE];
} Graph;

typedef struct Trav {
	Node *nMatch;
	Edge *eMatch;
	long *spc;
} Trav;

Graph g;

/////////////////////////////////////////////////////////
// stack-machine

#define DS_SIZE 16
long ds[DS_SIZE];
long *dsp = ds;
long boolFlag = 0;

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

#define source(e)   ((e)->src)
#define target(e)   ((e)->tgt)
#define outChain(e) (&(e)->outList)
#define inChain(e)  (&(e)->inList)

#define chainFor(n)        (&(n)->index)
#define outListFor(n)      (&(n)->outEdges)
#define inListFor(n)       (&(n)->inEdges)

#define indeg(n)    (inListFor(n)->data.count)
#define outdeg(n)   (outListFor(n)->data.count)
#define loopdeg(n)  ((n)->loops)

#define index(sig) &(g.idx[sig])

#define getNodeById(id) &(g.pool[(id)].n)

/////////////////////////////////////////////////////////
// graph manipulation

void freeNodge(Nodge *ne) {
	ne->free = g.freeList;
	g.freeList = ne;
}
#define freeEdge(e) do { freeNodge( (Nodge *) (e) ); } while (0);
#define freeNode(n) do { freeNodge( (Nodge *) (n) ); } while (0);


void indexNode(Node *n) {
	long sig = signature(n);
	prependElem(index(sig), chainFor(n));
}
void unindexNode(Node *n) {
	removeElem(chainFor(n));
}

Nodge *allocNodge() {
	Nodge *ne = g.freeList;
	if (ne == NULL) {
		ne = &(g.pool[g.freeId++]);
	} else {
		g.freeList = ne->free;
	}
	return ne;
}
#define allocNode() &( allocNodge()->n ) 
#define allocEdge() &( allocNodge()->e )

Node *addNode() {
	Node *n = allocNode();
	chainFor(n)->data.node = n;
	indexNode(n);
	return n;
}
void addLoop(Node *n) {
	n->loops++;
}
Edge *addEdge(Node *src, Node *tgt) {
	Edge *e = allocEdge();
	unindexNode(src);
	unindexNode(tgt);
	prependElem( outListFor(src), outChain(e) );
	prependElem( inListFor(tgt), inChain(e) );
	e->src = src;
	e->tgt = tgt;
	outChain(e)->data.edge = e;
	inChain(e)->data.edge = e;
	indexNode(src);
	indexNode(tgt);
	return e;
}
void addEdgeById(long sid, long tid) {
	addEdge(getNodeById(sid), getNodeById(tid));
}

void deleteNode(Node *n) {
	if (indeg(n) + outdeg(n) + loopdeg(n)) {
		printf("Dangling condition violated\n");
		exit(1);
	}
	unindexNode(n);
	freeNode(n);
}
void deleteLoop(Node *n) {
	n->loops--;
}
void deleteEdge(Edge *e) {
	Node *src = source(e);
	Node *tgt = target(e);
	unindexNode(src);
	unindexNode(tgt);
	removeElem(outChain(e));
	removeElem(inChain(e));
	freeEdge(e);
	indexNode(src);
	indexNode(tgt);
}

/////////////////////////////////////////////////////////
// graph search

Trav travs[TRAV_COUNT];

#define getTrav(id) (&travs[(id)])
#define boundEdge(tid) ((getTrav(tid))->eMatch)
#define boundNode(tid) ((getTrav(tid))->nMatch)

void bindNode(long tid, Node *n) {
	boundNode(tid) = n;
	n->bound = 1;
}
void bindEdge(long tid, Edge *e) {
	boundEdge(tid) = e;
	e->bound = 1;
}
void unbindNode(long tid) {
	boundNode(tid)->bound = 0;
	getTrav(tid)->nMatch = NULL;
}
void unbindEdge(long tid) {
	boundEdge(tid)->bound = 0;
	getTrav(tid)->eMatch = NULL;
}

void findNode(long tid) {
	Trav *t = getTrav(tid);
	if (! t->nMatch) {
		
	} 
}

void followOutEdge(long src, long tgt) {
	Node *s = boundNode(src);
	DList *es = outListFor(s);
}


/////////////////////////////////////////////////////////
// Graph manipulation via travs

void deleteEdgeByTrav(long tid) {
	Edge *e = boundEdge(tid);
	unbindEdge(tid);
	deleteEdge(e);
}
void deleteNodeByTrav(long tid) {
	Node *n = boundNode(tid);
	unbindNode(tid);
	deleteNode(n);
}

void addNodeByTrav(long tid) {
	Node *n = addNode();
	bindNode(tid, n);
}
void addEdgeByTrav(long tid, long src, long tgt) {
	Node *s = boundNode(src), *t = boundNode(tgt);
	Edge *e = addEdge(s,t);
	bindEdge(tid, e);
}



/////////////////////////////////////////////////////////
// utilities

#define getId(ne) (((Nodge *) (ne)) - g.pool)
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
			n = index->data.node;
			//printf("%lx %lx\n", (long) n, (long) g.pool);
			printf("\t( n%ld, empty)\n", getId(n) );
		}
	}
	printf("|\n");
	// Dump edges
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		while ( (index = nextElem(index)) ) {
			n = index->data.node;
			out = outListFor(n);
			while ( (out = nextElem(out)) ) {
				e = out->data.edge;
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
	g.pool = malloc(sizeof(Nodge) * DEFAULT_POOL_SIZE);
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


