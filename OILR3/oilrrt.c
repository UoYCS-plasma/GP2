#include <stdio.h>
#include <stdlib.h>

#define OILR_INDEX_SIZE (1<<3)
#define DEFAULT_POOL_SIZE (1024)

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
	long loops;
	long matchedLoops;
	DList index;
	DList outEdges;
	DList inEdges;
} Node;

typedef struct Edge {
	long matched;
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

#define POP    (*(dsp--))
#define TOS    (*(dsp))
#define LIT(n) do { *(++dsp) = (n); } while (0);
#define ADD    do { long sum = POP + TOS ; TOS = sum; } while (0);
#define SUB    do { long val = POP ; TOS = TOS - val; } while (0);
#define SHL    do { long bits = POP ; TOS = TOS << bits } while (0);
#define SHR    do { long bits = POP ; TOS = TOS >> bits } while (0);
#define LT     do { boolFlag = ( POP >= POP ); } while (0);
#define GT     do { boolFlag = ( POP <= POP ); } while (0);
#define EMIT   do { printf("%ld\n", POP); } while (0);


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

void addNode() {
	Node *n = allocNode();
	indexNode(n);
}

void addEdge(Node *src, Node *tgt) {
	Edge *e = allocEdge();
	unindexNode(src);
	unindexNode(tgt);
	prependElem( outListFor(src), outChain(e) );
	prependElem( inListFor(tgt), inChain(e) );
	indexNode(src);
	indexNode(tgt);
}

void deleteNode(Node *n) {
	if (indeg(n) + outdeg(n) + loopdeg(n)) {
		printf("Dangling condition violated\n");
		exit(1);
	}
	unindexNode(n);
	freeNode(n);
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
// utilities

void dumpGraph() {
	long i, edgeCount=0, nodeCount=0;
	DList *index;
	Node *n;
	printf("[\n");
	// Dump nodes
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		while (index = nextElem(index)) {
			n = index->data.node;
			printf("\t( n%ld, empty)\n", nodeCount++);
		}
	}
	printf("|\n");
	// Dump edges
	nodeCount=0;
	for (i=0; i<OILR_INDEX_SIZE; i++) {
		index = &(g.idx[i]);
		while (index = nextElem(index)) {
			n = index->data.node;
			out = outListFor(n);
			while (out = nextElem(out)) {
				printf("\t( n%ld, empty)\n", nodeCount++);
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

	// LIT(1) LIT(2) LIT(3) ADD ADD LIT(3) SUB EMIT
	LIT(3) LIT(2) LT
	printf("boolFlag: %ld\n", boolFlag);
	
	printf("Node: %ld, Edge: %ld, Nodge: %ld\n", sizeof(Node), sizeof(Edge), sizeof(Nodge));
	addNode();
	dumpGraph();
	free(g.pool);
	return 0;
}
