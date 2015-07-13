#include <stdio.h>
#include <stdlib.h>

#define OILR_INDEX_SIZE (1<<3)
#define DEFAULT_POOL_SIZE (1024)


struct Node;
struct Edge;

typedef union ListPayload {
		struct Node *node;
		struct Edge *edge;
		long count;
} ListPayload ;


typedef struct DList {
	// Doubly-linked list
	union ListPayload data;
	struct DList *next;
	struct DList *prev;
} DList;

typedef struct Node {
	long sig;
	long loops;
	long matchedLoops;
	DList index;
	DList outedges;
	DList inedges;
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
	Nodge *pool;
	DList idx[OILR_INDEX_SIZE];
} Graph;


/////////////////////////////////////////////////////////
// doubly-linked list support

#define nextElem(dl) ((dl)->next)
#define prevElem(dl) ((dl)->prev)

void headInsertElem(DList *dl, DList *elem) {
	elem->prev = NULL ;
	elem->next = dl->next;
	dl->next->prev = elem;
	dl->next = elem;
}

void removeElem(DList *elem) {
	DList *nx = elem->next;
	DList *pv = elem->prev;
	if (nx) nx->prev = pv;
	if (pv) pv->next = nx;
	elem->next = NULL;
	elem->prev = NULL;
}










/////////////////////////////////////////////////////////
// main

Graph g;

int main(int argc, char **argv) {
	g.pool = malloc(sizeof(Nodge) * DEFAULT_POOL_SIZE);
	if (!g.pool)
		exit(1);
	printf("Node: %ld, Edge: %ld, Nodge: %ld\n", sizeof(Node), sizeof(Edge), sizeof(Nodge));
	free(g.pool);
	return 0;
}
