#include "oilrinst.h"

#ifndef NDEBUG
#include <stdio.h>
void trace0(FILE *f) {
	if (trav(0).match)
		fprintf(f, "n%d ", trav(0).match);
	else
		fprintf(f, "n_ ");
}
void trace1(FILE *f) {
	if (trav(1).match)
		fprintf(f, "e%d n%d ", trav(1).edge, trav(1).match);
	else
		fprintf(f, "e_ n_ ");
}
void trace2(FILE *f) {
	if (trav(2).match)
		fprintf(f, "e%d n%d ", trav(2).edge, trav(2).match);
	else
		fprintf(f, "e_ n_ ");
}
void trace3(FILE *f) {
	fprintf(f, "-x- ");
}
Tracer tracers[] = {&trace0, &trace1, &trace2, &trace3};


#endif

/* int signature(Node *n) {
	return (min(1, outdeg(n)) << 1) | min(1, indeg(n));
} */

OILR(1, 1, 0, 0)


int pred0(Node *n) {
	return (outdeg(n) >= 1 && indeg(n) >= 1);
}
int pred1(Edge *e) {
	Node *n = &elem(e->src);
	return (outdeg(n) >= 1);
}
int pred2(Edge *e) {
	Node *n = &elem(e->tgt);
	return (indeg(n) >= 1);
}
int pred3(Edge *e) {
	return (e->tgt == trav(2).match);
}


PLANS
SPC(3)  // o>=1 && i>=1

SPC(2)  // o>=1
SPC(3)

SPC(1)  // i>=1
SPC(3)
ENDP

TRAVS
TRAV(0, 0, pred0)
TRAV(1, 2, pred1)
TRAV(3, 4, pred2)
TRAV(0, 0, pred3)
ENDT


PROC(link)

	MTCH
	NODE(0)
	trace(0, 3, 0);
	//fprintf(stderr, "Node: %d has %d outEdges\n", trav(0).match, elem(trav(0).match).outEdges.len);
	ORFAIL(0)

	MTCH
	EDFR(0, 1)
	trace(0, 3, 1);
	ORBACK(1)

	MTCH
	EDTO(0, 2)
	trace(0, 3, 2);
	ORBACK(2)

	MTCH
	EDNO(1, 3)   // ugly! The target is contained in the pred! :(
	trace(0, 3, 3);
	ORBACK(3)

	ENDM
	ENDM
	ENDM
	ENDM

	//printf("success!\n");

	NEWE(1, 2)

	RESET(0)
	RESET(1)
	RESET(2)
RET



void GPMAIN() {
	ALAP(link)
}

#define NNODS 100
void _HOST() {
	int i;
	for (i=1; i<=NNODS; i++)
		NEWN
	for (i=1; i<NNODS; i++)
		addEdge(i, i+1);
}
