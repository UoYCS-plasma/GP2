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
		fprintf(f, "n%d ", trav(2).match);
	else
		fprintf(f, "n_ ");
}
Tracer tracers[] = {&trace0, &trace1, &trace2};


#endif

OILR(1, 1, 1, 0)


int pred0(Node *n) {
	return (outdeg(n) >= 1 && indeg(n) == 0);
}
int pred1(Edge *e) {
	Node *n = &elem(e->tgt);
	return (indeg(n) >= 1);
}
int pred2(Node *n) {
	return (indeg(n) == 0 && outdeg(n) == 0 && loopdeg(n) == 0);
}

PLANS

SPC(4)  // o>=1 && i==0 && l==0
SPC(5)  // o>=1 && i==0 && l>=1

SPC(2)  // o==0 && i>=1 && l==0
SPC(3)  // o==0 && i>=1 && l>=1
SPC(6)  // o>=1 && i>=1 && l==0
SPC(7)  // o>=1 && i>=1 && l==1

SPC(0)  // o==0 && i==0 && l==0

ENDP

TRAVS
TRAV(0, 1, pred0)
TRAV(2, 5, pred1)

TRAV(6, 6, pred2)
ENDT


PROC(delete)

	MTCH
	NODE(0)
	trace(0, 1, 0);
	ORFAIL(0)

	MTCH
	EDTO(0, 1)
	trace(0, 1, 1);
	ORBACK(1)

	ENDM
	ENDM

	DELE(1)

	RESET(0)
	RESET(1)
RET

PROC(delete_isolated)
	MTCH
	NODE(2)
	trace(2, 2, 2);
	ORFAIL(2)
	ENDM

	DELN(2)

	RESET(2)
RET

void GPMAIN() {
	ALAP(delete)
	ALAP(delete_isolated)
}

/* void _HOST() {
	NEWN NEWN
	addEdge(1, 2);
} */

#define NNODS 10000
void _HOST() {
	int i;
	for (i=1; i<=NNODS; i++)
		NEWN
	for (i=1; i<NNODS; i++)
		addEdge(i, i+1);
//	addEdge(NNODS, 1);
}
