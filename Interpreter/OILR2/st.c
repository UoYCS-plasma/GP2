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

int signature(Node *n) {
	return (min(1, outdeg(n)) << 1) | min(1, indeg(n));
}

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

/* spaces
 *
 * 
 *
 *
 */

PLANS
ENDP

TRAVS
/* init 0-3 */
TRAV()
TRAV()
TRAV()
TRAV()

/* inc 4 */
TRAV()

/* prepare 5-6 */
TRAV()

/* expand */
TRAV()
TRAV()
TRAV()
TRAV()

TRAV()
TRAV()
TRAV()

ENDT


PROC(link)
RET

void init() {
	NODE(pred0)
	ORFAIL

	NEWT(1)
	NEWT(2)
	NEWT(3)

	NEWE(1, 2)
	NEWE(2, 3)
	NEWE(3, 1)
	NEWL(1)
}

void inc() {
	NODE(pred0)
	ORFAIL
	LOOP(4)
	ORFAIL
	DELL(4)
}

void prepare() {
	RESET(5)
	NODE(5)
	ORFAIL

	LOOP(5)
	ORBACK

	RESET(6)
	EDTO(6)
	ORBACK
	
	NEWE(6, 5)
}

void expand() {
	RESET(7)
	NODE(7)
	ORFAIL

	RESET(8)
	EDTO(7, 8)
	ORBACK

	RESET(9)
	EDFR(7, 9)
	ORBACK

	RESET(10)
	EDGE(7, 10)
	ORBACK


}

void Modify() {
	CALL(inc)
	ORFAIL
	ALAP(prepare)
	ALAP(expand)
}

void GPMAIN() {
	CALL(init)
	ORFAIL
	ALAP(Modify)
	CALL(clean)
	ORFAIL
}

#define GEN 5
void _HOST() {
	NEWN()
	ROOT(1)
	for (i=0; i<GEN; i++) {
		NEWL(1)
	}
}
