#define O_BITS 0
#define I_BITS 0
#define L_BITS 0
#define R_BITS 0

#include "oilrinst.h"

PLANS
SPC(0)
ENDP

TRAVS
TRAV(0, 0, 0, 0, 0, 0)
TRAV(0, 0, 0, 0, 0, 0)
ENDT


PROC(adde)
	NODE(0)
	ORFAIL
	NODE(1)
	ORFAIL
	RESET(0)
	RESET(1)
	NEWE(0, 1)
RET

PROC(addl)
	NODE(0)
	ORFAIL
	NODE(1)
	ORFAIL
	RESET(0)
	RESET(1)
	NEWL(0)
	NEWL(1)
RET

PROC(delnode)
	NODE(0)
	ORFAIL
	RESET(0)
	DELN(0)
RET



void GPMAIN() {
	CALL(addl)
	CALL(adde)
	//CALL(rule)
	//CALL(rule)
	//ALAP(delnode)
}

void _HOST() {
	int i;
	for (i=0; i<10000; i++)
	//for (i=0; i<3; i++)
		NEWN
}
