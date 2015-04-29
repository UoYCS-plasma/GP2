#ifndef OILR_INSTRUCTIONS
#define OILR_INSTRUCTIONS

#include "graph.h"
#include "oilrrt.h"

extern Graph *gsp;

#define PLANS int searchSpaces[] = {
#define ENDP };

#define SPC(sig) (sig),

/* typedef struct Trav {
	const int first;
	const int last;
	int cur;
	int next;

	int o, i, l, r;
	EdgeId edge;
	NodeId match;

} Trav; */

#define TRAVS Trav travs[] = {
#define ENDT };

#define TRAV(fst, lst, pred) { \
	.first  = (fst), \
	.last   = (lst), \
	.p      = (pred), \
	.cur    = (fst), \
	.next   = 0, \
},

#define NODE(travid) search(&travs[travid]);
#define EDGE(src, travid) edgeBetween(&travs[src], &travs[travid], 0);
#define EDNO(src, travid) edgeBetween(&travs[src], &travs[travid], 1);

#define EDTO(src, tgt) followOutEdge(&travs[src], &travs[tgt]);
#define EDFR(tgt, src) followInEdge(&travs[tgt], &travs[src]);

#define LOOP(t) loopExists(&trav(t));

#define PROC(label) void label() {
#define RET return; \
}
#define CALL(label) label();

#define MTCH do {
#define ENDM } while (!success);

#define RESET(t) reset(&trav(t));
#define ORFAIL(t) if (!success) { RESET(t) ; return; }
#define ORBACK(t) if (!success) { RESET(t) ; break; }

#define ALAP(label) do { \
	label(); \
} while (success); \
success = 1;

#define DELE(t)  deleteEdge(travs[t].edge);
#define DELN(t)  deleteNode(travs[t].match);
#define DELL(t)  deleteLoop(travs[t].match);

#define NEWN  addNode();
#define NEWE(src, tgt)  addEdge(travs[src].match, travs[tgt].match);
#define NEWL(t) addLoop(travs[t].match);

#define ROOT(r) setRoot(&travs[r].n, 1);
#define TOOR(r) setRoot(&travs[r].n, 0);

/* ************* Signature functions *********** */
/* o, i, l, and r are number of bits devoted to 
 * each indexing dimension */

#define O_BITS
#define I_BITS
#define L_BITS
#define R_BITS


#define OILR(o, i, l, r)  \
int signature(Node *n) { \
	return  ( min( (1<<o)-1, outdeg(n)  ) << (i+l+r) \
			| min( (1<<i)-1, indeg(n)   ) << (l+r)   \
			| min( (1<<l)-1, loopdeg(n) ) << r       \
			| min( (1<<r)-1, root(n)    ) );         \
}




#endif
