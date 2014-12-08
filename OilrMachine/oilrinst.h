#ifndef OILR_INSTRUCTIONS
#define OILR_INSTRUCTIONS

#include "oilrrt.h"

extern Traverser travStack[];
extern OilrGraph *gsp;


#define PROC(label) void label() {

#define RET clearTravs(); testInvariants(); return; \
}

#define CALL(label) label();
#define LOOP(label) do { \
	trace("[35mLooping PROC(%s)[0m\n", #label); \
	label(); \
	trace("[35mPROC(%s) %s[0m\n", #label, success ? "succeeded" : "failed"); } while (success);
#define ZTRF if (!success) { clearTravs(); testInvariants(); return; }

#define TN(o,i,l)    newNodeTrav(false, o, i, l, false);
#define TIN(o,i,l)   newNodeTrav(true, o, i, l, false);
#define TRN(o,i,l)   newNodeTrav(false, o, i, l, true);
#define TRIN(o,i,l)  newNodeTrav(true, o, i, l, true);

#define TE(src, tgt) newEdgeTrav(&(travStack[src].n), &(travStack[tgt].n));

#define XE(src, tgt) newNegatedEdgeTrav(&(travStack[src].n), &(travStack[tgt].n));

#define FIXO(r) constrainO(&(travStack[r].n));
#define FIXI(r) constrainI(&(travStack[r].n));
#define FIXL(r) constrainL(&(travStack[r].n));

#define DELE  trace("deleteEdges()"); deleteEdges();
#define DELN  trace("deleteNonInterfaceNodes()"); deleteNonInterfaceNodes();

#define NEWN  addNewOilrNode(false);
#define NEWE(src, tgt)  addNewEdge(&(travStack[src].n), &(travStack[tgt].n));

#define ROOT(r) setRoot(&(travStack[r].n), true);
#define TOOR(r) setRoot(&(travStack[r].n), false);


#define GO runSearch();

#endif
