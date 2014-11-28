#ifndef OILR_INSTRUCTIONS
#define OILR_INSTRUCTIONS

extern Traverser travStack[];
extern OilrGraph oilrGraphStack[];
extern OilrGraph *gsp;


#define PROC(label) void label() {

#define RET return; \
}

#define CALL(label) label();
#define LOOP(label) do { label(); } while (success);
#define JS(label) if (success) goto label;
#define JF(label) if (!success) goto label;
#define ZTRF if (!success) return;

#define TN(o,i,l)    newNodeTrav(false, o, i, l, false);
#define TIN(o,i,l)   newNodeTrav(true, o, i, l, false);
#define TRN(o,i,l)   newNodeTrav(false, o, i, l, true);
#define TRIN(o,i,l)  newNodeTrav(true, o, i, l, true);

#define TE(src, tgt) newEdgeTrav(&(travStack[src].n), &(travStack[tgt].n));

#define XE(src, tgt) newNegatedEdgeTrav(&(travStack[src].n), &(travStack[tgt].n));

#define FIXO(r) constrainO(&(travStack[r].n);
#define FIXI(r) constrainI(&(travStack[r].n);
#define FIXL(r) constrainL(&(travStack[r].n);

#define DELE  deleteEdges();
#define DELN  deleteNonInterfaceNodes();

#define NEWN()  addNewOilrNode(false);
#define NEWE(src, tgt)  addNewEdge(&(travStack[src]), &(travStack[tgt]));

#define ROOT(r) setRoot(&(travStack[r].n), true);
#define TOOR(r) setRoot(&(travStack[r].n), false);


#define GO runSearch();

#endif
