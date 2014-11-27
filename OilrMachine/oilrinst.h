
extern false;
extern true;
extern success;
extern travStack[];


#define PROC(label) void label() { \
label:

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

#define TE(src, tgt) newEdgeTrav(&(travStack[src]), &(travStack[tgt]));

#define XE(src, tgt) newNegatedEdgeTrav(src, tgt);

#define FIXO(r) constrainO(&(travStack[r]);
#define FIXI(r) constrainI(&(travStack[r]);
#define FIXL(r) constrainL(&(travStack[r]);

#define DELE  deleteEdges();
#define DELN  deleteNonInterfaceNodes();

#define NEWN()  newNode();
#define NEWE(src, tgt)  addNewOilrEdge(&(travStack[src]), &(travStack[tgt]));

#define ROOT(r) setRoot(&(travStack[r]));
#define TOOR(r) unsetRoot(&(travStack[r]));


#define GO runSearch();

