
extern false;
extern true;
extern success;


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

#define TE(src, tgt) newEdgeTrav(src, tgt);

#define XE(src, tgt) newEdgeAntiTrav(src, tgt);

#define FIXO(r)  
#define FIXI(r)
#define FIXL(r)

#define DELE  deleteEdges();
#define DELN  deleteNonInterfaceNodes();

#define NEWN()  newNode();
#define NEWE(src, tgt)  newEdge(src, tgt);

#define ROOT(r) setRoot(r);
#define TOOR(r) unsetRoot(r);


#define GO

