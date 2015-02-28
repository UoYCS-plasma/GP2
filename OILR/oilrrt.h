#ifndef OILRRT_H
#define OILRRT_H

extern void _HOST();
extern void GPMAIN();

#define trav(n) (travs[n])
#define available(n) (!((n)->matched))

#define match(n) do {(n)->matched = 1;} while (0);  
#define unmatch(n) do {(n)->matched = 0;} while (0);

#define source(e) (&elem(e)->src)
#define target(e) (&elem(e)->tgt)

#define matchTarget(e) do { match( target(e) ); } while (0)
#define unmatchTarget(e) do { unmatch( target(e) ); } while (0)
#define matchSource(e) do { match( source(e) ); } while (0)
#define unmatchSource(e) do { unmatch( source(e) ); } while (0)

#define matchLoop(n) do { \
	(n)->matchedLoops++; } while (0)
#define unmatchLoop(n) do { \
	(n)->matchedLoops--; } while (0)
#define availableLoops(n) (loopdeg(n) - (n)->matchedLoops)

typedef struct Trav {
	union {
		NodeSignature oilr;
		int sig;
	};
	const int first;
	const int last;
	int cur;
	int next;

	int o, i, l, r;
	NodeId match;
	NodeList *locn;

} Trav;

typedef struct SearchPlan {
	const int first;
	const int last;
} SearchPlan;

typedef int TravId;

extern int searchSpaces[];
extern Trav travs[];


int success;

#endif
