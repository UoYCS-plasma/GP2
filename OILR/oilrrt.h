#ifndef OILRRT_H
#define OILRRT_H

extern void _HOST();
extern void GPMAIN();

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
