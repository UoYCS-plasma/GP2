#ifndef OILRRT_H
#define OILRRT_H


#ifndef MAX_TRAVS
#define MAX_TRAVS 128
#endif


extern int searchSpaces[];


typedef struct Trav {
	union {
		NodeSignature oilr;
		int sig;
	};
	const int first;
	const int last;
	int cur;
	int pos;

	int o, i, l, r;
	NodeId match;

} Trav;


Trav searchPlans[MAX_TRAVS];
int success;

#endif
