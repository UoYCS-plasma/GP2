#include <assert.h>
#include <stdio.h>

#include "graph.h"



int main(int argc, char **argv) {
	int i;
	Graph *g = newGraph(DEF_NODE_POOL);
	int howMany = DEF_NODE_POOL+1;
	for (i=0; i<howMany; i++) {
		addNode(g);
		assert(g->free == i+1);
	}
	for (i=0; i<howMany; i++) {
		addEdge(g, i, (i+1)%howMany);
	}

	fprintf(stderr,"node-sig: %d, node: %d, edge: %d, graph: %d\n",
			(int) sizeof(NodeSignature),
			(int) sizeof(Node),
			(int) sizeof(Edge),
			(int) sizeof(Graph));
	printGraph(g);

	deleteGraph(g);
	return 0;
}
