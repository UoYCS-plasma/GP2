#define true 1
#define false 0

#include <assert.h>
#include <stdio.h>

#include "graph.h"

void checkEdgeIntegrity(Graph *g, NodeId nid, int eid) {
	Node *n = &node(g, nid);
	NodeId tgt = edge(n, eid);
	assert(tgt < nodePool.free);
}

void checkNodeIntegrity(Graph *g, NodeId nid) {
	Node *n = &node(g, nid);
	int i;
	for (i=0; i<outdeg(n); i++)
		checkEdgeIntegrity(g, nid, i);
}

void checkGraphIntegrity(Graph *g) {
	int i;
	for (i=0; i<g->nodes.len; i++)
		checkNodeIntegrity(g, i);
}

int compareNodes(Node *n1, Node *n2) {
	if (   outdeg(n1)  == outdeg(n2)
		&& indeg(n1)   == indeg(n2)
		&& loopdeg(n1) == loopdeg(n2)
		&& rooted(n1)  == rooted(n2) ) {
		return true;
	}
	return false;
}

int compareGraphs(Graph *g1, Graph *g2) {
	Node *n1, *n2;
	int i;
	checkGraphIntegrity(g1);
	checkGraphIntegrity(g2);
	if (  g1->nodes.len == g2->nodes.len ) {
		for (i=0; i<g1->nodes.len; i++) {
			n1 = &node(g1, i);
			n2 = &node(g2, i);
			if (!compareNodes(n1, n2) ) {
				return false;
			}
		}
		return true;
	} 
	return false;
}






int main(int argc, char **argv) {
	Graph *g1, *g2;
	int nodePoolSize = DEF_NODE_POOL;
	int i;

	g1 = newGraph();
	addNode(g1);
	for (i=1; i<nodePoolSize*2+1; i++) {
		addNode(g1);
		addEdge(g1, i, i-1);
	}
	addEdge(g1, nodePoolSize*2, 0);

	assert(nodePool.size == nodePoolSize*4);

	g2 = cloneGraph(g1);
	assert( compareGraphs(g1, g2) );
	addEdge(g2, 0, 8);
	assert( !compareGraphs(g1, g2) );
	addEdge(g1, 0, 8);
	assert( compareGraphs(g1, g2) );

	deleteGraph(g1);
	deleteGraph(g2);

	printf("graph: %d  node: %d  nodelist: %d\n", (int) sizeof(Graph), (int) sizeof(Node), (int)sizeof(NodeList) );
	return 0;
}

