#define fail(...) do { \
	printf ("\n==========\n") ; \
	printf (__VA_ARGS__) ; \
	printf ("\n") ; \
	error(1, 0, "Invariant violation"); \
} while (false);

int countNodes() {
	return getNodeId(freeNode);
}

int countOilrNodes() {
	int i, total=0;
	for (i=0; i<MAX_NODES; i++) {
		if (!isDeletedOilrNode(&(oilrNodePool[i])))
			total++;
	}
	return total;
}

int countEdges() {
	int i, total=0;
	for (i=0; i<MAX_EDGES; i++) {
		if ( !isDeletedEdge(&(edgePool[i])) )
			total++;
	}
	return total;
}

void INV_all_OilrNodes_have_corresponding_Node() {
	OilrNode *on;
	int i;
	if (countOilrNodes() != countNodes() )
		fail("Differing numbers of Nodes and OilrNodes");

	for (i=0; i<MAX_NODES; i++) {
		on = &(oilrNodePool[i]);
		if ( !isDeletedOilrNode(on) ) {
			if ( getNodeId(on->node) != i )
				fail("Node and OilrNode ids do not correspond");
		}
	}
}

int walk_chain(OilrNode *head) {
	OilrNode *on = head;
	int steps=0, nodes=0;
	while ( nextInChain(on) ) {
		on = nextInChain(on);
		if (hasRealNode(on))
			steps++;
	}
	nodes = steps;
	while ( prevInChain(on) ) {
		on = prevInChain(on);
		if (hasRealNode(on))
			steps--;
	}
	if (on != head)
		fail("Asymmetrical chain.");

	return nodes;
}

void INV_check_indices() {
	int o,i,l;
	int seenNodes=0;
	Index *ind;
	OilrNode *head;
	for (o=0; o<TOOMANYO; o++) {
		for (i=0; i<TOOMANYI; i++) {
			for (l=0; l<TOOMANYL; l++) {
				ind = &(gsp->indices[o][i][l][false]);
				head = &(ind->head);

				if (!isMatched(head))
					fail("dummy node with unset matched flag: [%d][%d][%d][false]", o, i, l);

				seenNodes += walk_chain(head);
			}
		}
	}
	if (seenNodes != countNodes() )
		fail("Number of nodes (%d) != number of index entries (%d)", countNodes(), seenNodes);

}

void INV_sum_of_outdegrees() {
	OilrNode *on;
	Node *n;
	int es = countEdges();
	int i, j, edgeCount=0;
	for (i=0; i<countOilrNodes(); i++) {
		on = &(oilrNodePool[i]);
		if (isDeletedOilrNode(on))
			continue;
		n = on->node;
		edgeCount += getO(n) + getL(n);
		for (j=0; j<n->outdegree; j++) {
			if (n->out_edges[j] < edgePool
					|| n->out_edges[j] > &(edgePool[MAX_EDGES]))
				fail("out-edge %d of node %d not in edgePool", j, i);
		}
	}
	if (es != edgeCount)
		fail("Sum of out-edges (%d) != number of edges (%d)", edgeCount, es);
}

void INV_check_search_spaces() {
	Traverser *t = travStack;
	SearchSpace *spc;
	while (t <= tsp) {
		if (isNodeTrav(t)) {
			spc = &(t->n.searchSpace);
			if (spc->edgeFrom && spc->index) {
				fail("Search space has both edgeFrom and index defined");
			} else if (spc->edgeFrom && !t->n.oilrNode) {
				
			} else if (spc->index) {
			}
		}
		t++;
	}
}



void testInvariants() {
	INV_all_OilrNodes_have_corresponding_Node();
	INV_check_indices();
	INV_check_search_spaces();
	INV_sum_of_outdegrees();

	/* sum of lens in indices
	 * 	== nodeCount
	 * 	== oilrNodeCount */
				/* root chain -> prev -> next shoudl be root chain. */

				/* every real node n in chain ch points back to index s, at the
				 * head of ch */

					/* on->next is never equal to on->prev or on! */

				/* every chain must be the same length in both directions, and 
				 * prevs must end at the head where nexts started. */

	/* total number of live edges in graph
	 *   == sum of outdegrees of all nodes
	 *   == sum or indegrees of all nodes */

	/* for every node the number of used out_edges must be the node's outdegree */
		/* every node, n with a loopdegree of l has exactly 
		*   l out-edges with target=n */
	/* for every edge e, e is an element of s->source->out_edges */
}
