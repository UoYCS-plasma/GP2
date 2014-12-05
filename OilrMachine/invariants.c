#define fail(...) do { \
	printf ("\n ** ") ; \
	printf (__VA_ARGS__) ; \
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

void walk_chain(OilrNode *head) {
	OilrNode *on = head;
	int steps=0;
	while ( nextInChain(on) ) {
		on = nextInChain(on);
		steps++;
	}
	while ( prevInChain(on) ) {
		on = prevInChain(on);
		steps--;
	}
	if (on != head)
		fail("Asymmetrical chain.");

}

void INV_check_indices() {
	int o,i,l;
	Index *ind;
	OilrNode *head;
	for (o=0; o<TOOMANYO; o++) {
		for (i=0; i<TOOMANYI; i++) {
			for (l=0; l<TOOMANYL; l++) {
				ind = &(gsp->indices[o][i][l][false]);
				head = &(ind->head);

				if (!isMatched(head))
					fail("dummy node with unset matched flag: [%d][%d][%d][false]", o, i, l);

				walk_chain(head);
			}
		}
	}

}

void INV_check_search_spaces() {
}


bool testInvariants() {
	bool passed = true;
	int o, i, l, sum=0, steps=0, skips=0, loops=0;
	/* TODO: will these still hold we start deleting nodes?! */
	int nodeCount = getNodeId(freeNode);
	int edgeCount = countEdges();
	int oilrNodeCount = countOilrNodes();
	int deletedEdgeCount = 0;
	Index *s;
	Node *n;
	Edge *e;
	OilrNode *on;

	for (i=0; i<edgeCount; i++) {
		if ( isDeletedEdge(&(edgePool[i])) )
			deletedEdgeCount++;
	}

	printf("Testing invariants... ");
	/* sum of lens in indices
	 * 	== nodeCount
	 * 	== oilrNodeCount */
	if (nodeCount != oilrNodeCount) {
		fail("Number of Nodes is not the same as number of OilrNodes");
	}
	for (o=0; o<TOOMANYO; o++) {
		for (i=0; i<TOOMANYI; i++) {
			for (l=0; l<TOOMANYL; l++) {
				steps = 0;
				skips = 0;
				s = &(gsp->indices[o][i][l][false]);
				/* root chain -> prev -> next shoudl be root chain. */
				if (&(s->head) != s->head.chain.next->chain.prev)
					fail("Root chaining incorrectly initialised");


				sum += s->len;

				/* every real node n in chain ch points back to index s, at the
				 * head of ch */
				on = &(s->head);
				while ( on->chain.next != NULL && steps <= MAX_NODES) {
					/* on->next is never equal to on->prev or on! */
					if (on->chain.next == on)
						fail("Link with self-referential next pointer in chain [%d][%d][%d]",
								o, i, l);
					if (on->chain.prev == on)
						fail("Link with self-referential prev pointer");
					if (on->chain.next == on->chain.prev)
						fail("Link with identical next and prev");
					on = on->chain.next;
					steps++;
					if (on->node == NULL) /* skip dummy nodes */
						skips++;
					if (on->node != NULL && on->chain.index != s) {
						fail("Found node with incorrect index pointer");
					}
					if (hasRealNode(on) && isMatched(on))
						fail("Found a spurious matched node: %d", getOilrNodeId(on));
					/*if (!passed)
						break;*/
				}
				if (steps-skips != s->len)
					fail("Found an incorrect chain length. %d-%d != s->len: %d",
							steps, skips, s->len);

				while ( on->chain.prev != NULL && steps >= 0 ) {
					on = on->chain.prev;
					steps--;
				}

				/* every chain must be the same length in both directions, and 
				 * prevs must end at the head where nexts started. */
				if (on != &(s->head) )
					fail("Found an inconsistency in a chain.");
				if (steps != 0)
					fail("asymmetric chain");

				s = &(gsp->indices[o][i][l][true]);
				if (!isMatched(&(s->head)))
					fail("Found a dummy node on root chain with unset matched flag");

			}
		}
	}
	if (sum != nodeCount) {
		fail("nodeCount (%d) differs from number of index table entries (%d)",
				nodeCount, sum);
	}

	/* total number of live edges in graph
	 *   == sum of outdegrees of all nodes
	 *   == sum or indegrees of all nodes */
	o = 0;
	i = 0;
	for (l=0; l<nodeCount; l++) {
		o += nodePool[l].outdegree;
		i += nodePool[l].indegree;
	}
	if (edgeCount-deletedEdgeCount != o) {
		fail("Number of Edges (%d) differs from sum of outdegrees (%d)",
				edgeCount, o);
	}
	if (edgeCount-deletedEdgeCount != i) {
		fail("Number of Edges (%d) differs from sum of indegrees (%d)",
				edgeCount, o);
	}

	/* for every node the number of used out_edges must be the node's outdegree */
	o = 0;
	i = 0;
	for (steps=0; steps<nodeCount; steps++) {
		i=0; o=0; loops = 0;
		n = &(nodePool[steps]);
		while ( (e = n->out_edges[i++]) != NULL) {
			if (e->source == e->target)
				loops++;
			o++;
		}
		if (o != n->outdegree) {
			fail("Found a node with an incorrect outdegree");
		}
		/* every node, n with a loopdegree of l has exactly 
		*   l out-edges with target=n */
		if (loops != n->loopdegree) {
			fail("Found a node with an incorrect loopdegree");
		}
	}
		

	/* for every edge e, e is an element of s->source->out_edges */
	if (passed)
		printf("[32mOK[0m\n");
	else
		printf("\n[31mFailed[0m\n");
	return passed;
}
