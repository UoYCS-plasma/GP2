


/* ************* Predicates ******************** */
/* this is a hack. These should be generated at
 * compile time, as this library could grow large */

int pred_oGEq1_iGEq1(Node *n) {
	return (outdeg(n) >= 1 && indeg(n) >= 1);
}
int pred_oGEq1(Edge *e) {
	Node *n = &elem(e->src);
	return (outdeg(n) >= 1);
}
int pred_iGEq1(Edge *e) {
	Node *n = &elem(e->tgt);
	return (indeg(n) >= 1);
}
int pred_tgtEqTrav2(Edge *e) {
	return (e->tgt == trav(2).match);
}



