/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.h - Chris Bak (14/08/2014)
  ================================
                             
  Header file for the rule matching module. Defines the data structures for
  variables, conditions, rules and graph morphisms.
  
/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_MATCH_H
#define INC_MATCH_H

#include "rule.h"
#include <glib.h>

typedef struct Morphism {
   GraphMapping *node_matches;
   GraphMapping *edge_matches;
   Assignment *assignment;
} Morphism;

void freeMorphism(Morphism *morphism);

Morphism *staticSearchplan(Graph *lhs, Graph *host, VariableList *variables);
int matchRootNode(Node *rule_root, Graph *host, VariableList *variables,
                  int position, GraphMapping *node_matches);
int matchEdge(Edge *rule_edge, Graph *host, VariableList *variables,
              bool match_from_source, int index, GraphMapping *edge_matches); 
int matchIncidentNode(Edge *rule_edge, Edge *host_edge, VariableList *variables,
                      GraphMapping *node_matches, bool match_from_source);
int matchNode(Node *rule_node, Graph *host, VariableList *variables,
              int index, GraphMapping *node_matches);
 
/* Calls verifyAssignment and takes its return value. */
bool labelMatch (VariableList *variables, Label rule_label, Label host_label);
bool compareAtoms(ListElement *rule_atom, ListElement *host_atom,
		  VariableList *variables);
string isPrefix(const string test, const string str);
string isSuffix(const string test, const string str);


/* applyRule will call findMatch */
void applyRule (Rule *rule, Morphism *match, Graph *host);


#endif /* INC_MATCH_H */
