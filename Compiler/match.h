/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.h - Chris Bak (14/08/2014)
  ================================
                             
  Header file for the rule matching module. Defines the data structures for
  variables, conditions, rules and graph morphisms.
  
/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_MATCH_H
#define INC_MATCH_H

#include "ast.h"
#include "graph.h"
#include "structures.h"
#include <glib.h>


typedef struct NodeMorphism {
   Node *rule_node;
   Node *host_node;
} NodeMorphism;

typedef struct EdgeMorphism {
   Edge *rule_edge;
   Edge *host_edge;
} EdgeMorphism;

typedef struct Assignment {
  string name;
  GList *value;
} Assignment;

typedef struct Morphism {
   Stack *node_matches;
   Stack *edge_matches;
   Stack *assignment;
} Morphism;

bool matchRootNodes(Graph *lhs, Graph *host); 
bool matchSourceAndTargets(Graph *lhs, Graph *host);
bool matchEdge(Edge *rule_edge, Graph *lhs, Graph *host); 
bool matchNode(Node *rule_node, Graph *lhs, Graph *host);

Morphism *makeMorphism(Graph *lhs, Graph *host, GraphMapping *node_matches, 
		       GraphMapping *edge_matches, Stack *assignment);
bool addAssignment(Morphism *morphism, string name, GList *value);
void freeMorphism(Morphism *morphism);


typedef enum {INT_VAR = 0, CHAR_VAR, STRING_VAR, ATOM_VAR, LIST_VAR} GPType;

typedef struct Variable {
   string name;
   GPType type;
} Variable;


typedef struct Condition {
  CondExpType exp_type;		/* From ast.h */
  union {
    string var; 		/* INT_CHECK, CHAR_CHECK, STRING_CHECK, 
				 * ATOM_CHECK */
    struct {
      string source; 
      string target; 
      /* Edge predicate can take an optional label argument, but how to represent
       * marks in a textual rule condition? Perhaps a list might be better. */
      GList *list;
    } edge_pred; 		/* EDGE_PRED */

    struct { 
      GList *left_list;
      GList *right_list; 
    } list_cmp; 		/* EQUAL, NOT_EQUAL */

    struct { 
      GList *left_exp; 
      GList *right_exp; 
    } atom_cmp; 		/* GREATER, GREATER_EQUAL, LESS, LESS_EQUAL */

    struct Condition *not_exp;  /* BOOL_NOT */

    struct { 
      struct Condition *left_exp; 
      struct Condition *right_exp; 
    } bin_exp; 			/* BOOL_OR, BOOL_AND */
  } value;
} Condition;


typedef struct Rule {
   string name;
   /* List of Variables, probably generated from the symbol table.
    * Not sure if GSList is most appropriate type. */
   GSList *variables;
   Graph *lhs;
   Graph *rhs; 
   /* Association list between LHS nodes and RHS nodes. Bidir edges too. */
   GSList *interface;
   Condition *condition;
   /* True if the rule does not change the host graph. */
   bool is_predicate; 
} Rule;

/* I am hoping I am able to evaluate the condition during rule matching. */
Morphism *findMatch (Graph *lhs, Graph *host, int number_of_variables);

/* Calls addAssignment and takes its return value. */
bool labelMatch (Stack *assignment, Label rule_label, Label host_label);



/* applyRule will call findMatch */
void applyRule (Rule *rule, Morphism *match, Graph *host);


#endif /* INC_MATCH_H */
