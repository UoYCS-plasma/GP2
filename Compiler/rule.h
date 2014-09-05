/* ///////////////////////////////////////////////////////////////////////////

  ===============================
  rule.h - Chris Bak (23/08/2014)
  ===============================
                             
  Contains definitions for the structures necessary for rule application
  except for graphs: rules, conditions, stacks and association lists.
  
/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_STRUCTURES_H
#define INC_STRUCTURES_H

#include "graph.h"

typedef enum {INT_VAR = 0, CHAR_VAR, STRING_VAR, ATOM_VAR, LIST_VAR} GPType;

typedef struct VariableList {
  string variable;
  GPType type; 
  struct VariableList *next;
} VariableList;

void addVariable(VariableList *variable_list, string name, GPType type);
GPType lookupType(VariableList *variable_list, string name);
void freeVariableList(VariableList *variable_list);

/* Association list for mappings between rule nodes/edges and host nodes/edges. 
 * The integers refer to the item's indices in the pointer arrays of their 
 * respective graphs.
 */

typedef struct GraphMapping {
   int rule_item;
   int host_item;
   /* Controls backtracking.
    * For a node, the flag is true if we need to search for another 
    * match for this node while backtracking.
    * For an edge, the flag is true if the edge was matched from its
    * source node. */
   bool flag;
   struct GraphMapping *next;
} GraphMapping;

/* addMap called with first argument NULL creates a new GraphMapping. */
GraphMapping *addMapping(GraphMapping *mapping, int rule_item, int host_item, 
                         bool flag); 
GraphMapping *removeMapping(GraphMapping *mapping);
int lookupFromRule(GraphMapping *mapping, int rule_item);
int lookupFromHost(GraphMapping *mapping, int rule_item);
void freeMapping(GraphMapping *mapping);


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
   /* Generated from the AST with list->value == NULL. Type information is 
    * needed at this stage. */
   VariableList *variables;
   int number_of_variables;
   Graph *lhs;
   Graph *rhs; 
   /* Association list between LHS nodes and RHS nodes. Bidir edges too. */
   GraphMapping *interface;
   Condition *condition;
   /* True if the rule does not change the host graph. */
   bool is_predicate; 
} Rule;


/* Association list for mappings between variables and values. */

typedef struct Assignment {
  string variable;
  GList *value;
  struct Assignment *next;
} Assignment;

bool verifyList(Assignment *assignment, string name, GList *value);
bool verifyAtom(Assignment *assignment, string name, ListElement *value);
bool compareConstants(ListElement *atom, ListElement *test_atom);
Assignment *addAssignment(Assignment *assignment, string name, GList *value);
GList *lookupValue(Assignment *assignment, string name);
void freeAssignment(Assignment *assignment);


/* Generic stack implementation. Used to keep track of the host graph changes
 * for try statements, where we may need to roll back to an older graph. 
 * If not used at all for rules, move this to another module. */

typedef struct Stack {
   int top; /* index to the top item in the stack */
   int max_size; /* can be determined statically by examining # nodes, edges in the rule. */
   void **items; /* array of stack items */
} Stack;


Stack *newStack (int maxSize);
void push (Stack *stack, void *data);
void *pop (Stack *stack);
void freeStack (Stack *stack);

#endif /* INC_RULE_H */
