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

/* The parameter list of a rule. Each variable has one of the five GP 2 types 
 * according to the rule declaration. Used in the matching algorithm to check
 * the type of a variable for label matching.
 */

typedef enum {INTEGER_VAR = 0, CHARACTER_VAR, STRING_VAR, ATOM_VAR, LIST_VAR} 
  GPType;

typedef struct VariableList {
  string variable;
  GPType type; 
  struct VariableList *next;
} VariableList;

VariableList *addVariable(VariableList *variable_list, string name, GPType type);
GPType lookupType(VariableList *variable_list, string name);
void freeVariableList(VariableList *variable_list);


typedef struct Condition {
  CondExpType exp_type;		/* From ast.h */
  union {
    string var; 		/* INT_CHECK, CHAR_CHECK, STRING_CHECK, 
				 * ATOM_CHECK */
    struct {
      string source; 
      string target; 
      Label label;
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
   VariableList *variables;
   int number_of_variables;
   Graph *lhs;
   Graph *rhs; 
   /* Define data structure for interface. */
   int interface;
   Condition *condition;
   /* True if the rule does not change the host graph. */
   bool is_predicate; 
} Rule;



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
