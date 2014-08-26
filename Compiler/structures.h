/* ///////////////////////////////////////////////////////////////////////////

  =====================================
  structures.h - Chris Bak (23/08/2014)
  =====================================
                             
  Contains implementations of stacks and association lists.
  
/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_STRUCTURES_H
#define INC_STRUCTURES_H

#include "ast.h"

typedef struct Stack {
   int top; /* index to the top item in the stack */
   int max_size; /* can be determined statically by examining # nodes, edges in the rule. */
   void **items; /* array of stack items */
} Stack;


Stack *newStack (int maxSize);
void push (Stack *stack, void *data);
void *pop (Stack *stack);
bool stackIsFull (Stack *stack);
bool stackIsEmpty (Stack *stack);
void freeStack (Stack *stack);


typedef struct GraphMapping {
   int rule_item;
   int host_item;
   struct GraphMapping *next;
} GraphMapping;

/* addMap called with first argument NULL creates a new GraphMapping. */
void addMap(GraphMapping *list, int rule_item, int host_item);
int lookupFromRule(GraphMapping *list, int rule_item);
int lookupFromHost(GraphMapping *list, int rule_item);
void freeMapping(GraphMapping *list);

#endif /* INC_STRUCTURES_H */
