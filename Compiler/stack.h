/* ///////////////////////////////////////////////////////////////////////////

  ============
  Stack Module
  ============

  Contains an implementation of a stack as a linked list. All possible stack
  values are wrapped in a union StackData.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_STACK_H
#define INC_STACK_H

#include "error.h"
#include "globals.h"

typedef union StackData {
   struct {
      int left_index;
      int host_index;
   } map;
   int free_slot;
   struct Graph *graph;
   struct Rule *rule;
} StackData;

typedef struct StackNode
{
   StackData *data;
   struct StackNode *next;
} StackNode;

typedef struct Stack
{
   StackNode *top;
} Stack;

Stack *newStack();

/* Pushes data to the stack and updates stack->top. Should only be passed a 
 * pointer returned by newStack. */
void push (Stack *stack, StackData *data);

/* Returns the data pointer from the top stack node. It then frees that node 
 * and updates stack->top. */
StackData *pop (Stack *stack);

int findHostIndex(Stack *stack, int left_index);

/* Frees all StackData and StackNode structs and the Stack itself. If the 
 * StackData contains a pointers to heap memory, it needs to be freed 
 * explicitly. */
void freeStack (Stack *stack);

#endif /* INC_STACK_H */
