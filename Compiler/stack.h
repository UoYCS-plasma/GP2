/* ///////////////////////////////////////////////////////////////////////////

  ================================
  stack.h - Chris Bak (26/09/2014)
  ================================
                             
  Contains an implementation of a stack via a linked list.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_STACK_H
#define INC_STACK_H

#include "globals.h"

typedef union StackData {
   int index;
   struct Graph *graph;
} StackData;

typedef struct StackNode
{
   StackData data;
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

/* Frees all the StackNodes and the stack itself. Should only be called for 
 * stacks whose data is not in heap. */
void freeStack (Stack *stack);

#endif /* INC_STACK_H */
