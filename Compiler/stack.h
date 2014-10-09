/* ///////////////////////////////////////////////////////////////////////////

  ================================
  stack.h - Chris Bak (26/09/2014)
  ================================
                             
  Contains an implementation of a stack via a linked list.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_STACK_H
#define INC_STACK_H

#include "globals.h"

typedef struct StackNode
{
   void *data;
   struct StackNode *next;
} StackNode;

typedef struct Stack
{
   StackNode *top;
} Stack;

Stack *newStack();

/* Pushes data to the stack and updates stack->top. Should only be passed a 
 * pointer returned by newStack. */
void push (Stack *stack, void *data);

/* Returns the data pointer from the top stack node. It then frees that node 
 * and updates stack->top. */
void *pop (Stack *stack);

/* Frees the stack. Dynamically allocated data is freed with the passed
 * freeData function. */
void freeStack (Stack *stack, void (*freeData)(void *));

#endif /* INC_STACK_H */
