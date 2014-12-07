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

/* Frees all StackData and StackNode structs and the Stack itself. If the 
 * StackData contains a pointers to heap memory, it needs to be freed 
 * explicitly. */
void freeStack (Stack *stack);

#endif /* INC_STACK_H */
