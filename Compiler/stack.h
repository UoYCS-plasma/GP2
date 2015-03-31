/* ///////////////////////////////////////////////////////////////////////////

  ============
  Stack Module
  ============

  Contains an implementation of a stack as an array.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_STACK_H
#define INC_STACK_H

#define GRAPH_STACK_SIZE 4
#define GRAPH_CHANGE_STACK_SIZE 32

#include "error.h"
#include "globals.h"

typedef union StackData
{
   struct Graph *graph;
   struct GraphChange *graph_change;
} StackData;

/* The rightmost entry of the array is the top of the stack. */
typedef struct Stack
{
   /* Top stores the array index after the topmost item. */
   int top;
   int size;
   StackData *data;
} Stack;

Stack *newStack(int size);

/* Pushes data to the stack and updates stack->top. Should only be passed a 
 * pointer returned by newStack. */
void push(Stack *stack, StackData data);

/* Returns the data pointer from the top stack node. It then frees that node 
 * and updates stack->top. */
StackData pop(Stack *stack);

/* Frees the stack array. If the stack contains pointers to heap memory, it
 * must be freed explicitly. */
void freeStack(Stack *stack);

#endif /* INC_STACK_H */
