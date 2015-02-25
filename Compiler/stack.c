/* ///////////////////////////////////////////////////////////////////////////

  ================================
  stack.c - Chris Bak (26/09/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "stack.h"

Stack *newStack(int size)
{
   Stack *stack = malloc(sizeof(Stack));
   if(stack == NULL) 
   {
      print_to_log("Error: Memory exhausted during stack construction.\n");
      exit(1);
   }

   stack->top = 0;
   stack->size = size;
   stack->data = calloc(size, sizeof(StackData));
   
   if(stack->data == NULL) 
   {
      print_to_log("Error: Memory exhausted during stack construction.\n");
      exit(1);
   }
   return stack;
}


void push(Stack *stack, StackData data) 
{
   if(stack == NULL) 
   {
      print_to_log("Error: push called with a NULL stack pointer.\n");
      exit(1);
   }

   if(stack->top == stack->size) 
   {
      print_to_console("Warning: Push called on full stack. No data pushed.\n");
      return;
   }
   stack->data[stack->top++] = data;
}

StackData pop(Stack *stack) 
{
   if(stack->top == 0) 
   {
      print_to_console("Warning: Pop called on empty stack.\n");
      exit(1);
   }
   stack->top--;
   return stack->data[stack->top];
}

void freeStack(Stack *stack)
{
   if(stack == NULL) return;
   free(stack->data);
   free(stack);
}


