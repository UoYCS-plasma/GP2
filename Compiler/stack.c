/* ///////////////////////////////////////////////////////////////////////////

  ================================
  stack.c - Chris Bak (26/09/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "stack.h"

Stack *newStack()
{
   Stack *new_stack = malloc(sizeof(Stack));

   if(new_stack == NULL) 
   {
      print_to_log("Error: Memory exhausted during stack construction.\n");
      exit(1);
   }

   new_stack->top = NULL;
}


void push (Stack *stack, void *data) 
{
   if(stack == NULL) 
   {
      print_to_log("Error: push called with a NULL stack pointer.\n");
      exit(1);
   }

   StackNode *new_node = malloc(sizeof(StackNode));

   if(new_node == NULL) 
   {
      print_to_log("Error: Memory exhausted during stack node construction.\n");
      exit(1);
   }
  
   new_node->data = data;
   new_node->next = stack->top;
   stack->top = new_node;
}

void *pop (Stack *stack) 
{
   if(stack == NULL || stack->top == NULL) return NULL;

   void *data = stack->top->data;
   StackNode *node = stack->top;
   stack->top = stack->top->next;

   free(node);
   
   return data;
}

