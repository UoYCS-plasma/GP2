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
  
   return new_stack;
}


void push (Stack *stack, StackData *data) 
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

StackData *pop (Stack *stack) 
{
   if(stack == NULL || stack->top == NULL) return NULL;

   StackData *data = stack->top->data;
   StackNode *node = stack->top;
   stack->top = stack->top->next;

   free(node);
   
   return data;
}

/* int findHostIndex(Stack *stack, int left_index)
{
   if(stack == NULL) return -1;

   StackNode *iterator = stack->top;

   while(iterator != NULL)
   {
      if(iterator->data->map.left_index == left_index)
         return iterator->data->map.host_index;
      iterator = iterator->next;
   }
   return -1;
} */


void freeStack(Stack *stack)
{
   if(stack == NULL) return;

   StackNode *iterator = stack->top;

   while(iterator != NULL)
   {
      if(iterator->data != NULL) free(iterator->data);
      StackNode *temp = iterator;
      iterator = iterator->next;
      free(temp);
   }
   free(stack);
}


