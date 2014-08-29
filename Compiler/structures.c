/* ///////////////////////////////////////////////////////////////////////////

  =====================================
  structures.c - Chris Bak (23/08/2014)
  =====================================

/////////////////////////////////////////////////////////////////////////// */

#include "structures.h"

Stack *newStack (int max_size) {

   Stack *new_stack = malloc(sizeof(Stack));

   if(new_stack == NULL) {
      print_to_log("Memory exhausted during stack construction.\n");
      exit(1);
   }
 
   void **items = calloc(max_size, sizeof(void*));

   if(items == NULL) {
      print_to_log("Memory exhausted during stack construction.\n");
      exit(1);
   } 

   /* Array indexing starts at 0, so an empty stack has -1 top */
   new_stack->top = -1;
   new_stack->max_size = max_size;
   new_stack->items = items;

   return new_stack;
}

void push (Stack *stack, void *data) {

   if(stackIsFull(stack)) print_to_log("Warning: Trying to push to a full stack.\n");
   else {
      stack->top++;
      stack->items[stack->top] = data;
   }
}

void *pop (Stack *stack) {

   void *popped_item = NULL;

   if(stackIsEmpty(stack)) {
      print_to_log("Warning: Trying to pop from an empty stack.\n");
      return NULL;
   }
   else {
      popped_item = stack->items[stack->top];
      stack->top--;
   }
   
   return popped_item;
}


bool stackIsFull (Stack *stack) {
   return stack->top == stack->max_size;
}

bool stackIsEmpty (Stack *stack) {
   return stack->top == -1;
}

void freeStack (Stack *stack) {
   /* Need to free the items in the stack rule_item. */
   free(stack->items);
   free(stack);
}


void addMap(GraphMapping *mapping, int rule_item, int host_item) {

   GraphMapping *new_mapping = malloc(sizeof(GraphMapping));

   if(new_mapping == NULL) {
      print_to_log("Memory exhausted during mapping construction.\n");
      exit(1);
   }

   new_mapping->rule_item = rule_item; 
   new_mapping->host_item = host_item;
   new_mapping->next = mapping;
}

int lookupFromRule(GraphMapping *mapping, int rule_item) {
 
   while(mapping != NULL) {
     if(mapping->rule_item == rule_item) return mapping->host_item;
     mapping = mapping->next;
   }
   return -1;
         
}

int lookupFromHost(GraphMapping *mapping, int host_item) {
 
   while(mapping != NULL) {
     if(mapping->host_item == host_item) return mapping->rule_item;
     mapping = mapping->next;
   }
   return -1;
         
}

void freeMapping(GraphMapping *mapping) {
   while(mapping->next != NULL) {
      freeMapping(mapping->next);
   }
   free(mapping);
}
   

