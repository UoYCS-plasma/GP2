/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.c - Chris Bak (14/08/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "match.h"

/* Assignment functions. */

Assignment *addAssignment(Assignment *assignment, string name, GList *value)
{
   Assignment *new_assignment = malloc(sizeof(Assignment));

   if(new_assignment == NULL) 
   {
      print_to_log("Memory exhausted during assignment construction.\n");
      exit(1);
   }

   new_assignment->variable = name;
   new_assignment->value = value;   
   new_assignment->next = assignment;

   return new_assignment;
}


Assignment *removeAssignment(Assignment *assignment)
{
   Assignment *new_assignment = assignment->next;
   if(assignment->variable) free(assignment->variable); 
   if(assignment->value) g_list_free_full(assignment->value, freeListElement);
   if(assignment) free(assignment);
   return new_assignment;
}


GList *lookupValue(Assignment *assignment, string name)
{
   while(assignment != NULL) 
   {
     if(strcmp(assignment->variable, name) == 0) return assignment->value;
     assignment = assignment->next;
   }
   return NULL;
         
}

void freeAssignment(Assignment *assignment)
{
   if(assignment == NULL) return;
   if(assignment->variable) free(assignment->variable); 
   if(assignment->value) g_list_free_full(assignment->value, freeListElement);
   if(assignment->next) freeAssignment(assignment->next);
   free(assignment);
}


/* Morphism functions. */

Morphism *makeMorphism(void)
{
   Morphism *new_morphism = malloc(sizeof(Morphism));

   if(new_morphism == NULL)
   {
      print_to_log("Memory exhausted during morphism construction.\n");
      exit(1);
   }

   new_morphism->node_images = newStack();
   new_morphism->edge_images = newStack();
   new_morphism->assignment = NULL;

   return new_morphism;
}


void printMorphism(Morphism *morphism)
{
   if(morphism == NULL)
   {
      printf("No morphism exists.\n\n");
      return;
   }

   printf("Morphism\n=======\n");

   StackNode *iterator = morphism->node_images->top;

   while(iterator != NULL)
   {
      printf("Host Node %d\n", iterator->data.index);
      iterator = iterator->next;
   }
  
   printf("\n");

   iterator = morphism->edge_images->top;

   while(iterator != NULL)
   {
      printf("Host Edge %d\n", iterator->data.index);
      iterator = iterator->next;
   }

   printf("\n");

   Assignment *assignment = morphism->assignment;

   while(assignment != NULL)
   {
      printf("Variable %s -> ", assignment->variable);
      printList(assignment->value);
      printf("\n");
      assignment = assignment->next;
   }
   printf("\n");
}

void freeMorphism(Morphism *morphism)
{
   if(morphism == NULL) return;
   if(morphism->node_images) freeStack(morphism->node_images);
   if(morphism->edge_images) freeStack(morphism->edge_images);
   if(morphism->assignment) freeAssignment(morphism->assignment);
   free(morphism);
}



