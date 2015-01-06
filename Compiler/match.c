/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.c - Chris Bak (14/08/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "match.h"

/* Assignment functions. */

Assignment *addAssignment(Assignment *assignment, string name, GP2List *value)
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
   if(assignment->value) freeGP2List(assignment->value);
   if(assignment) free(assignment);
   return new_assignment;
}


GP2List *lookupValue(Assignment *assignment, string name)
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
   if(assignment->value) freeGP2List(assignment->value);
   if(assignment->next) freeAssignment(assignment->next);
   free(assignment);
}


Map *addMap(Map *map, int right_index, Node *host_node)
{
   Map *new_map = malloc(sizeof(Map));

   if(new_map == NULL) 
   {
      print_to_log("Error: Memory exhausted during map construction.\n");
      exit(1);
   }

   new_map->right_index = right_index;
   new_map->host_node = host_node;
   new_map->next = map;

   return new_map;
}

Node *findHostNode(Map *map, int right_index)
{
   while(map != NULL)
   {
      if(right_index == map->right_index) return map->host_node;
      else map = map->next;
   }
   return NULL;
}

void freeMap(Map *map)
{
   if(map == NULL) return;
   if(map->next) freeMap(map->next);
   free(map);
}


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

   StackNode *iterator = morphism->node_images->top;

   printf("\nNode Mappings\n=============\n");
   while(iterator != NULL)
   {
      printf("%d --> %d\n", iterator->data->map.left_index, 
             iterator->data->map.host_index);
      iterator = iterator->next;
   }
  
   printf("\n");

   iterator = morphism->edge_images->top;

   printf("Edge Mappings\n=============\n");
   while(iterator != NULL)
   {
      printf("%d --> %d\n", iterator->data->map.left_index, 
             iterator->data->map.host_index);
      iterator = iterator->next;
   }

   printf("\n");

   Assignment *assignment = morphism->assignment;

   while(assignment != NULL)
   {
      printf("Variable %s -> ", assignment->variable);
      printGP2List(assignment->value);
      printf("\n");
      assignment = assignment->next;
   }
}

void freeMorphism(Morphism *morphism)
{
   if(morphism == NULL) return;
   if(morphism->node_images) freeStack(morphism->node_images);
   if(morphism->edge_images) freeStack(morphism->edge_images);
   if(morphism->assignment) freeAssignment(morphism->assignment);
   free(morphism);
}



