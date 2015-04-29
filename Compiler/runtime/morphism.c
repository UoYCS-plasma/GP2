#include "morphism.h"

Morphism *makeMorphism(int nodes, int edges, int variables)
{
   Morphism *morphism = malloc(sizeof(Morphism));
   if(morphism == NULL)
   {
      print_to_log("Memory exhausted during morphism construction.\n");
      exit(1);
   }
   morphism->nodes = nodes;
   morphism->node_map_index = 0;

   int count;
   if(nodes > 0) 
   {
      morphism->node_map = calloc(nodes, sizeof(Map));
      if(morphism->node_map == NULL)
      {
         print_to_log("Memory exhausted during morphism construction.\n");
         exit(1);
      }
      for(count = 0; count < nodes; count++)
      {
         morphism->node_map[count].left_index = -1;
         morphism->node_map[count].host_index = -1;
         morphism->node_map[count].added_variables = -1;
      }
   }
   else morphism->node_map = NULL;

   morphism->edges = edges;
   morphism->edge_map_index = 0;

   if(edges > 0) 
   {
      morphism->edge_map = calloc(edges, sizeof(Map));
      if(morphism->edge_map == NULL)
      {
         print_to_log("Memory exhausted during morphism construction.\n");
         exit(1);
      }
      for(count = 0; count < edges; count++)
      {
         morphism->edge_map[count].left_index = -1;
         morphism->edge_map[count].host_index = -1;
         morphism->edge_map[count].added_variables = -1;
      }
   }
   else morphism->edge_map = NULL;

   morphism->variables = variables;
   morphism->assignment_index = 0;

   if(variables > 0) 
   {
      morphism->assignment = calloc(variables, sizeof(Assignment));
      if(morphism->assignment == NULL)
      {
         print_to_log("Memory exhausted during morphism construction.\n");
         exit(1);
      }
      for(count = 0; count < variables; count++)
      {
         morphism->assignment[count].variable = NULL;
         morphism->assignment[count].length = 0;
         morphism->assignment[count].value = NULL;
      }
   }
   else morphism->assignment = NULL;
   return morphism;
}

void clearMorphism(Morphism *morphism)
{ 
   morphism->node_map_index = 0;
   int count;
   if(morphism->nodes > 0) 
   {
      for(count = 0; count < morphism->nodes; count++)
      {
         morphism->node_map[count].left_index = -1;
         morphism->node_map[count].host_index = -1;
         morphism->node_map[count].added_variables = -1;
      }
   }

   morphism->edge_map_index = 0;
   if(morphism->edges > 0) 
   {
      for(count = 0; count < morphism->edges; count++)
      {
         morphism->edge_map[count].left_index = -1;
         morphism->edge_map[count].host_index = -1;
         morphism->edge_map[count].added_variables = -1;
      }
   }

   morphism->assignment_index = 0;
   if(morphism->variables > 0) 
   {
      for(count = 0; count < morphism->variables; count++)
      {
         morphism->assignment[count].variable = NULL;
         morphism->assignment[count].length = 0;
         morphism->assignment[count].value = NULL;
      }
   }
}

void addNodeMap(Morphism *morphism, int left_index, int host_index)
{
   morphism->node_map[morphism->node_map_index].left_index = left_index;
   morphism->node_map[morphism->node_map_index].host_index = host_index;
   morphism->node_map_index++;
}

void addEdgeMap(Morphism *morphism, int left_index, int host_index)
{
   morphism->edge_map[morphism->edge_map_index].left_index = left_index;
   morphism->edge_map[morphism->edge_map_index].host_index = host_index;
   morphism->edge_map_index++;
}

void addAssignment(Morphism *morphism, string variable, int length, Atom *value)
{
   morphism->assignment[morphism->assignment_index].variable = strdup(variable);
   morphism->assignment[morphism->assignment_index].length = length;
   morphism->assignment[morphism->assignment_index].value = copyList(value, length);
   morphism->assignment_index++;
}

void removeNodeMap(Morphism *morphism)
{
   morphism->node_map_index--;
   morphism->node_map[morphism->node_map_index].left_index = -1;
   morphism->node_map[morphism->node_map_index].host_index = -1;
}

void removeEdgeMap(Morphism *morphism)
{
   morphism->edge_map_index--;
   morphism->edge_map[morphism->edge_map_index].left_index = -1;
   morphism->edge_map[morphism->edge_map_index].host_index = -1;
}

void removeAssignments(Morphism *morphism, int number)
{
   int count;
   for(count = 0; count < number; count++)
   {
      morphism->assignment_index--;
      Assignment *assignment = &(morphism->assignment[morphism->assignment_index]);

      if(assignment->variable) free(assignment->variable);
      freeList(assignment->value, assignment->length);

      assignment->variable = NULL;
      assignment->length = 0;
      assignment->value = NULL;
   }
}

int lookupVariable(Morphism *morphism, string variable)
{
   int count;
   for(count = 0; count < morphism->variables; count++)
   {
      if(morphism->assignment[count].variable == NULL) continue;
      if(!strcmp(morphism->assignment[count].variable, variable)) return count;
   }
   return -1;
}

int findHostIndex(Morphism *morphism, int left_index)
{
   int count;
   for(count = 0; count < morphism->nodes; count++)
   {
      if(morphism->node_map[count].left_index == left_index) 
         return morphism->node_map[count].host_index;
   }
   return -1;
}

void printMorphism(Morphism *morphism)
{
   if(morphism == NULL)
   {
      printf("No morphism exists.\n\n");
      return;
   }
   int count;
   if(morphism->node_map != NULL)
   {
      printf("\nNode Mappings\n=============\n");
      for(count = 0; count < morphism->nodes; count++)
          printf("%d --> %d\n", morphism->node_map[count].left_index, 
                 morphism->node_map[count].host_index);
      printf("\n");
   }
   if(morphism->edge_map != NULL)
   {
      printf("Edge Mappings\n=============\n");
      for(count = 0; count < morphism->edges; count++)
          printf("%d --> %d\n", morphism->edge_map[count].left_index, 
                 morphism->edge_map[count].host_index);
      printf("\n");
   }
   if(morphism->assignment != NULL)
   {
      for(count = 0; count < morphism->variables; count++)
      {
         if(morphism->assignment[count].variable != NULL)
         {
            printf("Variable %s -> ", morphism->assignment[count].variable);
            printList(morphism->assignment[count].value, 
                      morphism->assignment[count].length, stdout);
            printf("\n");
            printf("Length: %d\n\n", morphism->assignment[count].length);
         }
      }
   }
}

void freeMorphism(Morphism *morphism)
{
   if(morphism == NULL) return;
   if(morphism->node_map) free(morphism->node_map);
   if(morphism->edge_map) free(morphism->edge_map);
   if(morphism->assignment)
   {
      int index;
      for(index = 0; index < morphism->variables; index++)
      {
         Assignment assignment = morphism->assignment[index];
         if(assignment.variable != NULL) free(assignment.variable);
         freeList(assignment.value, assignment.length);
      }
      free(morphism->assignment);
   }
   free(morphism);
}

