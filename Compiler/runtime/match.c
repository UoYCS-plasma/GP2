/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.c - Chris Bak (14/08/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "match.h"

/*
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
} */

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
         morphism->assignment[count].value.first = NULL;
         morphism->assignment[count].value.last = NULL;
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
         morphism->assignment[count].value.first = NULL;
         morphism->assignment[count].value.last = NULL;
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

void addAssignment(Morphism *morphism, string variable, GP2List value)
{
   morphism->assignment[morphism->assignment_index].variable = variable;
   morphism->assignment[morphism->assignment_index].value = value;
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

void removeAssignment(Morphism *morphism)
{
   morphism->assignment_index--;
   morphism->assignment[morphism->assignment_index].variable = NULL;
   morphism->assignment[morphism->assignment_index].value.first = NULL;
   morphism->assignment[morphism->assignment_index].value.last = NULL;
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

   if(morphism->node_map)
   {
      printf("\nNode Mappings\n=============\n");
      for(count = 0; count < morphism->nodes; count++)
          printf("%d --> %d\n", morphism->node_map[count].left_index, 
                 morphism->node_map[count].host_index);
      printf("\n");
   }

   if(morphism->edge_map)
   {
      printf("Edge Mappings\n=============\n");
      for(count = 0; count < morphism->edges; count++)
          printf("%d --> %d\n", morphism->edge_map[count].left_index, 
                 morphism->edge_map[count].host_index);
      printf("\n");
   }

   if(morphism->assignment)
   {
      for(count = 0; count < morphism->variables; count++)
      {
         printf("Variable %s -> ", morphism->assignment[count].variable);
         printGP2List(morphism->assignment[count].value);
         printf("\n");
      }
   }
}

void freeMorphism(Morphism *morphism)
{
   if(morphism == NULL) return;
   if(morphism->node_map) free(morphism->node_map);
   if(morphism->edge_map) free(morphism->edge_map);
   if(morphism->assignment) free(morphism->assignment);
   free(morphism);
}



