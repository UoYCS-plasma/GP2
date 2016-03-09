/* Copyright 2015-2016 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>. */

#include "morphism.h"

Morphism *makeMorphism(int nodes, int edges, int variables)
{
   Morphism *morphism = malloc(sizeof(Morphism));
   if(morphism == NULL)
   {
      print_to_log("Error (makeMorphism): malloc failure.\n");
      exit(1);
   }
   morphism->nodes = nodes;
   if(nodes > 0) 
   {
      morphism->node_map = calloc(nodes, sizeof(Map));
      if(morphism->node_map == NULL)
      {
         print_to_log("Error (makeMorphism): malloc failure.\n");
         exit(1);
      }
   }
   else morphism->node_map = NULL;

   morphism->edges = edges;
   if(edges > 0) 
   {
      morphism->edge_map = calloc(edges, sizeof(Map));
      if(morphism->edge_map == NULL)
      {
         print_to_log("Error (makeMorphism): malloc failure.\n");
         exit(1);
      }
   }
   else morphism->edge_map = NULL;

   morphism->variables = variables;
   morphism->variable_index = 0;
   if(variables > 0) 
   {
      morphism->assignment = calloc(variables, sizeof(Assignment));
      if(morphism->assignment == NULL)
      {
         print_to_log("Error (makeMorphism): malloc failure.\n");
         exit(1);
      }
      morphism->assigned_variables = calloc(variables, sizeof(int));
      if(morphism->assigned_variables == NULL)
      {
         print_to_log("Error (makeMorphism): malloc failure.\n");
         exit(1);
      }
   }
   else 
   {
      morphism->assignment = NULL;
      morphism->assigned_variables = NULL;
   }
   initialiseMorphism(morphism, NULL);
   return morphism;
}

void initialiseMorphism(Morphism *morphism, Graph *graph)
{ 
   int index;
   for(index = 0; index < morphism->nodes; index++)
   {
      if(graph != NULL && morphism->node_map[index].host_index >= 0)
         resetMatchedNodeFlag(graph, morphism->node_map[index].host_index);
      morphism->node_map[index].host_index = -1;
      morphism->node_map[index].assignments = 0;
   }
   for(index = 0; index < morphism->edges; index++)
   {
      if(graph != NULL && morphism->edge_map[index].host_index >= 0)
         resetMatchedEdgeFlag(graph, morphism->edge_map[index].host_index);
      morphism->edge_map[index].host_index = -1;
      morphism->edge_map[index].assignments = 0;
   }
   morphism->variable_index = 0;
   for(index = 0; index < morphism->variables; index++)
   {
      if(morphism->assignment[index].type == 's')
      {
         free(morphism->assignment[index].str);
         morphism->assignment[index].str = NULL;
      }
      if(morphism->assignment[index].type == 'l')
      {
         removeHostList(morphism->assignment[index].list);
         morphism->assignment[index].list = NULL;
      }
      morphism->assignment[index].type = 'n';
      morphism->assigned_variables[index] = -1;
   }
}

void addNodeMap(Morphism *morphism, int left_index, int host_index, int assignments)
{
   assert(left_index < morphism->nodes);
   morphism->node_map[left_index].host_index = host_index;
   morphism->node_map[left_index].assignments = assignments;
}

void addEdgeMap(Morphism *morphism, int left_index, int host_index, int assignments)
{
   assert(left_index < morphism->edges);
   morphism->edge_map[left_index].host_index = host_index;
   morphism->edge_map[left_index].assignments = assignments;
}

int addListAssignment(Morphism *morphism, int id, HostList *list) 
{
   /* Search the morphism for an existing assignment to the passed variable. */
   assert(id < morphism->variables);
   if(morphism->assignment[id].type == 'n') 
   {
      morphism->assignment[id].type = 'l';
      #ifdef LIST_HASHING
         addHostList(list);
         morphism->assignment[id].list = list;
      #else
         HostList *list_copy = copyHostList(list);
         morphism->assignment[id].list = list_copy;
      #endif
      pushVariableId(morphism, id);
      return 1;
   }
   /* Compare the list in the assignment to the list passed to the function. */
   else 
   {
      if(morphism->assignment[id].list == list) return 0;
      else return -1;
   }
}

int addIntegerAssignment(Morphism *morphism, int id, int num)
{
   assert(id < morphism->variables);
   if(morphism->assignment[id].type == 'n') 
   {
      morphism->assignment[id].type = 'i';
      morphism->assignment[id].num = num;
      pushVariableId(morphism, id);
      return 1;
   }
   else
   {
      if(morphism->assignment[id].num == num) return 0;
      else return -1;
   }
}

int addStringAssignment(Morphism *morphism, int id, string str)
{
   assert(id < morphism->variables);
   if(morphism->assignment[id].type == 'n') 
   {
      morphism->assignment[id].type = 's';
      morphism->assignment[id].str = strdup(str);
      pushVariableId(morphism, id);
      return 1;
   }
   else
   {
      if(strcmp(morphism->assignment[id].str, str) == 0) return 0;
      else return -1;
   }
}

void removeNodeMap(Morphism *morphism, int left_index)
{
   morphism->node_map[left_index].host_index = -1;
   removeAssignments(morphism, morphism->node_map[left_index].assignments);
   morphism->node_map[left_index].assignments = 0;
}

void removeEdgeMap(Morphism *morphism, int left_index)
{
   morphism->edge_map[left_index].host_index = -1;
   removeAssignments(morphism, morphism->edge_map[left_index].assignments);
   morphism->edge_map[left_index].assignments = 0;
}

void removeAssignments(Morphism *morphism, int number)
{
   int count;
   for(count = 0; count < number; count++)
   {
      int id = popVariableId(morphism);
      if(morphism->assignment[id].type == 's')
      {
         free(morphism->assignment[id].str);
         morphism->assignment[id].str = NULL;
      }
      if(morphism->assignment[id].type == 'l')
      {
         removeHostList(morphism->assignment[id].list);
         morphism->assignment[id].list = NULL;
      }
      morphism->assignment[id].type = 'n';
   }
}

void pushVariableId(Morphism *morphism, int id)
{
   assert(morphism->variable_index < morphism->variables);
   morphism->assigned_variables[morphism->variable_index++] = id;
}

int popVariableId(Morphism *morphism)
{
   assert(morphism->variable_index > 0);
   morphism->variable_index--;
   return morphism->assigned_variables[morphism->variable_index];
}

int lookupNode(Morphism *morphism, int left_index)
{
   return morphism->node_map[left_index].host_index;
}

int lookupEdge(Morphism *morphism, int left_index)
{
   return morphism->edge_map[left_index].host_index;
}

int getIntegerValue(Morphism *morphism, int id)
{
   assert(id < morphism->variables);
   return morphism->assignment[id].num;
}

string getStringValue(Morphism *morphism, int id)
{
   assert(id < morphism->variables);
   return morphism->assignment[id].str;
}

Assignment getAssignment(Morphism *morphism, int id)
{
   assert(id < morphism->variables);
   return morphism->assignment[id];
}

int getAssignmentLength(Assignment assignment)
{
   if(assignment.type != 'l') return 1;
   if(assignment.list == NULL) return 0;
   HostListItem *item = assignment.list->first;
   int length = 0;
   while(item != NULL) 
   {
      length++;
      item = item->next;
   }
   return length;
}

/* If rule_string is a prefix of host_string, return the position in host_string
 * immediately after the end of rule_string. Otherwise return -1. */
int isPrefix(const string rule_string, const string host_string)
{
   int length = strlen(rule_string);
   int offset = strlen(host_string) - length;
   if(offset < 0) return -1;
   /* strncmp compares rule_string against the first strlen(rule_string) characters
    * of host_string. */
   if(!strncmp(host_string, rule_string, length)) return length;
   else return -1;
}

/* If rule_string is a proper suffix of host_string, return the position in 
 * host_string immediately before the start of rule_string. If rule_string
 * equals host_string, return 0. Otherwise return -1. */
int isSuffix(const string rule_string, const string host_string)
{
   int offset = strlen(host_string) - strlen(rule_string);
   if(offset < 0) return -1;
   /* Compare the last strlen(rule_string) characters of str with rule_string. */
   if(!strcmp(host_string + offset, rule_string)) 
      return offset == 0 ? 0 : offset - 1;
   else return -1;
}

void printMorphism(Morphism *morphism)
{
   if(morphism == NULL)
   {
      printf("No morphism exists.\n\n");
      return;
   }
   int index;
   if(morphism->node_map != NULL)
   {
      printf("\nNode Mappings\n=============\n");
      for(index = 0; index < morphism->nodes; index++)
         printf("%d --> %d\n", index, morphism->node_map[index].host_index);
      printf("\n");
   }
   if(morphism->edge_map != NULL)
   {
      printf("Edge Mappings\n=============\n");
      for(index = 0; index < morphism->edges; index++)
         printf("%d --> %d\n", index, morphism->edge_map[index].host_index);
      printf("\n");
   }
   if(morphism->assignment != NULL)
   {
      for(index = 0; index < morphism->variables; index++)
      {
         printf("Variable %d -> ", index);
         if(morphism->assignment[index].type == 'n') printf("Unassigned");
         if(morphism->assignment[index].type == 'i') 
           printf("%d", morphism->assignment[index].num);
         if(morphism->assignment[index].type == 's')
           printf("\"%s\"", morphism->assignment[index].str);
         if(morphism->assignment[index].type == 'l')
         {
            if(morphism->assignment[index].list == NULL) printf("empty");
            else printHostList(morphism->assignment[index].list->first, stdout);
         }
         printf("\n\n");
      }
   }
}

void freeMorphism(Morphism *morphism)
{
   if(morphism == NULL) return;
   if(morphism->node_map != NULL) free(morphism->node_map);
   if(morphism->edge_map != NULL) free(morphism->edge_map);
   if(morphism->assignment != NULL)
   {
      int index;
      for(index = 0; index < morphism->variables; index++)
      {
         if(morphism->assignment[index].type == 's') 
            free(morphism->assignment[index].str);
         #ifdef LIST_HASHING
            if(morphism->assignment[index].type == 'l')
               removeHostList(morphism->assignment[index].list);
         #endif
      }
      free(morphism->assignment);
   }
   if(morphism->assigned_variables != NULL) free(morphism->assigned_variables);
   free(morphism);
}

