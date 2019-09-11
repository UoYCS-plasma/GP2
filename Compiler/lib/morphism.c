/* Copyright 2015-2017 Christopher Bak

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
   Morphism *morphism = mallocSafe(sizeof(Morphism), "makeMorphism");
   morphism->nodes = nodes;
   if(nodes > 0) morphism->node_map = callocSafe(nodes, sizeof(Map), "makeMorphism");
   else morphism->node_map = NULL;

   morphism->edges = edges;
   if(edges > 0) morphism->edge_map = callocSafe(edges, sizeof(Map), "makeMorphism");
   else morphism->edge_map = NULL;

   morphism->variables = variables;
   morphism->variable_index = 0;
   if(variables > 0) 
   {
      morphism->assignment = callocSafe(variables, sizeof(Assignment), "makeMorphism");
      morphism->assigned_variables = mallocSafe(variables * sizeof(int), "makeMorphism");
   }
   else 
   {
      morphism->assignment = NULL;
      morphism->assigned_variables = NULL;
   }
   initialiseMorphism(morphism);
   return morphism;
}

void initialiseMorphism(Morphism *morphism)
{ 
   for(int index = 0; index < morphism->nodes; index++)
      removeNodeMap(morphism, index);
     
   for(int index = 0; index < morphism->edges; index++)
      removeEdgeMap(morphism, index);

   morphism->variable_index = 0;
   for(int index = 0; index < morphism->variables; index++)
   {
      if(morphism->assignment[index].type == 's')
      {
         free(morphism->assignment[index].str);
         morphism->assignment[index].str = NULL;
      }
      else if(morphism->assignment[index].type == 'l')
      {
         #ifndef MINIMAL_GC
         removeHostList(morphism->assignment[index].list);
         #endif
         morphism->assignment[index].list = NULL;
      }
      morphism->assignment[index].type = 'n';
   }
}

void clearMatched(Morphism *morphism)
{ 
   for(int index = 0; index < morphism->nodes; index++)
   {
      if (morphism->node_map[index].node != NULL)
         clearNodeMatched(morphism->node_map[index].node);
   }
     
   for(int index = 0; index < morphism->edges; index++)
   {
      if (morphism->edge_map[index].edge != NULL)
         clearEdgeMatched(morphism->edge_map[index].edge);
   }
}

void addNodeMap(Morphism *morphism, int left_index, Node *node, int assignments)
{
   assert(left_index < morphism->nodes);
   morphism->node_map[left_index].node = node;
   morphism->node_map[left_index].assignments = assignments;
}

void addEdgeMap(Morphism *morphism, int left_index, Edge *edge, int assignments)
{
   assert(left_index < morphism->edges);
   morphism->edge_map[left_index].edge = edge;
   morphism->edge_map[left_index].assignments = assignments;
}

int addListAssignment(Morphism *morphism, int id, HostList *list) 
{
   assert(id < morphism->variables);
   
   /* Search the morphism for an existing assignment to the passed variable. */
   if(morphism->assignment[id].type == 'n') 
   {
      morphism->assignment[id].type = 'l';
      #ifndef MINIMAL_GC
      addHostList(list);
      #endif
      morphism->assignment[id].list = list;
      pushVariableId(morphism, id);
      return 1;
   }

   /* Compare the list in the assignment to the list passed to the function. */
   if(morphism->assignment[id].list == list) return 0;
   
   return -1;
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

   if(morphism->assignment[id].num == num) return 0;
   
   return -1;
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
   
   if(strcmp(morphism->assignment[id].str, str) == 0) return 0;
   
   return -1;
}

void removeNodeMap(Morphism *morphism, int left_index)
{
   morphism->node_map[left_index].node = NULL;
   removeAssignments(morphism, morphism->node_map[left_index].assignments);
   morphism->node_map[left_index].assignments = 0;
}

void removeEdgeMap(Morphism *morphism, int left_index)
{
   morphism->edge_map[left_index].edge = NULL;
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
      else if(morphism->assignment[id].type == 'l')
      {
         #ifndef MINIMAL_GC
         removeHostList(morphism->assignment[id].list);
         #endif
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

Node *lookupNode(Morphism *morphism, int left_index)
{
   return morphism->node_map[left_index].node;
}

Edge *lookupEdge(Morphism *morphism, int left_index)
{
   return morphism->edge_map[left_index].edge;
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

#ifndef MINIMAL_GC
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
         if(morphism->assignment[index].type == 'l')
            removeHostList(morphism->assignment[index].list);
      }
      free(morphism->assignment);
   }
   if(morphism->assigned_variables != NULL) free(morphism->assigned_variables);
   free(morphism);
}
#endif
