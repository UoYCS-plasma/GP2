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
   initialiseMorphism(morphism);
   return morphism;
}

void initialiseMorphism(Morphism *morphism)
{ 
   int index;
   for(index = 0; index < morphism->nodes; index++)
   {
      morphism->node_map[index].host_index = -1;
      morphism->node_map[index].assignments = 0;
   }
   for(index = 0; index < morphism->edges; index++)
   {
      morphism->edge_map[index].host_index = -1;
      morphism->edge_map[index].assignments = 0;
   }
   morphism->variable_index = 0;
   for(index = 0; index < morphism->variables; index++)
   {
      morphism->assignment[index].type = NO_ASSIGNMENT;
      removeHostList(morphism->assignment[index].value);
      morphism->assignment[index].value = NULL;
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

void addAssignment(Morphism *morphism, int id, AssignmentType type, HostList *value)
{
   assert(id < morphism->variables);
   morphism->assignment[id].type = type;
   morphism->assignment[id].value = value;
   pushVariableId(morphism, id);
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
      morphism->assignment[id].type = NO_ASSIGNMENT;
      removeHostList(morphism->assignment[id].value);
      morphism->assignment[id].value = NULL;
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

Assignment lookupAssignment(Morphism *morphism, int id)
{
   assert(id < morphism->variables);
   return morphism->assignment[id];
}

int addListAssignment(Morphism *morphism, int id, HostList *list) 
{
   /* Search the morphism for an existing assignment to the passed variable. */
   Assignment assignment = lookupAssignment(morphism, id);
   if(assignment.type == NO_ASSIGNMENT) 
   {
      AssignmentType type = LIST_VAR;
      /* Assign the minimum type to the assignment. For lists of length 1, this is
       * either INTEGER_ASSIGNMENT or STRING_ASSIGNMENT. Otherwise it is
       * LIST_ASSIGNMENT. */
      if(list != NULL && list->first->next == NULL)
      {
         HostAtom atom = list->first->atom;
         assert(atom.type == 'i' || atom.type == 's');
         if(atom.type == 'i') type = INTEGER_ASSIGNMENT;
         else type = STRING_ASSIGNMENT;
      }
      #ifdef LIST_HASHING
         if(list != NULL)
         {
            assert(list_store[list->hash] != NULL);
            list_store[list->hash]->reference_count++;
         }
         addAssignment(morphism, id, type, list);
      #else
         HostList *list_copy = copyHostList(list);
         addAssignment(morphism, id, type, list_copy);
      #endif
      return 1;
   }
   /* Compare the list in the assignment to the list passed to the function. */
   else if(list == assignment.value) return 0; else return -1;
}

int addIntegerAssignment(Morphism *morphism, int id, int value)
{
   Assignment assignment = lookupAssignment(morphism, id);
   if(assignment.type == NO_ASSIGNMENT) 
   {
      HostAtom array[1];
      array[0].type = 'i';
      array[0].num = value;
      HostList *list = addHostList(array, 1, false);
      addAssignment(morphism, id, INTEGER_ASSIGNMENT, list);
      return 1;
   }
   else
   {
      if(assignment.value == NULL) return -1;
      HostAtom atom = assignment.value->first->atom;
      if(atom.type == 'i' && atom.num == value) return 0;
   }
   return -1;
}

int addStringAssignment(Morphism *morphism, int id, string value)
{
   Assignment assignment = lookupAssignment(morphism, id);
   if(assignment.type == NO_ASSIGNMENT) 
   {
      HostAtom array[1];
      array[0].type = 's';
      array[0].str = value;
      HostList *list = addHostList(array, 1, false);
      addAssignment(morphism, id, STRING_ASSIGNMENT, list);
      return 1;
   }
   else
   {
      if(assignment.value == NULL) return -1;
      HostAtom atom = assignment.value->first->atom;
      if(atom.type == 's' && !strcmp(atom.str, value)) return 0;
   }
   return -1;
}

int getIntegerValue(Morphism *morphism, int id)
{
   assert(id < morphism->variables);
   return morphism->assignment[id].value->first->atom.num;
}

string getStringValue(Morphism *morphism, int id)
{
   assert(id < morphism->variables);
   return morphism->assignment[id].value->first->atom.str;
}

HostList *getListValue(Morphism *morphism, int id)
{
   assert(id < morphism->variables);
   return morphism->assignment[id].value;
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
         HostList *list = morphism->assignment[index].value;
         if(list == NULL) printf("empty");
         else printHostList(list->first, stdout);
         printf("\n\n");
      }
   }
}

void freeMorphism(Morphism *morphism)
{
   if(morphism == NULL) return;
   if(morphism->node_map != NULL) free(morphism->node_map);
   if(morphism->edge_map != NULL) free(morphism->edge_map);
   #ifdef LIST_HASHING
      if(morphism->assignment != NULL) free(morphism->assignment);
   #else
      if(morphism->assignment != NULL)
      {
         int index;
         for(index = 0; index < morphism->variables; index++)
            removeHostList(morphism->assignment[index].value);
         free(morphism->assignment);
      }
   #endif
   if(morphism->assigned_variables != NULL) free(morphism->assigned_variables);
   free(morphism);
}

