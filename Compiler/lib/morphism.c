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
   morphism->assignment_index = 0;
   if(variables > 0) 
   {
      morphism->assignment = calloc(variables, sizeof(Assignment));
      if(morphism->assignment == NULL)
      {
         print_to_log("Error (makeMorphism): malloc failure.\n");
         exit(1);
      }
   }
   else morphism->assignment = NULL;
   initialiseMorphism(morphism);
   return morphism;
}

void initialiseMorphism(Morphism *morphism)
{ 
   int index;
   for(index = 0; index < morphism->nodes; index++)
   {
      morphism->node_map[index].host_index = -1;
      morphism->node_map[index].variables = 0;
   }
   for(index = 0; index < morphism->edges; index++)
   {
      morphism->edge_map[index].host_index = -1;
      morphism->edge_map[index].variables = 0;
   }
   morphism->assignment_index = 0;
   for(index = 0; index < morphism->variables; index++)
   {
      morphism->assignment[index].variable = NULL;
      morphism->assignment[index].type = LIST_VAR;
      #ifndef LIST_HASHING
         freeHostList(morphism->assignment[index].value);
      #endif
      morphism->assignment[index].value = NULL;
   }
}

void addNodeMap(Morphism *morphism, int left_index, int host_index, int variables)
{
   assert(left_index < morphism->nodes);
   morphism->node_map[left_index].host_index = host_index;
   morphism->node_map[left_index].variables = variables;
}

void addEdgeMap(Morphism *morphism, int left_index, int host_index, int variables)
{
   assert(left_index < morphism->edges);
   morphism->edge_map[left_index].host_index = host_index;
   morphism->edge_map[left_index].variables = variables;
}

void addAssignment(Morphism *morphism, string variable, GPType type, HostList *value)
{
   assert(morphism->assignment_index < morphism->variables);
   morphism->assignment[morphism->assignment_index].variable = variable;
   morphism->assignment[morphism->assignment_index].type = type;
   morphism->assignment[morphism->assignment_index].value = value;
   morphism->assignment_index++;
}

void removeNodeMap(Morphism *morphism, int left_index)
{
   morphism->node_map[left_index].host_index = -1;
   removeAssignments(morphism, morphism->node_map[left_index].variables);
   morphism->node_map[left_index].variables = 0;
}

void removeEdgeMap(Morphism *morphism, int left_index)
{
   morphism->edge_map[left_index].host_index = -1;
   removeAssignments(morphism, morphism->edge_map[left_index].variables);
   morphism->edge_map[left_index].variables = 0;
}

void removeAssignments(Morphism *morphism, int number)
{
   int count;
   for(count = 0; count < number; count++)
   {
      morphism->assignment_index--;
      morphism->assignment[morphism->assignment_index].variable = NULL;
      morphism->assignment[morphism->assignment_index].type = LIST_VAR;
      #ifndef LIST_HASHING
         freeHostList(morphism->assignment[morphism->assignment_index].value);
      #endif
      morphism->assignment[morphism->assignment_index].value = NULL;
   }
}

int lookupNode(Morphism *morphism, int left_index)
{
   return morphism->node_map[left_index].host_index;
}

int lookupEdge(Morphism *morphism, int left_index)
{
   return morphism->edge_map[left_index].host_index;
}

Assignment *lookupVariable(Morphism *morphism, string variable)
{
   int count;
   for(count = 0; count < morphism->variables; count++)
   {
      Assignment assignment = morphism->assignment[count];
      if(assignment.variable == NULL) continue;
      if(!strcmp(assignment.variable, variable)) return &(morphism->assignment[count]);
   }
   return NULL;
}

int addListAssignment(Morphism *morphism, string name, HostList *list) 
{
   /* Search the morphism for an existing assignment to the passed variable. */
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      GPType type = LIST_VAR;
      /* Assign the minimum type to the assignment. For lists of length 1, this is
       * either INTEGER_VAR or STRING_VAR. Otherwise it is LIST_VAR. */
      if(list != NULL && list->first->next == NULL)
      {
         HostAtom atom = list->first->atom;
         assert(atom.type == 'i' || atom.type == 's');
         if(atom.type == 'i') type = INTEGER_VAR;
         else type = STRING_VAR;
      }
      #ifdef LIST_HASHING
         addAssignment(morphism, name, type, list);
      #else
         HostList *list_copy = copyHostList(list);
         addAssignment(morphism, name, type, list_copy);
      #endif
      return 1;
   }
   /* Compare the list in the assignment to the list passed to the function. */
   if(list == assignment->value) return 0; else return -1;
}

int addIntegerAssignment(Morphism *morphism, string name, int value)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      HostAtom array[1];
      array[0].type = 'i';
      array[0].num = value;
      HostList *list = addHostList(array, 1, false);
      addAssignment(morphism, name, INTEGER_VAR, list);
      return 1;
   }
   else
   {
      if(assignment->value == NULL) return -1;
      HostAtom atom = assignment->value->first->atom;
      if(atom.type == 'i' && atom.num == value) return 0;
   }
   return -1;
}

int addStringAssignment(Morphism *morphism, string name, string value)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      HostAtom array[1];
      array[0].type = 's';
      array[0].str = value;
      HostList *list = addHostList(array, 1, false);
      addAssignment(morphism, name, STRING_VAR, list);
      return 1;
   }
   else
   {
      if(assignment->value == NULL) return -1;
      HostAtom atom = assignment->value->first->atom;
      if(atom.type == 's' && !strcmp(atom.str, value)) return 0;
   }
   return -1;
}

int getIntegerValue(Morphism *morphism, string name)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      print_to_log("Error (getIntegerValue): Variable %s is not in the morphism.\n", name);
      return 0;
   }
   assert(assignment->value != NULL);
   return assignment->value->first->atom.num;
}

string getStringValue(Morphism *morphism, string name)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      print_to_log("Error (getStringValue): Variable %s is not in the morphism.\n", name);
      return NULL;
   }
   assert(assignment->value != NULL);
   return assignment->value->first->atom.str;
}

HostList *getListValue(Morphism *morphism, string name)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      print_to_log("Error (getListValue): Variable %s is not in the morphism.\n", name);
      return NULL;
   }
   return assignment->value;
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
         Assignment assignment = morphism->assignment[index];
         if(assignment.variable != NULL)
         {
            printf("Variable %s -> ", assignment.variable);
            if(assignment.value == NULL) printf("empty");
            else printHostList(assignment.value->first, stdout);
            printf("\n\n");
         }
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
         for(index = 0; index < morphism->assignment_index; index++)
         {
            Assignment assignment = morphism->assignment[index];
            if(assignment.value != NULL) freeHostList(assignment.value);
         }
         free(morphism->assignment);
      }
   #endif
   free(morphism);
}

