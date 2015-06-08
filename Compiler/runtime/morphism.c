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
   if(nodes > 0) 
   {
      morphism->node_map = calloc(nodes, sizeof(Map));
      if(morphism->node_map == NULL)
      {
         print_to_log("Memory exhausted during morphism construction.\n");
         exit(1);
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
   }
   else morphism->assignment = NULL;
   initialiseMorphism(morphism);
   return morphism;
}

void initialiseMorphism(Morphism *morphism)
{ 
   morphism->node_map_index = 0;
   int count;
   for(count = 0; count < morphism->nodes; count++)
   {
      morphism->node_map[count].left_index = -1;
      morphism->node_map[count].host_index = -1;
      morphism->node_map[count].variables = 0;
   }
   morphism->edge_map_index = 0;
   for(count = 0; count < morphism->edges; count++)
   {
      morphism->edge_map[count].left_index = -1;
      morphism->edge_map[count].host_index = -1;
      morphism->edge_map[count].variables = 0;
   }

   morphism->assignment_index = 0;
   for(count = 0; count < morphism->variables; count++)
   {
      Assignment *assignment = &(morphism->assignment[count]);
      if(assignment->variable != NULL)
      {  
         free(assignment->variable); 
         assignment->variable = NULL;
      }
      assignment->type = LIST_VAR;
      if(assignment->value != NULL)
      {  
         freeList(assignment->value); 
         assignment->value = NULL;
      }
   }
}

void addNodeMap(Morphism *morphism, int left_index, int host_index, int variables)
{
   assert(morphism->node_map_index < morphism->nodes);
   morphism->node_map[morphism->node_map_index].left_index = left_index;
   morphism->node_map[morphism->node_map_index].host_index = host_index;
   morphism->node_map[morphism->node_map_index].variables = variables;
   morphism->node_map_index++;
}

void addEdgeMap(Morphism *morphism, int left_index, int host_index, int variables)
{
   assert(morphism->edge_map_index < morphism->edges);
   morphism->edge_map[morphism->edge_map_index].left_index = left_index;
   morphism->edge_map[morphism->edge_map_index].host_index = host_index;
   morphism->edge_map[morphism->edge_map_index].variables = variables;
   morphism->edge_map_index++;
}

void addAssignment(Morphism *morphism, string variable, GPType type, GPList *value)
{
   assert(morphism->assignment_index < morphism->variables);
   morphism->assignment[morphism->assignment_index].variable = strdup(variable);
   morphism->assignment[morphism->assignment_index].type = type;
   morphism->assignment[morphism->assignment_index].value = value;
   morphism->assignment_index++;
}

void removeNodeMap(Morphism *morphism)
{
   morphism->node_map_index--;
   morphism->node_map[morphism->node_map_index].left_index = -1;
   morphism->node_map[morphism->node_map_index].host_index = -1;
   removeAssignments(morphism, morphism->node_map[morphism->node_map_index].variables);
   morphism->node_map[morphism->node_map_index].variables = 0;
}

void removeEdgeMap(Morphism *morphism)
{
   morphism->edge_map_index--;
   morphism->edge_map[morphism->edge_map_index].left_index = -1;
   morphism->edge_map[morphism->edge_map_index].host_index = -1;
   removeAssignments(morphism, morphism->edge_map[morphism->edge_map_index].variables);
   morphism->edge_map[morphism->edge_map_index].variables = 0;
}

void removeAssignments(Morphism *morphism, int number)
{
   int count;
   for(count = 0; count < number; count++)
   {
      morphism->assignment_index--;
      Assignment *assignment = &(morphism->assignment[morphism->assignment_index]);

      if(assignment->variable != NULL) free(assignment->variable);
      if(assignment->value != NULL) freeList(assignment->value);

      assignment->variable = NULL;
      assignment->value = NULL;
   }
}

int lookupNode(Morphism *morphism, int left_index)
{
   int count;
   for(count = 0; count < morphism->nodes; count++)
   {
      if(morphism->node_map[count].left_index == left_index) 
         return morphism->node_map[count].host_index;
   }
   return -1;
}

int lookupEdge(Morphism *morphism, int left_index)
{
   int count;
   for(count = 0; count < morphism->edges; count++)
   {
      if(morphism->edge_map[count].left_index == left_index) 
         return morphism->edge_map[count].host_index;
   }
   return -1;
}

Assignment *lookupVariable(Morphism *morphism, string variable)
{
   int count;
   for(count = 0; count < morphism->variables; count++)
   {
      Assignment assignment = morphism->assignment[count];
      if(assignment.variable == NULL) continue;
      if(!strcmp(assignment.variable, variable)) 
         return &(morphism->assignment[count]);
   }
   return NULL;
}

int addListAssignment(string name, GPList *list, Morphism *morphism) 
{
   /* Assign the minimum type to the assignment. For lists of length 1, this is
    * either INTEGER_CONSTANT or STRING_CONSTANT. Otherwise it is LIST_VAR. */
   GPType type = LIST_VAR;
   if(list != NULL && list->next == NULL)
   {
      assert(list->atom.type == INTEGER_CONSTANT || list->atom.type == STRING_CONSTANT);
      if(list->atom.type == INTEGER_CONSTANT) type = INTEGER_VAR;
      else type = STRING_VAR;
   }
   /* Search the morphism for an existing assignment to the passed variable. */
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      addAssignment(morphism, name, type, list);
      return 1;
   }
   /* Compare the list in the assignment to the list passed to the function. */
   else
   {
      GPList *assigned_list = assignment->value;
      while(list != NULL)
      {
         /* The passed list is longer than the list in the assignment. */
         if(assigned_list == NULL) return -1;
         Atom atom = list->atom;
         Atom assigned_atom = assigned_list->atom;
         switch(atom.type)
         {
            case INTEGER_CONSTANT: 
                 if(assigned_atom.type != INTEGER_CONSTANT ||
                    assigned_atom.number != atom.number) return -1;
                 break;
                     
            case STRING_CONSTANT:     
                 if(assigned_atom.type != STRING_CONSTANT ||
                    strcmp(assigned_atom.string, atom.string) != 0) return -1;
                 break;

            default:
                 print_to_log("Error (addListAssignment): Unexpected atom "
                              "type %d.\n", atom.type);
                 return -1;
         }
         assigned_list = assigned_list->next;
         list = list->next;
      }
   }
   /* If the function has not exited at this point, the passed list is equal to
    * the value of the list variable in the assignment. */
   return 0;
}

int addIntegerAssignment(string name, int value, Morphism *morphism)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      GPList *list = appendIntegerAtom(NULL, value);
      addAssignment(morphism, name, INTEGER_VAR, list);
      return 1;
   }
   else
   {
      Atom atom = assignment->value->atom;
      if(atom.type == INTEGER_CONSTANT && atom.number == value) return 0;
   }
   return -1;
}

int addStringAssignment(string name, string value, Morphism *morphism)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      GPList *list = appendStringAtom(NULL, value);
      addAssignment(morphism, name, STRING_VAR, list);
      return 1;
   }
   else
   {
      Atom atom = assignment->value->atom;
      if(atom.type == STRING_CONSTANT && !strcmp(atom.string, value)) return 0;
   }
   return -1;
}

int getIntegerValue(string name, Morphism *morphism)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      print_to_log("Error (getIntegerValue): Variable %s is not in the "
                   "morphism.\n", name);
      return 0;
   }
   return assignment->value->atom.number;
}

string getStringValue(string name, Morphism *morphism)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      print_to_log("Error (getStringValue): Variable %s is not in the "
                   "morphism.\n", name);
      return NULL;
   }
   return assignment->value->atom.string;
}

GPList *getListValue(string name, Morphism *morphism)
{
   Assignment *assignment = lookupVariable(morphism, name);
   if(assignment == NULL) 
   {
      print_to_log("Error (getListValue): Variable %s is not in the "
                   "morphism.\n", name);
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
            printList(morphism->assignment[count].value, stdout);
            printf("\n\n");
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
         if(assignment.value != NULL) freeList(assignment.value);
      }
      free(morphism->assignment);
   }
   free(morphism);
}

