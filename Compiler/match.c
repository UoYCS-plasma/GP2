/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.c - Chris Bak (14/08/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "match.h"

/* GraphMapping functions. */

GraphMapping *addMapping(GraphMapping *mapping, int rule_item, int host_item, 
                         bool flag) 
{
   GraphMapping *new_mapping = malloc(sizeof(GraphMapping));

   if(new_mapping == NULL) 
   {
      print_to_log("Memory exhausted during mapping construction.\n");
      exit(1);
   }

   new_mapping->rule_item = rule_item; 
   new_mapping->host_item = host_item;
   new_mapping->flag = flag;
   new_mapping->next = mapping;

   return new_mapping;
}


GraphMapping *removeMapping(GraphMapping *mapping)
{
   GraphMapping *new_mapping = mapping->next;
   free(mapping);
   return new_mapping;
}


int lookupFromRule(GraphMapping *mapping, int rule_item) 
{
   while(mapping != NULL) 
   {
     if(mapping->rule_item == rule_item) return mapping->host_item;
     mapping = mapping->next;
   }
   return -1;
         
}


int lookupFromHost(GraphMapping *mapping, int host_item) 
{
   while(mapping != NULL) 
   {
     if(mapping->host_item == host_item) return mapping->rule_item;
     mapping = mapping->next;
   }
   return -1;
}


void freeMapping(GraphMapping *mapping) 
{
   if(mapping == NULL) return;
   if(mapping->next) freeMapping(mapping->next);
   if(mapping) free(mapping);
}
   

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

void printMorphism(Morphism *morphism)
{
   printf("Morphism\n=======\n");

   GraphMapping *node_matches = morphism->node_matches;

   while(node_matches != NULL)
   {
      printf("Left node %d -> Host node %d\n",
             node_matches->rule_item, node_matches->host_item);
      node_matches = node_matches->next;
   }
  
   printf("\n");

   GraphMapping *edge_matches = morphism->edge_matches;

   while(edge_matches != NULL)
   {
      printf("Left edge %d -> Host edge %d\n",
             edge_matches->rule_item, edge_matches->host_item);
      edge_matches = edge_matches->next;
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
}

void freeMorphism(Morphism *morphism)
{
   if(morphism->node_matches) freeMapping(morphism->node_matches);
   if(morphism->edge_matches) freeMapping(morphism->edge_matches);
   if(morphism->assignment) freeAssignment(morphism->assignment);
   free(morphism);
}


ListElement *copyListElement(ListElement *atom)
{ 
   ListElement *atom_copy = malloc(sizeof(ListElement));

   if(atom_copy == NULL)
   {
      print_to_log("Error (copyListElement): Memory exhausted during "
                   "creation of a list element.\n");
      exit(1);
   }

   /* Duplicate any string values and recursively copy any sub-expressions. */
   atom_copy->type = atom->type;

   switch(atom->type)
   {
      case VARIABLE:

           atom_copy->value.name = strdup(atom->value.name);

           break;
      
      case INTEGER_CONSTANT:

           atom_copy->value.number = atom->value.number;

           break;

      case CHARACTER_CONSTANT:

      case STRING_CONSTANT:

           atom_copy->value.string = strdup(atom->value.string);

           break;

      case NEG:
 
           atom_copy->value.exp = copyListElement(atom->value.exp);

           break;

      case CONCAT:

           atom_copy->value.bin_op.left_exp = 
              copyListElement(atom->value.bin_op.left_exp);
           atom_copy->value.bin_op.right_exp = 
              copyListElement(atom->value.bin_op.right_exp);

           break;

      default:
             print_to_log("Error (copyListElement): Atom type %d should not "
                          "occur here.\n", atom->type);
             return NULL;
   }

   return atom_copy;
}


Assignment *assignment = NULL;

bool labelMatch(Label rule_label, Label host_label, VariableList *variables)
{
   /* Keeps track of the number of assignments made in the scope of this
    * function so that they can be removed should the lists fail to match.
    */
   int new_assignments = 0;

   /* Error checking. The lists should never be NULL. */
   if(rule_label.list == NULL)
   {
      print_to_log("Error: labelMatch has been passed a rule label with a NULL "
                   "list.\n");
      return false;
   }

   if(host_label.list == NULL)
   {
      print_to_log("Error: labelMatch has been passed a host label with a NULL "
                   "list.\n");
      return false;
   }

   /* First check if the marks line up. */
   if(rule_label.mark != CYAN && rule_label.mark != host_label.mark) return false;

   GList *rule_list = g_list_first(rule_label.list);
   GList *host_list = g_list_first(host_label.list);

   string list_variable = NULL;

   ListElement *rule_atom = (ListElement*)rule_list->data;
   ListElement *host_atom = (ListElement*)host_list->data;

   /* Handle the cases involving empty lists. */
   if(rule_atom->type == EMPTY && host_atom->type == EMPTY) return true;

   /* If the host list is empty, a match is only possible if the current
    * rule atom is a list variable, which can be assigned the empty list.
    * If the rule atom is not a list variable, then return false. */
   if(host_atom->type == EMPTY) 
   {         
      if(rule_atom->type == VARIABLE) 
      {
         if(lookupType(variables, rule_atom->value.name) == LIST_VAR) 
         {  
            list_variable = rule_atom->value.name;
            GList *host_list_copy = g_list_copy(host_list);
            return verifyListVariable(list_variable, host_list_copy);
         }
      }
      /* If this point is reached then rule_atom is either not a list
       * variable (inner if statement) or not a variable (outer if
       * statement). Return false. */
      return false;
   }
   else if(rule_atom->type == EMPTY) return false;

   /* The loop compares elements from the starts of the lists until a list
    * variable is found in the rule list, in which case the elements are
    * compared from the ends of the lists; or a failure condition is met.
    */
   while(rule_list != NULL) 
   {
      rule_atom = (ListElement*)rule_list->data;

      /* If the end of the host list has been reached, a match is possible
       * only if rule_list has one element left and that element is a list 
       * variable. */
      if(host_list == NULL)
      {
         if(rule_atom->type == VARIABLE && rule_list->next == NULL) 
         {
            if(lookupType(variables, rule_atom->value.name) == LIST_VAR) 
            {  
               list_variable = rule_atom->value.name;
               ListElement atom = { .type = EMPTY };
               GList *list = g_list_prepend(NULL, &atom);
               if(verifyListVariable(list_variable, list)) 
                  return true;
            }
         }
         /* If this point is reached then the label match failed: Either the
          * call to verifyListVariable returned false, the current rule atom 
          * is not a list variable, the current rule atom is not a variable,
          * or there are elements in the rule list after the current one.
          * Remove any assignments made in this function and return false. */
         int counter;
         for(counter = 0; counter < new_assignments; counter++)
             assignment = removeAssignment(assignment);
         return false;
      }

      host_atom = (ListElement*)host_list->data;

      /* Break if a list variable is encountered. */
      if(rule_atom->type == VARIABLE) 
      {
         if(lookupType(variables, rule_atom->value.name) == LIST_VAR) 
         {   
            list_variable = rule_atom->value.name;
            break;
         }
      }
      
      int result = compareAtoms(rule_atom, host_atom, variables);

      if(result >= 0) new_assignments += result;
      else 
      {
         int counter;
         for(counter = 0; counter < new_assignments; counter++)
             assignment = removeAssignment(assignment);
         return false;
      }

      rule_list = rule_list->next;
      host_list = host_list->next;
   }

   /* The while loop breaks if the rule list contains a list variable, but this
    * is not guaranteed. Hence we must explicitly check if the end of the rule
    * list has been reached. Then the return value depends on whether the end
    * of the host list has also been reached. 
    * If so, then the lists have equal length. Return true.
    * If not, then some host list elements have not been matched, but there are
    * no rule elements left for them to be matched against. Return false.
    */
   if(rule_list == NULL) return host_list == NULL;

   /* If this point is reached then a list variable has been encountered in
    * the rule list. We need to record the current position in the host list. 
    * This will be the first element of the value assigned to the list 
    * variable. It is also used for length checking in the subsequent loop.
    */
   GList *marker = host_list;
   int marker_position = g_list_position(host_label.list, marker);

   /* The elements are now compared from the ends of both lists. */
   rule_list = g_list_last(rule_label.list);
   host_list = g_list_last(host_label.list);
   
   while(rule_list != NULL) 
   {
      ListElement *rule_atom = (ListElement*)rule_list->data;
      int current_position = g_list_position(host_label.list, host_list);

      /* When the list variable is encountered, assign it the unchecked
       * segment of the host list. 
       */
      if(rule_atom->type == VARIABLE) 
      {
         if(lookupType(variables, rule_atom->value.name) == LIST_VAR) 
         {
            /* Create a new list and assign it to list_variable. */
            GList *list = NULL;
            int counter;

            /* If the marker position has been passed, assign the empty list
             * to list, otherwise copy the list from current_position to
             * marker_position. */
            if(current_position < marker_position)
            {
               ListElement atom = { .type = EMPTY };
               list = g_list_prepend(list, &atom);
            }
            else
            {
               /* Iterate backwards through the host list from its last element
                * to the marker. This allows prepending items to the list at each
                * step which is cheaper than appending.
                */
               for(counter = current_position; counter >= marker_position; counter--)
               {
                  ListElement *atom = g_list_nth_data(host_label.list, counter);
                  list = g_list_prepend(list, atom);
               }
            } 
            if(verifyListVariable(list_variable, list)) return true;
            else
            {
               /* Remove the assignments made in the duration of this function. */
               for(counter = 0; counter < new_assignments; counter++)
                   assignment = removeAssignment(assignment);
               return false;
            }
         }
      }
  
      /* Check if the current host element occurs earlier in the list than the 
       * marker. If so, then it is not possible for the lists to match because
       * the host list has too few elements. 
       */
      if(current_position < marker_position) return false;      
        
      ListElement *host_atom = (ListElement*)host_list->data;

      int result = compareAtoms(rule_atom, host_atom, variables);

      if(result >= 0) new_assignments += result;
      else 
      {
         int counter;
         for(counter = 0; counter < new_assignments; counter++)
             assignment = removeAssignment(assignment);
         return false;
      }

      rule_list = rule_list->prev;
      host_list = host_list->prev;
   }

   return true;
}
    

int compareAtoms(ListElement *rule_atom, ListElement *host_atom, 
                 VariableList *variables) 
{
   switch(rule_atom->type) 
   {
      case VARIABLE:
      {
         string variable = rule_atom->value.name;

         switch(lookupType(variables, variable)) 
         {
            case INTEGER_VAR:
            
               if(host_atom->type == INTEGER_CONSTANT)
                  return(verifyAtomVariable(variable, host_atom));
               else return -1;
               
               break;


            case CHARACTER_VAR:

               if(host_atom->type == CHARACTER_CONSTANT)
                  return(verifyAtomVariable(variable, host_atom));
               else return -1;
               
               break;


            case STRING_VAR:

               if(host_atom->type == CHARACTER_CONSTANT || 
                  host_atom->type == STRING_CONSTANT)
                  return(verifyAtomVariable(variable, host_atom));
               else return -1;
               
               break;


            case ATOM_VAR:

               if(host_atom->type == INTEGER_CONSTANT   ||
                  host_atom->type == CHARACTER_CONSTANT || 
                  host_atom->type == STRING_CONSTANT)
                  return(verifyAtomVariable(variable, host_atom));
               else return -1;
                       
               break;


            case LIST_VAR:
        
                print_to_log("Error: A list variable has appeared in the "
                             "compareAtoms function!\n");
                return -1;
                break;
         }

      }
      break; 
 
      case INTEGER_CONSTANT:
   
           /* if if return -1 (no elses) may be more concise */
           if(host_atom->type == INTEGER_CONSTANT)
              if(rule_atom->value.number == host_atom->value.number) return 0; 
              else return -1;
           else return -1;
           
           break;
          

      case CHARACTER_CONSTANT:

           if(host_atom->type == CHARACTER_CONSTANT)
              if(strcmp(rule_atom->value.string, host_atom->value.string) == 0)
                 return 0;
              else return -1;
           else return -1;
           
           break;


      case STRING_CONSTANT:

           if(host_atom->type == CHARACTER_CONSTANT ||
              host_atom->type == STRING_CONSTANT)
              if(strcmp(rule_atom->value.string, host_atom->value.string) == 0)
                 return 0;
              else return -1;
           else return -1;
           
           break;

           
      case NEG:

           if(host_atom->type == INTEGER_CONSTANT)
              if(rule_atom->value.number == -(host_atom->value.number)) 
                 return 0;
              else return -1;
           else return -1;
           
           break;
           

      /* I assume that expressions involving empty strings and the concat of
       * two constant strings are simplified in the translation phase.
       * I also assume a right-associative representation.
       * Hence the possibilities are:
       * (1) concat(stringVar, stringConst)
       * (2) concat(stringConst, stringVar)
       * (3) concat(stringConst, concat(stringVar, stringConst))
       */
      case CONCAT:
      
           if(host_atom->type == CHARACTER_CONSTANT ||
              host_atom->type == STRING_CONSTANT)
           {
              GList *string_exp = concatExpToList(rule_atom);
              int result = verifyStringExp(string_exp, host_atom, variables);
              g_list_free_full(string_exp, freeStringExp);   
              return result;
           }
           break;
           
      default: print_to_log("Error: Unexpected list element type %d "
                            "encountered during label matching.\n", 
                            rule_atom->type);
               break;
   }
   return -1;
}  

/* This will be moved to the AST translation phase. Any strings from string_exp
 * are strdup'd so that string_exp can be freed without accidentally freeing
 * the strings to retain for the GList. */


void freeStringExp(gpointer data)
{
   StringExp *exp = (StringExp*)data;
   if(exp->value) free(exp->value); 
   if(exp) free(exp);
}

GList *concatExpToList(ListElement *string_exp)
{
   switch(string_exp->type)
   {
      case CONCAT:
      {
           GList *first_exp = concatExpToList(string_exp->value.bin_op.left_exp);
           GList *second_exp = concatExpToList(string_exp->value.bin_op.right_exp);
           return g_list_concat(first_exp, second_exp);
           break;
      }

      case CHARACTER_CONSTANT:
 
      case STRING_CONSTANT:
      {
           StringExp *exp = malloc(sizeof(StringExp));
           if(exp == NULL)
           {
              printf("Error: Memory exhausted during StringExp construction.\n");
              exit(1);
           }
           exp->type = CONSTANT_S;
           exp->value = strdup(string_exp->value.string); 
           return g_list_prepend(NULL, exp); 
           break;           
      }

      case VARIABLE:
      {
           StringExp *exp = malloc(sizeof(StringExp));
           if(exp == NULL)
           {
              printf("Error: Memory exhausted during StringExp construction.\n");
              exit(1);
           }
           exp->type = VARIABLE_S;
           exp->value = strdup(string_exp->value.name); 
           return g_list_prepend(NULL, exp);
           break;
      }

      default: print_to_log("Error (concatExpToList): Unexpected type: %d\n",
                            (int)string_exp->value.bin_op.left_exp->type);
               return NULL;
   }
   return NULL;
}

/* host_atom is of type CHARACTER_CONSTANT or STRING_CONSTANT */
int verifyStringExp(GList *string_exp, ListElement *host_atom,
                    VariableList *variables)
{
   int new_assignments = 0;
   int return_value = 0;
   
   string host_value = strdup(host_atom->value.string);
   string host_string = host_value;

   /* Go through string_exp until the string variable or the end of the list. */
   while(string_exp != NULL)
   {
      StringExp *exp = (StringExp*)string_exp->data;

      if(host_string[0] == '\0')
      {
         if(exp->type == VARIABLE_S)
         {
            if(lookupType(variables, exp->value) == STRING_VAR &&
               string_exp->next == NULL)
            {
               ListElement atom = { .type = STRING_CONSTANT,
                                    .value.string = "" };
               int result = verifyAtomVariable(exp->value, &atom);
               if(result >= 0)
               {
                  new_assignments += result;
                  return_value = new_assignments;
                  break;
               }
            }
         }
         return_value = -1;
         break;
      }

      if(exp->type == CONSTANT_S) 
      {
         int prefix_length = isPrefix(exp->value, host_string);
         if(prefix_length == -1) 
         {
            return_value = -1;
            break;
         }
         else host_string = host_string + prefix_length;
      }

      else /* exp_type == VARIABLE_S */
      {
         if(lookupType(variables, exp->value) == CHARACTER_VAR)
         { 
            char first_character[2] = {host_string[0], '\0'};

            ListElement atom = { .type = CHARACTER_CONSTANT,
                                 .value.string = first_character };
            int result = verifyAtomVariable(exp->value, &atom); 
            if(result >= 0)
            {
               new_assignments += result;
               return_value = new_assignments;
               host_string = host_string + 1;
            }
            else       
            { 
               return_value = -1;
               break;
            }
         }

         if(lookupType(variables, exp->value) == STRING_VAR) break;
      }
      string_exp = string_exp->next;
   }

   if(string_exp == NULL) 
   {
      if(host_string[0] == '\0') return_value = new_assignments;
      else 
      {
         int counter;
         for(counter = 0; counter < new_assignments; counter++)
             assignment = removeAssignment(assignment);
         return_value = -1;
      }
      free(host_value);
      return return_value;
   }

   string_exp = g_list_last(string_exp);
    
   while(string_exp != NULL)
   {
      StringExp *exp = (StringExp*)string_exp->data;
      
      if(exp->type == CONSTANT_S) 
      {
         int suffix_length = isSuffix(exp->value, host_string);
         if(suffix_length == -1)
         {
            return_value = -1;
            break;
         }
         else host_string[suffix_length] = '\0';
      }

      else /* exp_type == VARIABLE_S */
      {
         if(lookupType(variables, exp->value) == CHARACTER_VAR)
         { 
            int host_length = strlen(host_string);
            char last_character[2] = {host_string[host_length - 1], '\0'};

            ListElement atom = { .type = CHARACTER_CONSTANT,
                                 .value.string = last_character };
            int result = verifyAtomVariable(exp->value, &atom); 
            if(result >= 0) 
            {
               new_assignments += result;
               host_string[host_length - 1] = '\0';
               return_value = new_assignments;
            }
            else 
            {
               return_value = -1;
               break;
            }
         }

         if(lookupType(variables, exp->value) == STRING_VAR) 
         {
            ListElement atom = { .type = STRING_CONSTANT,
                                 .value.string = host_string };
            int result = verifyAtomVariable(exp->value, &atom);
            if(result >= 0) 
            {
               new_assignments += result;
               return_value = new_assignments;
               break;
            }
            else 
            {
               return_value = -1;
               break;
            }  
         }
       }

       string_exp = string_exp->prev;
    }
    free(host_value);
    if(return_value == -1)
    {
       int counter;
       for(counter = 0; counter < new_assignments; counter++)
           assignment = removeAssignment(assignment);
    }  
    return return_value;
}
            

/* Check if test is a prefix of str. If so, return the rest of str after the
 * matched prefix. Otherwise return NULL.
 */

int isPrefix(const string test, const string str)
{
   int offset = strlen(str) - strlen(test);
   if(offset < 0) return -1;
   /* strncmp compares test against the first strlen(test) characters
    * of str. */
   if(strncmp(str, test, strlen(test)) == 0) return strlen(test);
   else return -1;
}

/* Check if test is a suffix of str. If so, return the rest of str before the
 * matched suffix. Otherwise return NULL.
 */

int isSuffix(const string test, const string str)
{
   int offset = strlen(str) - strlen(test);
   if(offset < 0) return -1;
   /* compare the last strlen(test) characters of str with test. */
   if(strcmp(str + offset, test) == 0) return offset;
   else return -1;
}
 


bool verifyListVariable(string name, GList *list) 
{
   GList *assigned_list = lookupValue(assignment, name);
   /* If the variable does not have a value in the current assignment,
    * there is no verification to be performed. */
   if(assigned_list == NULL) 
   {
      /* Make a copy of list in heap and add it to the assignment. */
      GList *list_copy = NULL;
      /* list is copied in reverse order so that elements are prepended at each
       * step. */
      GList *list_to_copy = g_list_last(list);
 
      while(list_to_copy != NULL)
      {
         ListElement *atom = (ListElement*)list_to_copy->data;
         ListElement *atom_copy = copyListElement(atom);
         list_copy = g_list_prepend(list_copy, atom_copy);
         list_to_copy = list_to_copy->prev;
      }
      string name_copy = strdup(name);
      assignment = addAssignment(assignment, name_copy, list_copy);
      g_list_free(list);
      return true;
   }

   /* Otherwise, a list is already assigned to this variable. We need to check
    * if the value passed to this function is equal to the existing one. This 
    * is done by stepping through both list and assigned_list (GLists) and 
    * comparing their elements with compareConstants.
    * Return false if an inconsistency is found.
    */
   GList *iterator = list;
  
   while(assigned_list != NULL)
   {
      ListElement *assigned_atom = (ListElement*)assigned_list->data;
      
      /* Check if list is shorter than assigned_list. */
      if(iterator == NULL) 
      {
         g_list_free(list);
         return false;
      }

      ListElement *atom = (ListElement*)iterator->data;

      if(compareConstants(assigned_atom, atom) == -1)
      {
         g_list_free(list);
         return false;
      }

      assigned_list = assigned_list->next;
      iterator = iterator->next;
   }

   /* At this point the while loop has exited without returning false, so all  
    * elements in assigned_list match up with their counterparts in list. 
    * If list has more elements, then the lists are not of equal length and
    * are not compatible. */
    if(iterator == NULL)
    {
       g_list_free(list);
       return true;
    }
    else 
    {
       g_list_free(list); 
       return false;
    }
}	 


int verifyAtomVariable(string name, ListElement *atom)
{
   GList *assigned_value = lookupValue(assignment, name);

   /* If the variable does not have a value in the current assignment,
    * there is no verification to be performed. */
   if(assigned_value == NULL) 
   {
      /* Make a copy of atom in heap, lift it to a GList and add it to the 
       * assignment. 
       */
      ListElement *atom_copy = copyListElement(atom); 
      assigned_value = g_list_prepend(assigned_value, atom_copy);
      string name_copy = strdup(name);
      assignment = addAssignment(assignment, name_copy, assigned_value);
      return 1;
   }

   /* Check if assigned_value has more than one element. It shouldn't, as this
    * function is called when considering the assignment of a variable with
    * atomic type. */
   if(assigned_value->next != NULL)
   {
      print_to_log("Error (verifyAtomVariable): non-list variable %s assigned "
		   "a non-atomic value.\n", name);
      return -1;
   }

   ListElement *assigned_atom = (ListElement *)assigned_value->data;
   return(compareConstants(assigned_atom, atom)); 
}


int compareConstants(ListElement *atom, ListElement *test_atom)
{

   switch (test_atom->type)
   {
      case EMPTY:
           
           if(atom->type != EMPTY) return -1;

           break;
           
     
      case INTEGER_CONSTANT: 
                  
           if(atom->type != INTEGER_CONSTANT || 
              test_atom->value.number != atom->value.number)
                return -1;
               
           break;
                  
             
      case CHARACTER_CONSTANT:      	 
           
           if(atom->type != CHARACTER_CONSTANT ||
              strcmp(test_atom->value.string, atom->value.string) != 0)
                 return -1;
                  
           break;
             

      case STRING_CONSTANT:     
          
           if((atom->type != CHARACTER_CONSTANT &&
               atom->type != STRING_CONSTANT) ||
              strcmp(test_atom->value.string, atom->value.string) != 0)
                 return -1;
               
           break;
             
               
      default: print_to_log("Error: Unexpected list element type %d "
                             "encountered during variable assignment.\n",
	     	                test_atom->type);
           return -1;
   }
   return 0;
}
