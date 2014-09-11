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


bool labelMatch(Label rule_label, Label host_label, VariableList *variables,
                Assignment **assignment) 
{
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
            return verifyListVariable(assignment, list_variable, host_list);
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

      /* If the end of the host list has been reached, a match if possible
       * only if rule_atom is a list variable. */
      if(host_list == NULL)
      {
         if(rule_atom->type == VARIABLE) 
         {
            if(lookupType(variables, rule_atom->value.name) == LIST_VAR) 
            {  
               list_variable = rule_atom->value.name;
               ListElement atom = { .type = EMPTY };
               GList *list = g_list_prepend(NULL, &atom);
               return verifyListVariable(assignment, list_variable, list);
            }
         }
         /* If this point is reached then rule_atom is either not a list
          * variable (inner if statement) or not a variable (outer if
          * statement). Return false. */
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
      
      if(!compareAtoms(rule_atom, host_atom, variables, assignment)) 
         return false;

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
            return verifyListVariable(assignment, list_variable, list);
         }
      }
  
      /* Check if the current host element occurs earlier in the list than the 
       * marker. If so, then it is not possible for the lists to match because
       * the host list has too few elements. 
       */
      if(current_position < marker_position) return false;      
        
      ListElement *host_atom = (ListElement*)host_list->data;

      if(!compareAtoms(rule_atom, host_atom, variables, assignment)) 
         return false;

      rule_list = rule_list->prev;
      host_list = host_list->prev;
   }

   return true;
}
    

bool compareAtoms(ListElement *rule_atom, ListElement *host_atom, 
            VariableList *variables, Assignment **assignment) 
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
                  return(verifyAtomVariable(assignment, variable, host_atom));
               else return false;
               
               break;


            case CHARACTER_VAR:

               if(host_atom->type == CHARACTER_CONSTANT)
                  return(verifyAtomVariable(assignment, variable, host_atom));
               else return false;
               
               break;


            case STRING_VAR:

               if(host_atom->type == CHARACTER_CONSTANT || 
                  host_atom->type == STRING_CONSTANT)
                  return(verifyAtomVariable(assignment, variable, host_atom));
               else return false;
               
               break;


            case ATOM_VAR:

               if(host_atom->type == INTEGER_CONSTANT   ||
                  host_atom->type == CHARACTER_CONSTANT || 
                  host_atom->type == STRING_CONSTANT)
                    return verifyAtomVariable(assignment, variable, host_atom);
               else return false;
                       
               break;


            case LIST_VAR:
        
                print_to_log("Error: A list variable has appeared in the "
                             "compareAtoms function!\n");
                return false;
                break;
         }

      }
      break; 
 
      case INTEGER_CONSTANT:

           if(host_atom->type == INTEGER_CONSTANT)
              return(rule_atom->value.number == host_atom->value.number);
           else return false;
           
           break;
          

      case CHARACTER_CONSTANT:

           if(host_atom->type == CHARACTER_CONSTANT)
              return(strcmp(rule_atom->value.string, host_atom->value.string) == 0);
           else return false;
           
           break;


      case STRING_CONSTANT:

           if(host_atom->type == CHARACTER_CONSTANT ||
              host_atom->type == STRING_CONSTANT)
              return(strcmp(rule_atom->value.string, host_atom->value.string) == 0);
           else return false;
           
           break;

           
      case NEG:

           if(host_atom->type == INTEGER_CONSTANT)
              return(rule_atom->value.number == -(host_atom->value.number));
           else return false;
           
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
              /* Possibility (1) */
              if(rule_atom->value.bin_op.left_exp->type == VARIABLE) 
              {
                 string variable = rule_atom->value.bin_op.left_exp->value.name;
                 /* Semantic checking ensures that right_exp is a string or 
                  * character constant. Need to check that right_exp is a 
                  * suffix of host_string. */
                 string host_string = host_atom->value.string;
                 string rule_string = 
                    rule_atom->value.bin_op.right_exp->value.string;
                 
                 string result = isSuffix(rule_string, host_string);
                 
                 if(result != NULL)  
                 {  
                    ListElement *atom = NULL;
                    atom->type = STRING_CONSTANT;
                    atom->value.string = result;
                    bool is_valid = verifyAtomVariable(assignment, variable, atom);
                    free(result);
                    return is_valid;
                 }
                 else return false;
              }
              else 
              {
                 /* Check if left_exp is a prefix of the host string. */
                 string host_string = host_atom->value.string;
                 string rule_string = 
                    rule_atom->value.bin_op.left_exp->value.string;

                 string host_suffix = isPrefix(rule_string, host_string); 
                 
                 if(host_suffix != NULL)
                 {
                    /* Possibility (2) */
                    if(rule_atom->value.bin_op.right_exp->type == VARIABLE) 
                    {                 
                       string variable = rule_atom->value.bin_op.right_exp->value.name;
                       ListElement *atom = NULL;
                       atom->type = STRING_CONSTANT;
                       atom->value.string = host_suffix;
                       bool is_valid = verifyAtomVariable(assignment, variable, atom);
                       free(host_suffix);
                       return is_valid;
                    }
                    /* Possibility (3) */
                    else /* right_exp is a concat */
                    {
                       ListElement *str = rule_atom->value.bin_op.right_exp; 

                       assert(str->type == CONCAT);
                       assert(str->value.bin_op.left_exp->type == VARIABLE);
                       assert(str->value.bin_op.right_exp->type == CHARACTER_CONSTANT ||
                              str->value.bin_op.right_exp->type == STRING_CONSTANT);

                       /* Check str->right_exp is a suffix of host_remainder. */
                       rule_string = str->value.bin_op.right_exp->value.string;

                       string result = isSuffix(rule_string, host_suffix);
                       
                       if(result != NULL) 
                       {
                          string variable = rule_atom->value.bin_op.left_exp->value.name;
                          ListElement *atom = NULL;
                          atom->type = STRING_CONSTANT;
                          atom->value.string = result;
                          bool is_valid = verifyAtomVariable(assignment, variable, atom);
                          free(result);
                          return is_valid;
                       }
                       else return false;
                    }
                 }
                 else return false;
              }
           }
           else return false;
           
           break;
           
      default: print_to_log("Error: Unexpected list element type %d "
                            "encountered during label matching.\n", 
                            rule_atom->type);
           break;
   }
   return false;
}   


/* Check if test is a prefix of str. If so, return the rest of str after the
 * matched prefix. Otherwise return NULL.
 */

string isPrefix(const string test, const string str)
{
   int offset = strlen(str) - strlen(test);
   if(offset < 0) return NULL;
   /* strncmp compares test against the first strlen(test) characters
    * of str. */
   if(strncmp(str, test, strlen(test)) == 0) 
   {
      string rest = malloc(offset + 1);
      if(rest == NULL)
      {
         print_to_log("Error (isPrefix): Memory exhausted during string "
		      "construction.\n");
         exit(1);
      }
      /* Copy offset + 1 characters from str starting from the character after
       * the one at strlen(test). The +1 is for the terminating NULL character.
       */
      rest = strcpy(rest, str + strlen(test));
      return rest;
   }
   else return NULL;
}

/* Check if test is a suffix of str. If so, return the rest of str before the
 * matched suffix. Otherwise return NULL.
 */

string isSuffix(const string test, const string str)
{
   int offset = strlen(str) - strlen(test);
   if(offset < 0) return NULL;
   /* compare the last strlen(test) characters of str with test. */
   if(strcmp(str + offset, test) == 0) 
   {
      /* Make a string containing the first offset characters of str. */
      string rest = malloc(offset + 1);
      if(rest == NULL)
      {
         print_to_log("Error (isSuffix): Memory exhausted during string "
		      "construction.\n");
         exit(1);
      }
      strncpy(rest, str, offset);
      rest[offset] = '\0';
      return rest;
   }
   else return NULL;
}
  

bool verifyListVariable(Assignment **assignment, string name, GList *list) 
{
   GList *assigned_list = lookupValue(*assignment, name);
   
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
         ListElement *atom_copy = malloc(sizeof(ListElement));

         if(atom_copy == NULL)
         {
            print_to_log("Error (verifyListVariable): Memory exhausted during "
	                 "creation of a list element.\n");
            exit(1);
         }

         atom_copy = memcpy(atom_copy, atom, sizeof(ListElement));

         list_copy = g_list_prepend(list_copy, atom_copy);
     
         list_to_copy = list_to_copy->prev;
      }

      *assignment = addAssignment(*assignment, name, list_copy);
      return true;
   }

   /* Otherwise, a list is already assigned to this variable. We need to check
    * if the value passed to this function is equal to the existing one. This 
    * is done by stepping through both value and assigned_list (GLists) and 
    * comparing their elements with compareConstants.
    * Return false if an inconsistency is found.
    */
   while(assigned_list != NULL)
   {
      ListElement *assigned_atom = (ListElement*)assigned_list->data;
      
      /* Check if list is shorter than assigned_list. */
      if(list == NULL) return false;

      ListElement *atom = (ListElement*)list->data;

      if(!compareConstants(assigned_atom, atom)) return false;

      assigned_list = assigned_list->next;
      list = list->next;
   }

   /* At this point the while loop has exited without returning false.
    * One final check needs to be made: have we reached the end of the list?
    * If not, then list contains more elements than assigned_list, hence they
    * are not equal. If so, the lists match but list already exists in the assignment
    * and it needs to be freed.
    */
   return list == NULL;
}	 


bool verifyAtomVariable(Assignment **assignment, string name, ListElement *atom)
{
   GList *assigned_value = lookupValue(*assignment, name);

   /* If the variable does not have a value in the current assignment,
    * there is no verification to be performed. */
   if(assigned_value == NULL) 
   {
      /* Make a copy of atom in heap, lift it to a GList and add it to the 
       * assignment. 
       */
      ListElement *atom_copy = malloc(sizeof(ListElement));
      if(atom_copy == NULL)
      {
         print_to_log("Error (verifyAtomVariable): Memory exhausted during "
                      "creation of a list element.\n");
         exit(1);
      }

      atom_copy = memcpy(atom_copy, atom, sizeof(ListElement));

      assigned_value = g_list_prepend(assigned_value, atom_copy);

      *assignment = addAssignment(*assignment, name, assigned_value);
      return true;
   }

   /* Check if assigned_value has more than one element. It shouldn't, as this
    * function is called when considering the assignment of a variable with
    * atomic type. */
   if(assigned_value->next != NULL)
   {
      print_to_log("Error (verifyAtomVariable): non-list variable %s assigned "
		   "a non-atomic value.\n", name);
      return false;
   }

   ListElement *assigned_atom = (ListElement *)assigned_value->data;
   return(compareConstants(assigned_atom, atom));
}


bool compareConstants(ListElement *atom, ListElement *test_atom)
{

   switch (test_atom->type)
   {
      case EMPTY:
           
           if(atom->type != EMPTY) return false;

           break;
           
     
      case INTEGER_CONSTANT: 
                  
           if(atom->type != INTEGER_CONSTANT || 
              test_atom->value.number != atom->value.number)
                return false;
               
           break;
                  
             
      case CHARACTER_CONSTANT:      	 
           
           if(atom->type != CHARACTER_CONSTANT ||
              strcmp(test_atom->value.string, atom->value.string) != 0)
                 return false;
                  
           break;
             

      case STRING_CONSTANT:     
          
           if((atom->type != CHARACTER_CONSTANT &&
               atom->type != STRING_CONSTANT) ||
              strcmp(test_atom->value.string, atom->value.string) != 0)
                 return false;
               
           break;
             
               
      default: print_to_log("Error: Unexpected list element type %d "
                             "encountered during variable assignment.\n",
	     	                test_atom->type);
           return false;
   }
   return true;
}
