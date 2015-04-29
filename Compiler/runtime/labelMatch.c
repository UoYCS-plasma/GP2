#include "labelMatch.h"

int labelMatch(Label rule_label, Label host_label, Morphism *morphism)
{
   /* First check if the marks line up. */
   if(rule_label.mark != ANY && rule_label.mark != host_label.mark) return -1;

   /* Check all cases where either the rule list or the host list is empty. */
   if(rule_label.length == 0) 
   {
      /* Both lists empty is a valid match. An empty rule list never matches a
       * non-empty host list. */
      if(host_label.length == 0) return 0; else return -1;
   } 
   else
   {
      if(host_label.length == 0)
      /* The rule label is non-empty and the host label is empty. A match 
       * exists only if the rule list is a single list variable. */
      {
         if(rule_label.length != 1) return -1;
         Atom atom = rule_label.list[0];
         if(atom.type != VARIABLE) return -1;
         else
         {
            if(atom.variable.type == LIST_VAR)
               /* NULL is the empty list. */
               return verifyListVariable(atom.variable.name, NULL, 0, morphism);
            else return -1;
         }
      }
   }
   /* new_assignments keeps track of the number of assignments made in the
    * scope of this function so that they can be removed should the lists 
    * fail to match without damaging any previous assignments. */
   int new_assignments = 0, index;
   bool match = true;

   /* The loop compares elements from the starts of the lists until a list
    * variable is found in the rule list, in which case the elements are
    * compared from the ends of the lists, or a failure condition is met. */
   for(index = 0; index < rule_label.length; index++)
   {
      Atom rule_atom = rule_label.list[index];
      /* If the end of the host list has been reached, a match is possible
       * only if rule_list has one element left and that element is a list 
       * variable. */
      if(index == host_label.length)
      {
         if(rule_atom.type == VARIABLE && rule_atom.variable.type == LIST_VAR &&
            index == rule_label.length - 1) 
         {  
            int result = verifyListVariable(rule_atom.variable.name, NULL, 0, morphism);
            /* There are no more host atoms to match, so return. */
            if(result >= 0) return new_assignments + result;
         }
         /* If this point is reached then the label match failed: Either the
          * call to verifyListVariable returned false, the current rule atom 
          * is not a list variable, or there are more than one rule atoms 
          * remaining. */
         else
         {
            match = false;
            break;
         }
      }
      /* Break if a list variable is encountered. */
      if(rule_atom.type == VARIABLE && rule_atom.variable.type == LIST_VAR) break;

      Atom host_atom = host_label.list[index];
      int result = compareAtoms(rule_atom, host_atom, morphism);
      if(result >= 0) new_assignments += result;
      else match = false;
   }
   if(!match)
   {
      removeAssignments(morphism, new_assignments);
      return -1;
   }
   /* The loop exits in two cases:
    * (1) The end of the rule list is reached. Check if the end of the host
    *     list has also been reached. If so, all is good. If not, there are
    *     some host atoms to match, but no rule atoms to match them!
    * (2) The rule list contains a list variable. Compare atoms from the
    *     ends of both lists until the list variable is reached. */
   if(index == rule_label.length) 
   {
      if(index == host_label.length) return new_assignments;
      else 
      {
         removeAssignments(morphism, new_assignments);
         return -1;
      }
   }
   /* Record the current position in the host list. 
    * This is the start of the list assigned to the list variable.
    * It is also used for length checking in the subsequent loop. */
   int marker = index;
   int rule_index, host_index;

   /* The elements are now compared from the ends of both lists. The lengths
    * of the lists may not be the same, so the following loop uses two 
    * index variables. */
   for(rule_index = rule_label.length - 1, host_index = host_label.length - 1;
       rule_index >= 0; rule_index--, host_index--)
   {
      Atom rule_atom = rule_label.list[rule_index];
      /* If host_index < marker - 1, then it is not possible for the lists to 
       * match because the host list has too few elements. 
       * host_index == marker - 1 is valid only if the current rule atom is
       * the list variable, which can be assigned the empty list. */
      if(host_index < marker - 1) 
      {
         match = false;  
         break;
      }
      /* When the list variable is encountered, assign it the unchecked
       * segment of the host list. */
      if(rule_atom.type == VARIABLE && rule_atom.variable.type == LIST_VAR) 
      {
         int result = 0;
         /* If the host index is one position before the marker, the list variable
          * is assigned the empty list. Otherwise, it is assigned the sublist
          * of the host list starting at marker and ending at the current index. */
         if(host_index == marker - 1)
            result = verifyListVariable(rule_atom.variable.name, NULL, 0, morphism);
         else
         {
            /* host_index > marker - 1. */             
            Atom list[host_index - marker + 1];
            int counter, list_index = 0;
            for(counter = marker; counter <= host_index; counter++)
            {
               list[list_index].type = host_label.list[counter].type;
               if(host_label.list[counter].type == INTEGER_CONSTANT)
                  list[list_index].number = host_label.list[counter].number;
               if(host_label.list[counter].type == STRING_CONSTANT)
                  list[list_index].string = host_label.list[counter].string;
               list_index++;
            }
            result = verifyListVariable(rule_atom.variable.name, list, 
                                        host_index - marker + 1, morphism);
         }
         if(result >= 0) new_assignments += result;
         else match = false;
         break;
      }
      /* The current rule atom is not a list variable. This means the current 
       * host atom must not have already been matched. That is, the index of
       * the current host atom is not smaller than marker. If this holds, 
       * call compareAtoms and act according to its output. */
      else
      {
         if(host_index < marker) 
         {
            match = false;
            break;
         }
         Atom host_atom = host_label.list[host_index];
         int result = compareAtoms(rule_atom, host_atom, morphism);
         if(result >= 0) new_assignments += result;
         else match = false;
      }
   }
   if(!match) 
   {
      removeAssignments(morphism, new_assignments);
      return -1;
   }
   else return new_assignments;
}
    
int compareAtoms(Atom rule_atom, Atom host_atom, Morphism *morphism) 
{
   switch(rule_atom.type) 
   {
      case VARIABLE:
      {
         string name = rule_atom.variable.name;
         switch(rule_atom.variable.type) 
         {
            case INTEGER_VAR:
                 if(host_atom.type == INTEGER_CONSTANT)
                    return verifyAtomVariable(name, host_atom, morphism);
                 else return -1;

            case CHARACTER_VAR:
                 if(host_atom.type == STRING_CONSTANT)
                 {
                    if(strlen(host_atom.string) == 1)
                    {
                       return verifyAtomVariable(name, host_atom, morphism);
                    }
                    else return -1;
                 }
                 else return -1;

            case STRING_VAR:
                 if(host_atom.type == STRING_CONSTANT)
                    return verifyAtomVariable(name, host_atom, morphism);
                 else return -1;

            case ATOM_VAR:
                 if(host_atom.type == INTEGER_CONSTANT ||
                    host_atom.type == STRING_CONSTANT)
                    return verifyAtomVariable(name, host_atom, morphism);
                 else return -1;

            case LIST_VAR:
                 print_to_log("Error (compareAtoms): List variable %s "
                              "encountered!\n", name);
                 return -1;
         }
         break;
      }
      case INTEGER_CONSTANT:
           if(host_atom.type == INTEGER_CONSTANT)
              if(rule_atom.number == host_atom.number) return 0; 
           return -1;
           break;
          
      case STRING_CONSTANT:
           if(host_atom.type == STRING_CONSTANT)
              if(!strcmp(rule_atom.string, host_atom.string)) return 0;
           return -1;
           break;

      case NEG:
           if(host_atom.type == INTEGER_CONSTANT)
              if(rule_atom.number == -(host_atom.number)) return 0;
           return -1;
           break;

      case CONCAT:
           if(host_atom.type == STRING_CONSTANT)
           {
              if(!strcmp(host_atom.string, "")) return -1;
              StringList *list = NULL;
              list = stringExpToList(list, rule_atom.bin_op.left_exp);
              list = stringExpToList(list, rule_atom.bin_op.right_exp);
              int result = verifyStringExp(list, host_atom.string, morphism);
              freeStringList(list);
              return result;
           }
           else return -1;
           break;
           
      default: print_to_log("Error: Unexpected list element type %d encountered "
                            "during label matching.\n", rule_atom.type);
               break;
   }
   return -1;
} 

StringList *stringExpToList(StringList *list, Atom *string_exp)
{
   switch(string_exp->type)
   {
      case STRING_CONSTANT:
           list = appendStringExp(list, 1, string_exp->string);
           break;

      case VARIABLE:
           if(string_exp->variable.type == CHARACTER_VAR)
              list = appendStringExp(list, 2, string_exp->variable.name);
           else if(string_exp->variable.type == STRING_VAR)
              list = appendStringExp(list, 3, string_exp->variable.name);
           else
           {
              print_to_log("Error (concatExpToList): Unexpected variable %s "
                           "of type %d.\n", string_exp->variable.name, 
                           string_exp->variable.type);
              return list;
           }
           break;
      
      case CONCAT:
           list = stringExpToList(list, string_exp->bin_op.left_exp);
           list = stringExpToList(list, string_exp->bin_op.right_exp);
           break;

      default:
           print_to_log("Error (stringExpToList): Unexpected atom type %d.\n",
                        string_exp->type);
           break;
   }
   return list;
}

StringList *appendStringExp(StringList *list, int type, string value)
{
   /* Locate the last element in the passed list. This may be NULL. */
   StringList *iterator = list;
   while(iterator != NULL)
   {
      if(iterator->next == NULL) break;
      iterator = iterator->next;
   }
   StringList *string_exp = malloc(sizeof(StringList));
   if(string_exp == NULL)
   {
      print_to_log("Error (appendStringExp): malloc failure.\n");
      exit(1);
   }
   string_exp->type = type;
   string_exp->value = strdup(value);
   string_exp->prev = iterator;
   string_exp->next = NULL;
   if(iterator == NULL) return string_exp;
   else 
   {
      iterator->next = string_exp;
      return list;
   }
}

void freeStringList(StringList *list)
{
   if(list == NULL) return;
   freeStringList(list->next);
   if(list->value) free(list->value);
   free(list);
}

int verifyStringExp(StringList *list, string host_string, Morphism *morphism)
{
   int new_assignments = 0;
   /* The index of the first character of the unmatched host substring. */
   unsigned int start_position = 0;
   bool match = true;
   while(list != NULL)
   {
      /* If there are still rule expressions to match and the start position
       * has exceeded the host string length, break the loop. */
      if(start_position > strlen(host_string)) 
      {
         match = false;
         break;
      }
      /* If the end of the host string is reached, the only expression that
       * matches is a string variable assigned the empty string. */
      if(start_position == strlen(host_string))
      {
         if(list->type == 3) 
         {
            Atom atom = { .type = STRING_CONSTANT, .string = "" };
            int result = verifyAtomVariable(list->value, atom, morphism);
            if(result >= 0) return new_assignments + result;
         }
         /* list->type != 3 or result == -1. In both cases, no match exists. */
         match = false;
         break;
      }
      /* String Constant */
      if(list->type == 1)
      {
         int offset = isPrefix(list->value, host_string + start_position);
         if(offset == -1) 
         {
            match = false;
            break;
         }
         else start_position += offset;
      }
      /* Character Variable */
      else if(list->type == 2)
      {
         char current_character[2] = {host_string[start_position], '\0'}; 
         Atom atom = { .type = STRING_CONSTANT, 
                       .string = current_character };
         start_position++;
         int result = verifyAtomVariable(list->value, atom, morphism);
         if(result == -1) 
         {
            match = false;
            break;
         }
         else new_assignments += result;
      }
      /* String Variable */
      else if(list->type == 3) break;
      else
      {
         print_to_log("Error (verifyStringExp): Unexpected StringList "
                      "type %d.\n", list->type);
         break;
      }
      list = list->next;
   }
   /* There are three cases to consider:
    * (1) The variable match is false, in which case it was established that
    *     no match is possible.
    * (2) The string list is exhausted. If the entire host string has not been
    *     matched, then a match cannot exist.
    * (3) A string variable was found. Start processing from the end of
    *     the list. */
   if(!match || (list == NULL && start_position >= strlen(host_string))) 
   {
      removeAssignments(morphism, new_assignments);
      return -1;
   }

   unsigned int last_position = strlen(host_string) - 1;
   /* Find the last element in the list. */
   while(list != NULL)
   {
      if(list->next == NULL) break;
      list = list->next;
   }
   /* Iterate backwards from the end of the list. */
   while(list != NULL)  
   {
      /* If last_position is less than start_position - 1, characters in the
       * host string have been matched twice. In this case, the host string is
       * not large enough to match the rule's string expression. */
      if(last_position < start_position - 1) 
      {
         match = false;
         break;
      }
      /* String Variable */
      if(list->type == 3)
      {
         int result = 0;
         if(last_position == start_position - 1) 
         {
            Atom atom = { .type = STRING_CONSTANT, .string = ""};
            result = verifyAtomVariable(list->value, atom, morphism);
         }
         else
         {
            /* last_position >= start_position */
            int length = last_position - start_position + 1;
            char host_substring[length];
            strncpy(host_substring, host_string + start_position, length);
            host_substring[length] = '\0';
            Atom atom = { .type = STRING_CONSTANT, .string = host_substring };
            result = verifyAtomVariable(list->value, atom, morphism);
         }
         if(result == -1) match = false;
         else new_assignments += result;        
         break;
      }
      /* The current rule expression is not a string variable. Therefore there
       * must be at least one more unmatched host character to match. This is
       * true if last_position >= start_position. */
      else if(last_position < start_position)
      {
         match = false;
         break;
      }
      /* String Constant */
      else if(list->type == 1)
      {
         int offset = isSuffix(list->value, host_string);
         if(offset == -1) 
         {
            match = false;
            break;
         }
         else last_position -= offset;
      }
      /* Character Variable */
      else if(list->type == 2)
      {
         char current_character[2] = {host_string[last_position], '\0'}; 
         Atom atom = { .type = STRING_CONSTANT, 
                       .string = current_character };
         last_position--;
         int result = verifyAtomVariable(list->value, atom, morphism);
         if(result == -1) 
         {
            match = false;
            break;
         }
         else new_assignments += result;
      }
      else
      {
         print_to_log("Error (verifyStringExp): Unexpected StringList "
                      "type %d.\n", list->type);
         break;
      }
      list = list->prev;
   } 
   if(!match)
   {
      removeAssignments(morphism, new_assignments);
      return -1;
   }  
   else return new_assignments;
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
 
int verifyListVariable(string name, Atom *list, int length, Morphism *morphism) 
{
   int index = lookupVariable(morphism, name);
   if(index == -1) 
   {
      addAssignment(morphism, name, length, list);
      return 1;
   }
   else
   {
      Assignment assignment = morphism->assignment[index];
      if(assignment.length != length) return -1;
      int list_index;
      for(list_index = 0; list_index < assignment.length; list_index++)
      {
         if(!compareConstants(list[length], assignment.value[length])) return -1;
      }
   }
   return 0;
}

int verifyAtomVariable(string name, Atom atom, Morphism *morphism)
{
   int index = lookupVariable(morphism, name);
   if(index == -1) 
   {
      Atom new_atom;
      new_atom.type = atom.type;
      if(atom.type == INTEGER_CONSTANT) new_atom.number = atom.number;
      if(atom.type == STRING_CONSTANT) new_atom.string = atom.string;
      addAssignment(morphism, name, 1, &new_atom);
      return 1;
   }
   Assignment assignment = morphism->assignment[index];
   if(compareConstants(atom, assignment.value[0])) return 0;
   else return -1;
}

bool compareConstants(Atom test_atom, Atom atom)
{
   switch(atom.type)
   {
      case INTEGER_CONSTANT: 
           if(test_atom.type == INTEGER_CONSTANT)
              if(test_atom.number == atom.number) return true;
           return false;
               
      case STRING_CONSTANT:     
           if(test_atom.type == STRING_CONSTANT)
              if(!strcmp(test_atom.string, atom.string)) return true;
           return false;
           break;

      default:
           print_to_log("Error (compareConstants): Unexpected atom type %d.\n",
	     	        atom.type);
           return false;
   }
   return true;
}
