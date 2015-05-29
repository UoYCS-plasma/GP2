#include "genLabel.h"

/* Concatenated string expressions are flattened into a doubly-linked list
 * structure. This is because it is difficult to match a tree structure
 * of string expressions, containing arbitrary character variables and a string 
 * variable, to a single C string. */
typedef struct StringList {
   int type; /* (1) string constant, (2) char variable, (3) string variable. */
   string value; /* The value of a string constant or a variable name. */
   struct StringList *next;
   struct StringList *prev;
} StringList;

static void generateAtomMatchingCode(Rule *rule, Atom atom, int indent);
static void generateVariableMatchingCode(Rule *rule, Atom atom, int indent);
static void generateConcatMatchingCode(Rule *rule, Atom atom, int indent);
static void generateStringMatchingCode(Rule *rule, StringList *string_exp, 
                                       bool prefix, int indent);
static void generateStringLengthCode(Atom atom, int indent);
static void generateStringExpression(Atom atom, bool first, int indent);

StringList *appendStringExp(StringList *list, int type, string value)
{
   StringList *string_exp = malloc(sizeof(StringList));
   if(string_exp == NULL)
   {
      print_to_log("Error (appendStringExp): malloc failure.\n");
      exit(1);
   }
   string_exp->type = type;
   string_exp->value = value;
   string_exp->next = NULL;

   if(list == NULL) 
   {
      string_exp->prev = NULL;
      return string_exp;
   }
   else
   {
      /* Locate the last element in the passed list. This may be NULL. */
      StringList *iterator = list;
      while(iterator != NULL)
      {
         if(iterator->next == NULL) break;
         iterator = iterator->next;
      }
      string_exp->prev = iterator;
      iterator->next = string_exp;
      return list;
   }
}

/* Uses appendStringExp to grow the passed StringList according to the value
 * of the passed Atom. */
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

void freeStringList(StringList *list)
{
   if(list == NULL) return;
   freeStringList(list->next);
   free(list);
}

void generateIteratorCode(Label label, int indent)
{
   PTFI("int mark, label_class;\n", indent);
   /* Generate the outer loop which iterates over the rows of the label class array.
    * If the LHS mark is ANY, then iterate over all marks. Otherwise only iterate
    * over the LHS mark. The latter for loop is technically unnecessary as there is
    * always exactly one iteration, but it is printed because the generated matching
    * code depends on the double for loop structure. */
   if(label.mark == ANY)
      PTFI("for(mark = 0; mark < NUMBER_OF_MARKS; mark++)\n", indent);
   else PTFI("for(mark = %d; mark < %d; mark++)\n", indent, label.mark, label.mark + 1);
   PTFI("{\n", indent);

   /* Generate the inner loop which iterates over the columns of the label class array.
    * If the LHS-list contains a list variable, the generated loop depends on the number 
    * of non-list variable atoms. */
   if(hasListVariable(label))
   {
      int number_of_atoms = label.length - 1;
      if(number_of_atoms == 0) 
           /* The LHS list contains only a single list variable. Iterate over all label
            * classes. */
           PTFI("for(label_class = 0; label_class < NUMBER_OF_CLASSES; label_class++)\n",
                indent + 3);
      else if(number_of_atoms == 1) 
           /* The LHS list contains a list variable and one other atom. Iterate over all
            * label classes except the one representing the empty list (0). */
           PTFI("for(label_class = 1; label_class < NUMBER_OF_CLASSES; label_class++)\n",
                indent + 3);
           /* The LHS list contains a list variable and more than one other atom. 
            * Iterate over all label classes representing lists of length > 1.
            * These are label classes 3-6. */
      else PTFI("for(label_class = %d; label_class < NUMBER_OF_CLASSES; label_class++)\n",
                indent + 3, number_of_atoms + 1);
   }
   /* For LHS-lists not containing a list variable, the generated loop is dependent
    * on the length of the list. */
   else
   {
      if(label.length == 0)
         /* The LHS-list is the empty list. Only one label class (0) matches the empty list. */
         PTFI("for(label_class = 0; label_class < 1; label_class++)\n", indent + 3);
      else if(label.length == 1)
      {

         Atom atom = label.list[0];
         switch(atom.type)
         {
            case INTEGER_CONSTANT:
                 PTFI("for(label_class = 1; label_class < 2; label_class++)\n", indent + 3);
                 break;

            case STRING_CONSTANT:
            case CONCAT:
                 PTFI("for(label_class = 2; label_class < 3; label_class++)\n", indent + 3);

            case VARIABLE:
                 if(atom.variable.type == INTEGER_VAR)
                    PTFI("for(label_class = 1; label_class < 2; label_class++)\n", indent + 3);
                 else if(atom.variable.type == STRING_VAR || 
                         atom.variable.type == CHARACTER_VAR)
                    PTFI("for(label_class = 2; label_class < 3; label_class++)\n", indent + 3);
                 else if(atom.variable.type == ATOM_VAR)
                    PTFI("for(label_class = 1; label_class < 3; label_class++)\n", indent + 3);
                 break;

            default:
                 print_to_log("Error (generateIteratorCode): Unexpected atom type %d.\n", 
                              atom.type);
                 break;
         }
      }
      else if(label.length > 1 || label.length < 5)
         /* For LHS-lists of length 2-4, generate the loop that iterates over the label
          * class of the appropriate length. These classes are LIST2_L (3), LIST3_L (4)
          * and LIST4_L (5). */
         PTFI("for(label_class = %d; label_class < %d; label_class++)\n", indent + 3,
              label.length + 1, label.length + 2);
      else 
         /* The LHS-list contains more than four atoms. Iterate over label class 
          * LONG_LIST_L (6). */
         PTFI("for(label_class = 6; label_class < 7; label_class++)\n", indent + 3);
   }
   PTFI("{\n", indent + 3);
}



/* The 'result' variable is used in the matching code to store results of variable-value 
 * assignments function calls. The 'result_declared' flag ensures that this variable is
 * declared at most once per label at runtime. */
bool result_declared = false;

void generateFixedListMatchingCode(Rule *rule, Label label, int indent)
{
   PTFI("/* Label Matching */\n", indent);
   PTFI("int new_assignments = 0;\n", indent);
   /* The empty rule list is only matched by the empty host list. */
   if(label.length == 0)
   {
      PTFI("/* Matching the empty list. */\n", indent);
      PTFI("match = label.length == 0 ? true : false;\n", indent);
      return;
   }
   PTFI("int host_list_position = 0;\n", indent);
   /* A do-while loop is generated so that the label matching code can be exited
    * at any time with a break statement the moment an atom match fails. */
   PTFI("do\n", indent);
   PTFI("{\n", indent);
   PTFI("/* The rule list does not contain a list variable, so there is no\n", indent);
   PTFI(" * match if the host list has a different length. */\n", indent);
   PTFI("if(label.length != %d) break;\n", indent + 3, label.length); 
   /* Lists without list variables admit relatively simple code generation as each
    * rule atom maps directly to the host atom in the same position. */
   int index;
   for(index = 0; index < label.length; index++)
   {
      Atom atom = label.list[index];
      PTFI("/* Matching rule atom %d. */\n", indent + 3, index);
      generateAtomMatchingCode(rule, atom, indent + 3);
      PTFI("host_list_position++;\n\n", indent + 3);
   }
   PTFI("/* If there are no more host atoms to match, success! */\n", indent + 3);
   PTFI("if(host_list_position == label.length) match = true;\n", indent + 3);
   PTFI("} while(false);\n\n", indent);
   /* Reset the flag before function exit. */
   result_declared = false;
}

void generateVariableListMatchingCode(Rule *rule, Label label, int indent)
{ 
   PTFI("/* Label Matching */\n", indent);
   /* Analyse the label for the list variable's index in the rule list.
    * This is used to control the two for loops that follow. */
   int list_variable_index = 0, index;
   string list_variable_name = NULL;
   for(index = 0; index < label.length; index++)
   {
      if(label.list[index].type == VARIABLE &&
         label.list[index].variable.type == LIST_VAR)
      {
         list_variable_index = index;
         list_variable_name = label.list[index].variable.name;
         break;
      }
   }
   PTFI("int new_assignments = 0;\n", indent);
   /* If the rule list contains only the list variable, generate code to assign
    * the list variable to the entire host list. */
   if(label.length == 1)
   {
      PTFI("/* Match list variable \"%s\" against the whole host list. */\n",
           indent, list_variable_name);
      if(!result_declared)
      {
         PTFI("int result;\n", indent);
         result_declared = true;
      }
      PTFI("result = addListAssignment(\"%s\", label.list, label.length, morphism);\n",
           indent, list_variable_name);
      generateVariableResultCode(rule, list_variable_name, true, indent);
      /* Reset the flag before function exit. */
      result_declared = false;
      return;
   }
   PTFI("int host_list_position = 0;\n", indent);
   /* A do-while loop is generated so that the label matching code can be exited
    * at any time with a break statement the moment an atom match fails. */
   PTFI("do\n", indent);
   PTFI("{\n", indent);
   /* Check if the host label has enough atoms to match those in the rule. 
    * Subtracting 1 from the rule label's length gives the number of atoms it
    * contains: the list variable is not counted because it can match the
    * empty list. */
   PTFI("if(label.length < %d) break;\n", indent + 3, label.length - 1); 
   PTFI("/* Matching from the start of the host list. */\n", indent + 3);
   for(index = 0; index < list_variable_index; index++)
   {
      Atom atom = label.list[index];
      PTFI("/* Matching rule atom %d */\n", indent + 3, index);
      generateAtomMatchingCode(rule, atom, indent + 3);
      PTFI("host_list_position++;\n", indent + 3);
   }   
   PTFI("/* The current host index marks the start of the list .\n", indent + 3);
   PTFI(" * that is assigned to the list variable. */\n", indent + 3);
   PTFI("int start_index = host_list_position;\n", indent + 3);
   PTFI("host_list_position = label.length - 1;\n", indent + 3);

   /* Iterate back from the end of the rule list. Note that this loop is not
    * executed if the list variable is the last element in the rule list. */
   PTF("\n");
   PTFI("/* Matching from the end of the host list. */\n", indent + 3);
   for(index = label.length - 1; index > list_variable_index; index--)
   {
      Atom atom = label.list[index];
      PTFI("/* Matching rule atom %d */\n", indent + 3, index);
      generateAtomMatchingCode(rule, atom, indent + 3);
      PTFI("host_list_position--;\n", indent + 3);
   }
   /* Assign the list variable to the rest of the host list. */
   if(!result_declared)
   {
      PTFI("int result;\n", indent + 3);
      result_declared = true;
   }
   PTF("\n");
   PTFI("/* Matching list variable \"%s\". */\n", indent + 3, list_variable_name);
   PTFI("if(host_list_position == start_index - 1)\n", indent + 3);
   PTFI("result = addListAssignment(\"%s\", NULL, 0, morphism);\n", 
        indent + 6, list_variable_name);
   PTFI("else\n", indent + 3);
   PTFI("{\n", indent + 3);
   PTFI("/* Assign the unmatched sublist of the host list to \"%s\". */\n",
        indent + 6, list_variable_name);
   PTFI("Atom list[host_list_position - start_index + 1];\n", indent + 6);
   PTFI("int counter;\n", indent + 6);
   /* Generate code to iterate over the unmatched portion of the host list, 
    * defined by runtime variables start_index and index, and construct
    * the sublist that is to be assigned to the list variable. */
   PTFI("for(counter = start_index; counter <= index; counter++)\n", indent + 6);
   PTFI("{\n", indent + 6);
   PTFI("int list_index = counter - start_index;\n", indent + 9);
   PTFI("list[list_index].type = label.list[counter].type;\n", indent + 9);
   PTFI("if(label.list[counter].type == INTEGER_CONSTANT)\n", indent + 9);
   PTFI("   list[list_index].number = label.list[counter].number;\n", indent + 9);
   PTFI("if(label.list[counter].type == STRING_CONSTANT)\n", indent + 9);
   PTFI("   list[list_index].string = label.list[counter].string;\n", indent + 9);
   PTFI("}\n", indent + 6);
   PTFI("result = addListAssignment(\"%s\", list, host_list_position - start_index + 1,"
        "morphism);\n", indent + 6, list_variable_name);
   PTFI("}\n", indent + 3);
   generateVariableResultCode(rule, list_variable_name, true, indent);
   PTFI("} while(false);\n\n", indent);
   /* Reset the flag before function exit. */
   result_declared = false;
}

static void generateAtomMatchingCode(Rule *rule, Atom atom, int indent)
{
   switch(atom.type)
   {
      case VARIABLE:
           generateVariableMatchingCode(rule, atom, indent);
           break;
      
      case INTEGER_CONSTANT:
           PTFI("if(label.list[index].type != INTEGER_CONSTANT) break;\n", indent);
           PTFI("else if(label.list[index].number != %d) break;\n", indent, atom.number);
           break;

      case STRING_CONSTANT:
           PTFI("if(label.list[index].type != STRING_CONSTANT) break;\n", indent);
           PTFI("else if(strcmp(label.list[index].string, \"%s\") != 0) break;\n",
                indent, atom.string);
           break;

      case CONCAT:
           PTFI("if(label.list[index].type != STRING_CONSTANT) break;\n", indent);
           PTFI("else\n", indent);
           PTFI("{\n", indent);
           generateConcatMatchingCode(rule, atom, indent + 3);
           PTFI("}\n", indent);
           break;

      default:
           print_to_log("Error (generateAtomMatchingCode): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}

static void generateVariableMatchingCode(Rule *rule, Atom atom, int indent)
{
   if(!result_declared)
   {
      PTFI("int result;\n", indent);
      result_declared = true;
   }
   switch(atom.variable.type)
   {
      case INTEGER_VAR:
           PTFI("/* Matching integer variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(label.list[index].type != INTEGER_CONSTANT) break;\n", indent);
           PTFI("result = addIntegerAssignment(\"%s\", label.list[index].number, "
                "morphism);\n", indent, atom.variable.name);
           generateVariableResultCode(rule, atom.variable.name, false, indent);
           break;

      case CHARACTER_VAR:
           PTFI("/* Matching character variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(label.list[index].type != STRING_CONSTANT) break;\n", indent);
           PTFI("if(strlen(label.list[index].string) != 1) break;\n", indent);
           PTFI("result = addStringAssignment(\"%s\", label.list[index].string, "
                "morphism);\n", indent, atom.variable.name);
           generateVariableResultCode(rule, atom.variable.name, false, indent);
           break;

      case STRING_VAR:
           PTFI("/* Matching string variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(label.list[index].type != STRING_CONSTANT) break;\n", indent);
           PTFI("result = addStringAssignment(\"%s\", label.list[index].string, "
                "morphism);\n", indent, atom.variable.name);
           generateVariableResultCode(rule, atom.variable.name, false, indent);
           break;

      case ATOM_VAR:
           PTFI("/* Matching atom variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(label.list[index].type == INTEGER_CONSTANT)\n", indent);
           PTFI("result = addIntegerAssignment(\"%s\", label.list[index].number, "
                "morphism);\n", indent, atom.variable.name);
           PTFI("else if(label.list[index].type == STRING_CONSTANT)\n", indent);
           PTFI("result = addStringAssignment(\"%s\", label.list[index].string, "
                "morphism);\n", indent, atom.variable.name);
           generateVariableResultCode(rule, atom.variable.name, false, indent);
           break;

      case LIST_VAR:
           print_to_log("Error (generateAtomMatchingCode): A list "
                        "variable sneaked into this function!\n");
           break;

      default:
           print_to_log("Error (generateVariableMatchingCode): Unexpected "
                        "variable type %d.\n", atom.variable.type);
           break;
   }
}

static void generateConcatMatchingCode(Rule *rule, Atom atom, int indent)
{
   StringList *list = NULL;
   list = stringExpToList(list, atom.bin_op.left_exp);
   list = stringExpToList(list, atom.bin_op.right_exp);

   StringList *iterator = list;
   bool has_string_variable = false;
   while(iterator != NULL)
   {
      if(iterator->type == 3) 
      {
         has_string_variable = true;
         break;
      }
      iterator = iterator->next;
   }
   iterator = list;
   PTFI("string host_string = label.list[index].string;\n", indent);
   PTFI("unsigned int start = 0, end = strlen(host_string) - 1;\n\n", indent);
   /* If there is no string variable, iterate through the StringList and 
    * generate code for each string expression. */
   if(!has_string_variable)
   {
      while(iterator != NULL) 
      {
         PTFI("if(start >= strlen(host_string)) break;\n", indent);
         generateStringMatchingCode(rule, iterator, true, indent);
         iterator = iterator->next;
      }
   }
   /* Otherwise, iterate through the StringList until the string variable
    * is reached, generating code for each string expression, then iterate
    * from the end of the StringList back to the string variable. */
   else
   {
      PTFI("/* Matching from the start of the host string. */\n", indent);
      while(iterator->type != 3) 
      {
         PTFI("if(start >= strlen(host_string)) break;\n", indent);
         generateStringMatchingCode(rule, iterator, true, indent);
         iterator = iterator->next;
      }
      PTFI("if(start > strlen(host_string)) break;\n", indent);
      /* Move iterator to the end of the list. */
      while(iterator != NULL) 
      {
         if(iterator->next == NULL) break;
         iterator = iterator->next;
      }
      /* This loop is not executed if the string variable is the last item in
       * the string list. */
      PTF("\n");
      PTFI("/* Matching from the end of the host string. */\n", indent);
      while(iterator->type != 3) 
      {
         PTFI("if(end < start) break;\n", indent );
         generateStringMatchingCode(rule, iterator, false, indent);
         iterator = iterator->prev;
      }
   }
   if(!result_declared)
   {
      PTFI("int result;\n", indent);
      result_declared = true;
   }
   /* Assign the string variable to the rest of the host string. */
   PTF("\n");
   PTFI("/* Matching string variable \"%s\". */\n", indent, iterator->value);
   PTFI("if(end == start - 1) ", indent);
   PTF("result = addStringAssignment(\"%s\", \"\", morphism);\n", iterator->value);
   PTFI("else\n", indent);
   PTFI("{\n", indent);
   PTFI("char substring[end - start + 1];\n", indent + 3);
   PTFI("strncpy(substring, host_string + start, end - start + 1);\n", indent + 3);
   PTFI("substring[end - start + 1] = '\\0';\n", indent + 3);
   PTFI("result = addStringAssignment(\"%s\", substring, morphism);\n", 
        indent + 3, iterator->value);
   generateVariableResultCode(rule, iterator->value, false, indent);
   PTFI("}\n", indent);
   freeStringList(list);
}

bool offset_declared = false, host_character_declared = false;

static void generateStringMatchingCode(Rule *rule, StringList *string_exp, 
                                       bool prefix, int indent)
{
   /* String Constant */
   if(string_exp->type == 1)
   {
      if(prefix)
      {  
         if(!offset_declared)
         {
            PTFI("unsigned int offset = 0;\n", indent);
            offset_declared = true;
         }
         PTFI("offset = isPrefix(\"%s\", host_string + start);\n",
              indent, string_exp->value);
         PTFI("if(offset == -1) break; else start += offset;\n", indent);
      }
      else
      {
         if(!offset_declared)
         {
            PTFI("unsigned int offset = 0;\n", indent);
            offset_declared = true;
         }
         PTFI("offset = isSuffix(\"%s\", host_string);\n", 
              indent, string_exp->value);
         PTFI("if(offset == -1) break; else end -= offset;\n", indent);
      }
   }
   /* Character Variable */
   else if(string_exp->type == 2)
   {
      /* This is not in the main scope at runtime, so do not set result_declared
       * to true. */
      if(!result_declared) 
      {
         PTFI("int result = 0;\n", indent);
         result_declared = true;
      }
      PTFI("/* Matching character variable \"%s\". */\n", indent, string_exp->value);
      if(!host_character_declared)
      {
         PTFI("char host_character[2] = ", indent);
         host_character_declared = true;
         if(prefix) PTF("{host_string[start++], '\\0'};\n");
         else PTF("{host_string[end--], '\\0'};\n");
      }
      else
      {
         PTFI("host_character[0] = ", indent);
         if(prefix) PTF("host_string[start++];\n");
         else PTF("host_string[end--];\n");
      }
      PTFI("result = addStringAssignment(\"%s\", host_character, morphism);\n",
           indent, string_exp->value);
      generateVariableResultCode(rule, string_exp->value, false, indent);
   }
}

void generateVariableResultCode(Rule *rule, string name, bool list_variable, int indent)
{
   PTFI("if(result >= 0)\n", indent);
   PTFI("{\n", indent);
   Variable *variable = getVariable(rule, name);
   assert(variable != NULL);
   if(variable->predicates != NULL)
   {
      PTFI("do\n", indent + 3);
      PTFI("{\n", indent + 3);
      PTFI("/* Update global booleans for the variable's predicates. */\n", indent + 6);
      int p_index;
      for(p_index = 0; p_index < variable->predicate_count; p_index++)
         PTFI("if(!evalPredicate%d()) break;", indent + 6, 
              variable->predicates[p_index]->bool_id);
      PTFI("if(!evalCondition_%s());\n", indent + 6, rule->name);
      PTFI("{\n", indent + 6);
      PTFI("/* Reset the boolean variables in the predicates of this variable. */\n", 
           indent + 9);
      int index;
      for(index = 0; index < variable->predicate_count; index++)
      { 
         Predicate *predicate = variable->predicates[index];
         if(predicate->negated) PTFI("b%d = false;\n", indent + 9, predicate->bool_id);
         else PTFI("b%d = true;\n", indent + 9, predicate->bool_id);
      }
      PTFI("result = -1;\n", indent + 9);
      PTFI("}\n", indent + 6);
      PTFI("} while(false);\n", indent + 3);
      PTFI("}\n", indent);
      PTFI("if(result >= 0)\n", indent);
      PTFI("{\n", indent);
   }
   PTFI("new_assignments += results;\n", indent + 3);
   if(list_variable) PTFI("match = true;\n", indent + 3);
   PTFI("}\n", indent);
   if(!list_variable) PTFI("else break;\n", indent); 
}

/* For each variable in the rule, code is generated to assign that variable's value
 * (from the morphism) to a local variable for use by the rule application code.
 * This prevents duplication if code if a variable occurs more than once in the RHS.
 * 
 * For example, the assignment (i -> 1, word -> "razzmatazz") will generate the
 * following code:
 * int i_var = 1;
 * string word_var = "razzmatazz";
 * 
 * Uniqueness of variable names in a rule ensures uniqueness of the runtime 
 * variable name generated. */
void generateVariableCode(string name, GPType type)
{
   switch(type)
   {
      case INTEGER_VAR:
           PTFI("int %s_var = getIntegerValue(\"%s\", morphism);\n", 3, name, name);
           break;

      case CHARACTER_VAR:
      case STRING_VAR:
           PTFI("string %s_var = getStringValue(\"%s\", morphism);\n", 3, name, name);
           break;

      case ATOM_VAR:
           /* Atom variable are conveniently encoded as a C union at runtime. */
           PTFI("\n   Assignment *assignment_%s = lookupVariable(morphism, \"%s\");\n",
                3, name, name);
           PTFI("union { int num; string str; } %s_var;\n", 3, name);
           PTFI("if(assignment_%s->type == INTEGER_VAR) "
                "%s_var.num = getIntegerValue(\"%s\", morphism);\n", 3, name, name, name);
           PTFI("else %s_var.str = getStringValue(\"%s\", morphism);\n\n", 3, name, name);
           break;
         
      case LIST_VAR:
           /* The list variable's value may not be required as rule application may
            * only require the length of the value. If the value is needed, it is
            * generated "on demand" by generateRHSLabelCode. */
           PTFI("Assignment *assignment_%s = lookupVariable(morphism, \"%s\");\n",
                3, name, name);
           break;
      
      default:
           print_to_log("Error (generateVariableCode): Unexpected type %d\n", type);
           break;
   }
}

/* Integers used to generate fresh runtime variables "length", "i" and "host_label"
 * which could be used multiple times or none at all. This will avoid repeated
 * declarations errors and unused variable warnings in the generated C code. */
int length_count = 0, i_count = 0, host_label_count = 0;

/* Labels to be evaluated occur in different contexts, each requiring slightly 
 * different code to be generated, although the overall code skeleton is the same. 
 * These contexts are identified by the 'context' argument:
 * 0 - RHS label
 * 1 - Label argument of the edge predicate in a condition.
 * 2 - First label argument of a relational expression in a condition.
 * 3 - Second label argument of a relational expression in a condition. 
 *
 * The 'count' argument is used to generate unique variable names. */
void generateLabelEvaluationCode(Label label, bool node, int count, int context, int indent)
{
   /* The name of the runtime variable to store the label depends on its context.
    * In particular, labels in relational expressions are directly compared, so they
    * require different names. */
   string label_name;
   if(context == 0 || context == 1) label_name = "label";
   else if(context == 2) label_name = "left_label";
   else if(context == 3) label_name = "right_label";

   if(label.length == 0)
   { 
      /* If the RHS has the 'ANY' mark, generate code to retrieve the mark of the
       * host item's label. */
      if(label.mark == ANY)
      {
         if(node) PTFI("Label host_label%d = getNodeLabel(host, host_node_index));\n", 
                       indent, host_label_count);
         else PTFI("Label host_label%d = getEdgeLabel(host, host_edge_index));\n", 
                   indent, host_label_count);
         PTFI("%s = makeEmptyLabel(host_label%d.mark);\n\n", indent, label_name,
              host_label_count);
      }
      else PTFI("%s = makeEmptyLabel(%d);\n\n", indent, label_name, label.mark);
      host_label_count++;
      return;
   }
   /* The length of evaluated list is not static because right labels contain an
    * arbitrary number of list variables. For each list variable in the label, add
    * its length to the runtime accumulator <list_var_length>. A compile-time
    * accumulator <number_of_atoms> counts the number of non-list-variable atoms. */
   int index, number_of_atoms = 0;
   PTFI("int list_var_length%d = 0;\n", indent, count);
   for(index = 0; index < label.length; index++)
   {
      Atom atom = label.list[index];
      if(atom.type == VARIABLE && atom.variable.type == LIST_VAR)
         PTFI("list_var_length%d += assignment_%s->length;\n", 
              indent, count, atom.variable.name);
      else number_of_atoms++;
   }
   /* Labels in the RHS graph are dynamically allocated because they are the new
    * labels in the host graph. Labels in predicates are automatic variables because
    * they are not required after the evaluation of the predicate. */
   if(context == 0) 
        PTFI("Atom *list%d = makeList(%d + list_var_length%d);\n", indent, count,
             number_of_atoms, count);
   else PTFI("Atom list%d[%d + list_var_length%d];\n", indent, count,
             number_of_atoms, count);

   /* Generate code to build the label. Strings in the RHS label are duplicated at
    * runtime; strings in predicate labels are not. */
   PTFI("int index%d = 0;\n", indent, count);
   for(index = 0; index < label.length; index++)
   {
      Atom atom = label.list[index];
      switch(atom.type)
      {
         case INTEGER_CONSTANT:
              PTFI("list%d[index%d].type = INTEGER_CONSTANT;\n", indent, count, count);
              PTFI("list%d[index%d++].number = %d;\n", indent, count, count, atom.number);
              break;

         case STRING_CONSTANT:
              PTFI("list%d[index%d].type = STRING_CONSTANT;\n", indent, count, count);
              if(context == 0) 
                    PTFI("list%d[index%d++].string = strdup(\"%s\");\n", indent, count, 
                         count, atom.string);
              else PTFI("list%d[index%d++].string = \"%s\";\n", indent, count, count,
                         atom.string);
              break;

         case VARIABLE:
         {
              /* Use the <variable_name>_var variables generated previously to
               * add the correct values to the runtime list. */
              string name = atom.variable.name;
              if(atom.variable.type == INTEGER_VAR)
              {
                 PTFI("list%d[index%d].type = INTEGER_CONSTANT;\n", indent, count, count);
                 PTFI("list%d[index%d++].number = %s_var;\n", indent, count, count, name);
              }
              else if(atom.variable.type == CHARACTER_VAR ||
                      atom.variable.type == STRING_VAR)
              {
                 PTFI("list%d[index%d].type = STRING_CONSTANT;\n", indent, count, count);
                 if(context == 0) 
                      PTFI("list%d[index%d++].string = strdup(%s_var);\n", 
                           indent, count, count, name);
                 else PTFI("list%d[index%d++].string = %s_var;\n", indent, count, count, name);

              }
              else if(atom.variable.type == ATOM_VAR)
              {
                 PTFI("if(assignment_%s->type == INTEGER_VAR)\n", indent, name);
                 PTFI("{\n", indent);
                 PTFI("list%d[index%d].type = INTEGER_CONSTANT;\n", 
                      indent + 3, count, count);
                 PTFI("list%d[index%d++].number = %s_var.num;\n", 
                      indent + 3, count, count, name);
                 PTFI("}\n", indent);
                 PTFI("else /* type == STRING_CONSTANT */\n", indent);
                 PTFI("{\n", indent);
                 PTFI("list%d[index%d].type = STRING_CONSTANT;\n", indent + 3, count, count);
                 if(context == 0)
                      PTFI("list%d[index%d++].string = strdup(%s_var.str);\n",
                           indent + 3, count, count, name);
                 else PTFI("list%d[index%d++].string = %s_var;\n", indent, count, count, name);
                 PTFI("}\n", indent);
              }
              else if(atom.variable.type == LIST_VAR)
              {
                 PTFI("Atom *%s_var = getListValue(\"%s\", morphism);\n", 3, name, name);
                 PTFI("int i%d;\n", indent, i_count);
                 PTFI("for(i%d = 0; i%d < assignment_%s->length; i%d++)\n", indent,
                      i_count, i_count, name, i_count);
                 PTFI("{\n", indent);
                 PTFI("if(%s_var[i%d].type == INTEGER_CONSTANT)\n", indent + 3, name, i_count);
                 PTFI("{\n", indent + 3);
                 PTFI("list%d[index%d].type = INTEGER_CONSTANT;\n", indent + 6, count, count);
                 PTFI("list%d[index%d++].number = %s_var[i%d].number;\n", 
                      indent + 6, count, count, name, i_count);
                 PTFI("}\n", indent + 3);
                 PTFI("else /* type == STRING_CONSTANT */\n", indent + 3);
                 PTFI("{\n", indent + 3);
                 PTFI("list%d[index%d].type = STRING_CONSTANT;\n", indent + 6, count, count);
                 if(context == 0) 
                      PTFI("list%d[index%d++].string = strdup(%s_var[i%d].string);\n",
                           indent + 6, count, count, name, i_count);
                 else PTFI("list%d[index%d++].string = %s_var[i%d].string;\n",
                           indent + 6, count, count, name, i_count);
                 PTFI("}\n", indent + 3);
                 PTFI("}\n", indent);
                 i_count++;
              }
              break;
         }
         /* When evaluating RHS labels, the results of degree operators in labels are
          * stored in variables by generateVariableCode. This is not the case for
          * degree operators in predicates: their values must be obtained directly. */
         case INDEGREE:
              PTFI("list%d[index%d].type = INTEGER_CONSTANT;\n", indent, count, count);
              if(context == 0)
                   PTFI("list%d[index%d++].number = indegree%d;\n", 
                        indent, count, count, atom.node_id); 
              else PTFI("list%d[index%d++].number = getIndegree(host, %d);\n", 
                        indent, count, count, atom.node_id); 
              break;
           
         case OUTDEGREE:
              PTFI("list%d[index%d].type = INTEGER_CONSTANT;\n", indent, count, count);
              if(context == 0)
                   PTFI("list%d[index%d++].number = outdegree%d;\n", 
                        indent, count, count, atom.node_id); 
              else PTFI("list%d[index%d++].number = getOutdegree(host, %d);\n", 
                        indent, count, count, atom.node_id); 
              break;

         case LENGTH:
         case NEG:
         case ADD:
         case SUBTRACT:
         case MULTIPLY:
         case DIVIDE:  
              PTFI("list%d[index%d].type = INTEGER_CONSTANT;\n", indent, count, count);
              PTFI("list%d[index%d++].number = ", indent, count, count);
              generateIntExpression(atom, context, false);
              PTF(";\n");
              break;

         case CONCAT:
              PTFI("unsigned int length%d = 0;\n", indent, length_count);
              /* Generates string variables to store string literals to be 
               * concatenated, and updates the runtime length variable with the 
               * total length of the concatenated string. */
              generateStringLengthCode(atom, indent);
              /* Build host_string from the evaluated strings that make up the
               * RHS label. */
              PTFI("char host_string%d[length%d + 1];\n", indent, length_count, length_count);
              generateStringExpression(atom, true, indent);
              PTFI("host_string%d[length%d] = '\\0';\n\n", indent, length_count, length_count);
              PTFI("list%d[index%d].type = STRING_CONSTANT;\n", indent, count, count);
              if(context == 0)
                   PTFI("list%d[index%d++].string = strdup(host_string%d);\n\n", 
                        indent, count, count, length_count);
              else PTFI("list%d[index%d++].string = host_string%d;\n\n", 
                        indent, count, count, length_count);
              length_count++;
              break;
      
         default:
              print_to_log("Error (generateLabelCode): Unexpected host atom "
                           "type %d.\n", atom.type);
              break;
      }
   }      
   if(label.mark == ANY)
   /* If the RHS has the 'ANY' mark, generate code to retrieve the mark of the
    * host item's label. */
   {
      if(node) PTFI("Label host_label%d = getNodeLabel(host, host_node_index);\n",
                    indent, host_label_count);
      else PTFI("Label host_label%d = getEdgeLabel(host, host_edge_index);\n",
                indent, host_label_count);
      PTFI("%s = makeHostLabel(host_label%d.mark, %d + list_var_length%d, list%d);\n\n", 
           indent, label_name, host_label_count, number_of_atoms, count, count);
      host_label_count++;
   }
   else PTFI("%s = makeHostLabel(%d, %d + list_var_length%d, list%d);\n\n", 
             indent, label_name, label.mark, number_of_atoms, count, count);
}

/* Navigates an integer expression tree and writes the arithmetic expression it 
 * represents. For example, given the label (i + 1) * length(s), where i is an
 * integer variable and s is a string variable, generateIntExpression prints:
 * (i_var + 1) * (int)strlen(s_var); */
void generateIntExpression(Atom atom, int context, bool nested)
{
   switch(atom.type)
   {
      case INTEGER_CONSTANT:
           PTF("%d", atom.number);
           break;

      case LENGTH:
           if(atom.variable.type == STRING_VAR)
              PTF("(int)strlen(%s_var)", atom.variable.name);

           else if(atom.variable.type == ATOM_VAR)
              PTF("((assignment_%s->type == STRING_VAR) ? (int)strlen(%s_var.str) : 1)", 
                  atom.variable.name, atom.variable.name);

           else if(atom.variable.type == LIST_VAR)
              PTF("assignment_%s->length", atom.variable.name);

           else
              print_to_log("Error (generateIntegerExpressionCode): Unexpected "
                           "variable type of length argument.\n");
           break;
      
      case VARIABLE:
           if(atom.variable.type == ATOM_VAR) PTF("%s_var.num", atom.variable.name);
           else PTF("%s_var", atom.variable.name);
           break;

      case INDEGREE:
           if(context == 0) PTF("indegree%d", atom.node_id);
           else PTF("getIndegree(host, %d)", atom.node_id);
           break;

      case OUTDEGREE:
           if(context == 0) PTF("outdegree%d", atom.node_id);
           else PTF("getOutdegree(host, %d)", atom.node_id);
           break;

      case NEG:
           PTF("(-%d)", atom.number);
           break;

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
           if(nested) PTF("(");
           generateIntExpression(*(atom.bin_op.left_exp), context, true);
           if(atom.type == ADD) PTF(" + ");
           if(atom.type == SUBTRACT) PTF(" - ");
           if(atom.type == MULTIPLY) PTF(" * ");
           if(atom.type == DIVIDE) PTF(" / ");
           generateIntExpression(*(atom.bin_op.right_exp), context, true);
           if(nested) PTF(")");
           break;

      default:
           print_to_log("Error (generateIntegerExpression): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}

/* The string case is analogous to the above but complicated by the fact that
 * the runtime code builds a string whose length needs to be known in advance.
 * Therefore two functions are used to generate the string expression.
 * The first obtains the total length of the evaluated RHS string and assigns
 * it to a runtime variable. The second generates the string.
 *
 * For example, "a".s.c (s string variable, c character variable) is as follows.
 * generateStringLengthCode prints:
 * length = 0; 
 * length += strlen("a"); 
 * length += strlen(s_var);
 * length += strlen(c_var);
 *
 * The character array host_string of size <length> is created by the caller
 * before calling generateStringExpression. 
 *
 * generateStringExpression prints:
 * host_string = strcpy(host_string, "a"); 
 * host_string = strcat(host_string, s_var); 
 * host_string = strcat(host_string, c_var);
 *
 * The 'bool first' argument of generateStringExpression is used to control
 * which of 'strcpy' and 'strcat' is printed. C doesn't like it when you call
 * strcpy on a newly-created character array. */ 
void generateStringLengthCode(Atom atom, int indent)
{
   switch(atom.type)
   {
      case STRING_CONSTANT:
           PTFI("length%d += strlen(\"%s\");\n", indent, length_count, atom.string);
           break;

      case VARIABLE:
           PTFI("length%d += strlen(%s_var);\n", indent, length_count, atom.variable.name);
           break;

      case CONCAT:
           generateStringLengthCode(*(atom.bin_op.left_exp), indent);
           generateStringLengthCode(*(atom.bin_op.right_exp), indent);
           break;
          
      default:
           print_to_log("Error (generateStringEvalCode): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}

void generateStringExpression(Atom atom, bool first, int indent)
{
   switch(atom.type)
   { 
      case STRING_CONSTANT:
           if(first) PTFI("strcpy(host_string%d, \"%s\");\n", indent, 
                          length_count, atom.string);
           else PTFI("strcat(host_string%d, \"%s\");\n", indent, 
                     length_count, atom.string);

      case VARIABLE:
           if(first) PTFI("strcpy(host_string%d, %s_var );\n", indent, 
                          length_count, atom.variable.name);
           else PTFI("strcat(host_string%d, %s_var);\n", indent, 
                     length_count, atom.variable.name);
           break;

      case CONCAT:
           generateStringExpression(*(atom.bin_op.left_exp), first, indent);
           generateStringExpression(*(atom.bin_op.right_exp), false, indent);
           break;
          
      default:
           print_to_log("Error (generateStringExpression): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}
