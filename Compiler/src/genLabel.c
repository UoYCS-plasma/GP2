#include "genLabel.h"

/* Concatenated string expressions are flattened into a doubly-linked list
 * structure. This is because it is difficult to match a tree structure
 * of string expressions, containing arbitrary character variables and a string 
 * variable, to a single C string. */
typedef struct StringList {
   int type; /* (1) string constant, (2) char variable, (3) string variable. */
   union {
      string constant; 
      int variable_id;
   }; /* The value of a string constant or a variable name. */
   struct StringList *next;
   struct StringList *prev;
} StringList;

static void generateAtomMatchingCode(Rule *rule, RuleAtom *atom, 
                                     string host_item, int indent);
static void generateVariableMatchingCode(Rule *rule, RuleAtom *atom,
                                         string host_item, int indent);
static void generateConcatMatchingCode(Rule *rule, RuleAtom *atom, int indent);
static void generateStringMatchingCode(Rule *rule, StringList *string_exp, 
                                       bool prefix, int indent);
static void generateStringLengthCode(RuleAtom *atom, int indent);
static void generateStringExpression(RuleAtom *atom, bool first, int indent);

StringList *appendStringExp(StringList *list, int type, string constant, int id)
{
   StringList *string_exp = malloc(sizeof(StringList));
   if(string_exp == NULL)
   {
      print_to_log("Error (appendStringExp): malloc failure.\n");
      exit(1);
   }
   string_exp->type = type;
   if(type == 1) string_exp->constant = constant;
   else string_exp->variable_id = id;
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
StringList *stringExpToList(StringList *list, RuleAtom *string_exp)
{
   switch(string_exp->type)
   {
      case STRING_CONSTANT:
           list = appendStringExp(list, 1, string_exp->string, -1);
           break;

      case VARIABLE:
           if(string_exp->variable.type == CHARACTER_VAR)
              list = appendStringExp(list, 2, NULL, string_exp->variable.id);
           else if(string_exp->variable.type == STRING_VAR)
              list = appendStringExp(list, 3, NULL, string_exp->variable.id);
           else
           {
              print_to_log("Error (concatExpToList): Unexpected variable %d "
                           "of type %d.\n", string_exp->variable.id, 
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

/* The 'result' variable is used in the matching code to store results of variable-value 
 * assignments function calls. The 'result_declared' flag ensures that this variable is
 * declared at most once per label at runtime. */
bool result_declared = false;

void generateFixedListMatchingCode(Rule *rule, RuleLabel label, int indent)
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

   RuleListItem *item = label.list->first;
   /* A do-while loop is generated so that the label matching code can be exited
    * at any time with a break statement immediately after an atom match fails. */
   PTFI("do\n", indent);
   PTFI("{\n", indent);
   if(label.length == 1)
   {
      PTFI("if(label.length != 1) break;\n", indent + 3);
      generateAtomMatchingCode(rule, item->atom, "label.list", indent + 3);
      PTFI("match = true;\n", indent + 3);
   }
   else
   {
      PTFI("/* The rule list does not contain a list variable, so there is no\n", indent + 3);
      PTFI(" * match if the host list has a different length. */\n", indent + 3);
      PTFI("if(label.length != %d) break;\n", indent + 3, label.length); 
      /* Lists without list variables admit relatively simple code generation as each
      * rule atom maps directly to the host atom in the same position. */
      PTFI("HostListItem *item = label.list.list->first;\n", indent + 3);
      int atom_count = 1;
      while(item != NULL)
      {
         PTFI("/* Check if the end of the host list has been reached. */\n", indent + 3);
         PTFI("if(item == NULL) break;\n", indent + 3);
         PTFI("/* Matching rule atom %d. */\n", indent + 3, atom_count);
         generateAtomMatchingCode(rule, item->atom, "item->atom", indent + 3);
         PTFI("item = item->next;\n\n", indent + 3);
         atom_count++;
         item = item->next;
      }
      PTFI("/* If there are no more host atoms to match, success! */\n", indent + 3);
      PTFI("if(item == NULL) match = true;\n", indent + 3);
   }
   PTFI("} while(false);\n\n", indent);
   /* Reset the flag before function exit. */
   result_declared = false;
}

void generateVariableListMatchingCode(Rule *rule, RuleLabel label, int indent)
{ 
   PTFI("/* Label Matching */\n", indent);
   PTFI("int new_assignments = 0;\n", indent);
   int list_variable_id = -1;
   RuleListItem *item = label.list->first;
   while(item != NULL)
   {
      if(item->atom->type == VARIABLE && item->atom->variable.type == LIST_VAR)
      {
         list_variable_id = item->atom->variable.id;
         break;
      }
      item = item->next;
   }
   /* If the rule list contains only the list variable, generate code to assign
    * the list variable to the entire host list. */
   if(label.length == 1)
   {
      PTFI("/* Match list variable %d against the whole host list. */\n",
           indent, list_variable_id);
      if(!result_declared)
      {
         PTFI("int result;\n", indent);
         result_declared = true;
      }
      PTFI("if(label.length == 1)\n", indent );
      PTFI("{\n", indent);
      PTFI("if(label.list.type == 'i') result = "
           "addIntegerAssignment(morphism, %d, label.list.num);\n", 
           indent + 3, list_variable_id);
      PTFI("else result = addStringAssignment(morphism, %d, label.list.str);\n",
           indent + 3, list_variable_id);
      PTFI("}\n", indent);
      PTFI("else result = addListAssignment(morphism, %d, label.list.list);\n",
           indent, list_variable_id);
      generateVariableResultCode(rule, list_variable_id, true, indent);
      /* Reset the flag before function exit. */
      result_declared = false;
      return;
   }

   item = label.list->first;
   /* A do-while loop is generated so that the label matching code can be exited
    * at any time with a break statement immediately after an atom match fails. */
   PTFI("do\n", indent);
   PTFI("{\n", indent);
   /* Check if the host label has enough atoms to match those in the rule. 
    * Subtracting 1 from the rule label's length gives the number of atoms it
    * contains: the list variable is not counted because it can match the
    * empty list. */
   PTFI("if(label.length < %d) break;\n", indent + 3, label.length - 1); 
   if(label.length == 2)
   {
      PTFI("if(label.length == 1)\n", indent + 3);
      PTFI("{\n", indent + 3);
      /* Match the atom that is not the list variable. */
      if(item->atom->type == VARIABLE && item->atom->variable.type == LIST_VAR)
         item = item->next;
      generateAtomMatchingCode(rule, item->atom, "label.list", indent + 6);
      /* Assign the empty list to the list variable. */
      if(!result_declared)
      {
         PTFI("int result = ", indent + 6);
         result_declared = true;
      }
      else PTFI("result = ", indent + 6);
      PTF("addListAssignment(morphism, %d, NULL);\n", list_variable_id);
      generateVariableResultCode(rule, list_variable_id, true, indent + 6);
      PTFI("break;\n", indent + 6);
      PTFI("}\n\n", indent + 3);
   }
   
   item = label.list->first;
   result_declared = false;
   PTFI("/* Matching from the start of the host list. */\n", indent + 3);
   PTFI("HostListItem *item = label.list.list->first;\n", indent + 3);
   int atom_count = 1;
   while(item != NULL)
   {
      if(item->atom->type == VARIABLE && item->atom->variable.type == LIST_VAR) break;
      PTFI("/* Check if the end of the host list has been reached. */\n", indent + 3);
      PTFI("if(item == NULL) break;\n", indent + 3);
      PTFI("/* Matching rule atom %d. */\n", indent + 3, atom_count);
      generateAtomMatchingCode(rule, item->atom, "item->atom", indent + 3);
      PTFI("item = item->next;\n\n", indent + 3);
      atom_count++;
      item = item->next;
   }
   if(!result_declared)
   {
      PTFI("int result;\n", indent + 3);
      result_declared = true;
   }
   item = label.list->last;
   PTFI("HostListItem *start = item;\n", indent + 3);
   int host_atoms_matched = atom_count -1;
   atom_count = label.length;
   if(item->atom->type == VARIABLE && item->atom->variable.type == LIST_VAR)
   {
      PTFI("if(start == NULL) result = addListAssignment(morphism, %d, NULL);\n", 
         indent + 3, list_variable_id);
   }
   else
   {
      PTFI("/* The current host list position marks the start of the list that is\n", indent + 3);
      PTFI("   assigned to the list variable. */\n", indent + 3);
      PTFI("/* More rule atoms to match. If the end of the host list is reached, break. */\n",
         indent + 3);
      PTFI("if(start == NULL) break;\n\n", indent + 3);
      PTFI("/* Matching from the end of the host list. */\n", indent + 3);
      PTFI("item = label.list.list->last;\n", indent + 3);
      while(item != NULL)
      {
         if(item->atom->type == VARIABLE && item->atom->variable.type == LIST_VAR) break;
         PTFI("/* Check if the host list has passed \"start\". */\n", indent + 3);
         PTFI("if(item == start->prev) break;\n", indent + 3);
         PTFI("/* Matching rule atom %d */\n", indent + 3, atom_count);
         generateAtomMatchingCode(rule, item->atom, "item->atom", indent + 3);
         PTFI("item = item->prev;\n", indent + 3);
         atom_count--;
         item = item->prev;
      }
   }
   /* Assign the list variable to the rest of the host list. */
   PTFI("/* Matching list variable %d. */\n", indent + 3, list_variable_id);

   /* All host atoms are matched: assign the empty list to the list variable. */
   PTFI("if(item == start->prev) ", indent + 3);
   PTF("result = addListAssignment(morphism, %d, NULL);\n", list_variable_id);

   /* All but 1 host atoms are matched: assign the remaining host atom to the list variable. */
   PTFI("else if(item == start)\n", indent + 3);
   PTFI("{\n", indent + 3);
   PTFI("if(item->atom.type == 'i') result = addIntegerAssignment(morphism, %d, item->atom.num);\n", 
      indent + 6, list_variable_id);
   PTFI("else result = addStringAssignment(morphism, %d, item->atom.str);\n", 
      indent + 6, list_variable_id);
   PTFI("}\n", indent + 3);

   /* More than one host atoms are unmatched: assign the unmatched sublist to the list variable. */
   PTFI("else\n", indent + 3);
   PTFI("{\n", indent + 3);
   PTFI("/* Assign to variable %d the unmatched sublist of the host list. */\n",
         indent + 6, list_variable_id);
   host_atoms_matched += (label.length - atom_count);
   PTFI("HostAtom sublist[label.length - %d];\n", indent + 6, host_atoms_matched);
   PTFI("int list_index = 0;\n", indent + 6);
   PTFI("HostListItem *iterator = start;\n", indent + 6);
   PTFI("while(start != item->next)\n", indent + 6);
   PTFI("{\n", indent + 6);
   PTFI("sublist[list_index++] = iterator->atom;\n", indent + 9);
   PTFI("item = item->next;\n", indent + 9);
   PTFI("}\n", indent + 6);
   PTFI("HostList *list = makeHostList(sublist, label.length - %d, false);\n", indent + 6,  
      host_atoms_matched);
   PTFI("result = addListAssignment(morphism, %d, list);\n", indent + 6,
         list_variable_id);
   PTFI("}\n", indent + 3);

   generateVariableResultCode(rule, list_variable_id, true, indent + 3);
   PTFI("} while(false);\n\n", indent);
   /* Reset the flag before function exit. */
   result_declared = false;
}

static void generateAtomMatchingCode(Rule *rule, RuleAtom *atom, 
                                     string host_item, int indent)
{
   switch(atom->type)
   {
      case VARIABLE:
           generateVariableMatchingCode(rule, atom, host_item, indent);
           break;
      
      case INTEGER_CONSTANT:
           PTFI("if(%s.type != 'i') break;\n", indent, host_item);
           PTFI("if(%s.num != %d) break;\n", indent, host_item, atom->number);
           break;

      case STRING_CONSTANT:
           PTFI("if(%s.type != 's') break;\n", indent, host_item);
           PTFI("if(strcmp(%s.str, \"%s\") != 0) break;\n",
                indent, host_item, atom->string);
           break;

      case CONCAT:
           PTFI("if(%s.type != 's') break;\n", indent, host_item);
           generateConcatMatchingCode(rule, atom, indent);
           break;

      default:
           print_to_log("Error (generateAtomMatchingCode): Unexpected "
                        "atom type %d.\n", atom->type);
           break;
   }
}

static void generateVariableMatchingCode(Rule *rule, RuleAtom *atom, 
                                         string host_item, int indent)
{
   if(!result_declared)
   {
      PTFI("int result;\n", indent);
      result_declared = true;
   }
   switch(atom->variable.type)
   {
      case INTEGER_VAR:
           PTFI("/* Matching integer variable %d. */\n", indent, atom->variable.id);
           PTFI("if(%s.type != 'i') break;\n", indent, host_item);
           PTFI("result = addIntegerAssignment(morphism, %d, %s.num);\n",
                indent, atom->variable.id, host_item);
           generateVariableResultCode(rule, atom->variable.id, false, indent);
           break;

      case CHARACTER_VAR:
           PTFI("/* Matching character variable %d. */\n", indent, atom->variable.id);
           PTFI("if(%s.type != 's') break;\n", indent, host_item);
           PTFI("if(strlen(%s.str) != 1) break;\n", indent, host_item);
           PTFI("result = addStringAssignment(morphism, %d, %s.str);\n", 
                indent, atom->variable.id, host_item);
           generateVariableResultCode(rule, atom->variable.id, false, indent);
           break;

      case STRING_VAR:
           PTFI("/* Matching string variable %d. */\n", indent, atom->variable.id);
           PTFI("if(%s.type != 's') break;\n", indent, host_item);
           PTFI("result = addStringAssignment(morphism, %d, %s.str);\n",
                indent, atom->variable.id, host_item);
           generateVariableResultCode(rule, atom->variable.id, false, indent);
           break;

      case ATOM_VAR:
           PTFI("/* Matching atom variable %d. */\n", indent, atom->variable.id);
           PTFI("if(%s.type == 'i')\n", indent, host_item);
           PTFI("result = addIntegerAssignment(morphism, %d, %s.num);\n",
                indent + 3, atom->variable.id, host_item);
           PTFI("else /* %s.type == 's' */\n", indent, host_item);
           PTFI("result = addStringAssignment(morphism, %d, %s.str);\n",
                indent + 3, atom->variable.id, host_item);
           generateVariableResultCode(rule, atom->variable.id, false, indent);
           break;

      case LIST_VAR:
           print_to_log("Error (generateAtomMatchingCode): A item "
                        "variable sneaked into this function!\n");
           break;

      default:
           print_to_log("Error (generateVariableMatchingCode): Unexpected "
                        "variable type %d.\n", atom->variable.type);
           break;
   }
}

static void generateConcatMatchingCode(Rule *rule, RuleAtom *atom, int indent)
{
   StringList *list = NULL;
   list = stringExpToList(list, atom->bin_op.left_exp);
   list = stringExpToList(list, atom->bin_op.right_exp);

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
   PTFI("string host_string = item->atom.str;\n", indent);
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
   PTFI("/* Matching string variable %d. */\n", indent, iterator->variable_id);
   PTFI("if(end == start - 1) ", indent);
   PTF("result = addStringAssignment(morphism, %d, \"\");\n", iterator->variable_id);
   PTFI("else\n", indent);
   PTFI("{\n", indent);
   PTFI("char substring[end - start + 1];\n", indent + 3);
   PTFI("strncpy(substring, host_string + start, end - start + 1);\n", indent + 3);
   PTFI("substring[end - start + 1] = '\\0';\n", indent + 3);
   PTFI("result = addStringAssignment(morphism, %d, substring);\n", 
        indent + 3, iterator->variable_id);
   generateVariableResultCode(rule, iterator->variable_id, false, indent);
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
              indent, string_exp->constant);
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
              indent, string_exp->constant);
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
      PTFI("/* Matching character variable %d. */\n", indent, string_exp->variable_id);
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
      PTFI("result = addStringAssignment(morphism, %d, host_character);\n",
           indent, string_exp->variable_id);
      generateVariableResultCode(rule, string_exp->variable_id, false, indent);
   }
}

void generateVariableResultCode(Rule *rule, int id, bool list_variable, int indent)
{
   PTFI("if(result >= 0)\n", indent);
   PTFI("{\n", indent);
   PTFI("new_assignments += result;\n", indent + 3);
   assert(id < rule->variables);
   Variable variable = rule->variable_list[id];
   if(variable.predicates != NULL)
   {
      PTFI("/* Update global booleans for the variable's predicates. */\n", indent + 3);
      int index;
      for(index = 0; index < variable.predicate_count; index++)
         PTFI("evaluatePredicate%d(morphism);\n", indent + 3, 
              variable.predicates[index]->bool_id);
      PTFI("if(!evaluateCondition())\n", indent + 3);
      PTFI("{\n", indent + 3);
      PTFI("/* Reset the boolean variables in the predicates of this variable. */\n", 
           indent + 6);
      for(index = 0; index < variable.predicate_count; index++)
      { 
         Predicate *predicate = variable.predicates[index];
         if(predicate->negated) PTFI("b%d = false;\n", indent + 6, predicate->bool_id);
         else PTFI("b%d = true;\n", indent + 6, predicate->bool_id);
      }
      PTFI("break;\n", indent + 6);
      PTFI("}\n", indent + 3);
   }
   if(list_variable) PTFI("match = true;\n", indent + 3);
   PTFI("}\n", indent);
   if(!list_variable) PTFI("else break;\n", indent); 
}

/* For each variable in the rule, code is generated to assign that variable's value
 * (from the morphism) to a local variable for use by the rule application code.
 * This prevents duplication if code if a variable occurs more than once in the RHS.
 *
 * Uniqueness of variable names in a rule ensures uniqueness of the runtime 
 * variable name generated. */
void generateVariableCode(int id, GPType type)
{
   switch(type)
   {
      case INTEGER_VAR:
           PTFI("int var_%d = getIntegerValue(morphism, %d);\n", 3, id, id);
           break;

      case CHARACTER_VAR:
      case STRING_VAR:
           PTFI("string var_%d = getStringValue(morphism, %d);\n", 3, id, id);
           break;

      case ATOM_VAR:
           PTFI("GP2List var_%d = getListValue(morphism, %d);\n", 3, id, id);
           /* Values of atom variables are naturally encoded as a C union at runtime. 
           PTFI("\n   Assignment assignment_%d = lookupAssignment(morphism, %d);\n",
                3, id, id);
           PTFI("union { int num; string str; } var_%d;\n", 3, id);
           PTFI("if(assignment_%d.type == 'i') "
                "var_%d.num = getIntegerValue(morphism, %d);\n", 3, id, id, id);
           PTFI("else var_%d.str = getStringValue(morphism, %d);\n\n", 3, id, id); */
           break;
         
      case LIST_VAR:
           PTFI("GP2List var_%d = getListValue(morphism, %d);\n", 3, id, id);
           break;
      
      default:
           print_to_log("Error (generateVariableCode): Unexpected type %d\n", type);
           break;
   }
}

/* Integers used to generate fresh runtime variables "length", and "host_label"
 * which could be used multiple times or none at all. This will avoid repeated
 * declarations errors and unused variable warnings in the generated C code. */
int host_label_count = 0, length_count = 0;

/* Labels to be evaluated occur in different contexts, each requiring slightly 
 * different code to be generated, although the overall code skeleton is the same. 
 * These contexts are identified by the 'context' argument:
 * 0 - RHS label
 * 1 - Label argument of the edge predicate in a condition.
 * 2 - First list argument of a relational expression in a condition.
 * 3 - Second list argument of a relational expression in a condition. 
 *
 * The 'count' argument is used to generate unique variable names. */
void generateLabelEvaluationCode(RuleLabel label, bool node, int count, int context, int indent)
{
   /* Contexts 0 and 1 require code to populate an array of atoms and create a 
    * host label from them. Contexts 2 and 3 require only the list component and
    * its length, not the full label structure. */
   assert(context >= 0 && context < 4);
   if(label.length == 0)
   { 
      if(context < 2)
      {
         /* If the RHS has the 'ANY' mark, generate code to retrieve the mark of the
         * host item's label. */
         if(label.mark == ANY)
         {
            if(node) PTFI("HostLabel host_label%d = getNodeLabel(host, host_node_index);\n", 
                        indent, host_label_count);
            else PTFI("HostLabel host_label%d = getEdgeLabel(host, host_edge_index);\n", 
                     indent, host_label_count);
            PTFI("label = makeEmptyLabel(host_label%d.mark);\n\n", indent, host_label_count);
         }
         else PTFI("label = makeEmptyLabel(%d);\n\n", indent, label.mark);
         host_label_count++;
      }
      else PTFI("int length%d = 0;\n\n", indent, count);
      return;
   }
   /* The length of the evaluated list is not static because right labels contain an
    * arbitrary number of list variables. For each list variable in the label, add
    * its length to the runtime accumulator <list_var_length>. A compile-time
    * accumulator <number_of_atoms> counts the number of non-list-variable atoms. */
   int number_of_atoms = 0;
   PTFI("int list_var_length%d = 0;\n", indent, count);
   RuleListItem *item = label.list->first;
   while(item != NULL)
   {
      if(item->atom->type == VARIABLE && item->atom->variable.type == LIST_VAR)
         PTFI("list_var_length%d += getListVariableLength(var_%d);\n", 
              indent, count, item->atom->variable.id);
      else number_of_atoms++;
      item = item->next;
   }
   PTFI("int list_length%d = list_var_length%d + %d;\n", indent, count, count, number_of_atoms);
   PTFI("HostAtom array%d[list_length%d];\n", indent, count, count);
   PTFI("int index%d = 0;\n\n", indent, count);
   /* Generate code to build the list. */
   item = label.list->first;
   while(item != NULL)
   {
      RuleAtom *atom = item->atom;
      switch(atom->type)
      {
         case INTEGER_CONSTANT:
              PTFI("array%d[index%d].type = 'i';\n", indent, count, count);
              PTFI("array%d[index%d++].num = %d;\n", indent, count, count, atom->number);
              break;

         case STRING_CONSTANT:
              PTFI("array%d[index%d].type = 's';\n", indent, count, count);
              PTFI("array%d[index%d++].str = \"%s\";\n", indent, count, count, atom->string);
              break;

         case VARIABLE:
         {
              /* Use the <variable_name>_var variables generated previously to
               * add the correct values to the runtime list. */
              int id = atom->variable.id;
              if(atom->variable.type == INTEGER_VAR)
              {
                 PTFI("array%d[index%d].type = 'i';\n", indent, count, count);
                 PTFI("array%d[index%d++].num = var_%d;\n", indent, count, count, id);
              }
              else if(atom->variable.type == CHARACTER_VAR ||
                      atom->variable.type == STRING_VAR)
              {
                 PTFI("array%d[index%d].type = 's';\n", indent, count, count);
                 PTFI("array%d[index%d++].str = var_%d;\n", indent, count, count, id);
              }
              else if(atom->variable.type == ATOM_VAR)
              {
                 PTFI("if(var_%d.type == 'i')\n", indent, id);
                 PTFI("{\n", indent);
                 PTFI("array%d[index%d].type = 'i';\n", indent + 3, count, count);
                 PTFI("array%d[index%d++].num = var_%d.num;\n",
                      indent + 3, count, count, id);
                 PTFI("}\n", indent);
                 PTFI("else /* var_%d.type == 's' */\n", indent, id);
                 PTFI("{\n", indent);
                 PTFI("array%d[index%d].type = 's';\n", indent + 3, count, count);
                 PTFI("array%d[index%d++].str = var_%d.str;\n",
                      indent + 3, count, count, id);
                 PTFI("}\n", indent);
              }  
              else if(atom->variable.type == LIST_VAR)
              {
                 PTFI("if(var_%d.type == 'l' && var_%d.list != NULL)\n", indent, id, id);
                 PTFI("{\n", indent);
                 PTFI("HostListItem *item%d = var_%d.list->first;\n", indent + 3, count, id);
                 PTFI("while(item%d != NULL)\n", indent + 3, count);
                 PTFI("{\n", indent + 3);
                 PTFI("array%d[index%d++] = item%d->atom;\n", indent + 6, count, count, count);
                 PTFI("item%d = item%d->next;\n", indent + 6, count, count);
                 PTFI("}\n", indent + 3);
                 PTFI("}\n", indent);
                 PTFI("else if(var_%d.type == 'i')\n", indent, id);
                 PTFI("{\n", indent);
                 PTFI("array%d[index%d].type = 'i';\n", indent + 3, count, count);
                 PTFI("array%d[index%d++].num = var_%d.num;\n", indent + 3, count, count, id);
                 PTFI("}\n", indent);
                 PTFI("else if(var_%d.type == 's')\n", indent, id);
                 PTFI("{\n", indent);
                 PTFI("array%d[index%d].type = 's';\n", indent + 3, count, count);
                 PTFI("array%d[index%d++].str = var_%d.str;\n", indent + 3, count, count, id);
                 PTFI("}\n\n", indent);
              }
              break;
         }
         /* When evaluating RHS labels, the results of degree operators in labels are
          * stored in variables by generateVariableCode. This is not the case for
          * degree operators in predicates: their values must be obtained directly. */
         case INDEGREE:
              PTFI("array%d[index%d].type = 'i';\n", indent, count, count); 
              if(context == 0)
                   PTFI("array%d[index%d++].num = indegree%d;\n", 
                        indent, count, count, atom->node_id); 
              else PTFI("array%d[index%d++].num = getIndegree(host, n%d);\n", 
                        indent, count, count, atom->node_id); 
              break;
           
         case OUTDEGREE:
              PTFI("array%d[index%d].type = 'i';\n", indent, count, count); 
              if(context == 0)
                   PTFI("array%d[index%d++].num = outdegree%d;\n", 
                        indent, count, count, atom->node_id); 
              else PTFI("array%d[index%d++].num = getOutdegree(host, n%d);\n", 
                        indent, count, count, atom->node_id); 
              break;

         case LENGTH:
         case NEG:
         case ADD:
         case SUBTRACT:
         case MULTIPLY:
         case DIVIDE:  
              PTFI("array%d[index%d].type = 'i';\n", indent, count, count); 
              PTFI("array%d[index%d++].num = ", indent, count, count);
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
              PTFI("array%d[index%d].type = 's';\n", indent, count, count); 
              PTFI("array%d[index%d++].str = host_string%d;\n", indent, count, count, length_count);
              length_count++;
              break;
      
         default:
              print_to_log("Error (generateLabelCode): Unexpected host atom "
                           "type %d.\n", atom->type);
              break;
      }
      item = item->next;
   }
   if(context < 2)
   {
      /* If the RHS has the 'ANY' mark, generate code to retrieve the mark of the
       * host item's label. */
      if(label.mark == ANY)
      {
         if(node) PTFI("HostLabel host_label%d = getNodeLabel(host, host_node_index);\n",
                       indent, host_label_count);
         else PTFI("HostLabel host_label%d = getEdgeLabel(host, host_edge_index);\n",
                   indent, host_label_count);
         host_label_count++;
      }
      PTFI("if(list_length%d == 1)\n", indent, count);
      PTFI("{\n", indent);
      PTFI("if(array%d[0].type == 'i') ", indent + 3, count);
      if(label.mark == ANY)
         PTF("label = makeIntegerLabel(host_label%d.mark, array%d[0].num);\n",
             host_label_count, count);
      else PTF("label = makeIntegerLabel(%d, array%d[0].num);\n", label.mark, count);
      PTFI("else ", indent + 3);
      if(label.mark == ANY)
         PTF("label = makeStringLabel(host_label%d.mark, array%d[0].str);\n",
             host_label_count, count);
      else PTF("label = makeStringLabel(%d, array%d[0].str);\n", label.mark, count);
      PTFI("}\n", indent);
      PTFI("else\n", indent);
      PTFI("{\n", indent);
      PTFI("HostList *list%d = makeHostList(array%d, list_length%d, false);\n",
           indent + 3, count, count, count);
      if(label.mark == ANY)
          PTFI("label = makeListLabel(host_label%d.mark, list_length%d, list%d);\n\n", 
               indent + 3, host_label_count, count, count);
      else PTFI("label = makeListLabel(%d, list_length%d, list%d);\n", 
               indent + 3, label.mark, count, count);
      PTFI("}\n\n", indent);
   }
   else PTF("\n");
}

/* Navigates an integer expression tree and writes the arithmetic expression it 
 * represents. For example, given the label (i + 1) * length(s), where i is an
 * integer variable and s is a string variable, generateIntExpression prints:
 * (i_var + 1) * (int)strlen(s_var); */
void generateIntExpression(RuleAtom *atom, int context, bool nested)
{
   switch(atom->type)
   {
      case INTEGER_CONSTANT:
           PTF("%d", atom->number);
           break;

      case LENGTH:
           if(atom->variable.type == STRING_VAR)
              PTF("(int)strlen(var_%d)", atom->variable.id);

           else if(atom->variable.type == ATOM_VAR)
              PTF("((var_%d.type == 's') ? (int)strlen(var_%d.str) : 1)", 
                  atom->variable.id, atom->variable.id);

           else if(atom->variable.type == LIST_VAR)
              PTF("getListVariableLength(var_%d)", atom->variable.id);

           else
              print_to_log("Error (generateIntegerExpressionCode): Unexpected "
                           "variable type of length argument.\n");
           break;
      
      case VARIABLE:
           if(atom->variable.type == ATOM_VAR) PTF("var_%d.num", atom->variable.id);
           else PTF("var_%d", atom->variable.id);
           break;

      case INDEGREE:
           if(context == 0) PTF("indegree%d", atom->node_id);
           else PTF("getIndegree(host, n%d)", atom->node_id);
           break;

      case OUTDEGREE:
           if(context == 0) PTF("outdegree%d", atom->node_id);
           else PTF("getOutdegree(host, n%d)", atom->node_id);
           break;

      case NEG:
           PTF("(-%d)", atom->number);
           break;

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
           if(nested) PTF("(");
           generateIntExpression(atom->bin_op.left_exp, context, true);
           if(atom->type == ADD) PTF(" + ");
           if(atom->type == SUBTRACT) PTF(" - ");
           if(atom->type == MULTIPLY) PTF(" * ");
           if(atom->type == DIVIDE) PTF(" / ");
           generateIntExpression(atom->bin_op.right_exp, context, true);
           if(nested) PTF(")");
           break;

      default:
           print_to_log("Error (generateIntegerExpression): Unexpected "
                        "atom type %d.\n", atom->type);
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
void generateStringLengthCode(RuleAtom *atom, int indent)
{
   switch(atom->type)
   {
      case STRING_CONSTANT:
           PTFI("length%d += strlen(\"%s\");\n", indent, length_count, atom->string);
           break;

      case VARIABLE:
           PTFI("length%d += strlen(var_%d);\n", indent, length_count, atom->variable.id);
           break;

      case CONCAT:
           generateStringLengthCode(atom->bin_op.left_exp, indent);
           generateStringLengthCode(atom->bin_op.right_exp, indent);
           break;
          
      default:
           print_to_log("Error (generateStringEvalCode): Unexpected "
                        "atom type %d.\n", atom->type);
           break;
   }
}

void generateStringExpression(RuleAtom *atom, bool first, int indent)
{
   switch(atom->type)
   { 
      case STRING_CONSTANT:
           if(first) PTFI("strcpy(host_string%d, \"%s\");\n", indent, 
                          length_count, atom->string);
           else PTFI("strcat(host_string%d, \"%s\");\n", indent, 
                     length_count, atom->string);

      case VARIABLE:
           if(first) PTFI("strcpy(host_string%d, var_%d);\n", indent, 
                          length_count, atom->variable.id);
           else PTFI("strcat(host_string%d, var_%d);\n", indent, 
                     length_count, atom->variable.id);
           break;

      case CONCAT:
           generateStringExpression(atom->bin_op.left_exp, first, indent);
           generateStringExpression(atom->bin_op.right_exp, false, indent);
           break;
          
      default:
           print_to_log("Error (generateStringExpression): Unexpected "
                        "atom type %d.\n", atom->type);
           break;
   }
}
