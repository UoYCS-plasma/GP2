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
   /* A do-while loop is generated so that the label matching code can be exited
    * at any time with a break statement the moment an atom match fails. */
   PTFI("do\n", indent);
   PTFI("{\n", indent);
   PTFI("/* The rule list does not contain a list variable, so there is no\n", indent + 3);
   PTFI(" * match if the host list has a different length. */\n", indent + 3);
   PTFI("if(label.length != %d) break;\n", indent + 3, label.length); 

   /* Lists without list variables admit relatively simple code generation as each
    * rule atom maps directly to the host atom in the same position. */
   GPList *list = label.first;
   PTFI("GPList *list = label.first;\n", indent + 3);
   int atom_count = 0;
   while(list != NULL)
   {
      PTFI("/* Check if the end of the host list has been reached. */\n", indent + 3);
      PTFI("if(list == NULL) break;\n", indent + 3);
      Atom atom = list->atom;
      PTFI("/* Matching rule atom %d. */\n", indent + 3, atom_count);
      generateAtomMatchingCode(rule, atom, indent + 3);
      PTFI("list = list->next;\n\n", indent + 3);
      atom_count++;
      list = list->next;
   }
   PTFI("/* If there are no more host atoms to match, success! */\n", indent + 3);
   PTFI("if(list == NULL) match = true;\n", indent + 3);
   PTFI("} while(false);\n\n", indent);
   /* Reset the flag before function exit. */
   result_declared = false;
}

void generateVariableListMatchingCode(Rule *rule, Label label, int indent)
{ 
   PTFI("/* Label Matching */\n", indent);
   PTFI("int new_assignments = 0;\n", indent);
   string list_variable_name = NULL;
   GPList *list = label.first;
   while(list != NULL)
   {
      if(list->atom.type == VARIABLE && list->atom.variable.type == LIST_VAR)
      {
         list_variable_name = list->atom.variable.name;
         break;
      }
   }

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
      PTFI("GPList *list = copyList(label.first, NULL);\n", indent);
      PTFI("result = addListAssignment(\"%s\", list, morphism);\n",
           indent, list_variable_name);
      generateVariableResultCode(rule, list_variable_name, true, indent);
      /* Reset the flag before function exit. */
      result_declared = false;
      return;
   }

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
   PTFI("GPList *list = label.first;\n", indent + 3);
   list = label.first;
   int atom_count = 0;
   while(list != NULL)
   {
      if(list->atom.type == VARIABLE && list->atom.variable.type == LIST_VAR) break;
      PTFI("/* Check if the end of the host list has been reached. */\n", indent + 3);
      PTFI("if(list == NULL) break;\n", indent + 3);
      Atom atom = list->atom;
      PTFI("/* Matching rule atom %d. */\n", indent + 3, atom_count);
      generateAtomMatchingCode(rule, atom, indent + 3);
      PTFI("list = list->next;\n\n", indent + 3);
      atom_count++;
      list = list->next;
   }   

   PTFI("/* The current host list position marks the start of the list that is\n", indent + 3);
   PTFI("   assigned to the list variable. */\n", indent + 3);
   PTFI("GPList *start = list;\n\n", indent + 3);
   /* <start> may be NULL, in which case a match is only possible if the last rule 
    * list element is the list variable, which can be assigned the empty list. */
   list = label.last;
   if(list->atom.type == VARIABLE && list->atom.variable.type == LIST_VAR)
      PTFI("result = addListAssignment(\"%s\", NULL, morphism);\n", 
           indent + 3, list_variable_name);
   else
   {
      PTFI("/* More rule atoms to match. If the end of the host list is reached, break. */\n",
           indent + 3);
      PTFI("if(start == NULL) break;\n\n", indent + 3);
      PTFI("/* Matching from the end of the host list. */\n", indent + 3);
      PTFI("list = label.last;\n", indent + 3);
      list = label.last;
      atom_count = label.length;
      while(list != NULL)
      {
         if(list->atom.type == VARIABLE && list->atom.variable.type == LIST_VAR) break;
         PTFI("/* Check if the host list has passed \"start\". */\n", indent + 3);
         PTFI("if(list == start->prev) break;\n", indent + 3);
         Atom atom = list->atom;
         PTFI("/* Matching rule atom %d */\n", indent + 3, atom_count);
         generateAtomMatchingCode(rule, atom, indent + 3);
         PTFI("list = list->prev;\n", indent + 3);
         atom_count--;
         list = list->prev;
      }
      /* Assign the list variable to the rest of the host list. */
      if(!result_declared)
      {
         PTFI("int result;\n", indent + 3);
         result_declared = true;
      }
      PTF("\n");
      PTFI("/* Matching list variable \"%s\". */\n", indent + 3, list_variable_name);
      PTFI("if(list == start->prev)\n", indent + 3);
      PTFI("result = addListAssignment(\"%s\", NULL, morphism);\n", 
         indent + 6, list_variable_name);
      PTFI("else\n", indent + 3);
      PTFI("{\n", indent + 3);
      PTFI("/* Assign the unmatched sublist of the host list to \"%s\". */\n",
         indent + 6, list_variable_name);
      PTFI("GPList *sublist = NULL;\n", indent + 6);
      PTFI("sublist = copyList(start, list->next);\n", indent + 6);
      PTFI("result = addListAssignment(\"%s\", sublist, morphism);\n", indent + 6,
           list_variable_name);
      PTFI("}\n", indent + 3);
      generateVariableResultCode(rule, list_variable_name, true, indent + 3);
      PTFI("} while(false);\n\n", indent);
   }
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
           PTFI("if(list->atom.type != INTEGER_CONSTANT) break;\n", indent);
           PTFI("else if(list->atom.number != %d) break;\n", indent, atom.number);
           break;

      case STRING_CONSTANT:
           PTFI("if(list->atom.type != STRING_CONSTANT) break;\n", indent);
           PTFI("else if(strcmp(list->atom.string, \"%s\") != 0) break;\n",
                indent, atom.string);
           break;

      case CONCAT:
           PTFI("if(list->atom.type != STRING_CONSTANT) break;\n", indent);
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
           PTFI("if(list->atom.type != INTEGER_CONSTANT) break;\n", indent);
           PTFI("result = addIntegerAssignment(\"%s\", list->atom.number, morphism);\n",
                indent, atom.variable.name);
           generateVariableResultCode(rule, atom.variable.name, false, indent);
           break;

      case CHARACTER_VAR:
           PTFI("/* Matching character variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(list->atom.type != STRING_CONSTANT) break;\n", indent);
           PTFI("if(strlen(list->atom.string) != 1) break;\n", indent);
           PTFI("result = addStringAssignment(\"%s\", list->atom.string, morphism);\n", 
                indent, atom.variable.name);
           generateVariableResultCode(rule, atom.variable.name, false, indent);
           break;

      case STRING_VAR:
           PTFI("/* Matching string variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(list->atom.type != STRING_CONSTANT) break;\n", indent);
           PTFI("result = addStringAssignment(\"%s\", list->atom.string, morphism);\n",
                indent, atom.variable.name);
           generateVariableResultCode(rule, atom.variable.name, false, indent);
           break;

      case ATOM_VAR:
           PTFI("/* Matching atom variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(list->atom.type == INTEGER_CONSTANT)\n", indent);
           PTFI("result = addIntegerAssignment(\"%s\", list->atom.number, morphism);\n",
                indent, atom.variable.name);
           PTFI("else if(list->atom.type == STRING_CONSTANT)\n", indent);
           PTFI("result = addStringAssignment(\"%s\", list->atom.string, morphism);\n",
                indent, atom.variable.name);
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
   PTFI("string host_string = label.list[host_list_position].string;\n", indent);
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
   PTFI("new_assignments += result;\n", indent + 3);
   Variable *variable = getVariable(rule, name);
   assert(variable != NULL);
   if(variable->predicates != NULL)
   {
      PTFI("/* Update global booleans for the variable's predicates. */\n", indent + 3);
      int index;
      for(index = 0; index < variable->predicate_count; index++)
         PTFI("evaluatePredicate%d(morphism);\n", indent + 3, 
              variable->predicates[index]->bool_id);
      PTFI("if(!evaluateCondition())\n", indent + 3);
      PTFI("{\n", indent + 3);
      PTFI("/* Reset the boolean variables in the predicates of this variable. */\n", 
           indent + 6);
      for(index = 0; index < variable->predicate_count; index++)
      { 
         Predicate *predicate = variable->predicates[index];
         if(predicate->negated) PTFI("b%d = false;\n", indent + 6, predicate->bool_id);
         else PTFI("b%d = true;\n", indent + 6, predicate->bool_id);
      }
      if(!list_variable) PTFI("break;\n", indent + 6);
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
           PTFI("GPList *%s_var = getListValue(\"%s\", morphism);\n", 3, name, name);
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
 * 2 - First label argument of a relational expression in a condition.
 * 3 - Second label argument of a relational expression in a condition. 
 *
 * The 'count' argument is used to generate unique variable names. */
void generateLabelEvaluationCode(Label label, bool node, int count, int context, int indent)
{
   /* The name of the runtime variable to store the label depends on its context.
    * In particular, labels in relational expressions are directly compared, so they
    * require different names. */
   assert(context >= 0 && context < 4);
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
         if(node) PTFI("Label host_label%d = getNodeLabel(host, host_node_index);\n", 
                       indent, host_label_count);
         else PTFI("Label host_label%d = getEdgeLabel(host, host_edge_index);\n", 
                   indent, host_label_count);
         PTFI("%s = makeEmptyLabel(host_label%d.mark);\n\n", indent, label_name,
              host_label_count);
      }
      else PTFI("%s = makeEmptyLabel(%d);\n\n", indent, label_name, label.mark);
      host_label_count++;
      return;
   }

   PTFI("GPList *list%d = NULL;\n", indent, count);
   /* Generate code to build the label. */
   GPList *list = label.first;
   while(list != NULL)
   {
      Atom atom = list->atom;
      switch(atom.type)
      {
         case INTEGER_CONSTANT:
              PTFI("list%d = appendIntegerAtom(list%d, %d);\n",
                   indent, count, count, atom.number);
              break;

         case STRING_CONSTANT:
              PTFI("list%d = appendStringAtom(list%d, \"%s\");\n",
                   indent, count, count, atom.string);
              break;

         case VARIABLE:
         {
              /* Use the <variable_name>_var variables generated previously to
               * add the correct values to the runtime list. */
              string name = atom.variable.name;
              if(atom.variable.type == INTEGER_VAR)
                 PTFI("list%d = appendIntegerAtom(list%d, %s_var);\n", 
                      indent, count, count, name);
              else if(atom.variable.type == CHARACTER_VAR ||
                      atom.variable.type == STRING_VAR)
              {
                 PTFI("list%d = appendStringAtom(list%d, %s_var);\n", 
                      indent, count, count, name);
              }
              else if(atom.variable.type == ATOM_VAR)
              {
                 PTFI("if(assignment_%s->type == INTEGER_VAR)\n", indent, name);
                 PTFI("list%d = appendIntegerAtom(list%d, %s_var.num);\n", 
                      indent + 3, count, count, name);
                 PTFI("else /* type == STRING_CONSTANT */\n", indent);
                 PTFI("list%d = appendStringAtom(list%d, %s_var.str);\n", 
                      indent + 3, count, count, name);
              }
              else if(atom.variable.type == LIST_VAR)
                 PTFI("list%d = appendList(list%d, %s_var);\n", 3, count, count, name);
              break;
         }
         /* When evaluating RHS labels, the results of degree operators in labels are
          * stored in variables by generateVariableCode. This is not the case for
          * degree operators in predicates: their values must be obtained directly. */
         case INDEGREE:
              if(context == 0)
                   PTFI("list%d = appendIntegerAtom(list%d, indegree%d);\n", 
                        indent, count, count, atom.node_id); 
              else PTFI("list%d = appendIntegerAtom(list %d, getIndegree(host, %d));\n", 
                        indent, count, count, atom.node_id); 
              break;
           
         case OUTDEGREE:
              if(context == 0)
                   PTFI("list%d = appendIntegerAtom(list%d, outdegree%d);\n", 
                        indent, count, count, atom.node_id); 
              else PTFI("list%d = appendIntegerAtom(list %d, getOutdegree(host, %d));\n", 
                        indent, count, count, atom.node_id); 
              break;

         case LENGTH:
         case NEG:
         case ADD:
         case SUBTRACT:
         case MULTIPLY:
         case DIVIDE:  
              PTFI("list%d = appendIntegerAtom(list%d, ", indent, count, count);
              generateIntExpression(atom, context, false);
              PTF(");\n");
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
              PTFI("list%d = appendStringAtom(list%d, host_string%d);\n", 
                   indent, count, count, length_count);
              length_count++;
              break;
      
         default:
              print_to_log("Error (generateLabelCode): Unexpected host atom "
                           "type %d.\n", atom.type);
              break;
      }
      list = list->next;
   }      
   /* The length of the evaluated list is not static because right labels contain an
    * arbitrary number of list variables. For each list variable in the label, add
    * its length to the runtime accumulator <list_var_length>. A compile-time
    * accumulator <number_of_atoms> counts the number of non-list-variable atoms. */
   int number_of_atoms = 0;
   PTFI("int list_var_length%d = 0;\n", indent, count);
   list = label.first;
   while(list != NULL)
   {
      if(list->atom.type == VARIABLE && list->atom.variable.type == LIST_VAR)
         PTFI("list_var_length%d += getListLength(%s_var);\n", 
              indent, count, list->atom.variable.name);
      else number_of_atoms++;
      list = list->next;
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
              PTF("getListLength(%s_var)", atom.variable.name);

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