#include "genLabel.h"

/* Global flag set to true if the 'result' variable has been declared in the
 * emitted code. */
bool result_declared = false;

void generateFixedListMatchingCode(Label label, int indent, FILE *file)
{
   PTFI("/* Label Matching */\n", indent);
   PTFI("int new_assignments = 0;\n", indent);
   PTFI("index = 0;\n", indent);
   PTFI("do {\n", indent);
   PTFI("if(label.length != %d) break;\n", indent + 3, label.length); 
   int index;
   for(index = 0; index < label.length; index++)
   {
      Atom atom = label.list[index];
      PTFI("/* Matching rule atom %d. */\n", indent + 3, index);
      /* Break if the end of the rule list is reached. */
      if(index == label.length) break;
      generateAtomMatchingCode(atom, indent + 3, file);
      PTFI("index++;\n\n", indent + 3);
   }
   PTFI("/* If there are no more host atoms to match, success! */\n", indent + 3);
   PTFI("if(index == label.length) match = true;\n", indent + 3);
   PTFI("} while(false);\n\n", indent);
   result_declared = false;
}

void generateVariableListMatchingCode(Label label, int indent, FILE *file)
{ 
   PTFI("/* Label Matching */\n", indent);
   /* The empty rule list is only matched by the empty host list. */
   if(label.length == 0)
   {
      PTFI("bool match = label.length == 0 ? true : false;\n", indent);
      return;
   }
   
   /* Analyse the label for the list variable index for epic code generation. */
   int list_variable_index = 0, index;
   string list_variable_name;
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
   PTFI("bool match = false;\n", indent);
   PTFI("int new_assignments = 0;\n", indent);
   PTFI("index = 0;\n", indent);
   PTFI("do {\n", indent);
   /* Check if the host label has enough atoms to match those in the rule. 
    * Subtracting 1 from the rule label's length gives the number of atoms it
    * contains. The list variable is not counted because it can match the
    * empty list. */
   PTFI("if(label.length < %d) break;\n", indent + 3, label.length - 1); 
   PTFI("/* Matching from the start of the host list. */\n", indent + 3);
   for(index = 0; index < list_variable_index; index++)
   {
      Atom atom = label.list[index];
      PTFI("/* Matching rule atom %d */\n", indent + 3, index);
      generateAtomMatchingCode(atom, indent + 3, file);
      PTFI("index++;\n", indent + 3);
   }   
   /* Record the current host index. */
   PTFI("int start_index = index;\n", indent + 3);
   PTFI("index = label.length - 1;\n", indent + 3);

   /* Iterate back from the end of the rule list. Note that this loop is not
    * executed if the list variable is the last element in the rule list. */
   PTF("\n");
   PTFI("/* Matching from the end of the host list. */\n", indent + 3);
   for(index = label.length - 1; index > list_variable_index; index--)
   {
      Atom atom = label.list[index];
      PTFI("/* Matching rule atom %d */\n", indent + 3, index);
      generateAtomMatchingCode(atom, indent + 3, file);
      PTFI("index--;\n", indent + 3);
   }
   /* Assign the list variable to the rest of the host list. */
   if(!result_declared)
   {
      PTFI("int result;\n", indent + 3);
      result_declared = true;
   }
   PTF("\n");
   PTFI("/* Matching list variable \"%s\". */\n", indent + 3, list_variable_name);
   PTFI("if(index == start_index - 1)\n", indent + 3);
   PTFI("result = verifyListVariable(%s, NULL, 0, morphism);\n", 
        indent + 6, list_variable_name);
   PTFI("else\n", indent + 3);
   PTFI("{\n", indent + 3);
   PTFI("/* Assign the unmatched sublist of the host list to \"%s\". */\n",
        indent + 3, list_variable_name);
   PTFI("Atom list[index - start_index + 1];\n", indent + 6);
   PTFI("int counter;\n", indent + 6);
   PTFI("for(counter = start_index; counter <= index; counter++)\n", indent + 6);
   PTFI("{\n", indent + 6);
   PTFI("int list_index = counter - start_index;\n", indent + 9);
   PTFI("list[list_index].type = label.list[counter].type;\n", indent + 9);
   PTFI("if(label.list[counter].type == INTEGER_CONSTANT)\n", indent + 9);
   PTFI("   list[list_index].number = label.list[counter].number;\n", indent + 9);
   PTFI("if(label.list[counter].type == STRING_CONSTANT)\n", indent + 9);
   PTFI("   list[list_index].string = label.list[counter].string;\n", indent + 9);
   PTFI("}\n", indent + 6);
   PTFI("result = addListAssignment(\"%s\", list, index - start_index + 1, morphism);\n",
        indent + 6, list_variable_name);
   PTFI("if(result == -1) break;\n", indent + 6);
   PTFI("else\n", indent + 6);
   PTFI("{\n", indent + 6);
   PTFI("new_assignments += result;\n", indent + 9);
   PTFI("match = true;\n", indent + 9);
   PTFI("}\n", indent + 6);
   PTFI("}\n", indent + 3);
   PTFI("} while(false);\n\n", indent);
   result_declared = false;
}

void generateAtomMatchingCode(Atom atom, int indent, FILE *file)
{
   switch(atom.type)
   {
      case VARIABLE:
           generateVariableMatchingCode(atom, indent, file);
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
           generateConcatMatchingCode(atom, indent + 3, file);
           PTFI("}\n", indent);
           break;

      default:
           print_to_log("Error (generateAtomMatchingCode): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}

void generateVariableMatchingCode(Atom atom, int indent, FILE *file)
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
           PTFI("if(result == -1) break; else new_assignments += result;\n", indent);
           break;

      case CHARACTER_VAR:
           PTFI("/* Matching character variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(label.list[index].type != STRING_CONSTANT) break;\n", indent);
           PTFI("if(strlen(label.list[index].string) != 1) break;\n", indent);
           PTFI("result = addStringAssignment(\"%s\", label.list[index].string, "
                "morphism);\n", indent, atom.variable.name);
           PTFI("if(result == -1) break; else new_assignments += result;\n", indent);
           break;

      case STRING_VAR:
           PTFI("/* Matching string variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(label.list[index].type != STRING_CONSTANT) break;\n", indent);
           PTFI("result = addStringAssignment(\"%s\", label.list[index].string, "
                "morphism);\n", indent, atom.variable.name);
           PTFI("if(result == -1) break; else new_assignments += result;\n", indent);
           break;

      case ATOM_VAR:
           PTFI("/* Matching atom variable \"%s\". */\n", indent, atom.variable.name);
           PTFI("if(label.list[index].type == INTEGER_CONSTANT)\n", indent);
           PTFI("result = addIntegerAssignment(\"%s\", label.list[index].number, "
                "morphism);\n", indent, atom.variable.name);
           PTFI("else if(label.list[index].type == STRING_CONSTANT)\n", indent);
           PTFI("result = addStringAssignment(\"%s\", label.list[index].string, "
                "morphism);\n", indent, atom.variable.name);
           PTFI("if(result == -1) break; else new_assignments += result;\n", indent);
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

void generateConcatMatchingCode(Atom atom, int indent, FILE *file)
{
   StringList *list = NULL;
   list = stringExpToList(list, atom.bin_op.left_exp);
   list = stringExpToList(list, atom.bin_op.right_exp);

   /* Assign the index of the string variable to string_variable_index.
    * If there is no string variable, string_variable_index is -1. */
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
   if(!has_string_variable)
   {
      while(iterator != NULL) 
      {
         PTFI("if(start >= strlen(host_string)) break;\n", indent);
         generateStringMatchingCode(iterator, true, indent, file);
         iterator = iterator->next;
      }
   }
   else
   {
      PTFI("/* Matching from the start of the host string. */\n", indent);
      while(iterator->type != 3) 
      {
         PTFI("if(start >= strlen(host_string)) break;\n", indent);
         generateStringMatchingCode(iterator, true, indent, file);
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
         generateStringMatchingCode(iterator, false, indent, file);
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
   PTFI("if(result == -1) break; else new_assignments += result;\n", indent + 3);
   PTFI("}\n", indent);
   freeStringList(list);
}

bool offset_declared = false, host_character_declared = false;
void generateStringMatchingCode(StringList *string_exp, bool prefix, int indent, FILE *file)
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
      PTFI("if(result == -1) break; else new_assignments += result;\n", indent );
   }
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
   StringList *string_exp = malloc(sizeof(StringList));
   if(string_exp == NULL)
   {
      print_to_log("Error (appendStringExp): malloc failure.\n");
      exit(1);
   }
   string_exp->type = type;
   string_exp->value = strdup(value);
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

void freeStringList(StringList *list)
{
   if(list == NULL) return;
   freeStringList(list->next);
   if(list->value) free(list->value);
   free(list);
}

bool length_declared = false, assignment_declared = false, i_declared = false;
void generateRHSLabelCode(Label label, int count, int indent, FILE *file)
{
   if(label.length == 0)
   {
      if(label.mark == NONE) PTFI("label = blank_label;\n", indent);
      else PTFI("label = makeEmptyLabel(%d);\n", indent, label.mark);
      return;
   }
   int index;
   /* The length of the host label is determined at runtime if the rule label
    * contains a list variable. This code searches for a list variable in the
    * rule label. If one exists, code is generated to retrieve the length of
    * its value from the morphism. */
   bool has_list_variable = false;
   for(index = 0; index < label.length; index++)
   {
      if(label.list[index].type == VARIABLE &&
         label.list[index].variable.type == LIST_VAR)
      {
          PTFI("Assignment *assignment = lookupVariable(morphism, \"%s\");\n",
               indent, label.list[index].variable.name);
          PTFI("int list_var_length = assignment->length;\n", indent);
          has_list_variable = true;
          break;
      }
   }
   /* In the runtime, calloc's first argument depends on the presence of a
    * list variable in the rule label. */
   PTF("\n");
   if(has_list_variable)
        PTFI("Atom *list%d = makeList(%d + list_var_length);\n", 
             indent, count, label.length - 1);
   else PTFI("Atom *list%d = makeList(%d);\n", indent, count, label.length);
   PTFI("int index = 0;\n", indent);
   for(index = 0; index < label.length; index++)
   {
      Atom atom = label.list[index];
      switch(atom.type)
      {
         case INTEGER_CONSTANT:
              PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent, count);
              PTFI("list%d[index++].number = %d;\n", indent, count, atom.number);
              break;

         case STRING_CONSTANT:
              PTFI("list%d[index].type = STRING_CONSTANT;\n", indent, count);
              PTFI("list%d[index++].string = strdup(\"%s\");\n", 
                   indent, count, atom.string);
              break;

         case VARIABLE:
              if(atom.variable.type == INTEGER_VAR)
              {
                 PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent, count);
                 PTFI("list%d[index++].number = getIntegerValue(\"%s\", morphism);\n",
                      indent, count, atom.variable.name);
              }
              else if(atom.variable.type == CHARACTER_VAR ||
                      atom.variable.type == STRING_VAR)
              {
                 PTFI("list%d[index].type = STRING_CONSTANT;\n", indent, count);
                 PTFI("list%d[index++].string = strdup(getStringValue(\"%s\", morphism));\n",
                      indent, count, atom.variable.name);
              }
              else if(atom.variable.type == ATOM_VAR)
              {
                 if(!assignment_declared)
                 {
                    PTFI("Assignment *assignment = lookupVariable(morphism, \"%s\");\n",
                         indent, atom.variable.name);
                    assignment_declared = true;
                 }
                 else PTFI("assignment = lookupVariable(morphism, \"%s\");\n",
                           indent, atom.variable.name);
                 PTFI("if(assignment->type == INTEGER_VAR)\n", indent);
                 PTFI("{\n", indent);
                 PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent + 3, count);
                 PTFI("list%d[index++].number = assignment->value[0].number;\n",
                       indent + 3, count);
                 PTFI("}\n", indent);
                 PTFI("else", indent);
                 PTFI("{\n", indent);
                 PTFI("list%d[index].type = STRING_CONSTANT;\n", indent + 3, count);
                 PTFI("list%d[index++].string = strdup(assignment->value[0].string);\n",
                      indent + 3, count);
                 PTFI("}\n", indent);
              }
              else if(atom.variable.type == LIST_VAR)
              {
                 /* Assignment already declared in the runtime to get the list 
                  * variable's length. */
                 PTFI("assignment = lookupVariable(morphism, \"%s\");\n",
                      indent, atom.variable.name);
                 if(!i_declared)
                 {
                    PTFI("int i;\n", indent);
                    i_declared = true;
                 }
                 PTFI("for(i = 0; i < assignment->length; i++)\n", indent);
                 PTFI("{\n", indent);
                 PTFI("if(assignment->value[i].type == INTEGER_CONSTANT)\n", indent + 3);
                 PTFI("{\n", indent + 3);
                 PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent + 6, count);
                 PTFI("list%d[index++].number = assignment->value[i].number;\n", 
                      indent + 6, count);
                 PTFI("}\n", indent + 3);
                 PTFI("else /* type == STRING_CONSTANT */\n", indent + 3);
                 PTFI("{\n", indent + 3);
                 PTFI("list%d[index].type = STRING_CONSTANT;\n", indent + 6, count);
                 PTFI("list%d[index++].string = strdup(assignment->value[i].string);\n",
                       indent + 6, count);
                 PTFI("}\n", indent + 3);
                 PTFI("}\n", indent);
              }
              break;

         case INDEGREE:
         case OUTDEGREE:
         case LENGTH:
         case NEG:
         case ADD:
         case SUBTRACT:
         case MULTIPLY:
         case DIVIDE:  
              generateIntEvalCode(atom, count, indent, file);
              PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent, count);
              PTFI("list%d[index++].number = ", indent, count);
              generateIntExpression(atom, count, false, file);
              PTF(";\n");
              break;

         case CONCAT:
              if(!length_declared)
              {
                 PTFI("unsigned int length = 0;\n", indent);
                 length_declared = true;
              }
              else PTFI("length = 0;\n", indent);
              generateStringEvalCode(atom, count, indent, file);
              PTFI("\nlist%d[index].type = STRING_CONSTANT;\n", indent, count);
              /* Build host_string from the evaluated strings that make up the
              * RHS label. */
              PTFI("char host_string[length + 1];\n", indent);
              generateStringExpression(atom, count, true, indent, file);
              PTFI("host_string[length] = '\\0';\n", indent);
              PTFI("list%d[index++].string = strdup(host_string);\n\n", indent, count);
              break;
      
         default:
              print_to_log("Error (generateLabelCode): Unexpected host atom "
                           "type %d.\n", atom.type);
              break;
      }
   }      
   PTFI("label = makeHostLabel(%d, %d, list%d);\n",
         indent, label.mark, label.length, count);
}

void generateIntEvalCode(Atom atom, int count, int indent, FILE *file)
{
   static int int_count = 0;
   switch(atom.type)
   {
      case INTEGER_CONSTANT:
           PTFI("int i%d%d = %d;\n", indent, count, int_count++, atom.number);
           break;

      case VARIABLE:
           PTFI("int i%d%d = getIntegerValue(\"%s\", morphism);\n", indent, count,
                int_count++, atom.variable.name);
           break;

      case INDEGREE:
           PTFI("int node_index = lookupNode(morphism, %d);\n", indent, atom.node_id);
           PTFI("int%d%d = getIndegree(getNode(host, node_index));\n", indent, count, int_count++); 
           break;
           
      case OUTDEGREE:
           PTFI("int node_index = lookupNode(morphism, %d);\n", indent, atom.node_id);
           PTFI("int%d%d = getOutdegree(getNode(host, node_index));\n", indent, count, int_count++); 
           break;

      case LENGTH:
           if(atom.variable.type == STRING_VAR)
              PTFI("int%d%d = (int)strlen(getStringValue(\"%s\", morphism));\n", 
                   indent, count, int_count++, atom.variable.name);
           else if(atom.variable.type == ATOM_VAR)
           {
              PTFI("Assignment *assignment = lookupVariable(\"%s\", morphism);\n",
                   indent, atom.variable.name);
              PTFI("if(assignment->type == INTEGER_VAR) int%d%d = 1;\n", 
                   indent, count, int_count++);
              PTFI("else int%d%d = (int)strlen(atom.string);\n", indent, count, int_count++);
           }
           else if(atom.variable.type == LIST_VAR)
           {
              PTFI("Assignment *assignment = lookupVariable(\"%s\", morphism);\n",
                   indent, atom.variable.name);
              PTFI("int%d%d = assignment->length;\n", indent, count, int_count++);
           }
           else
              print_to_log("Error (generateIntegerExpressionCode): Unexpected "
                           "variable type of length argument.\n");
           break;

      case NEG:
           generateIntEvalCode(atom, count, indent, file);
           break;

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:     
           generateIntEvalCode(*(atom.bin_op.left_exp), count, indent, file);
           generateIntEvalCode(*(atom.bin_op.right_exp), count, indent, file);
           break;

      default:
           print_to_log("Error (generateIntEvalCode): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}

void generateIntExpression(Atom atom, int count, bool nested, FILE *file)
{
   static int int_count = 0;
   switch(atom.type)
   {
      case INTEGER_CONSTANT:
      case VARIABLE:
      case INDEGREE:
      case OUTDEGREE:
      case LENGTH:
           PTF("i%d%d", count, int_count++);
           break;

      case NEG:
           PTF("(-i%d%d)", count, int_count++);
           break;

      case ADD:
           if(nested) PTF("(");
           generateIntExpression(*(atom.bin_op.left_exp), count, true, file);
           PTF(" + ");
           generateIntExpression(*(atom.bin_op.right_exp), count, true, file);
           if(nested) PTF(")");
           break;

      case SUBTRACT:
           if(nested) PTF("(");
           generateIntExpression(*(atom.bin_op.left_exp), count, true, file);
           PTF(" - ");
           generateIntExpression(*(atom.bin_op.right_exp), count, true, file);
           if(nested) PTF(")");
           break;

      case MULTIPLY:
           if(nested) PTF("(");
           generateIntExpression(*(atom.bin_op.left_exp), count, true, file);
           PTF(" * ");
           generateIntExpression(*(atom.bin_op.right_exp), count, true, file);
           if(nested) PTF(")");
           break;

      case DIVIDE:     
           if(nested) PTF("(");
           generateIntExpression(*(atom.bin_op.left_exp), count, true, file);
           PTF(" / ");
           generateIntExpression(*(atom.bin_op.right_exp), count, true, file);
           if(nested) PTF(")");
           break;

      default:
           print_to_log("Error (generateIntegerExpression): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}
   
void generateStringEvalCode(Atom atom, int count, int indent, FILE *file)
{
   static int string_count = 0;
   switch(atom.type)
   {
      case STRING_CONSTANT:
           PTFI("string s%d%d = \"%s\";", indent, count, string_count, atom.string);
           PTFI("length += strlen(s%d%d);\n", 1, count, string_count++);
           break;

      case VARIABLE:
           PTFI("string s%d%d = getStringValue(\"%s\", morphism);", indent, count,
                string_count, atom.variable.name);
           PTFI("length += strlen(s%d%d);\n", 1, count, string_count++);
           break;

      case CONCAT:
           generateStringEvalCode(*(atom.bin_op.left_exp), count, indent, file);
           generateStringEvalCode(*(atom.bin_op.right_exp), count, indent, file);
           break;
          
      default:
           print_to_log("Error (generateStringEvalCode): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}

void generateStringExpression(Atom atom, int count, bool first, int indent, FILE *file)
{
   static int string_count = 0;
   switch(atom.type)
   { 
      case STRING_CONSTANT:
      case VARIABLE:
           if(first) PTFI("strcpy(host_string, s%d%d);\n", indent, count, string_count++);
           else PTFI("strcat(host_string, s%d%d);\n",indent, count, string_count++);
           break;

      case CONCAT:
           generateStringExpression(*(atom.bin_op.left_exp), count, first, indent, file);
           generateStringExpression(*(atom.bin_op.right_exp), count, false, indent, file);
           break;
          
      default:
           print_to_log("Error (generateStringExpression): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}
