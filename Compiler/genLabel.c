#include "genLabel.h"

void generateIteratorCode(Label label, int indent, FILE *file)
{
   PTFI("int mark_index, class_index;\n", indent);

   /* Generate the outer loop which iterates over the rows of the label class array. */
   if(label.mark == ANY)
      PTFI("for(mark_index = 0; mark_index < NUMBER_OF_MARKS; mark_index++)\n", indent);
   else PTFI("for(mark_index = %d; mark_index < %d; mark_index++)\n", 
             indent, label.mark, label.mark + 1);
   PTFI("{\n", indent);

   /* Generate the inner loop which iterates over the columns of the label class array. */
   if(getListVariable(label) != NULL)
   {
      /* The generated loop depends on the number of non-list variable atoms. */
      int atoms = label.length - 1;
      if(atoms < 2) 
           PTFI("for(class_index = %d; class_index < NUMBER_OF_CLASSES; class_index++)\n",
                indent + 3, atoms);
      /* There are two label class indices for lists of length 1, so the index for lists of
       * length x > 1 is x + 1. This is why atoms + 1 is passed to PTFI. */
      else PTFI("for(class_index = %d; class_index < NUMBER_OF_CLASSES; class_index++)\n",
                indent + 3, atoms + 1);
      PTFI("{\n", 3);
   }
   else
   {
      if(label.length == 0)
         PTFI("for(class_index = 0; class_index < 1; class_index++)\n", indent + 3);
      else if(label.length > 1)
         PTFI("for(class_index = %d; class_index < %d; class_index++)\n", indent + 3,
              label.length + 1, label.length + 2);
      else
      {
         Atom atom = label.list[0];
         switch(atom.type)
         {
            case INTEGER_CONSTANT:
                 PTFI("for(class_index = 1; class_index < 2; class_index++)\n", indent + 3);
                 break;

            case STRING_CONSTANT:
            case CONCAT:
                 PTFI("for(class_index = 2; class_index < 3; class_index++)\n", indent + 3);

            case VARIABLE:
                 if(atom.variable.type == INTEGER_VAR)
                    PTFI("for(class_index = 1; class_index < 2; class_index++)\n", indent + 3);
                 else if(atom.variable.type == STRING_VAR || 
                         atom.variable.type == CHARACTER_VAR)
                    PTFI("for(class_index = 2; class_index < 3; class_index++)\n", indent + 3);
                 else if(atom.variable.type == ATOM_VAR)
                    PTFI("for(class_index = 1; class_index < 3; class_index++)\n", indent + 3);
                 break;

            default:
                 print_to_log("Error (generateIteratorCode): Unexpected atom type %d.\n", 
                              atom.type);
                 break;
         }
      }
   }
   PTFI("{\n", indent + 3);
}

/* Global flag set to true if the 'result' variable has been declared in the
 * emitted code. These flags are scattered throughout the file before functions
 * that print variables that may be declared more than once in the runtime. */
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
      PTFI("match = label.length == 0 ? true : false;\n", indent);
      return;
   }
   
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
   PTFI("index = 0;\n", indent);
   PTFI("do {\n", indent);
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
      generateAtomMatchingCode(atom, indent + 3, file);
      PTFI("index++;\n", indent + 3);
   }   
   PTFI("/* The current host index marks the start of the list .\n", indent + 3);
   PTFI(" * that is assigned to the list variable. */\n", indent + 3);
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
   PTFI("result = addListAssignment(\"%s\", NULL, 0, morphism);\n", 
        indent + 6, list_variable_name);
   PTFI("else\n", indent + 3);
   PTFI("{\n", indent + 3);
   PTFI("/* Assign the unmatched sublist of the host list to \"%s\". */\n",
        indent + 3, list_variable_name);
   PTFI("Atom list[index - start_index + 1];\n", indent + 6);
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
   /* If there is no string variable, iterate through the StringList and 
    * generate code for each string expression. */
   if(!has_string_variable)
   {
      while(iterator != NULL) 
      {
         PTFI("if(start >= strlen(host_string)) break;\n", indent);
         generateStringMatchingCode(iterator, true, indent, file);
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

void generateVariableCode(string name, GPType type, FILE *file)
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
           PTFI("Assignment *assignment_%s = lookupVariable(morphism, \"%s\");\n",
                3, name, name);
           PTFI("union { int num; string str; } %s_var;\n", 3, name);
           PTFI("if(assignment_%s->type == INTEGER_VAR) "
                "%s_var.num = getIntegerValue(\"%s\", morphism);\n", 3, name, name, name);
           PTFI("else %s_var.str = getStringValue(\"%s\", morphism);\n", 3, name, name);
           break;
         
      case LIST_VAR:
           PTFI("Assignment *assignment_%s = lookupVariable(morphism, \"%s\");\n",
                3, name, name);
           break;
      
      default:
           print_to_log("Error (generateVariableCode): Unexpected type %d\n", type);
           break;
   }
}

bool length_declared = false, assignment_declared = false, i_declared = false;
bool node_index_declared = false, host_label_declared = false;

void generateRHSLabelCode(Label label, bool node, int count, int indent, FILE *file)
{
   if(label.length == 0)
   { 
      /* If the RHS has the 'ANY' mark, generate code to retrieve the mark of the
       * host item's label. */
      if(label.mark == ANY)
      {
         if(!host_label_declared)
         {
            PTFI("Label host_label;\n", indent);
            host_label_declared = true;
         }
         if(node) PTFI("host_label = getNodeLabel(getNode(host, "
                       "host_node_index));\n", indent);
         else PTFI("host_label = getEdgeLabel(getEdge(host, "
                   "host_edge_index));\n", indent);
         PTFI("label = makeEmptyLabel(host_label.mark);\n\n", indent);
      }
      else PTFI("label = makeEmptyLabel(%d);\n\n", indent, label.mark);
      return;
   }
   int index;
   /* In the runtime, calloc's first argument depends on the presence of a
    * list variable in the rule label. 
    * The <count> integer variable is used to create a fresh Atom * variable
    * name in case more than one list is created at runtime. */
   PTFI("Atom *list%d = NULL;\n", indent, count);
   string list_variable = getListVariable(label);
   if(list_variable == NULL) 
        PTFI("list%d = makeList(%d);\n", indent, count, label.length);
   else PTFI("list%d = makeList(%d + assignment_%s->length - 1);\n",
             indent, count, label.length, list_variable);

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
         {
              /* Use the <variable_name>_var variables generated previously to
               * add the correct values to the runtime list. */
              string name = atom.variable.name;
              if(atom.variable.type == INTEGER_VAR)
              {
                 PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent, count);
                 PTFI("list%d[index++].number = %s_var);\n", indent, count, name);
              }
              else if(atom.variable.type == CHARACTER_VAR ||
                      atom.variable.type == STRING_VAR)
              {
                 PTFI("list%d[index].type = STRING_CONSTANT;\n", indent, count);
                 PTFI("list%d[index++].string = strdup(%s_var);\n",
                      indent, count, name);
              }
              else if(atom.variable.type == ATOM_VAR)
              {
                 PTFI("if(assignment_%s->type == INTEGER_VAR)\n", indent, name);
                 PTFI("{\n", indent);
                 PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent + 3, count);
                 PTFI("list%d[index++].number = %s_var;\n", indent + 3, count, name);
                 PTFI("}\n", indent);
                 PTFI("else /* type == STRING_CONSTANT */\n", indent);
                 PTFI("{\n", indent);
                 PTFI("list%d[index].type = STRING_CONSTANT;\n", indent + 3, count);
                 PTFI("list%d[index++].string = strdup(%s_var);\n", indent + 3, count, name);
                 PTFI("}\n", indent);
              }
              else if(atom.variable.type == LIST_VAR)
              {
                 PTFI("Atom *%s_var = getListValue(\"%s\", morphism);\n", 3, name, name);
                 if(!i_declared)
                 {
                    PTFI("int i;\n", indent);
                    i_declared = true;
                 }
                 PTFI("for(i = 0; i < assignment_%s->length; i++)\n", indent, name);
                 PTFI("{\n", indent);
                 PTFI("if(%s_var[i].type == INTEGER_CONSTANT)\n", indent + 3, name);
                 PTFI("{\n", indent + 3);
                 PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent + 6, count);
                 PTFI("list%d[index++].number = %s_var[i].number;\n", 
                      indent + 6, count, name);
                 PTFI("}\n", indent + 3);
                 PTFI("else /* type == STRING_CONSTANT */\n", indent + 3);
                 PTFI("{\n", indent + 3);
                 PTFI("list%d[index].type = STRING_CONSTANT;\n", indent + 6, count);
                 PTFI("list%d[index++].string = strdup(%s_var[i].string);\n",
                       indent + 6, count, name);
                 PTFI("}\n", indent + 3);
                 PTFI("}\n", indent);
              }
              break;
         }
         case INDEGREE:
              PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent, count);
              PTFI("list%d[index++].number = indegree%d;\n", indent, count, atom.node_id); 
              break;
           
         case OUTDEGREE:
              PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent, count);
              PTFI("list%d[index++].number = outdegree%d;\n", indent, count, atom.node_id); 
              break;

         case LENGTH:
         case NEG:
         case ADD:
         case SUBTRACT:
         case MULTIPLY:
         case DIVIDE:  
              PTFI("list%d[index].type = INTEGER_CONSTANT;\n", indent, count);
              PTFI("list%d[index++].number = ", indent, count);
              generateIntExpression(atom, false, file);
              PTF(";\n");
              break;

         case CONCAT:
              if(!length_declared)
              {
                 PTFI("unsigned int length = 0;\n", indent);
                 length_declared = true;
              }
              else PTFI("length = 0;\n", indent);
              /* Generates string variables to store string literals to be 
               * concatenated, and updates the runtime length variable with the 
               * total length of the concatenated string. */
              generateStringLengthCode(atom, indent, file);
              /* Build host_string from the evaluated strings that make up the
               * RHS label. */
              PTFI("char host_string[length + 1];\n", indent);
              generateStringExpression(atom, true, indent, file);
              PTFI("host_string[length] = '\\0';\n\n", indent);
              PTFI("list%d[index].type = STRING_CONSTANT;\n", indent, count);
              PTFI("list%d[index++].string = strdup(host_string);\n\n", indent, count);
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
      if(!host_label_declared)
      {
         PTFI("Label host_label;\n", indent);
         host_label_declared = true;
      } 
      if(node) PTFI("host_label = getNodeLabel(getNode(host, "
                    "host_node_index));\n", indent);
      else PTFI("host_label = getEdgeLabel(getEdge(host, "
                "host_edge_index));\n", indent);
      if(list_variable == NULL) 
         PTFI("label = makeHostLabel(host_label.mark, %d, list%d);\n\n",
               indent, label.length, count);
      else PTFI("label = makeHostLabel(host_label.mark, %d + assignment_%s->length - 1, "
                "list%d);\n\n", indent, label.length, list_variable, count);
      PTFI("label = makeHostLabel(host_label.mark, %d, list%d);\n\n",
           indent, label.length, count);
   }
   if(list_variable == NULL) 
        PTFI("label = makeHostLabel(%d, %d, list%d);\n\n",
            indent, label.mark, label.length, count);
   else PTFI("label = makeHostLabel(%d, %d + assignment_%s->length - 1, list%d);\n\n",
             indent, label.mark, label.length, list_variable, count);
}

void generateIntExpression(Atom atom, bool nested, FILE *file)
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
           PTF("indegree%d", atom.node_id);
           break;

      case OUTDEGREE:
           PTF("outdegree%d", atom.node_id);
           break;

      case NEG:
           PTF("(-%d)", atom.number);
           break;

      case ADD:
           if(nested) PTF("(");
           generateIntExpression(*(atom.bin_op.left_exp), true, file);
           PTF(" + ");
           generateIntExpression(*(atom.bin_op.right_exp), true, file);
           if(nested) PTF(")");
           break;

      case SUBTRACT:
           if(nested) PTF("(");
           generateIntExpression(*(atom.bin_op.left_exp), true, file);
           PTF(" - ");
           generateIntExpression(*(atom.bin_op.right_exp), true, file);
           if(nested) PTF(")");
           break;

      case MULTIPLY:
           if(nested) PTF("(");
           generateIntExpression(*(atom.bin_op.left_exp), true, file);
           PTF(" * ");
           generateIntExpression(*(atom.bin_op.right_exp), true, file);
           if(nested) PTF(")");
           break;

      case DIVIDE:     
           if(nested) PTF("(");
           generateIntExpression(*(atom.bin_op.left_exp), true, file);
           PTF(" / ");
           generateIntExpression(*(atom.bin_op.right_exp), true, file);
           if(nested) PTF(")");
           break;

      default:
           print_to_log("Error (generateIntegerExpression): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}
   
void generateStringLengthCode(Atom atom, int indent, FILE *file)
{
   switch(atom.type)
   {
      case STRING_CONSTANT:
           PTFI("length += strlen(\"%s\");\n", indent, atom.string);
           break;

      case VARIABLE:
           PTFI("length += strlen(%s_var);\n", indent, atom.variable.name);
           break;

      case CONCAT:
           generateStringLengthCode(*(atom.bin_op.left_exp), indent, file);
           generateStringLengthCode(*(atom.bin_op.right_exp), indent, file);
           break;
          
      default:
           print_to_log("Error (generateStringEvalCode): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}

void generateStringExpression(Atom atom, bool first, int indent, FILE *file)
{
   switch(atom.type)
   { 
      case STRING_CONSTANT:
           if(first) PTFI("strcpy(host_string, \"%s\");\n", indent, atom.string);
           else PTFI("strcat(host_string, \"%s\");\n", indent, atom.string);

      case VARIABLE:
           if(first) PTFI("strcpy(host_string, %s_var );\n", indent, atom.variable.name);
           else PTFI("strcat(host_string, %s_var);\n", indent, atom.variable.name);
           break;

      case CONCAT:
           generateStringExpression(*(atom.bin_op.left_exp), first, indent, file);
           generateStringExpression(*(atom.bin_op.right_exp), false, indent, file);
           break;
          
      default:
           print_to_log("Error (generateStringExpression): Unexpected "
                        "atom type %d.\n", atom.type);
           break;
   }
}
