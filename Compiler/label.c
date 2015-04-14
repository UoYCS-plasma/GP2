#include "label.h"

Label blank_label = {NONE, {NULL, NULL}, 0, false};
Label red_label = {RED, {NULL, NULL}, 0, false};
Label green_label = {GREEN, {NULL, NULL}, 0, false};
Label blue_label = {BLUE, {NULL, NULL}, 0, false};
Label grey_label = {GREY, {NULL, NULL}, 0, false};
Label dashed_label = {DASHED, {NULL, NULL}, 0, false};

bool isConstantLabel(Label *label)
{
   if(label == NULL) return true;
   if(label == &blank_label || label == &red_label || label == &green_label ||
      label == &blue_label || label == &grey_label || label == &dashed_label)
      return true;
   else return false;
}

bool equalLabels(Label *left_label, Label *right_label)
{
   if(left_label->mark != right_label->mark) return false;
   if(left_label->list_length != right_label->list_length) return false;
   if(left_label->list_variable != right_label->list_variable) return false;

   GP2Atom *left_atom = left_label->list.first;
   GP2Atom *right_atom = right_label->list.first;

   while(left_atom != NULL)
   {
      if(left_atom->type != right_atom->type) return false;
      /* LHS labels are simple expressions, hence there are only a few cases
       * to consider. */
      switch(left_atom->type)
      {
         case VARIABLE:
              if(strcmp(left_atom->value.name, right_atom->value.name))
                 return false;
              break;

         case INTEGER_CONSTANT:
              if(left_atom->value.number != right_atom->value.number)
                 return false;
              break;

         case STRING_CONSTANT:
              if(strcmp(left_atom->value.string, right_atom->value.string))
                 return false;
              break;

         case NEG:
              if(left_atom->value.exp->type != INTEGER_CONSTANT) return false;
              if(right_atom->value.exp->type != INTEGER_CONSTANT) return false;
              if(left_atom->value.exp->value.number != 
                 right_atom->value.exp->value.number) return false;
              break;

         default:
              print_to_log("Error (equalLabels): Unexpected LHS atom type.\n");
              break;
      }
      left_atom = left_atom->next;
      right_atom = right_atom->next;
   }
   return true;
}

bool labelMatch(Label *rule_label, Label *host_label)
{
   if(rule_label == NULL) return host_label == &blank_label;
   /* Both labels are the same constant list. No variable assignments to make
    * in this case, so just return true. 
   if(equalLabels(rule_label, host_label)) return true; */
   return marksMatch(rule_label->mark, host_label->mark);
}

bool marksMatch(MarkType rule_mark, MarkType host_mark)
{
   if(rule_mark == ANY) return true;
   else return rule_mark == host_mark;
}

Label *makeEmptyList(MarkType mark)
{
   if(mark == NONE) return &blank_label;
   if(mark == RED) return &red_label;
   if(mark == GREEN) return &green_label;
   if(mark == BLUE) return &blue_label;
   if(mark == GREY) return &grey_label;
   if(mark == DASHED) return &dashed_label;
   
   /* Otherwise, create the empty label with the ANY mark. */
   Label *label = malloc(sizeof(Label));
   if(label == NULL)
   {
      print_to_log("Error: Memory exhausted during label creation.\n");
      exit(1);
   }
   label->mark = ANY;
   label->list.first = NULL;
   label->list.last = NULL;
   label->list_length = 0;
   label->list_variable = false;
   return label;
}

LabelClass getLabelClass(Label *label)
{
   int length = label->list_length;
   if(label->list_variable) return LISTVAR_L;
   if(label->list.first == NULL) return EMPTY_L;
   if(length > 1)
   {
      switch(length)
      {
         case 2: return LIST2_L;
         case 3: return LIST3_L;
         case 4: return LIST4_L;
         case 5: return LIST5_L;
         default: return LONG_LIST_L;
      }
   }
   /* The list has length 1. */
   switch(label->list.first->type)
   {
      case VARIABLE:
           return ATOMIC_VAR_L;     

      case INTEGER_CONSTANT:
      case NEG:
           return INT_L;

      case STRING_CONSTANT:
      case CONCAT:
           return STRING_L;

      default:
           print_to_log("Error (getLabelClass): First element of passed list "
                        "has unexpected type %d.\n", 
                        label->list.first->type);
           break;
   }
   return EMPTY_L;
}

Label *copyLabel(Label *label)
{
   if(label == &blank_label) return &blank_label;
   Label *label_copy = malloc(sizeof(Label));
   if(label_copy == NULL)
   {
      print_to_log("Error: Memory exhausted during label copying.\n");
      exit(1);
   }
   label_copy->mark = label->mark;
   copyGP2List(label->list, &label_copy->list);
   label_copy->list_length = label->list_length;
   label_copy->list_variable = label->list_variable;
   return label_copy;
}

void freeLabel(Label *label)
{
   if(label == NULL) return;
   freeGP2List(label->list);
   free(label);
}

void copyGP2List(GP2List source, GP2List *destination)
{
   destination->first = NULL;
   destination->last = NULL;

   GP2Atom *iterator = source.first;
   while(iterator != NULL)
   {
      GP2Atom *atom_copy = copyGP2Atom(iterator);
      append(destination, atom_copy);
      iterator = iterator->next;
   }
}

void append(GP2List *list, GP2Atom *atom)
{
   if(list->first == NULL) list->first = atom;
   if(list->last != NULL) list->last->next = atom;
   atom->prev = list->last;
   list->last = atom;
}


GP2Atom *copyGP2Atom(GP2Atom *atom)
{ 
   GP2Atom *atom_copy = malloc(sizeof(GP2Atom));
   if(atom_copy == NULL)
   {
      print_to_log("Error: Memory exhausted during GP2Atom copying.\n");
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

      case STRING_CONSTANT:
           atom_copy->value.string = strdup(atom->value.string);
           break;

      case NEG:
           atom_copy->value.exp = copyGP2Atom(atom->value.exp);
           break;

      case CONCAT:
           atom_copy->value.bin_op.left_exp = 
              copyGP2Atom(atom->value.bin_op.left_exp);
           atom_copy->value.bin_op.right_exp = 
              copyGP2Atom(atom->value.bin_op.right_exp);
           break;

      default:
             print_to_log("Error (copyGP2Atom): Atom type %d should not "
                          "occur here.\n", atom->type);
             return NULL;
   }
   return atom_copy;
}

void printGP2List(GP2List list, FILE *file) 
{
   GP2Atom *iterator = list.first;
   if(iterator == NULL) fprintf(file, "empty");
   while(iterator != NULL) 
   {
      printGP2Atom(iterator, file);
      if(iterator->next) fprintf(file, " : ");
      iterator = iterator->next;
   } 
}

void printGP2Atom(GP2Atom *atom, FILE *file) 
{
    switch(atom->type) 
    {
	case VARIABLE: 
	     fprintf(file, "%s", atom->value.name);
	     break;

	case INTEGER_CONSTANT: 
	     fprintf(file, "%d", atom->value.number);
	     break;

        case STRING_CONSTANT:
	     fprintf(file, "\"%s\"", atom->value.string);
	     break;

	case INDEGREE:
	     fprintf(file, "indeg(%s)", atom->value.node_id);
	     break;
 
	case OUTDEGREE:
	     fprintf(file, "outdeg(%s)", atom->value.node_id);
	     break;

	case LIST_LENGTH:
	     fprintf(file, "llength(");
	     printGP2List(*atom->value.list_arg, file);
	     fprintf(file, ")");
	     break;

	case STRING_LENGTH:
	     fprintf(file, "slength(");
	     printGP2Atom(atom->value.str_arg, file);
	     fprintf(file, ")");
	     break;

	case NEG:
	     fprintf(file, "- ");
	     printGP2Atom(atom->value.exp, file);
	     break;

	case ADD:
	     fprintf(file, "(");
	     printGP2Atom(atom->value.bin_op.left_exp, file);
	     fprintf(file, " + ");
	     printGP2Atom(atom->value.bin_op.right_exp, file);
	     fprintf(file, ")");
	     break;

	case SUBTRACT:
	     fprintf(file, "(");
	     printGP2Atom(atom->value.bin_op.left_exp, file);
	     fprintf(file, " - ");
	     printGP2Atom(atom->value.bin_op.right_exp, file);
	     fprintf(file, ")");
	     break;

	case MULTIPLY:
	     fprintf(file, "(");
	     printGP2Atom(atom->value.bin_op.left_exp, file);
	     fprintf(file, " * ");
	     printGP2Atom(atom->value.bin_op.right_exp, file);
	     fprintf(file, ")");
	     break;

	case DIVIDE:
	     fprintf(file, "(");
	     printGP2Atom(atom->value.bin_op.left_exp, file);
	     fprintf(file, " / ");
	     printGP2Atom(atom->value.bin_op.right_exp, file);
	     fprintf(file, ")");
	     break;

	case CONCAT:
	     fprintf(file, "(");
	     printGP2Atom(atom->value.bin_op.left_exp, file);
	     fprintf(file, " . ");
	     printGP2Atom(atom->value.bin_op.right_exp, file);
	     fprintf(file, ")");
	     break;

	default: fprintf(file, "Unexpected GP2Atom Type: %d\n",
		       (int)atom->type); 
		 break;
    }
}

void printMark(MarkType mark, bool verbose, FILE *file)
{
   switch(mark)
   {
      case NONE:
           break;

      case RED:
           if(verbose) fprintf(file, "Mark: Red\n");
           else fprintf(file, " # red");
           break;

      case GREEN:
           if(verbose) fprintf(file, "Mark: Green\n");
           else fprintf(file, " # green");
           break;

      case BLUE:
           if(verbose) fprintf(file, "Mark: Blue\n");
           else fprintf(file, " # blue");
           break;

      case GREY:
           if(verbose) fprintf(file, "Mark: Grey\n");
           else fprintf(file, " # grey");
           break;

      case DASHED:
           if(verbose) fprintf(file, "Mark: Dashed\n");
           else fprintf(file, " # dashed");
           break;

      case ANY:
           if(verbose) fprintf(file, "Mark: Any\n");
           else fprintf(file, " # any");
           break;

      default:
           print_to_log("Error (printMark): Unexpected mark type %d\n", mark);
           break;
   }
}

void freeGP2List(GP2List list)
{
   GP2Atom *iterator = list.first;
   while(iterator != NULL)
   {
      GP2Atom *temp = iterator;
      iterator = iterator->next;
      freeGP2Atom(temp);
   }
}

void freeGP2Atom(GP2Atom *atom)
{
   if(atom == NULL) return;
   switch(atom->type) 
   {
     case VARIABLE:
          if(atom->value.name) free(atom->value.name);
          break;

     case INTEGER_CONSTANT:
          break;

     case STRING_CONSTANT:
          if(atom->value.string) free(atom->value.string);
          break;

     case INDEGREE:

     case OUTDEGREE:
          if(atom->value.node_id) free(atom->value.node_id);
          break;

     case LIST_LENGTH:
          if(atom->value.list_arg) freeGP2List(*atom->value.list_arg);
          break;

     case STRING_LENGTH:
          if(atom->value.str_arg) freeGP2Atom(atom->value.str_arg);
          break;

     case NEG:
          if(atom->value.exp) freeGP2Atom(atom->value.exp);
          break;

     case ADD:

     case SUBTRACT:

     case MULTIPLY:

     case DIVIDE:

     case CONCAT:
          if(atom->value.bin_op.left_exp)
            freeGP2Atom(atom->value.bin_op.left_exp);
          if(atom->value.bin_op.right_exp)  
            freeGP2Atom(atom->value.bin_op.right_exp);
          break;

     default: printf("Unexpected GP2Atom Type: %d\n",
                     (int)atom->type); 
               break;
   }
   free(atom);
}

