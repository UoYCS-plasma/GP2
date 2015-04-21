#include "label.h"

Atom *makeList(int length)
{
   Atom *list = malloc(length * sizeof(Atom));
   if(list == NULL)
   {
      print_to_log("Error (makeLabel): malloc failure.\n");
      exit(1);
   }
   return list;
}

bool equalRuleLabels(Label left_label, Label right_label)
{
   if(left_label.mark != right_label.mark) return false;
   if(left_label.length != right_label.length) return false;
   if(left_label.list_variable != right_label.list_variable) return false;

   int index;
   for(index = 0; index < left_label.length; index++)
   {
      Atom *left_atom = &(left_label.list[index]);
      Atom *right_atom = &(right_label.list[index]);
      if(!equalRuleAtoms(left_atom, right_atom)) return false;
   }
   return true;
}

bool equalRuleAtoms(Atom *left_atom, Atom *right_atom)
{
   if(left_atom->type != right_atom->type) return false;
   /* LHS labels are simple expressions; there are only a few cases to consider. */
   switch(left_atom->type)
   {
      case VARIABLE:
           return !strcmp(left_atom->value.variable.name, right_atom->value.variable.name);

      case INTEGER_CONSTANT:
           return left_atom->value.number == right_atom->value.number;

      case STRING_CONSTANT:
           return !strcmp(left_atom->value.string, right_atom->value.string);

      case NEG:
           return !equalRuleAtoms(left_atom->value.neg_exp, right_atom->value.neg_exp);

      case CONCAT:
           if(!equalRuleAtoms(left_atom->value.bin_op.left_exp, 
                              right_atom->value.bin_op.left_exp)) return false;
           if(!equalRuleAtoms(left_atom->value.bin_op.right_exp, 
                              right_atom->value.bin_op.right_exp)) return false;
           return true;

      default:
           print_to_log("Error (equalRuleAtoms): Unexpected LHS atom type %d.\n",
                        left_atom->type);
           break;
   }
   return false;
}
    
void addAtom(Atom atom, Label label, int position)
{
   label.list[position] = atom;
}

void copyLabel(Label *source, Label *target)
{
   target->mark = source->mark;
   target->length = source->length;
   target->list_variable = source->list_variable;
   target->list = copyList(source->list, source->length);      
}

Atom *copyList(Atom *list, int length)
{
   Atom *list_copy = makeList(length);
   int index;
   for(index = 0; index < length; index++)
   {
      /* Populate the copied array with the appropriate values. Any strings
       * are duplicated in memory. Nested Atoms are memory-copied by the auxiliary
       * function copyAtom. */
      Atom atom = list[index];
      list_copy[index].type = atom.type;
      switch(atom.type) 
      {
         case VARIABLE:
         case LIST_LENGTH:
         case STRING_LENGTH:
              list_copy[index].value.variable.name = strdup(atom.value.variable.name);
              list_copy[index].value.variable.type = atom.value.variable.type;
              break;

         case INTEGER_CONSTANT:
              list_copy[index].value.number = atom.value.number;
              break;

         case STRING_CONSTANT:
              list_copy[index].value.string = strdup(atom.value.string);
              break;

         case INDEGREE:
         case OUTDEGREE:
              list_copy[index].value.node_id = atom.value.node_id;              
              break;

         case NEG:
              list_copy[index].value.neg_exp = copyAtom(atom.value.neg_exp);
              break;

         case ADD:
         case SUBTRACT:
         case MULTIPLY:
         case DIVIDE:
         case CONCAT:
              list_copy[index].value.bin_op.left_exp = copyAtom(atom.value.bin_op.left_exp);
              list_copy[index].value.bin_op.right_exp = copyAtom(atom.value.bin_op.right_exp);
              break;

         default: printf("Error (copyAtom): Unexpected atom type: %d\n", 
                        (int)atom.type); 
                  break;
      }
   }
   return list_copy;
}

Atom *copyAtom(Atom *atom)
{
   Atom *copy = makeList(1);
   copy->type = atom->type;
   switch(atom->type) 
   {
      case VARIABLE:
      case LIST_LENGTH:
      case STRING_LENGTH:
           copy->value.variable.name = strdup(atom->value.variable.name);
           copy->value.variable.type = atom->value.variable.type;
           break;

      case INTEGER_CONSTANT:
           copy->value.number = atom->value.number;
           break;

      case STRING_CONSTANT:
           copy->value.string = strdup(atom->value.string);
            break;

      case INDEGREE:
      case OUTDEGREE:
           copy->value.node_id = atom->value.node_id;              
           break;

      case NEG:
           copy->value.neg_exp = copyAtom(atom->value.neg_exp);
           break;

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case CONCAT:
           copy->value.bin_op.left_exp = copyAtom(atom->value.bin_op.left_exp);
           copy->value.bin_op.right_exp = copyAtom(atom->value.bin_op.right_exp);
           break;

      default: printf("Error (copyAtom): Unexpected atom type: %d\n", 
                     (int)atom->type); 
               break;
   }
   return copy;
}

LabelClass getLabelClass(Label label)
{
   if(label.list_variable) return LIST_VAR_L;
   if(label.length == 0) return EMPTY_L;
   if(label.length == 1)
   {
      Atom first_atom = label.list[0];
      {
         switch(first_atom.type)
         {
            case VARIABLE: return ATOMIC_VAR_L;     
 
            case INTEGER_CONSTANT:
            case INDEGREE:
            case OUTDEGREE:
            case LIST_LENGTH:
            case STRING_LENGTH:
            case NEG:
            case ADD:
            case SUBTRACT:
            case MULTIPLY:
            case DIVIDE: return INT_L;

            case STRING_CONSTANT:
            case CONCAT: return STRING_L;

            default:
               print_to_log("Error (getLabelClass): First rule atom of passed "
                            "list has unexpected type %d.\n", first_atom.type);
               break;
         }
      }
   }
   else
   {
      switch(label.length)
      {
         case 2: return LIST2_L;
         case 3: return LIST3_L;
         case 4: return LIST4_L;
         default: return LONG_LIST_L;
      }
   }
   return EMPTY_L;
}

void printLabel(Label label, FILE *file) 
{
   if(label.length == 0) fprintf(file, "empty");
   else
   {
      int index;
      for(index = 0; index < label.length; index++)
      {
         printAtom(&(label.list[index]), file);
         if(index != label.length - 1) fprintf(file, " : ");
      }
   }
   printMark(label.mark, file);
}

void printAtom(Atom *atom, FILE *file)
{
    switch(atom->type) 
    {
        case INTEGER_CONSTANT: 
             fprintf(file, "%d", atom->value.number);
             break;
              
        case STRING_CONSTANT:
             fprintf(file, "%s", atom->value.string);
	     break;

	case VARIABLE: 
	     fprintf(file, "%s", atom->value.variable.name);
	     break;

	case INDEGREE:
	     fprintf(file, "indeg(%d)", atom->value.node_id);
	     break;
 
	case OUTDEGREE:
	     fprintf(file, "outdeg(%d)", atom->value.node_id);
	     break;

	case LIST_LENGTH:
	case STRING_LENGTH:
	     fprintf(file, "length(%s)", atom->value.variable.name);
	     break;

	case NEG:
	     fprintf(file, "- ");
	     printAtom(atom->value.neg_exp, file);
	     break;

	case ADD:
	     printOperation(atom->value.bin_op.left_exp, 
                            atom->value.bin_op.right_exp, "+", file);
	     break;

	case SUBTRACT:
	     printOperation(atom->value.bin_op.left_exp, 
                            atom->value.bin_op.right_exp, "-", file);
	     break;

	case MULTIPLY:
	     printOperation(atom->value.bin_op.left_exp, 
                            atom->value.bin_op.right_exp, "*", file);
	     break;

	case DIVIDE:
	     printOperation(atom->value.bin_op.left_exp, 
                            atom->value.bin_op.right_exp, "/", file);
	     break;

	case CONCAT:
	     printOperation(atom->value.bin_op.left_exp, 
                            atom->value.bin_op.right_exp, ".", file);
	     break;

	default: fprintf(file, "Error (printAtom): Unexpected atom type: %d\n",
		        (int)atom->type); 
		 break;
    }
}

void printOperation(Atom *left_exp, Atom *right_exp, string const operation,
                    FILE *file)
{
   fprintf(file, "(");
   printAtom(left_exp, file);
   fprintf(file, " %s ", operation);
   printAtom(right_exp, file);
   fprintf(file, ")");
}

void printMark(MarkType mark, FILE *file)
{
   if(mark == NONE) return;
   if(mark == RED) { fprintf(file, " # red"); return; }
   if(mark == GREEN) { fprintf(file, " # green"); return; }
   if(mark == BLUE) { fprintf(file, " # blue"); return; }
   if(mark == GREY) { fprintf(file, " # grey"); return; }
   if(mark == DASHED) { fprintf(file, " # dashed"); return; }
   if(mark == ANY) { fprintf(file, " # any"); return; }
   print_to_log("Error (printMark): Unexpected mark type %d\n", mark);
}

void freeLabel(Label label)
{
   int index;
   for(index = 0; index < label.length; index++)
      /* freeAtom called with false because these atoms are not individually
       * freed. The complete array is freed after the for loop. */
      freeAtom(&(label.list[index]), false);
   free(label.list);
}

void freeAtom(Atom *atom, bool free_atom)
{
   if(atom == NULL) return;
   switch(atom->type) 
   {
     case VARIABLE:
          if(atom->value.variable.name) free(atom->value.variable.name);
          break;

     case INTEGER_CONSTANT:
          break;

     case STRING_CONSTANT:
          if(atom->value.string) free(atom->value.string);
          break;

     case INDEGREE:
     case OUTDEGREE:
          break;

     case LIST_LENGTH:
     case STRING_LENGTH:
          if(atom->value.variable.name) free(atom->value.variable.name);
          break;

     case NEG:
          if(atom->value.neg_exp) freeAtom(atom->value.neg_exp, true);
          break;

     case ADD:
     case SUBTRACT:
     case MULTIPLY:
     case DIVIDE:
     case CONCAT:
          if(atom->value.bin_op.left_exp) freeAtom(atom->value.bin_op.left_exp, true);
          if(atom->value.bin_op.right_exp) freeAtom(atom->value.bin_op.right_exp, true);
          break;

     default: printf("Error (freeAtom): Unexpected atom type: %d\n", 
                     (int)atom->type); 
              break;
   }
   if(free_atom) free(atom);
}

