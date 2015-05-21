#include "label.h"

Label blank_label = {NONE, 0, NULL};

Label makeEmptyLabel(MarkType mark)
{
   Label label = { .mark = mark, .length = 0, .list = NULL };
   return label;
}

Label makeHostLabel(MarkType mark, int length, Atom *list)
{
   Label label = { .mark = mark, .length = length, .list = list };
   return label;
}

bool equalRuleLabels(Label left_label, Label right_label)
{
   if(left_label.mark != right_label.mark) return false;
   if(left_label.length != right_label.length) return false;

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
           return !strcmp(left_atom->variable.name, right_atom->variable.name);

      case INTEGER_CONSTANT:
           return left_atom->number == right_atom->number;

      case STRING_CONSTANT:
           return !strcmp(left_atom->string, right_atom->string);

      case NEG:
           return !equalRuleAtoms(left_atom->neg_exp, right_atom->neg_exp);

      case CONCAT:
           if(!equalRuleAtoms(left_atom->bin_op.left_exp, 
                              right_atom->bin_op.left_exp)) return false;
           if(!equalRuleAtoms(left_atom->bin_op.right_exp, 
                              right_atom->bin_op.right_exp)) return false;
           return true;

      default:
           print_to_log("Error (equalRuleAtoms): Unexpected LHS atom type %d.\n",
                        left_atom->type);
           break;
   }
   return false;
}

LabelClass getLabelClass(Label label)
{
   if(label.length == 0) return EMPTY_L;
   if(label.length == 1)
   {
      switch(label.list[0].type)
      {
         case INTEGER_CONSTANT:
              return INT_L;

         case STRING_CONSTANT:
              return STRING_L;

         default:
              print_to_log("Error (getLabelClass): First atom of passed host "
                           "label has unexpected type %d.\n", label.list[0].type);
              break;
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

bool hasListVariable(Label label)
{
   int index;
   for(index = 0; index < label.length; index++)
   {
      if(label.list[index].type == VARIABLE && 
         label.list[index].variable.type == LIST_VAR) return true;
   }
   return false;
}

Atom *makeList(int length)
{
   if(length == 0) return NULL;
   Atom *list = malloc(length * sizeof(Atom));
   if(list == NULL)
   {
      print_to_log("Error (makeLabel): malloc failure.\n");
      exit(1);
   }
   return list;
}

void copyLabel(Label *source, Label *target)
{
   target->mark = source->mark;
   target->length = source->length;
   target->list = copyList(source->list, source->length);      
}

Atom *copyList(Atom *list, int length)
{
   Atom *list_copy = makeList(length);
   int index;
   for(index = 0; index < length; index++)
   {
      /* Populate the copied array with the appropriate . Any strings
       * are duplicated in memory. Nested Atoms are memory-copied by the auxiliary
       * function copyAtom. */
      Atom atom = list[index];
      list_copy[index].type = atom.type;
      switch(atom.type) 
      {
         case VARIABLE:
         case LENGTH:
              list_copy[index].variable.name = strdup(atom.variable.name);
              list_copy[index].variable.type = atom.variable.type;
              break;

         case INTEGER_CONSTANT:
              list_copy[index].number = atom.number;
              break;

         case STRING_CONSTANT:
              list_copy[index].string = strdup(atom.string);
              break;

         case INDEGREE:
         case OUTDEGREE:
              list_copy[index].node_id = atom.node_id;              
              break;

         case NEG:
              list_copy[index].neg_exp = copyAtom(atom.neg_exp);
              break;

         case ADD:
         case SUBTRACT:
         case MULTIPLY:
         case DIVIDE:
         case CONCAT:
              list_copy[index].bin_op.left_exp = copyAtom(atom.bin_op.left_exp);
              list_copy[index].bin_op.right_exp = copyAtom(atom.bin_op.right_exp);
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
      case LENGTH:
           copy->variable.name = strdup(atom->variable.name);
           copy->variable.type = atom->variable.type;
           break;

      case INTEGER_CONSTANT:
           copy->number = atom->number;
           break;

      case STRING_CONSTANT:
           copy->string = strdup(atom->string);
            break;

      case INDEGREE:
      case OUTDEGREE:
           copy->node_id = atom->node_id;              
           break;

      case NEG:
           copy->neg_exp = copyAtom(atom->neg_exp);
           break;

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case CONCAT:
           copy->bin_op.left_exp = copyAtom(atom->bin_op.left_exp);
           copy->bin_op.right_exp = copyAtom(atom->bin_op.right_exp);
           break;

      default: printf("Error (copyAtom): Unexpected atom type: %d\n", 
                     (int)atom->type); 
               break;
   }
   return copy;
}


void printLabel(Label label, FILE *file) 
{
   printList(label.list, label.length, file);
   printMark(label.mark, file);
}

void printList(Atom *atom, int length, FILE *file)
{
   if(length == 0) fprintf(file, "empty");
   else
   {
      int index;
      for(index = 0; index < length; index++)
      {
         printAtom(&(atom[index]), false, file);
         if(index != length - 1) fprintf(file, " : ");
      }
   }
}

void printAtom(Atom *atom, bool nested, FILE *file)
{
    switch(atom->type) 
    {
        case INTEGER_CONSTANT: 
             fprintf(file, "%d", atom->number);
             break;
              
        case STRING_CONSTANT:
             fprintf(file, "\"%s\"", atom->string);
	     break;

	case VARIABLE: 
	     fprintf(file, "%s", atom->variable.name);
	     break;

	case INDEGREE:
	     fprintf(file, "indeg(%d)", atom->node_id);
	     break;
 
	case OUTDEGREE:
	     fprintf(file, "outdeg(%d)", atom->node_id);
	     break;

	case LENGTH:
	     fprintf(file, "length(%s)", atom->variable.name);
	     break;

	case NEG:
	     fprintf(file, "- ");
	     printAtom(atom->neg_exp, true, file);
	     break;

	case ADD:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp, 
                            "+", nested, file);
	     break;

	case SUBTRACT:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp,
                            "-", nested, file);
	     break;

	case MULTIPLY:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp, 
                            "*", nested, file);
	     break;

	case DIVIDE:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp, 
                            "/", nested, file);
	     break;

	case CONCAT:
	     printOperation(atom->bin_op.left_exp, atom->bin_op.right_exp, 
                            ".", nested, file);
	     break;

	default: fprintf(file, "Error (printAtom): Unexpected atom type: %d\n",
		        (int)atom->type); 
		 break;
    }
}

void printOperation(Atom *left_exp, Atom *right_exp, string const operation,
                    bool nested, FILE *file)
{
   if(nested) fprintf(file, "(");
   printAtom(left_exp, true, file);
   fprintf(file, " %s ", operation);
   printAtom(right_exp, true, file);
   if(nested) fprintf(file, ")");
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
   if(label.list != NULL) freeList(label.list, label.length);
}

void freeList(Atom *atom, int length)
{
   int index;
   /* freeAtom called with false because these atoms are not individually
    * freed. The complete array is freed after the for loop. */
   for(index = 0; index < length; index++) freeAtom(&(atom[index]), false);
   if(atom) free(atom);
}

void freeAtom(Atom *atom, bool free_atom)
{
   if(atom == NULL) return;
   switch(atom->type) 
   {
     case VARIABLE:
          if(atom->variable.name) free(atom->variable.name);
          break;

     case INTEGER_CONSTANT:
          break;

     case STRING_CONSTANT:
          if(atom->string) free(atom->string);
          break;

     case INDEGREE:
     case OUTDEGREE:
          break;

     case LENGTH:
          if(atom->variable.name) free(atom->variable.name);
          break;

     case NEG:
          if(atom->neg_exp) freeAtom(atom->neg_exp, true);
          break;

     case ADD:
     case SUBTRACT:
     case MULTIPLY:
     case DIVIDE:
     case CONCAT:
          if(atom->bin_op.left_exp) freeAtom(atom->bin_op.left_exp, true);
          if(atom->bin_op.right_exp) freeAtom(atom->bin_op.right_exp, true);
          break;

     default: printf("Error (freeAtom): Unexpected atom type: %d\n", 
                     (int)atom->type); 
              break;
   }
   if(free_atom) free(atom);
}

