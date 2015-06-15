#include "label.h"

Label blank_label = {NONE, 0, NULL, NULL};

Label makeEmptyLabel(MarkType mark)
{
   Label label = { .mark = mark, .length = 0, .first = NULL, .last = NULL };
   return label;
}

Label makeHostLabel(MarkType mark, int length, GPList *list)
{
   Label label = { .mark = mark, .length = length, .first = list, 
                   .last = getLastElement(list) };
   return label;
}

GPList *appendList(GPList *list, GPList *list_to_append)
{
   while(list_to_append != NULL)
   {
      assert(list_to_append->atom.type == INTEGER_CONSTANT ||
             list_to_append->atom.type == STRING_CONSTANT);
      if(list_to_append->atom.type == INTEGER_CONSTANT)
         list = appendIntegerAtom(list, list_to_append->atom.number);
      else list = appendStringAtom(list, list_to_append->atom.string);
      list_to_append = list_to_append->next;
   }
   return list;
}

GPList *appendAtom(GPList *list, Atom atom)
{
   GPList *new_list = malloc(sizeof(GPList));
   if(new_list == NULL)
   {
      print_to_log("Error (appendAtom): malloc failure.\n");
      exit(1);
   }
   new_list->atom = atom;
   new_list->next = NULL;

   if(list == NULL)
   {
      new_list->prev = NULL;
      return new_list;
   }
   else
   {
      GPList *last = getLastElement(list);
      last->next = new_list;
      new_list->prev = last;
      return list;
   }
}

GPList *appendIntegerAtom(GPList *list, int value)
{
   Atom atom;
   atom.type = INTEGER_CONSTANT;
   atom.number = value;
   return appendAtom(list, atom);
}

GPList *appendStringAtom(GPList *list, string value)
{
   Atom atom;
   atom.type = STRING_CONSTANT;
   atom.string = strdup(value);
   return appendAtom(list, atom);
}

GPList *getLastElement(GPList *list)
{
   if(list == NULL) return NULL;
   while(list->next != NULL) list = list->next;
   return list;
}

int getListLength(GPList *list)
{
   if(list == NULL) return 0;
   int length = 0;
   while(list != NULL) 
   {
      length++;
      list = list->next;
   }
   return length;
}

bool equalLabels(Label left_label, Label right_label)
{
   if(left_label.mark != right_label.mark) return false;
   if(left_label.length != right_label.length) return false;

   GPList *left_list = left_label.first;
   GPList *right_list = right_label.first;
   if(left_list == NULL && right_list == NULL) return true;
  
   while(left_list != NULL)
   {
      if(right_list == NULL) return false;
      if(!equalAtoms(&(left_list->atom), &(right_list->atom))) return false;
      left_list = left_list->next;
      right_list = right_list->next;
   }
   return true;
}

bool equalAtoms(Atom *left_atom, Atom *right_atom)
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
           return !equalAtoms(left_atom->neg_exp, right_atom->neg_exp);

      case CONCAT:
           if(!equalAtoms(left_atom->bin_op.left_exp, 
                          right_atom->bin_op.left_exp)) return false;
           if(!equalAtoms(left_atom->bin_op.right_exp, 
                          right_atom->bin_op.right_exp)) return false;
           return true;

      default: break;
   }
   return false;
}

bool hasListVariable(Label label)
{
   if(label.first == NULL) return false;
   GPList *list = label.first;
   while(list != NULL)
   {
      if(list->atom.type == VARIABLE && 
         list->atom.variable.type == LIST_VAR) return true;
      list = list->next;
   }
   return false;
}

void copyLabel(Label *source, Label *target)
{
   target->mark = source->mark;
   target->length = source->length;
   GPList *list = copyList(source->first, NULL);
   target->first = list;
   target->last = getLastElement(list);
}

GPList *copyList(GPList *head, GPList *tail)
{
   if(head == NULL) return NULL;

   /* Stores the head of the copied list to return. */
   GPList *head_of_copy = NULL;
   /* A placeholder to facilitate the list copying. */
   GPList *previous_node = NULL;
      
   while(head != tail)
   {
      GPList *list_copy = malloc(sizeof(GPList));
      if(list_copy == NULL)
      {
         print_to_log("Error (copyLabel): malloc failure.\n");
         exit(1);
      }

      list_copy->atom = head->atom;
      /* Duplicate pointers to allocated memory in the atom. */
      AtomType type = head->atom.type;
      if(type == STRING_CONSTANT)
         list_copy->atom.string = strdup(head->atom.string);
      else if(type == VARIABLE || type == LENGTH)
         list_copy->atom.variable.name = strdup(head->atom.variable.name);
      else if(type == NEG)
         list_copy->atom.neg_exp = copyAtom(head->atom.neg_exp);
      else if(type == ADD || type == SUBTRACT || type == MULTIPLY ||
              type == DIVIDE || type == CONCAT)
      {
         list_copy->atom.bin_op.left_exp = copyAtom(head->atom.bin_op.left_exp);
         list_copy->atom.bin_op.right_exp = copyAtom(head->atom.bin_op.right_exp);
      }

      list_copy->next = NULL;
      list_copy->prev = previous_node;
      /* previous_node is NULL only on the first loop iteration. */
      if(previous_node == NULL) head_of_copy = list_copy;
      else previous_node->next = list_copy;
      previous_node = list_copy;
      head = head->next;
   }
   return head_of_copy;
}

Atom *copyAtom(Atom *atom)
{
   Atom *copy = malloc(sizeof(Atom));
   if(copy == NULL)
   {
      print_to_log("Error (copyAtom): malloc failure.\n");
      exit(1);
   }
   copy->type = atom->type;
   switch(atom->type) 
   {
      case INTEGER_CONSTANT:
           copy->number = atom->number;
           break;

      case STRING_CONSTANT:
           copy->string = strdup(atom->string);
           break;
      
      case VARIABLE:
      case LENGTH:
           copy->variable.name = strdup(atom->variable.name);
           copy->variable.type = atom->variable.type;
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
   if(label.length == 0) fprintf(file, "empty");
   else printList(label.first, file);
   printMark(label.mark, file);
}

void printList(GPList *list, FILE *file)
{
   while(list != NULL)
   {
      printAtom(&(list->atom), false, file);
      if(list->next != NULL) fprintf(file, " : ");
      list = list->next;
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
   freeList(label.first);
}

void freeList(GPList *list)
{
   if(list == NULL) return;
   freeAtom(&(list->atom), false);
   freeList(list->next);
   free(list);
}

void freeAtom(Atom *atom, bool free_atom)
{
   if(atom == NULL) return;
   switch(atom->type) 
   {
     case VARIABLE:
          if(atom->variable.name != NULL) free(atom->variable.name);
          break;

     case INTEGER_CONSTANT:
          break;

     case STRING_CONSTANT:
          if(atom->string != NULL) free(atom->string);
          break;

     case INDEGREE:
     case OUTDEGREE:
          break;

     case LENGTH:
          if(atom->variable.name != NULL) free(atom->variable.name);
          break;

     case NEG:
          if(atom->neg_exp != NULL) freeAtom(atom->neg_exp, true);
          break;

     case ADD:
     case SUBTRACT:
     case MULTIPLY:
     case DIVIDE:
     case CONCAT:
          if(atom->bin_op.left_exp != NULL) freeAtom(atom->bin_op.left_exp, true);
          if(atom->bin_op.right_exp != NULL) freeAtom(atom->bin_op.right_exp, true);
          break;

     default: printf("Error (freeAtom): Unexpected atom type: %d\n", 
                     (int)atom->type); 
              break;
   }
   if(free_atom) free(atom);
}

