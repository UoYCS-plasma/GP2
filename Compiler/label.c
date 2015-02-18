#include "label.h"

Label blank_label = {NONE, NULL, 0, false};

LabelClass getLabelClass(Label *label)
{
   int length = label->list_length;

   if(label->has_list_variable) return LISTVAR_L;
   if(label->list == NULL) return EMPTY_L;
   if(length > 1)
   {
      switch(length)
      {
         case 2: return LIST2_L;
         case 3: return LIST3_L;
         case 4: return LIST4_L;
         case 5: return LIST5_L;

         default: print_to_log("Error (getLabelClass): The length of the " 
                               "passed list exceeds the GP 2 maximum.\n");
      }
   }

   /* The list has length 1. */
   switch(label->list->first->atom->type)
   {
      case VARIABLE:

           return ATOMIC_VAR_L;     

      case INTEGER_CONSTANT:

      case NEG:

           return INT_L;

      case CHARACTER_CONSTANT:

      case STRING_CONSTANT:

      case CONCAT:

           return STRING_L;

      default:
           print_to_log("Error (getLabelClass): First element of passed list "
                        "has unexpected type %d.\n", 
                        label->list->first->atom->type);
           break;
   }

   return LISTVAR_L;
}

Label *copyLabel(Label *label)
{
   if(label == NULL || label == &blank_label) return label;

   Label *label_copy = malloc(sizeof(Label));

   if(label_copy == NULL) 
   {
      print_to_log("Memory exhausted during label copying.\n");
      exit(1);
   }

   label_copy->mark = label->mark;
   label_copy->list = copyGP2List(label->list);
   label_copy->list_length = label->list_length;
   label_copy->has_list_variable = label->has_list_variable;

   return label_copy;
}

GP2List *newGP2List(void)
{
   GP2List *new_list = malloc(sizeof(GP2List));

   if(new_list == NULL)
   {
      print_to_log("Error: Memory exhausted during GP2List copying.\n");
      exit(1);
   }

   new_list->first = NULL;
   new_list->last = NULL;

   return new_list;
}

GP2List *copyGP2List(GP2List *list)
{
   if(list == NULL) return NULL;

   GP2List *list_copy = newGP2List(); 
 
   GP2ListElement *iterator = list->first;
  
   while(iterator != NULL)
   {
      GP2Atom *atom_copy = copyGP2Atom(iterator->atom);
      append(list_copy, atom_copy);
   }

   return list_copy;
}

void append(GP2List *list, GP2Atom *atom)
{
   GP2ListElement *element = malloc(sizeof(GP2ListElement));

   if(element == NULL)
   {
      print_to_log("Error: Memory exhausted during GP2List appending.\n");
      exit(1);
   }

   element->atom = atom;
   element->prev = list->last;
   element->next = NULL;

   if(list->first == NULL) list->first = element;
   if(list->last == NULL) list->last = element;
   else
   {
      list->last->next = element;
      list->last = element;
   }
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

      case CHARACTER_CONSTANT:

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

void printGP2List(GP2List *list) 
{
    GP2ListElement *iterator = list->first;

    while(iterator != NULL) 
    {
        printGP2Atom(iterator->atom);
        if(iterator->next) printf(" : ");
        iterator = iterator->next;
    } 
}

void printGP2Atom(GP2Atom *atom) 
{
    switch(atom->type) 
    {
	case VARIABLE: 
	     printf("%s", atom->value.name);
	     break;

	case INTEGER_CONSTANT: 
	     printf("%d", atom->value.number);
	     break;

	case CHARACTER_CONSTANT:
	     printf("\"%s\"", atom->value.string);
	     break;

	case STRING_CONSTANT:
	     printf("\"%s\"", atom->value.string);
	     break;

	case INDEGREE:
	     printf("indeg(%s)", atom->value.node_id);
	     break;
 
	case OUTDEGREE:
	     printf("outdeg(%s)", atom->value.node_id);
	     break;

	case LIST_LENGTH:
	     printf("llength(");
	     printGP2List(atom->value.list_arg);
	     printf(")");
	     break;

	case STRING_LENGTH:
	     printf("slength(");
	     printGP2Atom(atom->value.str_arg);
	     printf(")");
	     break;

	case NEG:
	     printf("- ");
	     printGP2Atom(atom->value.exp);
	     break;

	case ADD:
	     printf("(");
	     printGP2Atom(atom->value.bin_op.left_exp);
	     printf(" + ");
	     printGP2Atom(atom->value.bin_op.right_exp);
	     printf(")");
	     break;

	case SUBTRACT:
	     printf("(");
	     printGP2Atom(atom->value.bin_op.left_exp);
	     printf(" - ");
	     printGP2Atom(atom->value.bin_op.right_exp);
	     printf(")");
	     break;

	case MULTIPLY:
	     printf("(");
	     printGP2Atom(atom->value.bin_op.left_exp);
	     printf(" * ");
	     printGP2Atom(atom->value.bin_op.right_exp);
	     printf(")");
	     break;

	case DIVIDE:
	     printf("(");
	     printGP2Atom(atom->value.bin_op.left_exp);
	     printf(" / ");
	     printGP2Atom(atom->value.bin_op.right_exp);
	     printf(")");
	     break;

	case CONCAT:
	     printf("(");
	     printGP2Atom(atom->value.bin_op.left_exp);
	     printf(" . ");
	     printGP2Atom(atom->value.bin_op.right_exp);
	     printf(")");
	     break;

	default: printf("Unexpected GP2Atom Type: %d\n",
		       (int)atom->type); 
		 break;
    }
}

void printMark(MarkType mark, bool verbose)
{
   switch(mark)
   {
      case NONE:

           break;

      case RED:
         
           if(verbose) printf("Mark: Red\n");
           else printf(" # red");

           break;

      case GREEN:

           if(verbose) printf("Mark: Green\n");
           else printf(" # green");

           break;

      case BLUE:

           if(verbose) printf("Mark: Blue\n");
           else printf(" # blue");

           break;

      case GREY:

           if(verbose) printf("Mark: Grey\n");
           else printf(" # grey");

           break;

      case DASHED:

           if(verbose) printf("Mark: Dashed\n");
           else printf(" # dashed");

           break;

      case ANY:

           if(verbose) printf("Mark: Any\n");
           else printf(" # any");

           break;

      default:
           print_to_log("Error (printMark): Unexpected mark type %d\n", mark);

           break;
   }
}

void freeLabel(Label *label)
{
   if(label == NULL) return;
   if(label->list) freeGP2List(label->list);
   free(label);
}

void freeGP2List(GP2List *list)
{
   if(list == NULL) return;

   GP2ListElement *iterator = list->first;

   while(iterator != NULL)
   {
      freeGP2Atom(iterator->atom);
      GP2ListElement *temp = iterator;
      iterator = iterator->next;
      free(temp);
   }
   free(list);
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


     case CHARACTER_CONSTANT:
       
     case STRING_CONSTANT:

           if(atom->value.string) free(atom->value.string);

           break;


     case INDEGREE:

     case OUTDEGREE:

           if(atom->value.node_id) free(atom->value.node_id);

           break;


     case LIST_LENGTH:

           if(atom->value.list_arg) freeGP2List(atom->value.list_arg);
             
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

