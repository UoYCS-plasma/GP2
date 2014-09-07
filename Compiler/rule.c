/* ///////////////////////////////////////////////////////////////////////////

  =====================================
  rule.c - Chris Bak (23/08/2014)
  =====================================

/////////////////////////////////////////////////////////////////////////// */

#include "rule.h"

void addVariable(VariableList *variable_list, string name, GPType type) 
{
   VariableList *new_variable_list = malloc(sizeof(VariableList));

   if(new_variable_list == NULL) 
   {
      print_to_log("Memory exhausted during rule construction.\n");
      exit(1);
   }

   new_variable_list->variable = name;
   new_variable_list->type = type;   
   new_variable_list->next = variable_list;
}

/* I assume this called only when a variable is known to be in the list. */
GPType lookupType(VariableList *variable_list, string name) 
{
   while(variable_list != NULL) 
   {
     if(variable_list->variable == name) return variable_list->type;
     variable_list = variable_list->next;
   }
   print_to_log("Error: lookupType called incorrectly.");
   exit(0);         
}

void freeVariableList(VariableList *variable_list)
{
   while(variable_list->next != NULL) 
   {
      freeVariableList(variable_list->next);
   }
   free(variable_list);
}


GraphMapping *addMapping(GraphMapping *mapping, int rule_item, int host_item, 
                         bool flag) 
{
   GraphMapping *new_mapping = malloc(sizeof(GraphMapping));

   if(new_mapping == NULL) 
   {
      print_to_log("Memory exhausted during mapping construction.\n");
      exit(1);
   }

   new_mapping->rule_item = rule_item; 
   new_mapping->host_item = host_item;
   new_mapping->flag = flag;
   new_mapping->next = mapping;

   return new_mapping;
}


GraphMapping *removeMapping(GraphMapping *mapping)
{
   GraphMapping *new_mapping = mapping->next;
   free(mapping);
   return new_mapping;
}


int lookupFromRule(GraphMapping *mapping, int rule_item) 
{
   while(mapping != NULL) 
   {
     if(mapping->rule_item == rule_item) return mapping->host_item;
     mapping = mapping->next;
   }
   return -1;
         
}

int lookupFromHost(GraphMapping *mapping, int host_item) 
{
   while(mapping != NULL) 
   {
     if(mapping->host_item == host_item) return mapping->rule_item;
     mapping = mapping->next;
   }
   return -1;
         
}

void freeMapping(GraphMapping *mapping) 
{
   while(mapping->next != NULL) freeMapping(mapping->next);
   free(mapping);
}
   

bool verifyList(Assignment *assignment, string name, GList *value) 
{
   GList *current_value = lookupValue(assignment, name);
   
   /* If the variable does not have a value in the current assignment,
    * there is no verification to be performed. */
   if(current_value == NULL) 
   {
      addAssignment(assignment, name, value);
      return true;
   }

   /* Otherwise, a value already exists for this variable. We need to check
    * if the value passed to this function is equal to the existing one. This 
    * is done by stepping through both value and existing_value (GLists) and 
    * comparing their elements, which should be constant integers or strings. 
    * Return false if an inconsistency is found.
    */
   while(current_value != NULL)
   {
      ListElement *current_atom = (ListElement*)current_value->data;
      
      /* Check if value is shorter than existing_value. */
      if(value == NULL) return false;

      ListElement *atom = (ListElement*)value->data;

      if(!compareConstants(current_atom, atom)) return false;

      current_value = current_value->next;
      value = value->next;
   }

   /* At this point the while loop has exited without returning false.
    * One final check needs to be made: have we reached the end of value?
    * If not, then value contains more elements than existing_value and they
    * are not equal lists.
    */
   return(value == NULL);
}	 


bool verifyAtom(Assignment *assignment, string name, ListElement *value)
{
   GList *current_value = lookupValue(assignment, name);
   
   /* If the variable does not have a value in the current assignment,
    * there is no verification to be performed. */
   if(current_value == NULL) 
   {
      /* Convert value to a GList so that it may be added to the assignment. */
      current_value = g_list_prepend(current_value, value);
      addAssignment(assignment, name, current_value);
      return true;
   }

   /* Check if current_value has more than one element. It shouldn't, as this
    * function is called when considering the assignment of a variable with
    * atomic type. */
   if(current_value->next != NULL)
   {
      print_to_log("Error: non-list variable %s assigned a non-atomic value.\n",
                   name);
      return false;
   }

   ListElement *current_atom = (ListElement *)current_value->data;
   return(compareConstants(current_atom, value));
}

bool compareConstants(ListElement *atom, ListElement *test_atom)
{

   switch (test_atom->type)
   {
     
      case INTEGER_CONSTANT: 
                  
            if(atom->type != INTEGER_CONSTANT || 
               test_atom->value.number != atom->value.number)
                 return false;
                
            break;
                  
             
      case CHARACTER_CONSTANT:      	 
           
            if(atom->type != CHARACTER_CONSTANT ||
               strcmp(test_atom->value.string, atom->value.string) != 0)
                  return false;
                  
            break;
             
      case STRING_CONSTANT:     
          
            if((atom->type != CHARACTER_CONSTANT &&
                atom->type != STRING_CONSTANT) ||
               strcmp(test_atom->value.string, atom->value.string) != 0)
                  return false;
               
            break;
             
               
      default: print_to_log("Error: Unexpected list element type %d "
                             "encountered during variable assignment.\n",
	     	                test_atom->type);
            return false;
   }
   return true;
}		

Assignment *addAssignment(Assignment *assignment, string name, GList *value)
{
   Assignment *new_assignment = malloc(sizeof(Assignment));

   if(new_assignment == NULL) 
   {
      print_to_log("Memory exhausted during assignment construction.\n");
      exit(1);
   }

   new_assignment->variable = name;
   new_assignment->value = value;   
   new_assignment->next = assignment;

   return new_assignment;
}


GList *lookupValue(Assignment *assignment, string name)
{
   while(assignment != NULL) 
   {
     if(assignment->variable == name) return assignment->value;
     assignment = assignment->next;
   }
   return NULL;
         
}

void freeAssignment(Assignment *assignment)
{
   /* Need to free GLists as well */
   while(assignment->next != NULL) freeAssignment(assignment->next);
   free(assignment);
}


Stack *newStack (int max_size) 
{
   Stack *new_stack = malloc(sizeof(Stack));

   if(new_stack == NULL) 
   {
      print_to_log("Memory exhausted during stack construction.\n");
      exit(1);
   }
 
   void **items = calloc(max_size, sizeof(void*));

   if(items == NULL) 
   {
      print_to_log("Memory exhausted during stack construction.\n");
      exit(1);
   } 

   new_stack->top = 0;
   new_stack->max_size = max_size;
   new_stack->items = items;

   return new_stack;
}

void push (Stack *stack, void *data) 
{
   if(stack->top == stack->max_size) 
      print_to_log("Warning: Trying to push to a full stack.\n");
   else 
   {
      stack->items[stack->top] = data;
      stack->top++;
   }
}

void *pop (Stack *stack) 
{
   void *popped_item = NULL;

   if(stack->top == 0) 
   {
      print_to_log("Warning: Trying to pop from an empty stack.\n");
      return NULL;
   }
   else 
   {
      stack->top--;
      popped_item = stack->items[stack->top];
   }
   
   return popped_item;
}

void freeStack (Stack *stack) 
{
   /* Need to free the items in the stack rule_item. */
   free(stack->items);
   free(stack);
}
