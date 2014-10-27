/* ///////////////////////////////////////////////////////////////////////////

  =====================================
  rule.c - Chris Bak (23/08/2014)
  =====================================

/////////////////////////////////////////////////////////////////////////// */

#include "rule.h"

VariableList *addVariable(VariableList *variable_list, string name, GPType type) 
{
   VariableList *new_variable_list = malloc(sizeof(VariableList));

   if(new_variable_list == NULL) 
   {
      print_to_log("Memory exhausted during rule construction.\n");
      exit(1);
   }

   new_variable_list->variable = strdup(name);
   new_variable_list->type = type;   
   new_variable_list->next = variable_list;

   return new_variable_list;
}

/* I assume this called only when a variable is known to be in the list. */
GPType lookupType(VariableList *variable_list, string name) 
{
   while(variable_list != NULL) 
   {
     if(strcmp(variable_list->variable, name) == 0) return variable_list->type;
     variable_list = variable_list->next;
   }
   print_to_log("Error: lookupType called incorrectly.");
   exit(0);         
}

void freeVariableList(VariableList *variable_list)
{
   if(variable_list == NULL) return;
   if(variable_list->variable) free(variable_list->variable);
   if(variable_list->next) freeVariableList(variable_list->next);  
   free(variable_list); 
}

