/* ////////////////////////////////////////////////////////////////////////////

                                seman.c                              
 
       Defines the semantic analysis function and its subprocedures.
 

                     Created on 24/10/13 by Chris Bak 

//////////////////////////////////////////////////////////////////////////// */

#include "ast.h" /* reverse */
#include "seman.h" /* struct Symbol */
#include <stdlib.h> /* malloc */
#include <stdio.h> /* fprintf */
#include <string.h> /* strdup, strcmp */
#include <stdbool.h> 
#include <glib.h> /* GHashTable and GSList */

/*
 * GLib is used for hashtables. GP2 identifiers are the keys. The values
 * are lists of struct Symbols, defined in seman.h. Lists are used as in some 
 * cases we want to store more than one occurrence of the same identifier. 
 * We further take advantage of GLib by using GLib's singly-linked lists (GSLists).
 *
 * add_symbol places the symbol object pointed to by SYMBOL in the bucket
 * of the symbol table with index hash(KEY).
 *
 * The first line creates a pointer to a GSList by calling g_hash_table_lookup.
 * This function looks up a name in the symbol table and returns a pointer to the 
 * identifier list if the name is already present, otherwise it returns NULL.
 * Note that g_hash_table_lookup returns a void pointer.
 *
 * The second line appends SYMBOL to the start of the created list symbol_list.
 * If symbol_list is NULL then a new list is created with the single element
 * SYMBOL.
 *
 * The third line adds the updated symbol_list into the symbol table. 
 */


#define add_symbol(KEY, SYMBOL)                                      \
  do {	  						             \
       GSList *symbol_list = g_hash_table_lookup(table, KEY);        \
       symbol_list = g_slist_prepend(symbol_list, SYMBOL);           \
       g_hash_table_insert(table, KEY, symbol_list);                 \
     }							             \
  while(0)



/* void destroy(gpointer key, gpointer value, gpointer data) 
{
   free(data); * identifiers stored in the heap by strdup in gplexer.l 	
   g_slist_free(value);
} */

/* declaration_scan traverses the global declaration list and any local 
 * declaration lists. It adds all procedure declarations and rule declarations
 * to the symbol table.
 * 
 * Argument 1: The head of a declaration list in the AST
 * Argument 2: The symbol table
 * Argument 3: The scope of the declaration list the function is traversing.
 *    This is either "Main" (initial value) or the name of a procedure.
 *
 * The function keeps track of the amount of Main declarations for error 
 * reporting. It recurses over declaration lists. The code after the
 * while loop is executed only when in global scope. That is, when the end
 * of the global declaration list is reached. This prevents the error messages
 * being printed more than once when exiting recursive calls. 
 *
 * The function also checks for multiple occurrences of a single procedure 
 * name and for multiple occurrences of the same rule name in the same scope,
 * which are both errors that should be reported. This is done while names are 
 * added to the symbol table.
 */

void declaration_scan(const List *ast, GHashTable *table, char *current_scope)
{

   char *proc_name = NULL, *rule_name = NULL;	
   static int main_count = 0;

   while(ast!=NULL) {     

      switch(ast->value.decl->decl_type) {

	 /* The MAIN_DECLARATION node points only to a command sequence: no 
          * declarations in the subtree.
          */
         case MAIN_DECLARATION:
   
              main_count += 1;

              break; 	 
  
         case PROCEDURE_DECLARATION:
         {
              /* Create a symbol for the procedure name */
	      Symbol *proc_symbol = malloc(sizeof(Symbol));

	      if(proc_symbol==NULL) {
                 fprintf(stderr,"Insufficient space.\n");
	         exit(0);
	      }

	      proc_symbol->type = strdup("Procedure");
	      proc_symbol->scope = strdup(current_scope);

	      /* Get the name of the procedure */
	      proc_name = ast->value.decl->value.proc->name;
	      
              GSList *symbol_list = g_hash_table_lookup(table, proc_name);

	      /* Report an error if the name already exiss in the table. */
              if(symbol_list != NULL)  
                 fprintf(stderr,"Error: Procedure \"%s\" declared more than " 
                         "once.\n\n", proc_name);

              symbol_list = g_slist_prepend(symbol_list, proc_symbol);           
              g_hash_table_insert(table, proc_name, symbol_list);        

              /* Scan for any local declarations with a new local scope. */
              declaration_scan(ast->value.decl->value.proc->local_decls, table,
                               proc_name);

              break;
         }
  
         case RULE_DECLARATION:
         {
	      /* Create a symbol for the rule name */

	      Symbol *rule_symbol = malloc(sizeof(Symbol));
 
	      if(rule_symbol==NULL) {
                 fprintf(stderr,"Insufficient space.\n");
	         exit(0);
	      }

	      rule_symbol->type = strdup("Rule");
	      rule_symbol->scope = strdup(current_scope);

	      /* Get the name of the rule */
  	      rule_name = ast->value.decl->value.rule->name;	
 
              GSList *symbol_list = g_hash_table_lookup(table, rule_name);

              /* Report an error if two rules with the same name are declared
               * in the same scope.
               */
              if(symbol_list != NULL) {
                 
              GSList *iterator = NULL;
              char *current_scope = ((Symbol*)symbol_list->data)->scope;

                 for(iterator = symbol_list; iterator!=NULL; 
	             iterator = iterator->next) {
                 
                     char *tmp_scope = ((Symbol*)iterator->data)->scope;

                     if(!strcmp(current_scope,tmp_scope)) {
                        fprintf(stderr,"Error: Rule \"%s\" declared twice " 
                                "within the scope \"%s\".\n\n", 
                                 rule_name, current_scope);                                 
                     /* Report the error only once for each violating rule */
                     break; 
                     }
                 }
              }             

              symbol_list = g_slist_prepend(symbol_list, rule_symbol);           
              g_hash_table_insert(table, rule_name, symbol_list);   

              break;
         }

         default: 
             fprintf(stderr, "Error: Unexpected node type %d at node %d\n\n", 
                     ast->value.decl->decl_type, ast->value.decl->node_id);

             break; 

      }     

   /* Proceed down the declaration list */
   ast = ast->next;  	

   }

   if(!strcmp(current_scope,"Global")) {
     if(main_count == 0) fprintf(stderr,"Error: No main procedure.\n\n");
     if(main_count > 1) 
      fprintf(stderr,"Error: More than one main procedure declared.\n\n");
   }
}



int semantic_check(List *ast)
{
   ast = reverse(ast); /* reverses the global decl list at the top of the AST */
   return 0;
}   
   
  
                              
