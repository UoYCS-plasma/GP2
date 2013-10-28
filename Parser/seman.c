/* ////////////////////////////////////////////////////////////////////////////

                                seman.c                              
 
       Defines the semantic analysis function and its subprocedures.
 

                     Created on 24/10/13 by Chris Bak 

//////////////////////////////////////////////////////////////////////////// */

#include "ast.h" /* reverse */
#include <glib.h> /* hashtable and linked list functions */

/* declaration_scan probes the AST for all rule and procedure declarations,
 * adding the names to the symbol table along with their type and scope.
 *
 * GLib is used for hashtables. GP2 identifiers are the keys. The values
 * are lists of struct Symbols, defined in seman.h. Lists are used as in some 
 * cases we want to store more than one occurrence of the same identifier. 
 * We further take advantage of GLib by using GLib's singly-linked lists (GSLists).
 */

/* Creates a new hashtable with strings as keys. g_str_equal is a 
 * string hashing function built into GLib. */
static GHashTable *symbol_table = g_hash_table_new(g_str_hash, g_str_equal);

void destroy(gpointer key, gpointer value, gpointer data) 
{
   free(data); /* identifiers stored in the heap by strdup in gplexer.l */	
   g_slist_free(value);
}

/* add_symbol places the symbol object pointed to by SYMBOL in the bucket
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
 * The third line adds the updated symbol_list into the symbol table. */


#define add_symbol(KEY, SYMBOL)                                      \
  do {	  						             \
       GSList *symbol_list = g_hash_table_lookup(symbol_table, KEY); \
       symbol_list = g_slist_prepend(symbol_list, SYMBOL);           \
       g_hash_table_insert(symbol_table, KEY, symbol_list);          \
     }							             \
  while(0)

void declaration_scan(List *ast)
{
   int main_count = 0;
   /* keeps track of the procedure being processed */
   static char *current_scope = "Main"; 

   while(ast!=NULL) {     

      /* The MAIN_DECLARATION node points only to a command sequence: no declarations. */
      if(ast->decl_type==MAIN_DECLARATION) {
	 main_count += 1;
	 declaration_scan(ast->next); 
      }	 
  
      if(ast->decl_type==PROCEDURE_DECLARATION) {

         /* Create a symbol for the procedure name */

	 Symbol *proc_symbol = malloc(sizeof(Symbol));

	 if(proc_symbol==NULL) {
           fprintf(stderr,"Insufficient space.\n");
	   exit(0);
	 }

	 Symbol->symbol_type_t = PROCEDURE;
	 Symbol->scope = current_scope;

         add_symbol(ast->value.proc->name,proc_symbol);

         /* Set the new scope and look for any local declarations. */

         current_scope = ast->value.proc->name;

         declaration_scan(ast->value.proc->local_decls);

         /* Set the scope back to Main and continue down the global 
          * declaration list. 
          */
 
         current_scope = "Main";

         declaration_scan(ast->next);
      }	 
	      
  
      if(ast->decl_type==RULE_DECLARATION) {

	 /* Create a symbol for the rule name */

	 Symbol *rule_symbol = malloc(sizeof(Symbol));

	 if(rule_symbol==NULL) {
           fprintf(stderr,"Insufficient space.\n");
	   exit(0);
	 }

	 Symbol->symbol_type_t = RULE;
	 Symbol->scope = current_scope;

         add_symbol(ast->value.rule->name,rule_symbol);

         declaration_scan(ast->next);
      }	 

      if(ast->decl_type!=NULL) 
         fprintf(stderr, "Error: Unexpected node type %d at node %d\n\n", 
                 ast->value.decl_type, ast->value.node_id);

      if(main_count == 0) fprintf(stderr,"Error: No main procedure.\n");

      if(main_count > 1) 
	fprintf(stderr,"Error: More than one main procedure declared.\n");

   }

int semantic_check(List *ast)
{
   ast = reverse(ast); /* reverses the global decl list at the top of the AST */
}   
   
  
                              
