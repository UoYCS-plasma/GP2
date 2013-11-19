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

void declaration_scan(const List *ast, GHashTable *table, char *scope)
{

   char *proc_name = NULL, *rule_name = NULL;	
   static int main_count = 0;

   while(ast!=NULL) {     

      switch(ast->value.declaration->decl_type) {

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
	      proc_symbol->scope = strdup(scope);
	      proc_symbol->containing_rule = NULL;

	      /* Get the name of the procedure */
	      proc_name = ast->value.declaration->value.procedure->name;
	      
              GSList *symbol_list = g_hash_table_lookup(table, proc_name);

	      /* Report an error if the name already exiss in the table. */
              if(symbol_list != NULL)  
                 fprintf(stderr,"Error: Procedure \"%s\" declared more than " 
                         "once.\n\n", proc_name);

              symbol_list = g_slist_prepend(symbol_list, proc_symbol);           
              g_hash_table_insert(table, proc_name, symbol_list);        

              /* Scan for any local declarations with a new local scope. */
              declaration_scan(ast->value.declaration->value.procedure->
			       local_decls, table, proc_name);

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
	      rule_symbol->scope = strdup(scope);
	      rule_symbol->containing_rule = NULL;

	      /* Get the name of the rule */
  	      rule_name = ast->value.declaration->value.rule->name;	
 
              GSList *symbol_list = g_hash_table_lookup(table, rule_name);

              /* Report an error if two rules with the same name are declared
               * in the same scope.
               */
              if(symbol_list != NULL) {
                 
              GSList *iterator = NULL;
              char *current_scope = ((Symbol*)symbol_list->data)->scope;

                 for(iterator = symbol_list; iterator!=NULL; 
	             iterator = iterator->next) {
                 
                     char *symbol_scope = ((Symbol*)iterator->data)->scope;

                     if(!strcmp(current_scope,symbol_scope)) {
                        fprintf(stderr,"Error: Rule \"%s\" declared twice " 
                                "within the scope \"%s\".\n\n", 
                                 rule_name, scope);                                  
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
                     ast->value.declaration->decl_type, 
		     ast->value.declaration->node_id);

             break; 

      }     

   /* Proceed down the declaration list */
   ast = ast->next;  	

   }

   if(!strcmp(scope,"Global")) {
     if(main_count == 0) fprintf(stderr,"Error: No main procedure.\n\n");
     if(main_count > 1) 
      fprintf(stderr,"Error: More than one main procedure declared.\n\n");
   }
}


/* This function performs semantic analysis on a GP program post-parsing. It
 * also reverses lists in the AST as Bison constructs these lists in reverse
 * order due to its left-recursive rules. 
 *
 * The first argument is a pointer to the abstract syntax tree of the input 
 * program.
 * The second argument is the symbol table. When semantic_check is called it 
 * only contains rule and procedure identifiers added by declaration_scan.
 * semantic_check will added other symbols to the symbol table and use
 * them for semantic analysis.
 * The third argument is the current scope. It is initially called with 
 * scope "Global".
 *
 * The main body recurses over declaration lists, handling each of the three 
 * declaration types with the help of subprocedures. table and scope are passed
 * to each of these subprocedures.scope is changed whenever a procedure 
 * declaration is encountered.
 */

int semantic_check(List *ast, GHashTable *table, char *scope)
{
   while(ast!=NULL) {

      GPDeclaration *current_declaration = ast->value.declaration;
   
      switch(current_declaration->decl_type) {

         case MAIN_DECLARATION:

              if(current_declaration->value.main_program != NULL)
		 statement_scan(current_declaration->value.main_program,
                                table, scope);
	      else fprintf(stderr,"Error: Main program is empty.\n");

              break;

         case PROCEDURE_DECLARATION: 
	 {
              /* Set scope to procedure name in case of local declarations */
              char *new_scope = current_declaration->value.procedure->name;

              if(current_declaration->value.procedure->cmd_seq != NULL)
                  statement_scan(current_declaration->value.procedure->cmd_seq,
                                 table, new_scope);
	      else fprintf(stderr,"Error: %s program is empty.\n",
			   current_declaration->value.procedure->name);

	      if(current_declaration->value.procedure->local_decls != NULL)
                  semantic_check(current_declaration->value.procedure->
				 local_decls, table, new_scope);

              break;
	 }    

         case RULE_DECLARATION:              

              rule_scan(current_declaration->value.rule, table, scope);

              break;  

         default: fprintf(stderr,"Error: Unexpected declaration type %d\n", 
                          (int)current_declaration->decl_type);
              break;
      } 

   /* Proceed down the declaration list */
   ast = ast->next;  

   }
 
   return 0;
}   

void rule_scan(GPRule *rule, GHashTable *table, char *scope)
{   
   char *rule_name = rule->name;
   List *current_var_list = rule->variables;

   /* enter variable declarations into symbol table */
   while(current_var_list != NULL) {

      switch(current_var_list->list_type) {
	   
         case INT_DECLARATIONS:

              enter_variables("Integer", current_var_list->value.variables,
			      table, scope, rule_name);

	      break;

         case STRING_DECLARATIONS:

      	      enter_variables("String", current_var_list->value.variables,
			      table, scope, rule_name);

              break;
   	
         case ATOM_DECLARATIONS:

	      enter_variables("Atom", current_var_list->value.variables,
			      table, scope, rule_name);

	      break; 

	 case LIST_DECLARATIONS:

	      enter_variables("List", current_var_list->value.variables,
			      table, scope, rule_name);

	      break;  	 

	 default:
	      fprintf(stderr,"Error: Unexpected list type %d\n",
		      (int)current_var_list->list_type);
      }
      current_var_list = current_var_list->next;
   }

   // graph_scan(rule->lhs);
   // graph_scan(rule->rhs);
   
   List *interface_list = rule->interface;

   while(interface_list != NULL) {
	/* perform semantic checking on each NODE_PAIR AST node.
	 * This includes checking to see if the nodes are declared
	 * and checking for injective properties if the rule is 
	 * injective (if (rule->injective == true))
	 */
      interface_list = interface_list->next;   
   }

   // condition_scan(rule->condition);
}   

void enter_variables(char *type, List *variables, GHashTable *table, 
		     char *scope, char *rule)
{
   while(variables != NULL) {
 
      char *variable_name = variables->value.variable_name;	   

      /* Create a symbol for the variable name */

      Symbol *var_symbol = malloc(sizeof(Symbol));

      if(var_symbol==NULL) {
         fprintf(stderr,"Insufficient space.\n");
         exit(0);
      }

      var_symbol->type = strdup(type);
      var_symbol->scope = strdup(scope);
      var_symbol->containing_rule = strdup(rule);

      GSList *symbol_list = g_hash_table_lookup(table, variable_name);
      symbol_list = g_slist_prepend(symbol_list, var_symbol);
      g_hash_table_insert(table, variable_name, symbol_list);

      variables = variables->next;
   }
}  


/* validate_call is called when a RULE_CALL or PROCEDURE_CALL AST node
 * is reached. The function will check the rule/procedure name to see if it
 * exists in the symbol table with an appropriate scope. If not,
 * an error is reported. 
 */

void validate_call(char *name, GHashTable *table, char *scope) {

   GSList *symbol_list = g_hash_table_lookup(table, name);

      if(symbol_list == NULL) fprintf(stderr, "Error: %s has not been "
                                           "declared.\n", name);
      else {
         char *symbol_scope = ((Symbol*)symbol_list->data)->scope;
                
         /* A rule call is valid if a rule is declared in global scope
          * or in the current scope of execution, specified by the
          * parameter 'scope'.
          * The while loop proceeds through the symbol list if the 
          * current symbol does not have a valid scope. If the end of the 
          * symbol list is reached, an error is reported.
          */
         while( strcmp(symbol_scope,scope) &&
                strcmp(symbol_scope,"Global") ) {

            symbol_list = symbol_list->next;

            if(symbol_list == NULL) {
               fprintf(stderr, "Error: %s called but not declared in a "
                       "visible scope.\n", name);     
               break;
            }
            else symbol_scope = ((Symbol*)symbol_list->data)->scope;             
          }
      }
}

   
/* statement_scan is called whenever a GPStatement node is reached in the AST.
 * The first argument is a pointer to the GPStatement node.
 * The second and third arguments are passed from semantic_check.
 * The function searches for rule and procedure calls and checks them with
 * the auxiliary function validate_call.
 */

void statement_scan(GPStatement *statement, GHashTable *table, char *scope) 
{
      switch(statement->statement_type) {

         case COMMAND_SEQUENCE: 
         {

	      /* A COMMAND_SEQUENCE node always points to a list of COMMAND
	       * nodes. This list needs to be reversed. 
	       *
	       * Since the list reversal is a global modification, the pointer
	       * to the head of the command list is modified by reference.
	       * Hence command_list points to the address of the pointer to
	       * the head.
	       */

              List **command_list = &(statement->value.cmd_seq);

	      *command_list = reverse(*command_list);  

	      /* Create a new list pointer to recurse over the reversed
	       * command list. 
	       */

	      List *temp_command = *command_list;

              while(temp_command != NULL) {
                 statement_scan(temp_command->value.command, 
                                       table, scope);    
                 temp_command = temp_command->next;   
              }           
     
              break;
         }

         case RULE_CALL:

              validate_call(statement->value.rule_name, table, scope); 
                              
              break;

         case PROCEDURE_CALL:   

              validate_call(statement->value.proc_name, table, scope);

              break;

         case RULE_SET_CALL: 
         {
              /* Reverse the list of RULE_CALL nodes pointed to by a 
	       * RULE_SET_CALL node.
	       *
	       * Since the list reversal is a global modification, the pointer
	       * to the head of the rule call list is modified by reference.
	       * Hence rule_list points to the address of the pointer to
	       * the head.
	       */

	      List **rule_list = &(statement->value.rule_set);

              /* not necessary to reverse as this list represents a set of 
               * rules to be called nondeterministically. If these are called 
               * in textual order, then the order of the AST list may have 
               * significance, otherwise it does not.     
               */
           
              *rule_list = reverse(*rule_list);
	
              /* Create a new list pointer to recurse over the reversed
	       * rule call list. 
	       */     
	      List *temp_list = *rule_list;
  
              while(temp_list != NULL) {
                 validate_call(temp_list->value.rule_name, table, scope);
                 temp_list = temp_list->next;
              }
    
              break;
         }
             

         case IF_STATEMENT:

              statement_scan(statement->value.cond_branch.condition, 
                                    table, scope);
   
              statement_scan(statement->value.cond_branch.then_stmt,
                                    table, scope);

              statement_scan(statement->value.cond_branch.else_stmt,
                                    table, scope);

              break;

         case TRY_STATEMENT:

              statement_scan(statement->value.cond_branch.condition, 
                                    table, scope);
   
              statement_scan(statement->value.cond_branch.then_stmt, 
                                    table, scope);

              statement_scan(statement->value.cond_branch.else_stmt, 
                                    table, scope);

              break;

         case ALAP_STATEMENT:

              statement_scan(statement->value.loop_stmt,table, scope);

	      break;

         case PROGRAM_OR:

              statement_scan(statement->value.or_stmt.left_stmt, table, scope);

              statement_scan(statement->value.or_stmt.right_stmt, table,
			     scope);

	      break;

         case SKIP_STATEMENT: /* do nothing */ break;
   
         case FAIL_STATEMENT: /* do nothing */ break;

         default: fprintf(stderr,"Error: Unexpected statement type %d\n", 
                          (int)statement->statement_type);
                 break;   

        }
}             
   
                                           
