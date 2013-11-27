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

/* GLib is used for hashtables. GP2 identifiers are the keys. The values
 * are lists of struct Symbols, defined in seman.h. We further take advantage
 * of GLib by using GLib's singly-linked lists (GSList).
 *
 * The following glib function calls are used extensively in this file.
 *
 *
 * GSList *list = g_hash_table_lookup(table, key);
 *
 * Creates a pointer to a GSList by calling g_hash_table_lookup. This function 
 * looks up a name in the symbol table and returns a pointer to the identifier
 * list if the name is already present, otherwise it returns NULL. 
 * Note that g_hash_table_lookup returns a void pointer.
 *
 *
 * list = g_slist_prepend(list, symbol);     
 *
 * Adds the symbol given by the second argument to the start of the GSList
 * list. If list is NULL then a new list is created with the single element
 * symbol.
 *
 *
 * g_hash_table_insert(table, key, list); 
 *
 * Inserts the GSList list to the symbol table. If the key already exists 
 * then the old value for that key is freed with <value_destroy_func> and 
 * replaced with the new value. This means that only one symbol list will 
 * exist for a particular key.
 *
 *
 * These three function calls are made in succession to ensure that the
 * symbol list for a particular identifier always contains all the identifiers
 * encountered during the semantic analysis.
 */




/* void destroy(gpointer key, gpointer value, gpointer data) 
{
   free(data); * identifiers stored in the heap by strdup in gplexer.l 	
   g_slist_free(value);
} */

/* declaration_scan traverses the global declaration list and any local 
 * declaration lists. It adds all procedure declarations and rule declarations
 * to the symbol table.
 * 
 * Argument 1: The root of the AST which is the head of the global declaration
 *             list.
 * Argument 2: The global symbol table.
 * Argument 3: The scope of the declaration list the function is traversing.
 *             This is either "Main" (initial value) or a procedure name.
 *
 * The function keeps track of the number of Main declarations seen for error 
 * reporting. It recurses over declaration lists. The code after the
 * while loop is executed only when in global scope. That is, when the end
 * of the global declaration list is reached. This prevents error messages
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

	      proc_symbol->type = "Procedure";
	      proc_symbol->scope = scope;
	      proc_symbol->containing_rule = NULL;
              proc_symbol->context.is_var = 0;
              proc_symbol->context.int_exp = 0;
              proc_symbol->context.string_exp = 0; 
              proc_symbol->context.in_lhs = 0;

	      /* Get the name of the procedure */
	      proc_name = ast->value.declaration->value.procedure->name;
	      

              GSList *symbol_list = g_hash_table_lookup(table, proc_name);

	      /* Report an error if the name already exists in the table. */
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

	      rule_symbol->type = "Rule";
	      rule_symbol->scope = scope;
	      rule_symbol->containing_rule = NULL;
              rule_symbol->context.is_var = 0;
              rule_symbol->context.int_exp = 0;
              rule_symbol->context.string_exp = 0; 
              rule_symbol->context.in_lhs = 0;

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


/* This function performs semantic analysis on a GP program after parsing. It
 * also reverses lists in the AST: Bison constructs these lists in reverse
 * order due to its left-recursive rules. 
 *
 * Argument 1: A pointer to the abstract syntax tree of the input 
 *             program.
 * Argument 2: The symbol table. When called for the first time, it contains
 *             only rule and procedure identifiers added by declaration_scan.
 *             semantic_check will added other symbols to the symbol table and 
 *             use them for semantic analysis. This argument is passed to all
 *             subfunctions.
 * Argument 3: The current scope. semantic_check is initially called with 
 *             scope "Global". 
 *
 * The main body recurses over declaration lists, handling each of the three 
 * declaration types with the help of subprocedures.
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
              /* Set scope to procedure name for scanning local declarations */
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

              validate_call(statement->value.rule_name, table, scope, "rule"); 
                              
              break;

         case PROCEDURE_CALL:   

              validate_call(statement->value.proc_name, table, scope, "procedure");

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
                 validate_call(temp_list->value.rule_name, table, scope, "rule");
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


/* validate_call searches the symbol list with key <name> for a symbol with
 * the same type whose scope is either Global or <scope>.
 *
 * Argument 1: The name of the rule or procedure in question. Used to hash
 *             into the symbol table.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The type of call, either "rule" or "procedure". This argument
 *             is passed by statement_scan.
 */

void validate_call(char *name, GHashTable *table, char *scope, char *call_type) {

   GSList *symbol_list = g_hash_table_lookup(table, name);

      if(symbol_list == NULL) fprintf(stderr, "Error: %s %s has not been "
                                              "declared.\n", call_type, name);
      else {

	 /* Keep track of the symbol currently being looked at */
         Symbol *cur_sym = (Symbol*)(symbol_list->data);
                
         /* Iterate through the symbol list while the current symbol does not
	  * have type <call_type> or does not have an appropriate scope. 
	  *
	  * If the end of the list is reached, then no such symbol exists.
	  * We must print an error and exit the loop.
	  *
	  * Otherwise, an appropriate symbol does exist. Thus the loop will 
	  * break when this symbol is reached, before the end of the list.
	  * Nothing else needs to be done as the call is valid.
	  */

         while( strcmp(cur_sym->type,call_type) ||
	      ( strcmp(cur_sym->scope,scope) && strcmp(cur_sym->scope,"Global") ) 
	      ) 
	 {
            /* Check if the end of the list has been reached */
            if(symbol_list->next == NULL) {
               fprintf(stderr, "Error: %s %s called but not declared in a "
                               "visible scope.\n", call_type, name);     
               break;
            }
            /* Update current_symbol to point to the next symbol */
            else cur_sym = (Symbol*)(symbol_list->next->data);             
         }
      }
}


void rule_scan(GPRule *rule, GHashTable *table, char *scope)
{   
   char *rule_name = rule->name;
   List *current_var_list = rule->variables;

   /* enter variable declarations into symbol table */
   while(current_var_list != NULL) {

      switch(current_var_list->list_type) {
	   
         case INT_DECLARATIONS:

              enter_variables("integer", current_var_list->value.variables,
			      table, scope, rule_name);

	      break;

         case STRING_DECLARATIONS:

      	      enter_variables("string", current_var_list->value.variables,
			      table, scope, rule_name);

              break;
   	
         case ATOM_DECLARATIONS:

	      enter_variables("atom", current_var_list->value.variables,
			      table, scope, rule_name);

	      break; 

	 case LIST_DECLARATIONS:

	      enter_variables("list", current_var_list->value.variables,
			      table, scope, rule_name);

	      break;  	 

	 default:
	      fprintf(stderr,"Error: Unexpected list type %d\n",
		      (int)current_var_list->list_type);
	      break;
      }
      current_var_list = current_var_list->next;
   }

   // graph_scan(rule->lhs);
   /* When scanning the LHS graph, all variables must be tagged with an in_lhs flag
    * so that RHS variables can be checked for consistency. This flag must
    * exist in the struct Symbol of the variable.
    */
   // graph_scan(rule->rhs);
   
   interface_scan(rule->interface, table, scope, rule_name);


   // condition_scan(rule->condition);
}   

/* enter_variables adds variable declarations from a rule's parameter list
 * into the symbol table. It also checks that each variable name in the
 * parameter list is unique. Variables are added to the symbol table
 * regardless of uniqueness as later semantic checking may be useful to
 * the user. This function is called only by rule_scan.
 *
 * Argument 1: Variable type, passed from rule_scan. It is one of "integer",
 *             "string", "atom", "list".
 * Argument 2: Pointer to the list of variables declared with a specific type
 *             in the AST.
 * Argument 3: The symbol table.
 * Argument 4: The current scope.
 * Argument 5: The current rule being processed. This extra information
 *             is required for variable symbols.
 */

void enter_variables(char *type, List *variables, GHashTable *table, 
		     char *scope, char *rule)
{
   while(variables != NULL) {
 
      char *variable_name = variables->value.variable_name;	   
      GSList *symbol_list = g_hash_table_lookup(table, variable_name);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;

      while(iterator != NULL) {
         
         Symbol *cur_var = (Symbol*)(iterator->data);

	 /* Print an error if there already exists a variable in the same rule
	  * and scope with the same name. */
         if(cur_var->context.is_var == 1 && cur_var->scope = scope &&
	    cur_var->containing_rule = rule)	
	       fprintf(stderr,"Error: Variable %s declared twice in rule %s.\n",
	               variable_name, rule);

         iterator = iterator->next;
      
      }

      /* Create a symbol for the variable */
      Symbol *var_symbol = malloc(sizeof(Symbol));

      if(var_symbol==NULL) {
         fprintf(stderr,"Insufficient space.\n");
         exit(0);
      }

      var_symbol->type = type;
      var_symbol->scope = scope;
      var_symbol->containing_rule = rule;
      var_symbol->context.is_var = 1;

      /* list and atom variables are allowed in both string and integer 
       * expressions */
      if(!strcmp(type,"list") || !strcmp(type,"atom")) {
	 var_symbol->context.int_exp = 1;
	 var_symbol->context.string_exp = 1;
      }
      else if (!strcmp(type,"integer")) { 
	 var_symbol->context.int_exp = 1;
	 var_symbol->context.string_exp = 0;
      }
      else if (!strcmp(type,"string")) {
	 var_symbol->context.int_exp = 0;
	 var_symbol->context.string_exp = 1;
      }
      
      /* This flag is set in gp_list_scan if called with side "l" */
      var_symbol->context.in_lhs = 0;

      symbol_list = g_slist_prepend(symbol_list, var_symbol);           
      g_hash_table_insert(table, variable_name, symbol_list);   


      /* Move to the next symbol in the list */
      variables = variables->next;
   }
}  

/* graph_scan is responsible for adding nodes and edges to the symbol table.
 * It also performs some semantic analysis: source and target nodes of edges
 * must exist, the union of node IDs and edge IDs in the graph must not contain 
 * duplicates, node labels cannot have the "dashed" mark, and edge labels
 * cannot have the "grey" mark. This function is called only by rule_scan.
 *
 * Argument 1: A pointer to a struct GPGraph.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 * Argument 5: "l" or "r" depending on which graph in the rule is being
 *             processed.
 */

void graph_scan(GPGraph *graph, GHashTable *table, char *scope, 
                char *rule_name, char *side)
{
   /* strings to store the symbol type and the graph for error messages */
   char *node_type = NULL, edge_type = NULL, graph = NULL;

   if(!strcmp(side,"l")) {
      node_type = "left_node";
      edge_type = "left_edge";
      graph = "LHS";
   }
   else if(!strcmp(side,"r")) {
      node_type = "right_node";
      edge_type = "right_edge";
      graph = "RHS";
   }   

   List *node_list = graph->nodes;

   while(node_list != NULL) {

      char *node_id = node_list->value.node->name;
      GSList *symbol_list = g_hash_table_lookup(table, node_id);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;

      while(iterator != NULL) {
         
         Symbol *cur_node = (Symbol*)(iterator->data);

	 /* Print an error if there already exists a node in the same rule
	  * and scope with the same name. */

         if( !strcmp(cur_node->type,node_type) && 
	     cur_node->scope = scope           && 
	     cur_node->containing_rule = rule)	
	      fprintf(stderr,"Error: Node ID %s not unique in %s of rule %s.\n",
	              variable_name, graph, rule);  

         iterator = iterator->next;
      
      }

      Symbol *node_symbol = malloc(sizeof(Symbol));

      if(node_symbol==NULL) {
         fprintf(stderr,"Insufficient space.\n");
         exit(0);
      }

      node_symbol->type = node_type;      
      node_symbol->scope = scope;
      node_symbol->containing_rule = rule;
      node_symbol->context.is_var = 0;
      node_symbol->context.int_exp = 0;
      node_symbol->context.string_exp = 0;
      node_symbol->context.in_lhs = 0;             
 
      symbol_list = g_slist_prepend(symbol_list, node_symbol);           
      g_hash_table_insert(table, node_id, symbol_list);          

      if(node_list->value.node->label->mark == DASHED)
	 fprintf(stderr,"Error: Node %s in LHS of rule %s has illegal mark " 
	         "\"dashed\".\n", node_id, rule_name);

      gp_list_scan(node_list->value.node->label->list, table, scope, 
		   rule_name, side);

      node_list = node_list->next;
   }


   List *edge_list = graph->edges;

   while(edge_list != NULL) {

      char *edge_id = edge_list->value.edge->name;
      char *source_id = edge_list->value.edge->source;
      char *target_id = edge_list->value.edge->target;

      GSList *symbol_list = g_hash_table_lookup(table, edge_id);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;

      while(iterator != NULL) {
         
         Symbol *cur_edge = (Symbol*)(iterator->data);

	 /* Print an error if there already exists a node in the same rule
	  * and scope with the same name. */

         if( ( !strcmp(cur_edge->type,node_type) ||
	       !strcmp(cur_edge->type,edge_type) ) && 
               cur_node->scope = scope  && 
	       cur_node->containing_rule = rule)

	         fprintf(stderr,"Error: Edge ID %s not unique in %s of rule %s.\n",
	                 variable_name, graph, rule);

         iterator = iterator->next; 

      }

      Symbol *edge_symbol = malloc(sizeof(Symbol));

      if(edge_symbol==NULL) {
         fprintf(stderr,"Insufficient space.\n");
         exit(0);
      }

      edge_symbol->type = edge_type;      
      edge_symbol->scope = scope;
      edge_symbol->containing_rule = rule;
      edge_symbol->context.is_var = 0;
      edge_symbol->context.int_exp = 0;
      edge_symbol->context.string_exp = 0;
      edge_symbol->context.in_lhs = 0;             
 
      symbol_list = g_slist_prepend(symbol_list, edge_symbol);           
      g_hash_table_insert(table, edge_id, symbol_list);          

      if(edge_list->value.edge->label->mark == GREY)
	 fprintf(stderr,"Error: Node %s in LHS of rule %s has illegal mark " 
	         "\"grey\".\n", edge_id, rule_name);

      /* Verify source and target nodes exist in the graph. */

      /* First, source */
      symbol_list = g_hash_table_lookup(table, source_id);

      /* Keep track of the symbol currently being looked at. */
      Symbol *cur_sym = (Symbol*)(symbol_list->data);
                
      while(  strcmp(cur_sym->type,node_type) ||
            ( strcmp(cur_sym->scope,scope) &&
              strcmp(cur_sym->containing_rule,rule_name) ) )
      {   
        /* Check if the end of the list has been reached */
        if(symbol_list->next == NULL) {
           fprintf(stderr, "Error: Source node %s of edge %s does not exist "
                           "in %s graph.\n", source_id, edge_id, graph);     
           break;
        }
         /* Update current_symbol to point to the next symbol */
         else cur_sym = (Symbol*)(symbol_list->next->data);             
      }

      /* ...repeat for target */
      symbol_list = g_hash_table_lookup(table, target_id);     

      /* Keep track of the symbol currently being looked at. */
      Symbol *cur_sym = (Symbol*)(symbol_list->data);
                
      while(  strcmp(cur_sym->type,node_type) ||
            ( strcmp(cur_sym->scope,scope) &&
              strcmp(cur_sym->containing_rule,rule_name) ) )
      {
   
        /* Check if the end of the list has been reached */
        if(symbol_list->next == NULL) {
           fprintf(stderr, "Error: Target node %s of edge %s does not exist "
                           "in %s graph.\n", target_id, edge_id, graph);     
           break;
        }
         /* Update current_symbol to point to the next symbol */
         else cur_sym = (Symbol*)(symbol_list->next->data);             
      }

      gp_list_scan(node_list->value.edge->label->list, table, scope, 
		   rule_name, side);

      edge_list = edge_list->next;

   }
 
}

          

void condition_scan(GPCondExp *condition, GHashTable *table, char *scope,
                    char *rule_name)
{
   switch(condition->exp_type) {

      /* The three AST nodes for the type checking predicates point to
       * a single variable name, so they are handled in the sclame way.
       */
      case INT_CHECK:

      case STRING_CHECK:

      case ATOM_CHECK: 

      {
           bool in_rule = false, is_var = false;

           GSList *var_list = g_hash_table_lookup(table, condition->value.var);

	   /* Go through the list of symbols with the name in question
            * to check if any variables exist in this rule.
            */
           while(var_list != NULL) {cl
 
 	      Symbol *current_var = (Symbol*)var_list->data;

	      is_var = !strcmp(current_var->type,"Integer") ||
                       !strcmp(current_var->type,"String") ||
                       !strcmp(current_var->type,"Atom") ||
                       !strcmp(current_var->type,"List") 
            
              if(is_var && !strcmp(current_var->scope,scope) && 
	         !strcmp(current_var->containing_rule,rule_name)) 
              {
                 in_rule = true;
                 break;
              }
                 
              var_list = var_list->next;            
           }
           
           /* I print a warning as I am assuming if a variable in a condition
            * doesn't exist then the condition evaluates to false.
            * Works for type checks but maybe not for other coclnditions.
            */
           if(!in_rule) fprintf(stderr,"Warning: Variable %s in condition"
                                " of rule %s in procedure %s not declared.\n",
                                condition->value.var, rule_name, scope);
           break;
      }

      case EDGE_PRED:

      {
           bool in_lhs = false;

           GSList *node_list = g_hash_table_lookup(table,
                               condition->value.edge_pred.source);
           Symbol* current_node_id = (Symbol*)node_list->data;      

           while(node_list != NULL) {

         	 if(!strcmp(current_node->type,"LHS Node") &&
                    !strcmp(current_node_id->scope,scope) && 
	            !strcmp(current_node_id->containing_rule,rule_name))  
                 {
                    in_lhs = true;
                    break;
                 }

                 node_list = node_list->next;
           }

           if(!in_lhs) fprintf(stderr,"Error: Node %s in edge predicate not "
                        "in LHS of %s.\n", condition->value.edge_pred.source, 
                        rule_name);cl

           in_lhs = false;

           /* Same again, but for the target node. This pattern occurs
            * so much that I shall modularise it somehow. */

           node_list = g_hash_table_lookup(table, 
                       condition->value.edge_pred.target);
           Symbol* current_node_id = (Symbol*)node_list->data;      

           while(node_list != NULL) {

         	 if(!strcmp(current_node->type,"LHS Node") &&
                    !strcmp(current_node_id->scope,scope) && 
	            !strcmp(current_node_id->containing_rule,rule_name))  
                 {
                    in_lhs = true;
                    break;
                 }

                 node_list = node_list->next;
           }

           if(!in_lhs) fprintf(stderr,"Error: Node %s in edge predicate not "
                        "in LHS of.\n", condition->value.edge_pred.source,
                        rule_name);

           in_lhs = false;

           if(condition->value.edge_pred.label != NULL)
           gp_list_scan(condition->value.edge_pred.label->gp_list, table, 
                        scope, rule_name, "condition");

           break;

      }

      case EQUAL:

      case NOT_EQUAL:

           gp_list_scan(condition->value.list_cmp.left_list, table, scope,
                        rule_name, "condition");

           gp_list_scan(condition->value.list_cmp.right_list, table, scope,
			rule_name, "condition");

           break;

      case GREATER:

      case GREATER_EQUAL:

      case LESS:
 
      case LESS_EQUAL:

           atomic_exp_scan(condition->value.atom_cmp.left_exp, table, scope,
                           rule_name, "condition");

           atomic_exp_scan(condition->value.atom_cmp.right_exp, table, scope,
                           rule_name, "condition");

           break;

      case BOOL_NOT:

           condition_scan(condition->value.not_exp, table, scope, rule_name,
                          "condition");

           break;

      case BOOL_OR:
  
      case BOOL_AND:

           condition_scan(condition->value.bin_exp.left_exp, table, scope, 
			  rule_name, "condition");

           condition_scan(condition->value.bin_exp.right_exp, table, scope, 
			  rule_name, "condition");

           break;

      default:
	      fprintf(stderr,"Error: Unexpected condition type %d\n",
		      (int)condition->exp_type);
	      break;

      }
}


void interface_scan(List *interface, GHashTable *table, char *scope, 
                    char *rule_name)
{	
   bool in_lhs, in_rhs;

   while(interface != NULL) {
      
      /* Reset the flags */
      in_lhs = false, in_rhs = false;

      GPNodePair *current_pair = interface->value.node_id;

      GSList *node_list = g_hash_table_lookup(table,current_pair->left_node);
      Symbol* current_node_id = (Symbol*)node_list->data;      

      while(node_list != NULL) {
 	 if(!strcmp(current_node_id->scope,scope) && 
	    !strcmp(current_node_id->containing_rule,rule_name))  
         {
            if(!strcmp(current_node->type,"LHS Node")) in_lhs = true;
	    if(!strcmp(current_node->type,"RHS Node")) in_rhs = true;
         }

	 /* If both the LHS node and RHS node have been found, no need to look
          * further down the symbol list of this name.
          */         
         if(in_lhs && in_rhs) break;
         node_list = node_list->next;

      }

      if(!in_lhs) fprintf(stderr,"Error: Interface node %s not in the LHS of "
                          "rule %s.\n", current_node_id, rule_name);
      if(!in_rhs) fprintf(stderr,"Error: Interface node %s not in the RHS of "
                          "rule %s.\n", current_node, rule_name);

      interface = interface->next;   
   }

}



void gp_list_scan(List *gp_list, GHashTable *table, char *scope,
                  char *rule_name, char *side)
{
   while(gp_list != NULL) {
       atomic_exp_scan(gp_list->value.atom, table, scope, rule_name, context);
       gp_list = gp_list->next;
   }
}

/* validate_call is called when a RULE_CALL or PROCEDURE_CALL AST node
 * is reached. The function will check the rule/procedure name to see if it
 * exists in the symbol table with an appropriate scope. If not,
 * an error is reported. 
 */



variable_check(char *name, GHashTable *table, char *scope, char *rule_name,
               char *context)
{
   GSList *var_list = g_hash_table_lookup(table,atom_exp->value.name);
   Symbol *current_var = (Symbol*)var_list->data;

   bool in_rule = false;

   /* if the variable is in a RHS label, need to check that the 
    * variable has been declared and that it is also present in the LHS.
    */

   /* Contexts that require semantic checking beyond the standard 
    * 'check if the variable has been declared' are right_label,
    * int_exp, string_exp and atom_exp.
    */

   switch(context) {

      /* Nothing out of the ordinary for these two - just check the variable
       * has been declared in the rule! 
       */
      case "left_label":

      case "condition":

           while(var_list != NULL) {

              if(!strcmp(current_var->scope,scope) &&
                 !strcmp(current_var->containing_rule,rule_name)) 
                 {
                    in_rule = true;
                    break;
                 }
           
           var_list = var_list->next;
     
           }
 
           if(!in_rule) fprintf(stderr,"Error: Variable %s in rule %s, "
                         "procedure %s not declared.\n", 
                          condition->value.var, rule_name, scope);

      break;
   
      /* Additional check to see if the variable exists in a left label. */
  
      case "right_label":   
   
      { 
 
         bool in_lhs = false;
  
         while(var_list != NULL) {

         if(!strcmp(current_var->scope,scope) &&
            !strcmp(current_var->containing_rule,rule_name))

            in_rule = true;

            if(current_var->in_lhs == TRUE)
            {
               in_lhs = true;
                break;
            }
                    
            node_list = node_list->next;
         }

         if(!in_rule) fprintf(stderr,"Error: Variable %s in rule %s, "
                              "procedure %s not declared.\n",         
                              condition->value.var, rule_name, scope);
         else {
            if(!in_lhs) fprintf(stderr,"Error: Variable %s is in RHS of "
                           "rule %s, procedure %s, but not in the LHS.\n",         
                           condition->value.var, rule_name, scope);
         }

         break;
      }

      case "int_exp":

      { 
 
         bool is_int = false;
  
         while(var_list != NULL) {

         if(!strcmp(current_var->scope,scope) &&
            !strcmp(current_var->containing_rule,rule_name))

            in_rule = true;

            if(!strcmp(current_var->type,"integer"))
            {
            /* Assuming that a variable with this name in this rule and this 
             * scope exists only once.
             */
               is_int = true;
               break;
            }
                    
            node_list = node_list->next;
         }

         if(!in_rule) fprintf(stderr,"Error: Variable %s in rule %s, "
                              "procedure %s not declared.\n",         
                              condition->value.var, rule_name, scope);
         else {
            if(!is_int) fprintf(stderr,"Error: Variable %s expected to be "
                           "integer in rule %s, procedure %s.\n",         
                           condition->value.var, rule_name, scope);
         }

         break;
      }

      case "string_exp":

      { 
 
         bool is_string = false;
  
         while(var_list != NULL) {

         if(!strcmp(current_var->scope,scope) &&
            !strcmp(current_var->containing_rule,rule_name))

            in_rule = true;

            if(!strcmp(current_var->type,"string"))
            {
            /* Assuming that a variable with this name in this rule and this 
             * scope exists only once.
             */
               is_string = true;
               break;
            }
                    
            node_list = node_list->next;
         }

         if(!in_rule) fprintf(stderr,"Error: Variable %s in rule %s, "
                              "procedure %s not declared.\n",         
                              condition->value.var, rule_name, scope);
         else {
            if(!is_string) fprintf(stderr,"Error: Variable %s expected to be "
                           "string in rule %s, procedure %s.\n",         
                           condition->value.var, rule_name, scope);
         }

         break;
      }

      case "atom_exp":

      { 
 
         bool is_atom = false;
  
         while(var_list != NULL) {

         if(!strcmp(current_var->scope,scope) &&
            !strcmp(current_var->containing_rule,rule_name))

            in_rule = true;

            if(!strcmp(current_var->type,"integer") ||
               !strcmp(current_var->type,"string"))
            {
            /* Assuming that a variable with this name in this rule and this 
             * scope exists only once.
             */
               is_atom = true;
               break;
            }
                    
            node_list = node_list->next;
         }

         if(!in_rule) fprintf(stderr,"Error: Variable %s in rule %s, "
                              "procedure %s not declared.\n",         
                              condition->value.var, rule_name, scope);
         else {
            if(!is_atom) fprintf(stderr,"Error: Variable %s expected to be "
                           "atom in rule %s, procedure %s.\n",         
                           condition->value.var, rule_name, scope);
         }

         break;
      }

      default: fprintf(stderr,"Error: Unexpected context %s in call to "
                       "variable_check.\n", context);

               break;
}


void atomic_exp_scan(GPAtomicExp *atom_exp, GHashTable *table, char *scope,
                     char *rule_name, char *context)
{
   switch(atom_exp->exp_type) {

      case EMPTY_LIST:

      case INT_CONSTANT:
  
      case STRING_CONSTANT:

           break;

      case VARIABLE:

      {
           variable_check(atom_exp->value.name, table, scope, rule_name, context);
 
           break;
      }
       
      case INDEGREE:

      case OUTDEGREE:

      {
 
           bool in_lhs = false;

           GSList *node_list = g_hash_table_lookup(table,
                               atom_exp->value.node_id);
           Symbol *current_node_id = (Symbol*)node_list->data;      

           while(node_list != NULL) {

         	if(!strcmp(current_node->type,"LHS Node") &&
                   !strcmp(current_node_id->scope,scope) && 
	           !strcmp(current_node_id->containing_rule,rule_name))  
                {
                   in_lhs = true;
                   break;
                }

                node_list = node_list->next;
           }

           if(!in_lhs) fprintf(stderr,"Error: Node %s in degree operator not "
                        "in LHS of %s.\n", condition->value.edge_pred.source, 
                        rule_name);

           break;
         
       }

       case LIST_LENGTH: 

            gp_list_scan(condition->value.list_arg, table, scope, rule_name,
                         location);

            break;

       case STRING_LENGTH:

            variable_check(atom_exp->value.str_arg, table, scope, rule_name, "string");

            break;           
            
       /* Need to work out the context for these, as variables in these expressions
        * may have more than one context i.e. int_exp and right_label.
        * Write down all the possible context for variables.
        */l

       case NEG:

            if(!strcmp(context,"left_label")) 
              fprintf(stderr,"Error: Arithmetic expressions not allowed in "
                      "left-hand side labels, rule, procedure.\n");

            else atomic_exp_scan(atom_exp->value.exp, table, scope, rule_name,
                                 "integer");

            break;

       case ADD:

            if(!strcmp(context,"left_label")) 
              fprintf(stderr,"Error: Arithmetic expressions not allowed in "
                      "left-hand side labels, rule, procedure.\n");

            else {
              atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, context);
              atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, context);
            }

            break;

       case SUBTRACT:

            if(!strcmp(context,"left_label")) 
              fprintf(stderr,"Error: Arithmetic expressions not allowed in "
                      "left-hand side labels, rule, procedure.\n");

            else {
              atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, context);
              atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, context);
            }

            break;


       case MULTIPLY:

            if(!strcmp(context,"left_label")) 
              fprintf(stderr,"Error: Arithmetic expressions not allowed in "
                      "left-hand side labels, rule, procedure.\n");
l

            else {
              atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, context);cl
              atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, context);
            }

            break;

       case DIVIDE:

            if(!strcmp(context,"left_label")) 
              fprintf(stderr,"Error: Arithmetic expressions not allowed in "
                      "left-hand side labels, rule, procedure.\n");

            else {
              atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, context);
              atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, context);
            }

            break;

       case CONCAT:

            if(!strcmp(context,"left_label")) 
              fprintf(stderr,"Error: Arithmetic expressions not allowed in "
                      "left-hand side labels, rule, procedure.\n");

            else {
              atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, context);
              atomic_exp_scan(atom_exp->value.bin_op.leftcl_exp, table, scope,
                              rule_name, context);
            }

            break;

       default: fprintf(stderr,"Error: Unexpected atomic expression type %d\n",
		        (int)atom_exp->exp_type);
                
       }

}

	    


   

   
                                           
