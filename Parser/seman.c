/* ////////////////////////////////////////////////////////////////////////////

                                seman.c                              
 
       Defines the semantic analysis function and its subprocedures.
 

                     Created on 24/10/13 by Chris Bak 

//////////////////////////////////////////////////////////////////////////// */

#include "ast.h" /* reverse */
#include "seman.h" /* struct Symbol */
#include <stdlib.h> /* malloc, free */
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
 * to the symbol table. It returns 0 if no name clashes have been detected, 
 * otherwise it returns 1.
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
 * The function also checks for name clashes: multiple occurrences of a single 
 * procedure or multiple occurrences of the same rule name in the same scope.
 * These are both errors that should be reported, and further semantic analysis
 * may produce confusing error messages. Hence the function generates a value
 * that is passed to main to control if further semantic analysis should be 
 * conducted.
 */

int declaration_scan(const List *ast, GHashTable *table, char *scope)
{

   char *proc_name = NULL, *rule_name = NULL;	
   static int main_count = 0;
   /* The return value. Set to 1 if a clash is detected. */
   static int name_clash = 0;

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

	      /* Get the name of the procedure */
	      proc_name = ast->value.declaration->value.procedure->name;	      

              GSList *symbol_list = g_hash_table_lookup(table, proc_name);
	      
	      /* Make a copy of symbol_list for list traversal as symbol_list
	       * needs to point to the head of the list in case the new symbol
	       * is added.
	       */
	      GSList *iterator = symbol_list;

	      /* We only want to add the procedure name to the symbol table if
	       * there is no rule clash.
	       */
              bool add_procedure = true;	  

	      /* Report an error if a procedure with that name already exists
	       * in the table. 
	       */
              while(iterator != NULL) {
		 if(!strcmp(((Symbol*)iterator->data)->type,"Procedure"))
                 {
		    fprintf(stderr,"Error: Procedure %s declared more " 
                            "than once.\n", proc_name);
		    add_procedure = false;
		    name_clash = 1;
                    /* Report the error only once for this declaration. */
		    break;
		 }
		 iterator = iterator->next;
              }
	      
              if(add_procedure) {
                 /* Create a symbol for the procedure name */
                 Symbol *proc_symbol = malloc(sizeof(Symbol));

	         if(proc_symbol==NULL) {
                 fprintf(stderr,"Insufficient space.\n");
                 exit(0); 
                 }

                 proc_symbol->type = "Procedure";
                 proc_symbol->scope = scope;
	         proc_symbol->containing_rule = NULL;
		 proc_symbol->is_var = false;
		 proc_symbol->in_lhs = false;

                 symbol_list = g_slist_prepend(symbol_list, proc_symbol);      

                 g_hash_table_insert(table, proc_name, symbol_list);       
	      }

	      /* Reverse local declaration list */
              ast->value.declaration->value.procedure->local_decls = 
	      reverse(ast->value.declaration->value.procedure->local_decls);

              /* Scan for any local declarations with a new local scope. */
              declaration_scan(ast->value.declaration->value.procedure->
			       local_decls, table, proc_name);

              break;
         }
  
         case RULE_DECLARATION:
         {
	      /* Get the name of the rule */
  	      rule_name = ast->value.declaration->value.rule->name;	
 
              GSList *symbol_list = g_hash_table_lookup(table, rule_name);      

	      /* Make a copy of symbol_list for list traversal as symbol_list
	       * needs to point to the head of the list in case the new symbol
	       * is added.
	       */
	      GSList *iterator = symbol_list;

	      /* We only want to add the rule name to the symbol table if 
	       * there is no rule clash.
	       */
              bool add_rule = true;	      

              /* Report an error if two rules with the same name are declared
               * in the same scope.
               */
              while(iterator != NULL) {

                 char *symbol_scope = ((Symbol*)iterator->data)->scope;
   
                 if(!strcmp(((Symbol*)iterator->data)->type,"Rule") &&
		    !strcmp(scope,symbol_scope))
		 {
                    fprintf(stderr,"Error: Rule %s declared twice within " 
                            "the scope %s.\n", rule_name, scope);
		    add_rule = false;
                    name_clash = 1;
		    /* Report the error only once for this dec*laration. */
                    break; 
                 }                 
		 iterator = iterator->next;
              }             


	      if(add_rule) {
	         /* Create a symbol for the rule name */

	         Symbol *rule_symbol = malloc(sizeof(Symbol));
 
	         if(rule_symbol==NULL) {
                    fprintf(stderr,"Insufficient space.\n");
	            exit(0);
	         }

	         rule_symbol->type = "Rule";
	         rule_symbol->scope = scope;
	         rule_symbol->containing_rule = NULL;
                 rule_symbol->is_var = false;
                 rule_symbol->in_lhs = false;

                 symbol_list = g_slist_prepend(symbol_list, rule_symbol);           
                 g_hash_table_insert(table, rule_name, symbol_list);   
	      }

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

   /* This code is only executed if the end of the global declaration list
    * is reached. That is, this code shouldn't be executed from a recursive 
    * call.
    */
   if(!strcmp(scope,"Global")) {
     if(main_count == 0) fprintf(stderr,"Error: No Main procedure.\n");
     if(main_count > 1) 
      fprintf(stderr,"Error: More than one Main declaration.\n");
     return name_clash;
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
 
 /* Why does this return an int? JUSTIFY YOUR BEHAVIOUR. */
int semantic_check(List *declarations, GHashTable *table, char *scope)
{
   while(declarations!=NULL) {

      GPDeclaration *current_declaration = declarations->value.declaration;
   
      switch(current_declaration->decl_type) {

         case MAIN_DECLARATION:

              if(current_declaration->value.main_program != NULL)
		 statement_scan(current_declaration->value.main_program,
                                table, scope);
	      /* An empty main program is does not comform to the grammar,
	       * hence the Bison parser should catch it and report a syntax error,
	       * but there's no harm in checking here as well.
	       */
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
   declarations = declarations->next;  

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
	       */   
              statement->value.cmd_seq = reverse(statement->value.cmd_seq);

	      List *command_list = statement->value.cmd_seq;

              while(command_list != NULL) {
                 statement_scan(command_list->value.command, table, scope);
		 command_list = command_list->next;   
              }           
     
              break;
         }

         case RULE_CALL:

              validate_call(statement->value.rule_name, table, scope, "Rule"); 
                              
              break;

         case PROCEDURE_CALL:   

              validate_call(statement->value.proc_name, table, scope, "Procedure");

              break;

         case RULE_SET_CALL: 
         {
              /* Reverse the list of RULE_CALL nodes pointed to by a 
	       * RULE_SET_CALL node.
	       */ 
              statement->value.rule_set = reverse(statement->value.rule_set);

	      List *rule_list = statement->value.rule_set;

              while(rule_list != NULL) {
                 validate_call(rule_list->value.rule_name, table, scope, "Rule");
		 rule_list = rule_list->next;   
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
 * the same type whose scope is either Global or <scope>. Called by 
 * statement_scan when a RULE_CALL or PROCEDURE_CALL AST node is reached.
 *
 * Argument 1: The name of the rule or procedure in question. Used to hash
 *             into the symbol table.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The type of call, either "rule" or "procedure". This argument
 *             is passed by statement_scan.
 */

void validate_call(char *name, GHashTable *table, char *scope,
                   char *call_type) {

   GSList *symbol_list = g_hash_table_lookup(table, name);

      if(symbol_list == NULL) fprintf(stderr, "Error: %s %s has not been "
                                              "declared.\n", call_type, name);
      else {

	 /* Keep track of the symbol currently being looked at */
         Symbol *current_sym = (Symbol*)(symbol_list->data);
                
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

         while( strcmp(current_sym->type,call_type) ||
	        ( strcmp(current_sym->scope,scope) && 
                  strcmp(current_sym->scope,"Global") ) )	       
	 {
            /* Check if the end of the list has been reached */
            if(symbol_list->next == NULL) {
               fprintf(stderr, "Error: %s %s called but not declared in a "
                               "visible scope.\n", call_type, name);     
               break;
            }
            /* Update current_symbol to point to the next symbol */
            else current_sym = (Symbol*)(symbol_list->next->data);             
         }
      }
}


void rule_scan(GPRule *rule, GHashTable *table, char *scope)
{   
   char *rule_name = rule->name;

   /* Reverse the list of declaration types */
   rule->variables = reverse(rule->variables);

   List *variable_list = rule->variables;

   while(variable_list != NULL) {
 
      /* Reverse the list of variables */
      variable_list->value.variables = reverse(variable_list->value.variables);	   

      switch(variable_list->list_type) {
	   
         case INT_DECLARATIONS:

              enter_variables("integer", variable_list->value.variables,
			      table, scope, rule_name);

	      break;

         case STRING_DECLARATIONS:

      	      enter_variables("string", variable_list->value.variables,
			      table, scope, rule_name);

              break;
   	
         case ATOM_DECLARATIONS:

	      enter_variables("atom", variable_list->value.variables,
			      table, scope, rule_name);

	      break; 

	 case LIST_DECLARATIONS:

	      enter_variables("list", variable_list->value.variables,
			      table, scope, rule_name);

	      break;  	 

	 default:
	      fprintf(stderr,"Error: Unexpected list type %d\n",
		      (int)variable_list->list_type);
	      break;
      }
      variable_list = variable_list->next;
   }

   graph_scan(rule->lhs, table, scope, rule_name, 'l');

   graph_scan(rule->rhs, table, scope, rule_name, 'r');
   
   interface_scan(rule->interface, table, scope, rule_name);

   condition_scan(rule->condition, table, scope, rule_name);
}   

/* enter_variables adds variable declarations from a rule's parameter list
 * into the symbol table. It also checks that each variable name in the
 * parameter list is unique. Variable names are not added to the symbol
 * table if a clash is found. This function is called only by rule_scan.
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
		     char *scope, char *rule_name)
{
   while(variables != NULL) {
 
      char *variable_name = variables->value.variable_name;	   
      GSList *symbol_list = g_hash_table_lookup(table, variable_name);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;

      bool add_variable = true;

      while(iterator != NULL) {
         
         Symbol *current_var = (Symbol*)(iterator->data);

	 /* Print an error if there already exists a variable in the same rule
	  * and scope with the same name. */
         if(current_var->is_var && 
            !strcmp(current_var->scope,scope) &&
	    !strcmp(current_var->containing_rule,rule_name))
	 {	 
	    fprintf(stderr,"Error: Variable %s declared twice in rule %s.\n",
	            variable_name, rule_name);
	    add_variable = false;
            break;
	 }

         iterator = iterator->next;      
      }

      if(add_variable) {
         /* Create a symbol for the variable */
         Symbol *var_symbol = malloc(sizeof(Symbol));

         if(var_symbol==NULL) {
            fprintf(stderr,"Insufficient space.\n");
            exit(0);
         }

         var_symbol->type = type;
         var_symbol->scope = scope;
         var_symbol->containing_rule = rule_name;
         var_symbol->is_var = true;      
         /* This flag is set in gp_list_scan if called with side 'l' */
         var_symbol->in_lhs = false;

         symbol_list = g_slist_prepend(symbol_list, var_symbol);           
         g_hash_table_insert(table, variable_name, symbol_list); 
      }
   
      /* Move to the next variable in the declaration list. */
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
 * Argument 5: Either 'l' for the LHS graph or 'r' for the RHS graph.
 *
 * This function probably adds everything to the symbol table even if an
 * identifier is repeated. This is probably not what I want.
 */

void graph_scan(GPGraph *graph, GHashTable *table, char *scope, 
                char *rule_name, char side)
{
   /* Strings to store the symbol types and the graph. These are used
    * in string comparisons and in the error messages.
    */
   char *node_type = NULL, *edge_type = NULL, *graph_type = NULL;

   if(side == 'l') {
      node_type = "left_node";
      edge_type = "left_edge";
      graph_type = "LHS";
   }
   else if(side == 'r') {
      node_type = "right_node";
      edge_type = "right_edge";
      graph_type = "RHS";
   }   

   /* Reverse the node list */
   graph->nodes = reverse(graph->nodes);
   
   List *node_list = graph->nodes;

   while(node_list != NULL) {

      char *node_id = node_list->value.node->name;
      GSList *symbol_list = g_hash_table_lookup(table, node_id);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;

      bool add_node = true;

      while(iterator != NULL) {
         
         Symbol *current_node = (Symbol*)(iterator->data);

	 /* Print an error if there already exists a node in the same graph, 
	  * rule and scope with the same name.
	  */

         if(!strcmp(current_node->type,node_type) && 
	    !strcmp(current_node->scope,scope)    && 
	    !strcmp(current_node->containing_rule,rule_name))	
	 {
	     fprintf(stderr,"Error: Node ID %s not unique in %s of rule %s.\n",
	             node_id, graph_type, rule_name);  
	     add_node = false;
	     break;
	 }

         iterator = iterator->next;      
      }

      if(add_node) {

         Symbol *node_symbol = malloc(sizeof(Symbol));

         if(node_symbol==NULL) {
            fprintf(stderr,"Insufficient space.\n");
            exit(0);
         }

         node_symbol->type = node_type;      
         node_symbol->scope = scope;
         node_symbol->containing_rule = rule_name;
         node_symbol->is_var = false;
         node_symbol->in_lhs = false;             
 
         symbol_list = g_slist_prepend(symbol_list, node_symbol);           
         g_hash_table_insert(table, node_id, symbol_list);          

      }	 

      if(node_list->value.node->label->mark == DASHED)
          fprintf(stderr,"Error: Node %s in LHS of rule %s has illegal mark " 
	          "\"dashed\".\n", node_id, rule_name);

      gp_list_scan(&(node_list->value.node->label->gp_list), table, scope, 
		   rule_name, side);

      node_list = node_list->next;     
   }   

   /* Reverse the edge list */
   graph->edges = reverse(graph->edges);

   List *edge_list = graph->edges;

   bool add_edge = true;

   while(edge_list != NULL) {

      char *edge_id = edge_list->value.edge->name;
      char *source_id = edge_list->value.edge->source;
      char *target_id = edge_list->value.edge->target;

      GSList *symbol_list = g_hash_table_lookup(table, edge_id);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;

      while(iterator != NULL) {
         
         Symbol *current_edge = (Symbol*)(iterator->data);

	 /* Print an error if there already exists an edge in the same graph,
	  * rule and scope with the same name. 
	  */

         if( ( !strcmp(current_edge->type,node_type) ||
	       !strcmp(current_edge->type,edge_type) ) && 
	     !strcmp(current_edge->scope,scope)    && 
	     !strcmp(current_edge->containing_rule,rule_name))	
         {
	      fprintf(stderr,"Error: Edge ID %s not unique in %s of rule %s.\n",
	              edge_id, graph_type, rule_name);
	      add_edge = false;
	      break;
	 }

         iterator = iterator->next; 
      }

      if(add_edge) {

         Symbol *edge_symbol = malloc(sizeof(Symbol));

         if(edge_symbol==NULL) {
            fprintf(stderr,"Insufficient space.\n");
            exit(0);
         }

         edge_symbol->type = edge_type;      
         edge_symbol->scope = scope;
         edge_symbol->containing_rule = rule_name;
         edge_symbol->is_var = false;
         edge_symbol->in_lhs = false;             
 
         symbol_list = g_slist_prepend(symbol_list, edge_symbol);           
         g_hash_table_insert(table, edge_id, symbol_list);          

      }

       if(edge_list->value.edge->label->mark == GREY)
            fprintf(stderr,"Error: Edge %s in LHS of rule %s has illegal mark " 
	            "\"grey\".\n", edge_id, rule_name);

      /* Verify source and target nodes exist in the graph. */

      /* First, source */
      symbol_list = g_hash_table_lookup(table, source_id);

      if(symbol_list == NULL)
	 fprintf(stderr, "Error: Source node %s of edge %s does not exist "
                 "in %s graph.\n", source_id, edge_id, graph_type);     

      else {
         /* Keep track of the symbol currently being looked at. */
         Symbol *current_sym = (Symbol*)(symbol_list->data);
                
         while(strcmp(current_sym->type,node_type) ||
               strcmp(current_sym->scope,scope)    ||
               strcmp(current_sym->containing_rule,rule_name))
         {   
           /* Check if the end of the list has been reached */
           if(symbol_list->next == NULL) {
              fprintf(stderr, "Error: Source node %s of edge %s does not exist "
                              "in %s graph.\n", source_id, edge_id, graph_type);     
              break;
           }
           /* Update current_symbol to point to the next symbol */
           else current_sym = (Symbol*)(symbol_list->next->data);             
         } 
      }

      /* ...repeat for target */
      symbol_list = g_hash_table_lookup(table, target_id);    
 
      if(symbol_list == NULL)
	 fprintf(stderr, "Error: Target node %s of edge %s does not exist "
                 "in %s graph.\n", target_id, edge_id, graph_type);     

      else {
         /* Keep track of the symbol currently being looked at. */
         Symbol *current_sym = (Symbol*)(symbol_list->data);
                
         while(strcmp(current_sym->type,node_type) ||
               strcmp(current_sym->scope,scope)    ||
               strcmp(current_sym->containing_rule,rule_name))
         {
   
           /* Check if the end of the list has been reached */
           if(symbol_list->next == NULL) {
              fprintf(stderr, "Error: Target node %s of edge %s does not exist "
                              "in %s graph.\n", target_id, edge_id, graph_type);     
              break;
           }
            /* Update current_symbol to point to the next symbol */
            else current_sym = (Symbol*)(symbol_list->next->data);             
         }
      }	 

      gp_list_scan(&(edge_list->value.edge->label->gp_list), table, scope, 
		   rule_name, side);

      edge_list = edge_list->next;

   }
 
}

/* interface_scan performs semantic checking on the interface list of a rule.
 * All nodes in the list are checked to see if they appear in both graph of the
 * rule. This function is called only by rule_scan.
 *
 * Argument 1: Pointer to the head of an AST interface list.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 *
 * Check for node ID uniqueness? Yes.
 */

void interface_scan(List *interface, GHashTable *table, char *scope, 
		   char *rule_name)
{
  /* Linked list to store node IDs encountered to check for uniqueness */
  GSList *interface_ids = NULL, *iterator = NULL;
  bool in_lhs, in_rhs;

  while(interface != NULL) {
     
     /* Reset the flags */
     in_lhs = false, in_rhs = false;

     char *current_node_id = interface->value.node_id;

     /* g_slist_insert_sorted inserts elements and maintains lexicographic
      * order of its elements with use of the function strcmp.
      */
     interface_ids = g_slist_insert_sorted(interface_ids, current_node_id,
		                           (GCompareFunc)strcmp);

     GSList *node_list = g_hash_table_lookup(table,current_node_id);     

     while(node_list != NULL) {

        Symbol *current_node = (Symbol*)node_list->data;     

	if(!strcmp(current_node->scope,scope) && 
	   !strcmp(current_node->containing_rule,rule_name))  
	{
	   if(!strcmp(current_node->type,"left_node")) in_lhs = true;
	   if(!strcmp(current_node->type,"right_node")) in_rhs = true;
	}

	/* If both the LHS node and RHS node have been found, no need to look
	 * further down the symbol list.
	 */         
	if(in_lhs && in_rhs) break;
	node_list = node_list->next;
     }

     if(!in_lhs) fprintf(stderr,"Error: Interface node %s not in the LHS of "
			 "rule %s.\n", current_node_id, rule_name);
     if(!in_rhs) fprintf(stderr,"Error: Interface node %s not in the RHS of "
			 "rule %s.\n", current_node_id, rule_name);

     interface = interface->next;   
  }
  
  /* Since interface_ids is sorted, each element in the list only needs to be 
   * compared to its successor. 
   */
  for(iterator = interface_ids; iterator->next != NULL;
      iterator = iterator->next) {
        if(!strcmp(iterator->data,iterator->next->data))
           fprintf(stderr,"Error: Node %s occurs twice in interface list.\n",
		   (char*)(iterator->data));
  }

}


/* condition_scan navigates a subtree of the AST with a GPCondExp node
 * as its root. It performs semantic checking on all possible types
 * of GP2 conditions, usually calling auxiliary functions. This function
 * is called only by rule_scan.
 *
 * Argument 1: Pointer to a struct GPCondExp.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 */           

void condition_scan(GPCondExp *condition, GHashTable *table, char *scope,
                    char *rule_name)
{
   switch(condition->exp_type) {

      /* The three AST nodes for the type checking predicates point to
       * a single variable name, so they are handled in the same way.
       */
      case INT_CHECK:

      case STRING_CHECK:

      case ATOM_CHECK: 

      {
           bool in_rule = false; 

           GSList *var_list = g_hash_table_lookup(table,condition->value.var);

	   /* Go through the list of symbols with the name in question
            * to check if any variables exist in this rule.
            */
           while(var_list != NULL) {
 
 	      Symbol *current_var = (Symbol*)var_list->data;

              if( current_var->is_var && 
                  !strcmp(current_var->scope,scope) && 
	          !strcmp(current_var->containing_rule,rule_name) ) 
              {
                  in_rule = true;
                  break;
              }
                 
              var_list = var_list->next;            
           }
           
           /* I print a warning as I am assuming if a variable in a condition
            * doesn't exist then the condition evaluates to false.
            * Works for type checks but maybe not for other conditions.
            */
           if(!in_rule) fprintf(stderr,"Warning: Variable %s in condition"
                                " of rule %s in procedure %s not declared.\n",
                                condition->value.var, rule_name, scope);
           break;
      }

      /* For an edge predicate, the source and target node IDs must be present
       * in the LHS of the rule, and the optional label argument is also 
       * scanned.
       */

      case EDGE_PRED:

      {
           bool in_lhs = false;

           GSList *node_list = g_hash_table_lookup(table,
                               condition->value.edge_pred.source);

           while(node_list != NULL) {

                 Symbol* current_node = (Symbol*)node_list->data;      

         	 if(!strcmp(current_node->type,"left_node") &&
                    !strcmp(current_node->scope,scope) && 
	            !strcmp(current_node->containing_rule,rule_name))  
                 {
                    in_lhs = true;
                    break;
                 }

                 node_list = node_list->next;
           }

           if(!in_lhs) fprintf(stderr,"Error: Node %s in edge predicate not "
                        "in LHS of %s.\n", condition->value.edge_pred.source, 
                        rule_name);

           in_lhs = false;

           /* Same again, but for the target node. This pattern occurs
            * so much that I shall modularise it somehow. */

           node_list = g_hash_table_lookup(table, 
                       condition->value.edge_pred.target);

           while(node_list != NULL) {
                 
		 Symbol* current_node = (Symbol*)node_list->data;      

         	 if(!strcmp(current_node->type,"LHS Node") &&
                    !strcmp(current_node->scope,scope) && 
	            !strcmp(current_node->containing_rule,rule_name))  
                 {
                    in_lhs = true;
                    break;
                 }

                 node_list = node_list->next;
           }

           if(!in_lhs) fprintf(stderr,"Error: Node %s in edge predicate not "
                        "in LHS of %s.\n", condition->value.edge_pred.target,
                        rule_name);

           in_lhs = false;

	   /* Scan the label argument, but only if it exists. */
           if(condition->value.edge_pred.label != NULL)
              gp_list_scan(&(condition->value.edge_pred.label->gp_list), 
			   table, scope, rule_name, 'c');

           break;

      }

      case EQUAL:

      case NOT_EQUAL:

           gp_list_scan(&(condition->value.list_cmp.left_list), table, scope,
                        rule_name, 'c');

           gp_list_scan(&(condition->value.list_cmp.right_list), table, scope,
			rule_name, 'c');

           break;

      case GREATER:

      case GREATER_EQUAL:

      case LESS:
 
      case LESS_EQUAL:

           atomic_exp_scan(condition->value.atom_cmp.left_exp, table, scope,
                           rule_name, 'c', true, false);

           atomic_exp_scan(condition->value.atom_cmp.right_exp, table, scope,
                           rule_name, 'c', true, false);

           break;

      case BOOL_NOT:

           condition_scan(condition->value.not_exp, table, scope, rule_name);

           break;

      case BOOL_OR:
  
      case BOOL_AND:

           condition_scan(condition->value.bin_exp.left_exp, table, scope, 
			  rule_name);

           condition_scan(condition->value.bin_exp.right_exp, table, scope, 
			  rule_name);

           break;

      default:
	      fprintf(stderr,"Error: Unexpected condition type %d\n",
		      (int)condition->exp_type);
	      break;

      }
}

/* gp_list_scan takes as input the head of a GP2 list expression in the AST.
 * It first reverses the list to place it in the correct order. Then the 
 * function traverses the list, calling atomic_exp_scan for each item
 * in the list.
 *
 * This function also removes unnecessary EMPTY_LIST nodes. EMPTY_LIST is
 * one of the types of struct GPAtomicExp. It is used only as a marker to
 * signify an empty list: a list with no other GPAtomicExp nodes.
 * Explicitly, EMPTY_LIST should only appear as follows, where List is
 * the head of the list. 
 *			 List --[next]-->NULL
 *  		  	  |
 *  			[value]-->EMPTY_LIST
 * In other cases, an EMPTY_LIST node has no meaning, due to the following
 * GP2 equivalence: empty:a = a = a:empty, where a is any atomic expression.
 *
 * Hence, if an EMPTY_LIST node occurs in the list but not in the above
 * scenario, they are removed by first redirecting pointers and then
 * freeing the memory.
 * 
 * Argument 1: Pointer to the head of a list in the AST, passed by reference.
 *             If called from graph_scan or case EDGE_PRED in condition_scan,
 *             this argument is the address of the gp_list pointer in a
 *             struct GPLabel.
 *             If called from cases EQUAL or NOT_EQUAL in condition_scan,
 *             this argument is the address of a pointer in the struct list_cmp
 *             of a struct GPConditionExp.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 * Argument 5: The location of the list in the program.
 *             Either [l]eft-hand graph, [r]ight-hand graph or [c]ondition.
 *             If called from graph_scan, this is the 'side' parameter
 *             passed to the graph_scan call. If called from condition_scan,
 *             this is 'c'.
 */

void gp_list_scan(List **gp_list, GHashTable *table, char *scope,
                  char *rule_name, char location)
{
   *gp_list = reverse(*gp_list);

   /* Discard any EMPTY_LIST nodes at the start of the list provided
    * the next node is non-empty.
    */ 
   while((*gp_list)->value.atom->exp_type == EMPTY_LIST) {
      if( (*gp_list)->next != NULL ) {
          /* Keep track of the AST List node to free */ 
          List *temp = *gp_list;
	  /* Redirect gp_list to point to the next (non-empty) node */
          *gp_list = (*gp_list)->next;
	  /* Free the EMPTY_LIST node */
	  free(temp->value.atom); 
	  /* Free the struct List that pointed to the EMPTY_LIST
	   * node. */ 
	  free(temp);
      }
      /* The EMPTY_LIST node is the only element in the list. 
       * Nothing to be done. */
      else break;
   }

   /* Make a copy of *gp_list in order to not modify the original pointer when
    * traversing the list.
    */
   List *iterator = *gp_list;

   while(iterator != NULL) {
       atomic_exp_scan(iterator->value.atom, table, scope, rule_name, 
		       location, false, false);

       /* Removing the EMPTY_LIST nodes affects the global AST. Hence
	* the address of iterator->next, the pointer which may be redirected,
	* is required.
	*/
       List **next_node = &(iterator->next);
       /* Discard any intermediate EMPTY_LIST nodes */
       /* The while loop terminates when the end of the list has been reached
	* or when the next node is not an EMPTY_LIST node.
	*/
       while( *next_node != NULL &&
	      (*next_node)->value.atom->exp_type == EMPTY_LIST)
       {
	       List *temp = *next_node;
	       /* Redirect the point to the node after the EMPTY_LIST
	        * node. */
	       *next_node = (*next_node)->next;
	       free(temp->value.atom);
	       free(temp);
       }
       iterator = iterator->next;
   }
}

/* atomic_exp_scan checks variables and nodes in expressions to see if they 
 * have been declared in the rule. If the function is called with location
 * 'l', it also checks for expressions that violate the simple list condition.
 * An expression e is simple if:
 * (1) e contains no arithmetic operators.
 * (2) e contains at most one occurrence of a list variable.
 * (3) any string expression in e must contain at most one string variable.
 *
 * Argument 1: Pointer to a struct GPAtomicExp
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 * Argument 5: The location of the atomic expression in the program.
 *             Either [l]eft-hand graph, [r]ight-hand graph or [c]ondition.
 *             Passed from the caller.
 * Argument 6: If true, then the expression pointed to by atom_exp is an
 *             integer expression. Erros are reported if string expressions
 *             are encountered.
 * Argument 7: If true, then the expression pointed to by atom_exp is a
 *             string expression. Errors are reported if integer expressions 
 *             are encountered.
 */

void atomic_exp_scan(GPAtomicExp *atom_exp, GHashTable *table, char *scope,
                     char *rule_name, char location, bool int_exp,
		     bool string_exp)
{
   switch(atom_exp->exp_type) {

      case EMPTY_LIST: 

           break;

      case INT_CONSTANT:

           if(string_exp) fprintf(stderr,"Error: Integer constant appears in "
                                  "a string expression.\n");

           break;
  
      case STRING_CONSTANT:

           if(int_exp) fprintf(stderr,"Error: String constant appears in "
                               "an integer expression.\n");

           break;

      case VARIABLE:

      {
           /* var_list always points to the start of the symbol list with key
            * equal to the name of the variable in question.
            */
	   GSList *var_list = g_hash_table_lookup(table,atom_exp->value.name);           
           
           bool in_rule = false, in_lhs = false, is_int = false, 
                is_string = false;

           while(var_list != NULL) {

              Symbol *current_var = (Symbol*)(var_list->data);
         
              if(current_var->is_var &&
                 !strcmp(current_var->scope,scope) &&
	         !strcmp(current_var->containing_rule,rule_name)) 
              {
                 in_rule = true;

                 if(location == 'l') current_var->in_lhs = true;   
 
                 if(location == 'r') 
                   if(current_var->in_lhs) in_lhs = true;

                 if(int_exp) {
                   /* atoms, lists and integers can match an int variable. */
                   if(strcmp(current_var->type,"string")) is_int = true;
                 }
                 if(string_exp) {
                   /* atoms, lists and string can match a string variable. */
                   if(strcmp(current_var->type,"integer")) is_string = true;         
	         }	   

 	         /* Found the variable in the rule with the appropriate name.
                  * enter_variables ensures there is only one such variable in 
                  * the symbol list. No need to look further.
                  */          
                  break;
              }
	      var_list = var_list->next;
	   }

           if(!in_rule) fprintf(stderr, "Error: Variable %s in expression but "
                         "not declared.\n", atom_exp->value.name);
	   /* No need to report other errors if the variable is not present. */
           else {
              if(location == 'r' && !in_lhs)
                 fprintf(stderr,"Error: Variable %s in RHS but not in LHS.\n",
                         atom_exp->value.name);

              if(int_exp && !is_int)
                 fprintf(stderr,"Error: Variable %s occurs in an integer "
                         "expression but declared as a string.\n",
                         atom_exp->value.name);

              if(string_exp && !is_string)
                 fprintf(stderr,"Error: Variable %s occurs in a string "
                         "expression but declared as an integer.\n",
                         atom_exp->value.name);
	   }

           break;
      }
       
      case INDEGREE:

      case OUTDEGREE:

      {
           if(string_exp) fprintf(stderr,"Error: Degree operator appears "
                                  "in string expression.\n");

           bool in_lhs = false;

           GSList *node_list = g_hash_table_lookup(table,
                               atom_exp->value.node_id);
 
           while(node_list != NULL) {

              Symbol *current_node = (Symbol*)node_list->data;     

              if(!strcmp(current_node->type,"left_node") &&
                 !strcmp(current_node->scope,scope) && 
	         !strcmp(current_node->containing_rule,rule_name))  
              {
                 in_lhs = true;
                 break;
              }

              node_list = node_list->next;
           }

           if(!in_lhs) fprintf(stderr,"Error: Node %s in degree operator "
                               "not in LHS of %s.\n", atom_exp->value.node_id,
                               rule_name);
           break;
         
       }

       case LIST_LENGTH: 

            if(string_exp) fprintf(stderr,"Error: Length operator appears "
                                   "in string expression.\n");

            gp_list_scan(&(atom_exp->value.list_arg), table, scope, rule_name,
                         location);

            break;

       case STRING_LENGTH:

       {
            if(string_exp) fprintf(stderr,"Error: Length operator appears "
                                   "in string expression.\n");

	    atomic_exp_scan(atom_exp->value.str_arg, table, scope, rule_name, 
			    location, false, true);
		    
            break;
       }         
            

       case NEG:

            if(string_exp) fprintf(stderr,"Error: Arithmetic operator appears "
                                   "in string expression.\n");

            if(location == 'l')
              fprintf(stderr,"Error: Arithmetic expressions forbidden in "
                      "left-hand side labels, rule, procedure.\n");

            atomic_exp_scan(atom_exp->value.exp, table, scope, rule_name,
                            location, true, false);
            break;


       case ADD:

            if(string_exp) fprintf(stderr,"Error: Arithmetic operator appears "
                                   "in string expression.\n");

            if(location == 'l') 
              fprintf(stderr,"Error: Arithmetic expressions forbidden in "
                      "left-hand side labels, rule, procedure.\n");

            atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                            rule_name, location, true, false);
            atomic_exp_scan(atom_exp->value.bin_op.right_exp, table, scope,
                            rule_name, location, true, false);
            break;


       case SUBTRACT:

            if(string_exp) fprintf(stderr,"Error: Arithmetic operator appears "
                                   "in string expression.\n");

            if(location == 'l') 
              fprintf(stderr,"Error: Arithmetic expressions forbidden in "
                      "left-hand side labels, rule, procedure.\n");

            atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, location, true, false);
            atomic_exp_scan(atom_exp->value.bin_op.right_exp, table, scope,
                              rule_name, location, true, false);
            break;


       case MULTIPLY:

            if(string_exp) fprintf(stderr,"Error: Arithmetic operator appears "
                                   "in string expression.\n");

            if(location == 'l') 
              fprintf(stderr,"Error: Arithmetic expressions forbidden in "
                      "left-hand side labels, rule, procedure.\n");

            atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, location, true, false);
            atomic_exp_scan(atom_exp->value.bin_op.right_exp, table, scope,
                              rule_name, location, true, false);    
            break;


       case DIVIDE:

            if(string_exp) fprintf(stderr,"Error: Arithmetic operator appears "
                                   "in string expression.\n");

            if(location == 'l') 
              fprintf(stderr,"Error: Arithmetic expressions forbidden in "
                      "left-hand side labels, rule, procedure.\n");

            atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                              rule_name, location, true, false);
            atomic_exp_scan(atom_exp->value.bin_op.right_exp, table, scope,
                              rule_name, location, true, false);       
            break;


       case CONCAT:

            if(int_exp) fprintf(stderr,"Error: String operator appears in "
                                "integer expression.\n");

            atomic_exp_scan(atom_exp->value.bin_op.left_exp, table, scope,
                            rule_name, location, false, true);
            atomic_exp_scan(atom_exp->value.bin_op.right_exp, table, scope,
                            rule_name, location, false, true); 
            break;

       default: fprintf(stderr,"Error: Unexpected atomic expression type %d\n",
		        (int)atom_exp->exp_type);
                
       }

} 

   
                                           
