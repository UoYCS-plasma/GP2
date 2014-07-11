/* ////////////////////////////////////////////////////////////////////////////

  ================================
  seman.c - Chris Bak (24/10/2013) 
  ================================

//////////////////////////////////////////////////////////////////////////// */

#include "seman.h" 

List *reverse (List * listHead) 
{
     List *currentNode = listHead;
     List *tempNode = NULL;
     List *previousNode = NULL;

     /* invariant: currentNode points to the node being worked on and
      * previousNode points to the original parent of currentNode.
      */

     while(currentNode) {
        /* Maintain a pointer to currentNode->next before reassignment. */
        tempNode = currentNode->next; 

        /* reversing the 'next' pointer of currentNode. */
	currentNode->next = previousNode; 

	/* setting the invariant for the next iteration */
	previousNode = currentNode;
	currentNode = tempNode;
     }

     /* Return the tail of the original list i.e. the head of the reversed 
      * list. */
     return previousNode;
}     



void reverseGraphAST (GPGraph *graph)
{
   if(graph->nodes) {
      graph->nodes = reverse(graph->nodes);  

      List *iterator = graph->nodes;

      while(iterator) {
           iterator->value.node->label->gp_list = 
             reverse(iterator->value.node->label->gp_list);
           iterator = iterator->next;
      }
   }
  
   if(graph->edges) {
      graph->edges = reverse(graph->edges);

      List *iterator = graph->edges;

      while(iterator) {
           iterator->value.edge->label->gp_list = 
             reverse(iterator->value.edge->label->gp_list);
           iterator = iterator->next;
      }
   }
}


/* freeSymbolList iterates through the symbol list, freeing each symbol
 * individually. struct Symbol contains some string fields which
 * are dynamically allocated. These are explicitly freed.
 */
void freeSymbolList(gpointer key, gpointer value, gpointer data) 
{
   /* iterator keeps a pointer to the current GSList node */
   GSList *iterator = (GSList*)value;

   while(iterator) {
  
      Symbol *symbol_to_free = (Symbol*)iterator->data;

      string symbol_scope = symbol_to_free->scope;
      string symbol_rule = symbol_to_free->containing_rule;

      if(symbol_scope) free(symbol_scope); 
      if(symbol_rule) free(symbol_rule); 
      if(symbol_to_free) free(symbol_to_free);

      iterator = iterator->next;
   }
   /* A glib function is used to free the GSList. */
   g_slist_free((GSList*)value);
}


/* declarationScan iterates over ast, which is a declaration list.
 * It is first called with the root of the program AST, the head
 * of the global declaration list. declarationScan is recursively
 * called on local declaration lists of procedures. Care is taken
 * to ensure the function returns only once, when the end of the
 * global declaration list is reached.
 */

bool declarationScan(List * ast, GHashTable *table,
                     string const scope)
{

   string proc_name = NULL, rule_name = NULL;
	
   /* These two variables are static as their values should not be reset
    * on recursive calls to declarationScan. 
    */
   static int main_count = 0;
   /* The return value. Set to 1 if a clash is detected. */
   static bool name_clash = false;

   GSList *symbol_list = NULL;

   while(ast) {     

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
	      proc_name = strdup(ast->value.declaration->value.procedure->name);	      

              symbol_list = g_hash_table_lookup(table, proc_name);
	      
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
              while(iterator) {
		 if( ((Symbol*)iterator->data)->type == PROCEDURE_S)
                 {
		    print_to_console("Error: Procedure %s declared more " 
                                     "than once.\n", 
                                     proc_name);
		    print_to_log("Error: Procedure %s declared more " 
                                 "than once.\n", 
                                 proc_name);
		    add_procedure = false;
		    name_clash = true;
                    /* Report the error only once for this declaration. */
		    break;
		 }
		 iterator = iterator->next;
              }
	      
              if(add_procedure) {

                 /* Create a symbol for the procedure name */

                 Symbol *proc_symbol = malloc(sizeof(Symbol));

	         if(proc_symbol == NULL) {
                    print_to_log("Memory exhausted during symbol management.\n");
                    exit(0); 
                 }

                 proc_symbol->type = PROCEDURE_S;
                 proc_symbol->scope = strdup(scope);
	         proc_symbol->containing_rule = NULL;
		 proc_symbol->is_var = false;
		 proc_symbol->in_lhs = false;
                 proc_symbol->wildcard = false; 
                 proc_symbol->bidirectional = false; 

                 symbol_list = g_slist_prepend(symbol_list, proc_symbol);      

                 g_hash_table_replace(table, proc_name, symbol_list); 

	      }

	      if(ast->value.declaration->value.procedure->local_decls) {

	         /* Reverse local declaration list */
                 ast->value.declaration->value.procedure->local_decls = 
	            reverse(ast->value.declaration->value.procedure->local_decls);

                 /* Scan for any local declarations. proc_name is passed
                  * as the scope argument. */
                 declarationScan(ast->value.declaration->value.procedure->
			          local_decls, table, proc_name);
              }

              if(!add_procedure && proc_name) free(proc_name);
 
              break;
         }
  
         case RULE_DECLARATION:
         {
	      /* Get the name of the rule */
  	      rule_name = strdup(ast->value.declaration->value.rule->name);	
 
              symbol_list = g_hash_table_lookup(table, rule_name);      

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
               * in the same scope. iterator points to the start of the symbol
               * list whose key is rule_name.
               */
              while(iterator) {

                 string symbol_scope = ((Symbol*)iterator->data)->scope;
   
                 if( ((Symbol*)iterator->data)->type == RULE_S &&
		     !strcmp(scope,symbol_scope))
		 {
                    if(!strcmp(scope,"Global")) {
                       print_to_console("Error: Rule %s declared twice in " 
                                        "global scope %s.\n", 
                                        rule_name, scope);
                       print_to_log("Error: Rule %s declared twice in " 
                                    "global scope %s.\n", 
                                    rule_name, scope);
                    }

                    else { 
                       print_to_console("Error: Rule %s declared twice in " 
                                        "procedure %s.\n", 
                                        rule_name, scope);
                       print_to_log("Error: Rule %s declared twice in " 
                                    "procedure %s.\n", 
                                    rule_name, scope);
                    }

		    add_rule = false;
                    name_clash = true;
		    /* Report the error only once for this declaration. */
                    break; 
                 }                 
		 iterator = iterator->next;
              }             


	      if(add_rule) {

	         /* Create a symbol for the rule name */

	         Symbol *rule_symbol = malloc(sizeof(Symbol));
 
	         if(rule_symbol == NULL) {
                    print_to_log("Memory exhausted during symbol management.\n");
	            exit(0);
	         }

	         rule_symbol->type = RULE_S;
	         rule_symbol->scope = strdup(scope);
	         rule_symbol->containing_rule = NULL;
                 rule_symbol->is_var = false;
                 rule_symbol->in_lhs = false;
                 rule_symbol->wildcard = false;
                 rule_symbol->bidirectional = false;

                 symbol_list = g_slist_prepend(symbol_list, rule_symbol);       
    
                 g_hash_table_replace(table, rule_name, symbol_list);  
 
	      }
              else if(rule_name) free(rule_name); 

              break;
         }

         default: 
             print_to_log("Error: Unexpected node type %d at AST node %d\n\n", 
                          ast->value.declaration->decl_type, 
		          ast->value.declaration->node_id);

             break; 

      }     

   ast = ast->next;  	

   }

   /* The code following the if statement should only be executed upon
    * reaching the end of the global declaration list. 
    * As declarationScan is a recursive function, I must check if the current 
    * invocation of declarationScan is in global scope. If so, then I know
    * the end of the global declaration list has been reached and I can return
    * knowing the whole AST has been scanned.
    */
   if(!strcmp(scope,"Global")) {

     if(main_count == 0) {
        print_to_console("Error: No Main procedure.\n");
        print_to_log("Error: No Main procedure.\n");
     }

     if(main_count > 1) {
        print_to_console("Error: More than one Main declaration.\n");
        print_to_log("Error: More than one Main declaration.\n");
     }

     return name_clash;
   }  
}


/* bidirectional_edges is a linked list of the bidirectional edges encountered
 * in the program. This is to ensure that there are no parallel bidirectional
 * edges. The list stores items of type struct BidirectionalEdge. It is used
 * in graph_scan and freed in semanticCheck.
 */

/* The main body recurses over declaration lists, handling each of the three 
 * declaration types with the help of subprocedures.
 */

BiEdgeList *bidirectional_edges = NULL; 

bool semanticCheck(List * declarations, GHashTable *table,
                   string const scope)
{
   while(declarations) {

      GPDeclaration *current_declaration = declarations->value.declaration;
   
      switch(current_declaration->decl_type) {

         case MAIN_DECLARATION:

	      /* An empty main program does not comform to the grammar. The
	       * parser should catch it and report a syntax error, but there is
	       * no harm in checking here as well.
	       */
              if(current_declaration->value.main_program)
		 statementScan(current_declaration->value.main_program,
                               table, scope);
	      else print_to_log("Error: Main procedure has no program, "
                                "not caught by parser. \n");

              break;

         case PROCEDURE_DECLARATION: 
	 {
              /* Set scope to procedure name for scanning local declarations */
              string new_scope = current_declaration->value.procedure->name;

	      /* An empty main program does not comform to the grammar. The
	       * parser should catch it and report a syntax error, but there is
	       * no harm in checking here as well.
	       */
              if(current_declaration->value.procedure->cmd_seq)
                  statementScan(current_declaration->value.procedure->cmd_seq,
                                 table, new_scope);
	      else print_to_log("Error: Procedure %s has no program, "
                                "not caught by parser. \n",
                                current_declaration->value.procedure->name);

	      if(current_declaration->value.procedure->local_decls)
                  semanticCheck(current_declaration->value.procedure->
				 local_decls, table, new_scope);

              break;
	 }    

         case RULE_DECLARATION:              

              ruleScan(current_declaration->value.rule, table, scope);

              break;  

         default: print_to_log("Error: Unexpected declaration type %d at AST "
                               "node %d\n", 
                               current_declaration->decl_type,
                               current_declaration->node_id);
              break;
      } 

   declarations = declarations->next;  

   }

   /* The code following the if statement should only be executed upon
    * reaching the end of the global declaration list. 
    * As semanticCheck is a recursive function, I must check if the current 
    * invocation of semanticCheck is in global scope. If so, then I know
    * the end of the global declaration list has been reached and I can
    * return knowing the whole AST has been scanned.
    */
   if(!strcmp(scope,"Global")) {
      freeBiEdgeList(bidirectional_edges);
      return abort_compilation;
   }
}   

void freeBiEdgeList(BiEdgeList *edge_list) 
{
    BiEdge bi_edge = edge_list->value;

    if(bi_edge.scope) free(bi_edge.scope);
    if(bi_edge.containing_rule) free(bi_edge.containing_rule);
    if(bi_edge.source) free(bi_edge.source);
    if(bi_edge.target) free(bi_edge.target);
      
    if(edge_list->next) freeBiEdgeList(edge_list->next);             

    if(edge_list) free(edge_list);
}


void statementScan(GPStatement * const statement, GHashTable *table,
                   string const scope) 
{
      switch(statement->statement_type) {

         case COMMAND_SEQUENCE: 
         {
	      /* A COMMAND_SEQUENCE node points to a list of COMMAND nodes.
	       * This list is reversed. 
	       */   
              statement->value.cmd_seq = reverse(statement->value.cmd_seq);

	      List *command_list = statement->value.cmd_seq;

              while(command_list) {
                 statementScan(command_list->value.command, table, scope);
		 command_list = command_list->next;   
              }           
     
              break;
         }

         case RULE_CALL:

              validateCall(statement->value.rule_name, table, scope, RULE_S); 
                              
              break;

         case PROCEDURE_CALL:   

              validateCall(statement->value.proc_name, table, scope, PROCEDURE_S);

              break;

         case RULE_SET_CALL: 
         {
              /* Reverse the list of RULE_CALL nodes.
	       */ 
              statement->value.rule_set = reverse(statement->value.rule_set);

	      List *rule_list = statement->value.rule_set;

              while(rule_list) {
                 validateCall(rule_list->value.rule_name, table, scope, RULE_S);                               
		 rule_list = rule_list->next;   
              }           
     
              break;
        }
             

         case IF_STATEMENT:

              statementScan(statement->value.cond_branch.condition, 
                                    table, scope);
   
              statementScan(statement->value.cond_branch.then_stmt,
                                    table, scope);

              statementScan(statement->value.cond_branch.else_stmt,
                                    table, scope);

              break;

         case TRY_STATEMENT:

              statementScan(statement->value.cond_branch.condition, 
                                    table, scope);
   
              statementScan(statement->value.cond_branch.then_stmt, 
                                    table, scope);

              statementScan(statement->value.cond_branch.else_stmt, 
                                    table, scope);

              break;

         case ALAP_STATEMENT:

              statementScan(statement->value.loop_stmt,table, scope);

	      break;

         case PROGRAM_OR:

              statementScan(statement->value.or_stmt.left_stmt, table, scope);

              statementScan(statement->value.or_stmt.right_stmt, table,
			     scope);

	      break;

         case SKIP_STATEMENT: /* do nothing */ break;
   
         case FAIL_STATEMENT: /* do nothing */ break;

         default: print_to_log("Error: Unexpected statement type %d at AST "
                               "node %d\n", 
                               statement->statement_type, statement->node_id);
                  abort_compilation = true;
                  break;   

        }
}             




void validateCall(string const name, GHashTable *table, string const scope,
                  SymbolType const type) {

   GSList *symbol_list = g_hash_table_lookup(table, name);

      string call_type = NULL;

      /* validateCall is called with type PROCEDURE_S or RULE_S. 
       */
      if(type == PROCEDURE_S) call_type = "Procedure";
      if(type == RULE_S) call_type = "Rule";

      if(symbol_list == NULL) {
	 print_to_console("Error: %s %s called but not declared.\n",
                          call_type, name);
	 print_to_log("Error: %s %s called but not declared.\n",
                      call_type, name);
         abort_compilation = true;
      }
      else {
	 /* Point to the first symbol in symbol_list. */
         Symbol *current_sym = (Symbol*)(symbol_list->data);
                
         /* Iterate through the symbol list while the current symbol does not
	  * have type <type> or does not have appropriate scope. 
	  *
	  * If the end of the list is reached, then no symbol exists with the
          * appropriate scope and call type. Print an error and exit the loop.
	  *
	  * Otherwise, an appropriate symbol does exist. The loop breaks when
          * this symbol is reached, before the end of the list. Nothing more
	  * needs to be done as the call is valid.
	  */

         while( current_sym->type != type 
	        || ( strcmp(current_sym->scope,scope) && 
                     strcmp(current_sym->scope,"Global") ) )	       
	 {
            /* Check if the end of the list has been reached */
            if(symbol_list->next == NULL) {
               print_to_console("Error: %s %s called but not declared in a "
                                "visible scope.\n", 
                                call_type, name);     
               print_to_log("Error: %s %s called but not declared in a "
                            "visible scope.\n", 
                            call_type, name);  
               abort_compilation = true;

               break;
            }
            /* Update current_symbol to point to the next symbol. */
            else current_sym = (Symbol*)(symbol_list->next->data);             
         }
      }
}


void ruleScan(GPRule * const rule, GHashTable *table, string const scope)
{   
   string rule_name = rule->name;

   /* Reverse the list of declaration types and the interface list. */
   if(rule->variables) rule->variables = reverse(rule->variables);
   if(rule->interface) rule->interface = reverse(rule->interface);

   List *variable_list = rule->variables;

   /* Variables to count how many times each type is encountered. These are
    * incremented, according to the list_type encountered, and a warning is
    * printed if any variable becomes greater than 1. Hence only the 
    * variables in the first declaration list for a type are added to the
    * symbol table; the rest are ignored.
    */ 
   int integer_count = 0, character_count = 0, string_count = 0, atom_count = 0, 
       list_count = 0;

   while(variable_list) {
 
      /* Reverse the list of variables */
      variable_list->value.variables = reverse(variable_list->value.variables);	   

      switch(variable_list->list_type) {
	   
         case INT_DECLARATIONS:

              if(++integer_count > 1) {
                 print_to_log("Warning (%s.%s): More than one integer list "
                              "in variable declaration section.",
                              scope, rule_name);
              }
              else enterVariables(INT_S, variable_list->value.variables,
			           table, scope, rule_name);

	      break;

         case CHAR_DECLARATIONS:

              if(++character_count > 1) {
                 print_to_log("Warning (%s.%s): More than one character list "
                              "in variable declaration section.",
                              scope, rule_name);
              }
              else enterVariables(CHAR_S, variable_list->value.variables,
			           table, scope, rule_name);

	      break;


         case STRING_DECLARATIONS:

              if(++string_count > 1) {
                 print_to_log("Warning (%s.%s): More than one 'string' list "
                              "in variable declaration section.", 
                              scope, rule_name);
              }
              else enterVariables(STRING_S, variable_list->value.variables,
			           table, scope, rule_name);

              break;
   	
         case ATOM_DECLARATIONS:

              if(++atom_count > 1) {
                 print_to_log("Warning (%s.%s): More than one 'atom' list "
                              "in variable declaration section.",
                              scope, rule_name);
              }
              else enterVariables(ATOM_S, variable_list->value.variables,
			           table, scope, rule_name);

	      break; 

	 case LIST_DECLARATIONS:

              if(++list_count > 1) {
                 print_to_log("Warning (%s.%s): More than one 'list' list "
                              "in variable declaration section.", 
                              scope, rule_name);
              }
              else enterVariables(LIST_S, variable_list->value.variables,
			           table, scope, rule_name);

	      break;  	 

	 default:
	      print_to_log("Error: Unexpected list type %d in AST node %d\n",
		           variable_list->list_type, variable_list->node_id);
              abort_compilation = true;
	      break;
      }
      variable_list = variable_list->next;
   }

   graphScan(rule->lhs, table, scope, rule_name, 'l');

   graphScan(rule->rhs, table, scope, rule_name, 'r');
   
   if(rule->interface) interfaceScan(rule->interface, table, scope, rule_name);

   if(rule->condition) conditionScan(rule->condition, table, scope, rule_name);
}   



void enterVariables(SymbolType const type, List * variables, 
                    GHashTable *table, string const scope, 
                    string const rule_name)
{
   while(variables) {
 
      string variable_name = strdup(variables->value.variable_name);	   
      GSList *symbol_list = g_hash_table_lookup(table, variable_name);
      /* symbol_list is preserved as a new symbol is prepended to it */
      GSList *iterator = symbol_list;

      bool add_variable = true;

      while(iterator) {
         
         Symbol *current_var = (Symbol*)(iterator->data);

	 /* Print an error if there already exists a variable in the same rule
	  * and scope with the same name. */
         if(current_var->is_var && 
            !strcmp(current_var->scope,scope) &&
	    !strcmp(current_var->containing_rule,rule_name))
	 {	 
	    print_to_console("Warning (%s.%s): Variable %s declared twice.\n", 
                             scope, rule_name, variable_name);
	    print_to_log("Warning (%s.%s): Variable %s declared twice.\n", 
                         scope, rule_name, variable_name);
	    add_variable = false;
            break;
	 }

         iterator = iterator->next;      
      }

      if(add_variable) {
         /* Create a symbol for the variable */
         Symbol *var_symbol = malloc(sizeof(Symbol));

         if(var_symbol == NULL) {
            print_to_log("Memory exhausted during symbol management.\n");
            exit(0);
         }

         var_symbol->type = type;
         var_symbol->scope = strdup(scope);
         var_symbol->containing_rule = strdup(rule_name);
         var_symbol->is_var = true;      
         var_symbol->in_lhs = false;
         var_symbol->wildcard = false;
         var_symbol->bidirectional = false;

         symbol_list = g_slist_prepend(symbol_list, var_symbol);  
         
         g_hash_table_replace(table, variable_name, symbol_list);
     }
     /* The malloc'd string variable_name is not used as a key to the symbol
      * table in the else case, hence it needs to be freed before the loop
      * breaks.
      */
     else free(variable_name);
  
     /* Move to the next variable in the declaration list. */
     variables = variables->next;
   }
}  



void graphScan(GPGraph *const graph, GHashTable *table, string const scope, 
                string const rule_name, char const side)
{
   /* Variables to store the symbol types and the graph for semantic checking
    * and error reporting.
    */
   SymbolType node_type, edge_type;
   string graph_type = NULL;

   GSList *symbol_list = NULL;
 
   if(side == 'l') {
      node_type = LEFT_NODE_S;
      edge_type = LEFT_EDGE_S;
      graph_type = "LHS";
   }
   else if(side == 'r') {
      node_type = RIGHT_NODE_S;
      edge_type = RIGHT_EDGE_S;
      graph_type = "RHS";
   }   

   /* Reverse the node list */
   graph->nodes = reverse(graph->nodes);
   
   List *node_list = graph->nodes;

   while(node_list) {

      /* node_id is used as a key, so it is duplicated as node_id will be freed
       * by g_hash_table_insert. I do not want to also free the node->name in
       * the AST. */
      string node_id = strdup(node_list->value.node->name);
      symbol_list = g_hash_table_lookup(table, node_id);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;

      bool add_node = true;

      while(iterator) {
         
         Symbol *current_node = (Symbol*)(iterator->data);

	 /* Print an error if there already exists a node in the same graph, 
	  * rule and scope with the same name.
	  */

         if( current_node->type == node_type && 
	     !strcmp(current_node->scope,scope) && 
	     !strcmp(current_node->containing_rule,rule_name) )	
	 {
	     print_to_log("Warning (%s.%s): Node ID %s not unique in the "
                          "%s.\n", 
                          scope, rule_name, node_id, graph_type);  
	     add_node = false;
	     break;
	 }

         iterator = iterator->next;      
      }


      if(node_list->value.node->label->mark == DASHED) {
          print_to_log("Error (%s.%s): Node %s in %s graph has invalid mark " 
	               "\"dashed\".\n", 
                       scope, rule_name, node_id, graph_type);
          abort_compilation = true; 
      }


      if(add_node) {

         Symbol *node_symbol = malloc(sizeof(Symbol));

         if(node_symbol == NULL) {
            print_to_log("Memory exhausted during symbol management.\n");
            exit(0);
         }

         node_symbol->type = node_type;      
         node_symbol->scope = strdup(scope);
         node_symbol->containing_rule = strdup(rule_name);
         node_symbol->is_var = false;
         node_symbol->in_lhs = false;      
         node_symbol->wildcard = (node_list->value.node->label->mark == CYAN);    
         node_symbol->bidirectional = false;      
 
         symbol_list = g_slist_prepend(symbol_list, node_symbol);         
  
         g_hash_table_replace(table, node_id, symbol_list);         
      }      

      /* If the node is in the RHS and has a cyan mark, the corresponding LHS node
       * must also have a cyan mark. */

      if(side == 'r' && node_list->value.node->label->mark == CYAN) {
 
         /* The current node has just been prepended to the symbol list.
          * Hence the first entry in the symbol list is an RHS node. 
          * The corresponding LHS node is to be located. Therefore I start
          * the saerch at the second element in the symbol list.
          */
         iterator = symbol_list->next;

         while(iterator) {
         
            Symbol *current_node = (Symbol*)(iterator->data);

	    /* Find a node in the same rule. If that node is in the LHS
             * and is not a wildcard (cyan mark) then report an error.
             * Note there is only one such LHS node because for each scope,
             * rule, and graph (LHS/RHS), only one node with a particular
             * ID is entered into the symbol table. 
             */

            if(!strcmp(current_node->scope,scope) && 
	       !strcmp(current_node->containing_rule,rule_name))	
  	    {
                if(current_node->type == LEFT_NODE_S &&
                   !current_node->wildcard) 
                {
	           print_to_log("Error (%s.%s): RHS wildcard node %s has no "
                                "matching LHS wildcard.", 
                                scope, rule_name, node_id);  
                   abort_compilation = true; 
                }
                /* Regardless of the outcome of the inner if statement, exit
                 * the loop as the single appropriate node has been located.
                 */
                break;   
	    }
            iterator = iterator->next;      
         }
      }

      gpListScan(&(node_list->value.node->label->gp_list), table, scope, 
		   rule_name, side);

      node_list = node_list->next;   
 
      if(!add_node && node_id) free(node_id);  

   }   

   /* Reverse the edge list */
   graph->edges = reverse(graph->edges);

   List *edge_list = graph->edges;

   bool add_edge = true;

   while(edge_list) {

      /* edge_id is used as a key, so it is duplicated as edge_id will be freed
       * by g_hash_table_insert. I do not also want to free the edge->name in
       * the AST. */
      string edge_id = strdup(edge_list->value.edge->name);
      string source_id = edge_list->value.edge->source;
      string target_id = edge_list->value.edge->target;

      symbol_list = g_hash_table_lookup(table, edge_id);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;

      while(iterator) {
         
         Symbol *current_edge = (Symbol*)(iterator->data);

	 /* Print an error if there already exists an edge in the same graph,
	  * rule and scope with the same name. 
	  */

         if( current_edge->type == edge_type && 
	     !strcmp(current_edge->scope,scope) && 
	     !strcmp(current_edge->containing_rule,rule_name) )	
         {
	      print_to_log("Warning (%s.%s): Edge ID %s not unique in the %s "
                           "graph.\n", 
                           scope, rule_name, edge_id, graph_type);
	      add_edge = false;
	      break;
	 }

         iterator = iterator->next; 
      }

      if(add_edge) {

         Symbol *edge_symbol = malloc(sizeof(Symbol));

         if(edge_symbol == NULL) {
            print_to_log("Memory exhausted during symbol management.\n");
            exit(0);
         }

         edge_symbol->type = edge_type;      
         edge_symbol->scope = strdup(scope);
         edge_symbol->containing_rule = strdup(rule_name);
         edge_symbol->is_var = false;
         edge_symbol->in_lhs = false;             
         edge_symbol->wildcard = (edge_list->value.edge->label->mark == CYAN); 
         edge_symbol->bidirectional = (edge_list->value.edge->bidirectional); 

         symbol_list = g_slist_prepend(symbol_list, edge_symbol);   
        
         g_hash_table_replace(table, edge_id, symbol_list);
  
      }

      if(side == 'r' && edge_list->value.edge->label->mark == CYAN) {
 
         /* The current edge has just been prepended to the symbol list.
          * Hence the first entry in the symbol list is this RHS edge. 
          * The corresponding LHS edge is to be located, hence search
          * might as well start at the second element in the symbol list.
          */
         iterator = symbol_list->next;

         while(iterator) {
         
            Symbol *current_edge = (Symbol*)(iterator->data);

	    /* Find an edge in the same rule. If that edge is in the LHS
             * and is not a wildcard (cyan mark) then report an error.
             * Note there is only one such LHS edge because for each scope,
             * rule, and graph (LHS/RHS), only one edge with a particular
             * ID is entered into the symbol table. 
             */

            if(!strcmp(current_edge->scope,scope) && 
	       !strcmp(current_edge->containing_rule,rule_name))	
  	    {
                if(current_edge->type == LEFT_EDGE_S &&
                   !current_edge->wildcard) 
                {
	           print_to_log("Error (%s.%s): RHS wildcard edge %s has no "
                                "matching LHS wildcard.\n", 
                                scope, rule_name, edge_id);  
                   abort_compilation = true;
                }
                /* Regardless of the outcome of the if statement, exit
                 * the loop as the single appropriate edge has been located.
                 */ 
                break;   
	    }
            iterator = iterator->next;      
         }
      }



      /* Two semantic checks are made for bidirectional edges (BEs):
       * (1) A BE in the RHS must have a corresponding BE in the LHS.
       * (2) At most one BE is allowed between a pair of nodes. 
       *
       * Items of type struct BidirectionalEdge, defined in the header,
       * are placed in GSList *bidirectional_edges to keep track of all
       * the bidirectional edges encountered.
       */

      if(edge_list->value.edge->bidirectional) {
    
         /* bidirectional_edges may be written to later, hence a copy is made
          * of the root pointer.
          */
         BiEdgeList *iterator = bidirectional_edges;

         bool add_bi_edge = true;
         bool matching_bi_edge = false;
 
         while(iterator) {

             BiEdge list_edge = iterator->value;

             /* Find edges in the list with the same scope, rule, source node and
              * target node as the current edge.
              */
             if(!strcmp(list_edge.scope,scope) &&
                !strcmp(list_edge.containing_rule,rule_name) &&
                !strcmp(list_edge.source,source_id) &&
                !strcmp(list_edge.target,target_id) ) 
             {
                /* Semantic check (1) */
                if(side == 'r' && list_edge.graph == 'l') 
                   matching_bi_edge = true;

                /* Semantic check (2) */
                else {
                   if(list_edge.graph == side) {
                      print_to_log("Error (%s.%s): Parallel bidirectional edges "
                                   "in %s.\n", scope, rule_name, graph_type);
                      abort_compilation = true;
                      add_bi_edge = false;
                   }
                }
             }
             iterator = iterator->next;       
         }

         if(side == 'r' && !matching_bi_edge) {
	    print_to_log("Error (%s.%s): RHS bidirectional edge %s has no "
                         "matching LHS bidirectional edge.\n", 
                         scope, rule_name, edge_id); 
            abort_compilation = true; 
         }

         /* Only add an edge to the BE list if it is not a parallel edge.
          */
         if(add_bi_edge) {

            BiEdge bi_edge =
                  {.scope = strdup(scope),
                   .containing_rule = strdup(rule_name),
                   .graph = side,
                   .source = strdup(source_id),
                   .target = strdup(target_id) };

            BiEdgeList *new_edge = malloc(sizeof(BiEdgeList));

            if(new_edge == NULL) {
               print_to_log("Memory exhausted during creation of BiEdgeList.\n");
               exit(0);
            }
            new_edge->value = bi_edge;
            new_edge->next = NULL;

            /* Append new_edge to bidirectional_edges. There are two cases:
             * bidirectional_edges is empty, in which case new_edge is directly
             * assigned to it; or it is not empty, in which case an iterator
             * walks through bidirectional_edges until the last node N is
             * reached, at which point new_edge is assigned to N->next.
             */
           
            if(bidirectional_edges == NULL) bidirectional_edges = new_edge;

            else { /* Walk to the last node of bidirectional_edges */ 
               BiEdgeList *iterator = bidirectional_edges;
               while(iterator->next) iterator = iterator->next; 
               iterator->next = new_edge;
            }
         }
      }

      /* Verify source and target nodes exist in the graph. */

      /* First, source */ 
      symbol_list = g_hash_table_lookup(table, source_id);

      if(symbol_list == NULL) {
	 print_to_log("Error (%s.%s): Source node %s of edge %s does not "
                      "exist in %s graph.\n",
                      scope, rule_name, source_id, edge_id, graph_type);     
         abort_compilation = true; 
      }

      else {
         /* Keep track of the symbol currently being looked at. */
         Symbol *current_sym = (Symbol*)(symbol_list->data);
                
         /* The while loop condition is true if the current symbol is not
          * the node in the same rule and graph as the current edge.
          * Hence the loop breaks if the appropriate node is found.
          * At each pass a check is made to see if the end of the list
          * has been reached. If not, check the next symbol in the symbol
          * list. Otherwise, print an error and break.
          */
         while(current_sym->type != node_type   ||
               strcmp(current_sym->scope,scope) ||
               strcmp(current_sym->containing_rule,rule_name))
         {   
           if(symbol_list->next == NULL) {
              print_to_log("Error (%s.%s): Source node %s of edge %s does "
                           "not exist in %s graph.\n", 
                           scope, rule_name, source_id, edge_id, graph_type);     
              abort_compilation = true; 
              break;
           }
           else current_sym = (Symbol*)(symbol_list->next->data);             
         } 
      }

      /* ...repeat for target */
      symbol_list = g_hash_table_lookup(table, target_id);    
 
      if(symbol_list == NULL) {
	 print_to_log("Error (%s.%s): Target node %s of edge %s does not "
                      "exist in %s graph.\n", 
                      scope, rule_name, target_id, edge_id, graph_type);     
         abort_compilation = true; 
      } 

      else {
         /* Keep track of the symbol currently being looked at. */
         Symbol *current_sym = (Symbol*)(symbol_list->data);

         /* The while loop condition is true if the current symbol is not
          * the node in the same rule and graph as the current edge.
          * Hence the loop breaks if the appropriate node is found.
          * At each pass a check is made to see if the end of the list
          * has been reached. If not, check the next symbol in the symbol
          * list. Otherwise, print an error and break.
          */               
         while(current_sym->type != node_type   ||
               strcmp(current_sym->scope,scope) ||
               strcmp(current_sym->containing_rule,rule_name))
         {
   
           if(symbol_list->next == NULL) {
              print_to_log("Error (%s.%s): Target node %s of edge %s does "
                           "not exist in %s graph.\n", 
                           scope, rule_name, target_id, edge_id, graph_type);   
              abort_compilation = true; 
              break;
           }
            else current_sym = (Symbol*)(symbol_list->next->data);             
         }
      }	 

      gpListScan(&(edge_list->value.edge->label->gp_list), table, scope, 
		   rule_name, side);

      edge_list = edge_list->next;

      if(!add_edge && edge_id) free(edge_id);

   }
 
}



void interfaceScan(List * interface, GHashTable *table, 
                   string const scope, string const rule_name)
{
  /* Linked list to store node IDs encountered to check for uniqueness */
  GSList *interface_ids = NULL, *iterator = NULL;
  bool in_lhs, in_rhs;

  while(interface) {
     
     /* Reset the flags on iteration. */
     in_lhs = false, in_rhs = false;

     string current_node_id = interface->value.node_id;

     /* g_slist_insert_sorted inserts elements and maintains lexicographic
      * order of its elements with use of the function strcmp. This makes
      * checking for duplicate nodes easier.
      */
     interface_ids = g_slist_insert_sorted(interface_ids, current_node_id,
		                           (GCompareFunc)strcmp);

     GSList *node_list = g_hash_table_lookup(table,current_node_id);     

     while(node_list) {

        Symbol *current_node = (Symbol*)node_list->data;     

	if(!strcmp(current_node->scope,scope) && 
	   !strcmp(current_node->containing_rule,rule_name))  
	{
	   if(current_node->type == LEFT_NODE_S) in_lhs = true;
	   if(current_node->type == RIGHT_NODE_S) in_rhs = true;
	}

	/* If both the LHS node and RHS node have been found, no need to look
	 * further down the symbol list.  
	 */         
	if(in_lhs && in_rhs) break;
	node_list = node_list->next;
     }

     if(!in_lhs) {
        print_to_log("Error (%s.%s): Interface node %s not in the LHS "
	   	     "graph.\n",
                     scope, rule_name, current_node_id);
        abort_compilation = true; 
     }

     if(!in_rhs) {
        print_to_log("Error (%s.%s): Interface node %s not in the RHS "
		     "graph.\n", 
                     scope, rule_name, current_node_id);
        abort_compilation = true; 
     }

     interface = interface->next;   
  }
  
  /* Since interface_ids is sorted, each element in the list only needs to be 
   * compared to its successor. 
   */
  for(iterator = interface_ids; iterator->next; iterator = iterator->next) {
        if(!strcmp(iterator->data,iterator->next->data))
           print_to_log("Warning (%s.%s): Node %s occurs twice in interface "
                        "list.\n",
		        scope, rule_name, (char*)(iterator->data));
  }

}


        

void conditionScan(GPCondExp * const condition, GHashTable *table, 
                   string const scope, string const rule_name)
{
   switch(condition->exp_type) {

      /* The four AST nodes for the type checking predicates point to
       * a single variable name, so they are handled in the same way.
       */
      case INT_CHECK:

      case CHAR_CHECK:

      case STRING_CHECK:

      case ATOM_CHECK: 

      {
           bool in_rule = false; 

           GSList *var_list = g_hash_table_lookup(table,condition->value.var);

	   /* Go through the list of symbols with the name in question
            * to check if any variables exist in this rule.
            */
           while(var_list) {
 
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
           if(!in_rule) {
              print_to_console("Warning (%s.%s): Variable %s in condition not "
                               "declared.\n", 
                               scope, rule_name, condition->value.var);
              print_to_log("Warning (%s.%s): Variable %s in condition not "
                           "declared.\n", 
                           scope, rule_name, condition->value.var);
           }

           break;
      }

      /* For an edge predicate, the source and target node IDs must be present
       * in the LHS of the rule. The optional label argument is also scanned.
       */

      case EDGE_PRED:

      {
           bool in_lhs = false;

           GSList *node_list = g_hash_table_lookup(table,
                               condition->value.edge_pred.source);

           while(node_list) {

                 Symbol* current_node = (Symbol*)node_list->data;      

         	 if(current_node->type == LEFT_NODE_S &&
                    !strcmp(current_node->scope,scope) && 
	            !strcmp(current_node->containing_rule,rule_name))  
                 {
                    in_lhs = true;
                    break;
                 }

                 node_list = node_list->next;
           }

           if(!in_lhs) {
              print_to_console("Error (%s.%s): Node %s in edge predicate not "
                               "in LHS.\n", 
                               scope, rule_name,
                               condition->value.edge_pred.source);
              print_to_log("Error (%s.%s): Node %s in edge predicate not in "
                           "LHS.\n", 
                           scope, rule_name,
                           condition->value.edge_pred.source);
              abort_compilation = true;  
           }

           in_lhs = false;



           node_list = g_hash_table_lookup(table, 
                       condition->value.edge_pred.target);

           while(node_list) {
                 
		 Symbol* current_node = (Symbol*)node_list->data;      

         	 if(current_node->type == LEFT_NODE_S &&
                    !strcmp(current_node->scope,scope) && 
	            !strcmp(current_node->containing_rule,rule_name))  
                 {
                    in_lhs = true;
                    break;
                 }

                 node_list = node_list->next;
           }

           if(!in_lhs) {
              print_to_console("Error (%s.%s): Node %s in edge predicate not "
                               "in LHS.\n", 
                               scope, rule_name,
                               condition->value.edge_pred.target);
              print_to_log("Error (%s.%s): Node %s in edge predicate not in "
                           "LHS.\n",
                           scope, rule_name, condition->value.edge_pred.target);
              abort_compilation = true;  
           }

           in_lhs = false;

	   /* Scan the label argument if it exists. */
           if(condition->value.edge_pred.label)
              gpListScan(&(condition->value.edge_pred.label->gp_list), 
			   table, scope, rule_name, 'c');

           break;

      }

      case EQUAL:

      case NOT_EQUAL:

           gpListScan(&(condition->value.list_cmp.left_list), table, scope,
                        rule_name, 'c');

           gpListScan(&(condition->value.list_cmp.right_list), table, scope,
			rule_name, 'c');

           break;

      case GREATER:

      case GREATER_EQUAL:

      case LESS:
 
      case LESS_EQUAL:

           atomicExpScan(condition->value.atom_cmp.left_exp, table, scope,
                           rule_name, 'c', true, false);

           atomicExpScan(condition->value.atom_cmp.right_exp, table, scope,
                           rule_name, 'c', true, false);

           break;

      case BOOL_NOT:

           conditionScan(condition->value.not_exp, table, scope, rule_name);

           break;

      case BOOL_OR:
  
      case BOOL_AND:

           conditionScan(condition->value.bin_exp.left_exp, table, scope, 
			  rule_name);

           conditionScan(condition->value.bin_exp.right_exp, table, scope, 
			  rule_name);

           break;

      default:
	      print_to_log("Error: Unexpected condition type %d at AST node %d\n",
		           condition->exp_type, condition->node_id);
              abort_compilation = true; 
 
	      break;
      }
}

/* Variables used by gpListScan and AtomicExpScan. */
static int list_var_count = 0;
static int string_var_count = 0;
static bool lhs_non_simple_exp = false;

void gpListScan(List **gp_list, GHashTable *table, string const scope,
                  string const rule_name, char location)
{
   if((*gp_list)->list_type == EMPTY_LIST) return;

   *gp_list = reverse(*gp_list);

   /* This code is no longer relevant as EMPTY_LIST can only occur as a 
    * single struct List. It will probably be useful after rule 
    * applications when lists with 'empty's could be created. 

    * Discard any EMPTY_LIST nodes at the start of the list provided
    * the next node is non-empty. 
    
   while((*gp_list)->value.atom->exp_type == EMPTY_LIST) {
      if( (*gp_list)->next != NULL ) {
          * Keep track of the AST List node to free 
          List *temp = *gp_list;
	  * Redirect gp_list to point to the next (non-empty) node 
          *gp_list = (*gp_list)->next;
	  * Free the EMPTY_LIST node 
	  free(temp->value.atom); 
	  * Free the struct List that pointed to the EMPTY_LIST
	   * node. *
	  free(temp);
      }
       * The EMPTY_LIST node is the only element in the list. 
       * Nothing to be done. 
      else break;
   }
   */

   /* Make a copy of *gp_list in order to not modify the original pointer when
    * traversing the list.
    */
   List *iterator = *gp_list;

   while(iterator) {
       atomicExpScan(iterator->value.atom, table, scope, rule_name, 
		       location, false, false);

       /* ### This code is no longer relevant as EMPTY_LIST can only occur as a 
        * single struct List. ###

        * Removing the EMPTY_LIST nodes affects the global AST. Hence
	* the address of iterator->next, the pointer which may be redirected,
	* is required.
	
       List **next_node = &(iterator->next);


        * Discard any intermediate EMPTY_LIST nodes 
        * The while loop terminates when the end of the list has been reached
	* or when the next node is not an EMPTY_LIST node.
	
       while( *next_node != NULL &&
	      (*next_node)->value.atom->exp_type == EMPTY_LIST)
       {
	       List *temp = *next_node;
	       * Redirect the pointer to the node after the EMPTY_LIST
	        * node. 
	       *next_node = (*next_node)->next;
	       free(temp->value.atom);
	       free(temp);
       } */

       iterator = iterator->next; 
   } 

   if(list_var_count > 1) { 
      print_to_console("Error (%s.%s): More than one list variable in LHS "
	               "label.\n", 
                       scope, rule_name);
      print_to_log("Error (%s.%s): More than one list variable in LHS "
	           "label.\n", 
                   scope, rule_name);
      abort_compilation = true;
   }

   if(string_var_count > 1) {
      print_to_console("Error (%s.%s): More than one string variable in LHS "
	               "string expression.\n", 
                       scope, rule_name);
      print_to_log("Error (%s.%s): More than one string variable in LHS "
	           "string expression.\n", 
                   scope, rule_name);
      abort_compilation = true;
   }

   if(lhs_non_simple_exp) {
      print_to_console("Error (%s.%s): Arithmetic expression in LHS label \n",
                       scope, rule_name);
      print_to_log("Error (%s.%s): Arithmetic expression in LHS label \n",
                   scope, rule_name);
      abort_compilation = true;
   }

   /* Reset variables for future calls to gpListScan */
   list_var_count = 0;
   string_var_count = 0;
   lhs_non_simple_exp = false;
}



void atomicExpScan(GPAtomicExp * const atom_exp, GHashTable *table, 
                   string const scope, string const rule_name, 
                   char const location, bool const int_exp,
		   bool const string_exp)
{
   switch(atom_exp->exp_type) {

      case INT_CONSTANT:

           if(string_exp) {
              print_to_console("Error (%s.%s): Integer constant appears in a "
                               "string expression.\n", 
                               scope, rule_name);
              print_to_log("Error (%s.%s): Integer constant appears in a "
                           "string expression.\n",
                           scope, rule_name);
              abort_compilation = true;
           }             

           break;


      case CHARACTER_CONSTANT:

           if(int_exp) {
              print_to_console("Error (%s.%s): Character constant appears in "
                               "an integer expression.\n", 
                               scope, rule_name);
              print_to_log("Error (%s.%s): Character constant appears in "
                           "an integer expression.\n", 
                           scope, rule_name);
	      abort_compilation = true;
           }

           break;


      case STRING_CONSTANT:

           if(int_exp) {
              print_to_console("Error (%s.%s): String constant appears in an "
                               "integer expression.\n", 
                               scope, rule_name);
              print_to_log("Error (%s.%s): String constant appears in an "
                           "integer expression.\n", 
                           scope, rule_name);
	      abort_compilation = true;
           }

           break;

      case VARIABLE:

      {
           /* var_list is pointed to the symbol_list of symbols with the same
            * identifier as the variable.
            */
	   GSList *var_list = g_hash_table_lookup(table,atom_exp->value.name);           
           
           bool in_rule = false;

           while(var_list) {

              Symbol *current_var = (Symbol*)(var_list->data);
              
              /* Locate the variable with the appropriate scope and rule 
               * name. If it exists, there is only one, as duplicates are 
               * erroneous and not entered into the symbol table.
               */
              if(current_var->is_var &&
                 !strcmp(current_var->scope,scope) &&
	         !strcmp(current_var->containing_rule,rule_name)) 
              {
                 in_rule = true;

                 if(location == 'l') {
		    current_var->in_lhs = true;   
		    /* We need to keep track of the number of list and string 
                     * variables in the LHS to verify that all expressions are
		     * simple.
	             */
		    if(current_var->type == LIST_S) list_var_count++;
		    if(current_var->type == STRING_S) string_var_count++;
		 }

	         if(!in_rule) {
                    print_to_console("Error (%s.%s): Variable %s in "
                                     "expression but not declared.\n",
                                     scope, rule_name, atom_exp->value.name);
                    print_to_log("Error (%s.%s): Variable %s in expression "
                                 "but not declared.\n", 
                                 scope, rule_name, atom_exp->value.name);
                    abort_compilation = true;
                 }

	         /* Other semantic errors are reported in the else clause:
	          * there is no need to report these if the variable has not
	          * been declared. */

                 else /* location == 'r' || location == 'c' */ {
		    /* Check if a RHS variable exists in the LHS */
                    if(location == 'r' && !(current_var->in_lhs)) {
                       print_to_console("Error (%s.%s): Variable %s in RHS but "
                                        "not in LHS.\n", 
                                        scope, rule_name, atom_exp->value.name);
                       print_to_log("Error (%s.%s): Variable %s in RHS but "
                                    "not in LHS.\n",
                                    scope, rule_name, atom_exp->value.name);
	               abort_compilation = true;
                    }

	            /* Type checking */
                    if(int_exp && current_var->type != INT_S) {
                       print_to_console("Error(%s.%s): Variable %s occurs in "
                                        "an integer expression but not declared "
                                        "as an integer.\n",
                                        scope, rule_name, atom_exp->value.name);
                       print_to_log("Error(%s.%s): Variable %s occurs in an "
                                    "integer expression but not declared as "
                                    "an integer.\n",
                                    scope, rule_name, atom_exp->value.name);
                       abort_compilation = true;
		    }

                    if(string_exp && current_var->type != CHAR_S
                                  && current_var->type != STRING_S ) {
                       print_to_console("Error(%s.%s): Variable %s occurs in a "
                               "string expression but not declared as a string "
                               "or character. \n",
                               scope, rule_name, atom_exp->value.name);
                       print_to_log("Error(%s.%s): Variable %s occurs in a "
                               "string expression but not declared as a string."
                               "or character. \n",
                               scope, rule_name, atom_exp->value.name);
                       abort_compilation = true;
                    }
	         }
	    
 	        /* We have found the variable in the rule with the appropriate
                 * name. enterVariables ensures there is only one such variable
                 * variable in the symbol list. There is no need to look further.
                 */          
                break;	
	      }              
	      var_list = var_list->next;
	   } 

           break;
      }
       
      case INDEGREE:

      case OUTDEGREE:

      {
           if(string_exp) {
	      print_to_console("Error (%s.%s): Degree operator appears in "
                               "string expression.\n", 
                               scope, rule_name);
	      print_to_log("Error (%s.%s): Degree operator appears in "
                           "string expression.\n", 
                           scope, rule_name);
	      abort_compilation = true;

           }

           if(location == 'l') lhs_non_simple_exp = true;

           /* If the degree operator is in a condition, its argument must exist
            * in the LHS graph.
            */

           if(location == 'c') {
	      bool in_lhs = false;

	      GSList *node_list = 
		      g_hash_table_lookup(table, atom_exp->value.node_id);
    
	      while(node_list != NULL) {

		 Symbol *current_node = (Symbol*)node_list->data;     

		 if(current_node->type == LEFT_NODE_S &&
		    !strcmp(current_node->scope,scope) && 
		    !strcmp(current_node->containing_rule,rule_name))  
		 {
		    in_lhs = true;
		    break;
		 }

		 node_list = node_list->next;
	      }

	      if(!in_lhs) {
		 print_to_console("Error (%s.%s): Node %s in degree operator is "
			 "not in the LHS.\n", 
			 scope, rule_name,atom_exp->value.node_id);
		 print_to_log("Error (%s.%s): Node %s in degree operator is "
			 "not in the LHS.\n", 
			 scope, rule_name,atom_exp->value.node_id);
              abort_compilation = true;
              }
           }

           /* If the degree operator is in a right-label, its argument must exist
            * in the RHS graph.
            */

           if(location == 'r') {
	      
              bool in_rhs = false;

	      GSList *node_list = 
		      g_hash_table_lookup(table, atom_exp->value.node_id);
    
	      while(node_list != NULL) {

		 Symbol *current_node = (Symbol*)node_list->data;     

		 if(current_node->type == RIGHT_NODE_S &&
		    !strcmp(current_node->scope,scope) && 
		    !strcmp(current_node->containing_rule,rule_name))  
		 {
		    in_rhs = true;
		    break;
		 }

		 node_list = node_list->next;
	      }

	      if(!in_rhs) {
		 print_to_console("Error (%s.%s): Node %s in degree operator is "
			 "not in the RHS.\n", 
			 scope, rule_name,atom_exp->value.node_id);
		 print_to_log("Error (%s.%s): Node %s in degree operator is "
			 "not in the RHS.\n", 
			 scope, rule_name,atom_exp->value.node_id);
              abort_compilation = true;
              }
           }

           break;
         
       }

       case LIST_LENGTH: 

            if(string_exp) {
                print_to_console("Error (%s.%s): Length operator appears in "
                                 "string expression.\n", 
                                 scope, rule_name);
                print_to_log("Error (%s.%s): Length operator appears in "
                             "string expression.\n",
                             scope, rule_name);
                abort_compilation = true;
            }

            if(location == 'l') lhs_non_simple_exp = true;

            gpListScan(&(atom_exp->value.list_arg), table, scope, rule_name,
                         location);

            break;

       case STRING_LENGTH:

            if(string_exp) {
               print_to_console("Error (%s.%s): Length operator appears in "
                                "string expression.\n", 
                                scope, rule_name);
               print_to_log("Error (%s.%s): Length operator appears in "
                            "string expression.\n",
                            scope, rule_name);
               abort_compilation = true; 
            }
         
            if(location == 'l') lhs_non_simple_exp = true;

	    atomicExpScan(atom_exp->value.str_arg, table, scope, rule_name, 
			    location, false, true);
		    
            break;


       case NEG:

            if(string_exp) {
               print_to_console("Error (%s.%s): Arithmetic operator appears "
                                "in string expression.\n", 
                                scope, rule_name);
               print_to_log("Error (%s.%s): Arithmetic operator appears in "
                            "string expression.\n", 
                            scope, rule_name);
            }

            if(location == 'l') lhs_non_simple_exp = true;

            atomicExpScan(atom_exp->value.exp, table, scope, rule_name,
                          location, true, false);
            break;


       case ADD:

       case SUBTRACT:

       case MULTIPLY:

       case DIVIDE:

            if(string_exp) {
               print_to_console("Error (%s.%s): Arithmetic operator appears in "
                                "string expression.\n", 
                                scope, rule_name);
               print_to_log("Error (%s.%s): Arithmetic operator appears in "
                            "string expression.\n", 
                            scope, rule_name);
               abort_compilation = true;
            }

            if(location == 'l') lhs_non_simple_exp = true;

            atomicExpScan(atom_exp->value.bin_op.left_exp, table, scope,
                          rule_name, location, true, false);
            atomicExpScan(atom_exp->value.bin_op.right_exp, table, scope,
                          rule_name, location, true, false);       
            break;


       case CONCAT:     

            if(int_exp) {
               print_to_console("Error (%s.%s): String operator appears in "
                                "integer expression.\n", 
                                scope, rule_name);
               print_to_log("Error (%s.%s): String operator appears in "
                            "integer expression.\n", 
                            scope, rule_name);
               abort_compilation = true;
            }

            atomicExpScan(atom_exp->value.bin_op.left_exp, table, scope,
                          rule_name, location, false, true);
            atomicExpScan(atom_exp->value.bin_op.right_exp, table, scope,
                          rule_name, location, false, true); 

	    if(string_var_count > 1) {
	       print_to_console("Error (%s.%s): More than one string variable "
		                "in LHS string expression.\n", 
                                scope, rule_name);
	       print_to_log("Error (%s.%s): More than one string variable "
		            "in LHS string expression.\n", 
                            scope, rule_name);
	       string_var_count = 0;
            }

            break;


       default: print_to_log("Error: Unexpected atomic expression type %d "
		             "at AST node %d.\n", 
                             atom_exp->exp_type, atom_exp->node_id);
                abort_compilation = true;

                break;                
       }
} 

   
                                           

