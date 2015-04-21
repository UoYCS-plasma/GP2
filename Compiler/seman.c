#include "seman.h" 

GHashTable *symbol_table = NULL; 

bool analyseProgram(List *gp_program, bool debug)
{
  /* Create a new GHashTable with strings as keys.
   * g_str_hash is glib's default string hashing function.
   * g_str_equal is glib's default function for comparing strings for hash
   * lookups.
   * free is the function called by glib to free keys during hash table
   * insertions and in the g_hash_table_destroy function.
   * The fourth argument is a function to free values. I do this explicitly
   * with the function freeSymbolList defined above. */
   symbol_table = g_hash_table_new_full(g_str_hash, g_str_equal, free, NULL); 

   bool abort = declarationScan(gp_program, "Main");
   if(abort) return false;
   abort = semanticCheck(gp_program, "Main");
   if(debug) printSymbolTable(symbol_table);

   /* The call to g_hash_table_foreach frees all the hash table values,
    * namely linked lists of struct Symbols, with the function freeSymbolList 
    * defined in this file.
    * g_hash_table_destroy uses the key-freeing function passed to 
    * g_hash_table_full above to free the dynamically allocated keys. */
   if(symbol_table) {
     g_hash_table_foreach(symbol_table, freeSymbolList, NULL);
     g_hash_table_destroy(symbol_table); 
   }
   return !abort;
}

bool declarationScan(List *ast, string scope)
{
   /* These variables are declared static so that their values are unchanged
    * on recursive calls. */
   static int main_count = 0;
   static bool name_clash = false;

   GSList *symbol_list = NULL;
   while(ast != NULL)
   {     
      switch(ast->value.declaration->decl_type) 
      {
         case MAIN_DECLARATION:
              main_count += 1;
              break; 	 
  
         case PROCEDURE_DECLARATION:
         {
              GPProcedure *procedure = ast->value.declaration->value.procedure;
              GPProcedure *duplicate = 
                 findProcedureDeclaration(ast, procedure->name, procedure);
              if(duplicate != NULL)
              {
                 print_error("Error: Procedure %s declared more than once.\n", 
                             procedure->name);
	         name_clash = true;
	      }
	      if(procedure->local_decls) 
              {
	         /* Reverse local declaration list */
                 procedure->local_decls = reverse(procedure->local_decls);

                 /* Scan for any local declarations. The procedure's name is
                  * passed as the new scope. */
                 name_clash = declarationScan(procedure->local_decls, 
                                              procedure->name);
              }
              break;
         }
         case RULE_DECLARATION:
         {
  	      string old_rule_name = ast->value.declaration->value.rule->name;
              string rule_name = makeRuleIdentifier(old_rule_name, scope);
              ast->value.declaration->value.rule->name = rule_name;
              free(old_rule_name);
 
              symbol_list = g_hash_table_lookup(symbol_table, rule_name);      
	      GSList *iterator = symbol_list;
              bool add_rule = true;	      

              /* Report an error if two rules with the same name are declared
               * in the same scope. iterator points to the start of the symbol
               * list whose key is rule_name. */
              while(iterator != NULL)   
              {
                 string symbol_scope = ((Symbol*)iterator->data)->scope;
   
                 if(((Symbol*)iterator->data)->type == RULE_S &&
		    !strcmp(scope,symbol_scope))
		 {
                    if(!strcmp(scope, "Main")) 
                         print_error("Error: Rule %s declared twice in " 
                                     "global scope %s.\n", rule_name, scope); 
                    else print_error("Error: Rule %s declared twice in " 
                                     "procedure %s.\n", rule_name, scope);
		    add_rule = false;
                    name_clash = true;
		    /* Report the error only once for this declaration. */
                    break; 
                 }                 
		 iterator = iterator->next;
              }             
	      if(add_rule) 
              {
	         Symbol *rule_symbol = makeSymbol(RULE_S, scope, NULL, false,
                                                  false, false, false);
                 symbol_list = g_slist_prepend(symbol_list, rule_symbol);      
                 /* If rule_name is not copied here, it would be freed twice: 
                  * once when the rule is freed, and again when the hash table
                  * keys are freed. */
                 string rule_name_copy = strdup(rule_name);
                 g_hash_table_replace(symbol_table, rule_name_copy, symbol_list);  
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
   /* The code following the if statement is only executed upon reaching the
    * end of the global declaration list. */
   if(!strcmp(scope, "Main")) 
   {
     if(main_count == 0) 
     {
        print_error("Error: No Main procedure.\n");
        return false;
     }
     if(main_count > 1) 
     {
        print_error("Error: More than one Main declaration.\n");
        return false;
     }
   }  
   return name_clash;
}

string makeRuleIdentifier(string rule_name, string scope)
{
   int length = strlen(rule_name) + strlen(scope) + 2;
   char *new_rule_name = malloc(length);
   if(new_rule_name == NULL)
   {
      print_to_log("Error: Memory exhausted during rule name creation.\n");
      exit(1);
   }
   new_rule_name = strcpy(new_rule_name, scope);
   new_rule_name = strcat(new_rule_name, "_");
   new_rule_name = strcat(new_rule_name, rule_name);
   return new_rule_name;
}

/* bidirectional_edges is a linked list of the bidirectional edges encountered
 * in the program. This is to ensure that there are no parallel bidirectional
 * edges. The list stores items of type struct BidirectionalEdge. It is used
 * in graph_scan and freed in semanticCheck.
 */
BiEdgeList *bidirectional_edges = NULL; 

/* The return value of semanticCheck. It is modified by many of the functions
 * called by semanticCheck, so I declare it as a static global variable. */
static bool abort_compilation = false; 

bool semanticCheck(List *declarations, string scope)
{
   while(declarations) 
   {
      GPDeclaration *current_declaration = declarations->value.declaration;
   
      switch(current_declaration->decl_type)
      {
         case MAIN_DECLARATION:
	      /* An empty main program does not comform to the grammar. The
	       * parser should catch it and report a syntax error, but there is
	       * no harm in checking here as well. */
              if(current_declaration->value.main_program)
		 commandScan(current_declaration->value.main_program,
                             scope, declarations, false);
	      else print_to_log("Error: Main procedure has no program. \n");
              break;

         case PROCEDURE_DECLARATION: 
	 {
              GPProcedure *procedure = current_declaration->value.procedure;
	      /* An empty procedure program does not comform to the grammar. The
	       * parser should catch it and report a syntax error, but there is
	       * no harm in checking here as well. */
              if(!procedure->commands)
                 print_to_log("Error: Procedure %s has no program, "
                              "not caught by parser. \n", procedure->name);
	      if(procedure->local_decls)
                 abort_compilation = semanticCheck(procedure->local_decls,
                                                   procedure->name);
              break;
	 }    
         case RULE_DECLARATION:              
              ruleScan(current_declaration->value.rule, scope);
              break;  

         default: print_to_log("Error (semanticCheck): Unexpected declaration "
                               "type %d at AST node %d\n", 
                               current_declaration->decl_type,
                               current_declaration->node_id);
              break;
      } 
   declarations = declarations->next;  
   }
   /* The code following the if statement is only executed upon reaching the
    * end of the global declaration list. */
   if(!strcmp(scope, "Main"))
   {
      if(bidirectional_edges) freeBiEdgeList(bidirectional_edges);
   }
   return abort_compilation;
}   

void commandScan(GPCommand *command, string scope, List *declarations, bool in_loop)
{
   switch(command->command_type) 
   {
      case COMMAND_SEQUENCE: 
      {
           command->value.commands = reverse(command->value.commands);
           List *command_list = command->value.commands;

           while(command_list) 
           {
              commandScan(command_list->value.command, scope, declarations, in_loop);
              command_list = command_list->next;   
           }           
           break;
      }
      case RULE_CALL:
      {
           string name = command->value.rule_call.rule_name;
           GPProcedure *procedure = NULL;
           /* If not in global scope, get a pointer to the procedure in which to
            * start search for the rule declaration. */
           if(strcmp(scope, "Main"))
              procedure = findProcedureDeclaration(declarations, scope, NULL);

           GPRule *rule = findRuleDeclaration(declarations, name, procedure);
           if(rule == NULL)
           {
              print_error("Error: Rule %s called but not declared in a "
                          "visible scope.\n", name);     
              abort_compilation = true;
           }
           else
           {
              free(name);
              command->value.rule_call.rule_name = strdup(rule->name);
              command->value.rule_call.rule = rule;
           }
           break;
      }
      case RULE_SET_CALL: 
      {
           command->value.rule_set = reverse(command->value.rule_set);
           List *rule_list = command->value.rule_set;
           while(rule_list)
           {
              string name = rule_list->value.rule_call.rule_name;
              GPProcedure *procedure = NULL;
              /* If not in global scope, get a pointer to the procedure in 
               * which to start search for the rule declaration. */
              if(strcmp(scope, "Main"))
                 procedure = findProcedureDeclaration(declarations, scope, NULL);

              GPRule *rule = findRuleDeclaration(declarations, name, procedure);
              if(rule == NULL)
              {
                 print_error("Error: Rule %s called but not declared in a "
                             "visible scope.\n", name);     
                 abort_compilation = true;
              }
              else
              {
                 free(name);
                 rule_list->value.rule_call.rule_name = strdup(rule->name);
                 rule_list->value.rule_call.rule = rule;
              }
              rule_list = rule_list->next;
           }           
           break;
      }
      case PROCEDURE_CALL:   
      {
           string name = command->value.proc_call.proc_name;
           GPProcedure *procedure = findProcedureDeclaration(declarations, name, NULL);
           if(procedure == NULL)
           {
              print_error("Error: Procedure %s called but not declared.\n", name);    
              abort_compilation = true;
           }
           else 
           {
              command->value.proc_call.procedure = procedure;
              commandScan(procedure->commands, name, declarations, in_loop);
           }
           break;
      }
      case IF_STATEMENT:
      case TRY_STATEMENT:
           commandScan(command->value.cond_branch.condition, scope, 
                       declarations, false);
           commandScan(command->value.cond_branch.then_command, scope, 
                       declarations, in_loop);
           commandScan(command->value.cond_branch.else_command, scope, 
                       declarations, in_loop);
           break;

      case ALAP_STATEMENT:
           commandScan(command->value.loop_stmt.loop_body, scope, 
                       declarations, true);
           break;

      case PROGRAM_OR:
           commandScan(command->value.or_stmt.left_command, scope,
                       declarations, in_loop);
           commandScan(command->value.or_stmt.right_command, scope, 
                       declarations, in_loop);
           break;

      case SKIP_STATEMENT: 
      case FAIL_STATEMENT:
           break;
      
      case BREAK_STATEMENT: 
           if(!in_loop) 
           {
              YYLTYPE location = command->location;
              print_to_console("Error: break command not in loop.\n");
              print_to_log("%d.%d-%d.%d: Error: break command not in loop.\n", 
                           location.first_line, location.first_column, 
                           location.last_line, location.last_column);
              abort_compilation = true;
           }
           break;

      default: print_to_log("Error (commandScan): Unexpected type %d at "
                             "AST node %d\n", 
                             command->command_type, command->node_id);
               abort_compilation = true;
               break;   
   }
}             

GPRule *findRuleDeclaration(List *global_declarations, string name,
                            GPProcedure *procedure)
{
   /* The code in the body of the if statement searches the local declarations
    * of the passed procedure. If the rule is not found there, or if no 
    * procedure was passed, the code falls through to the while loop that 
    * searches the global declaration list. */
   if(procedure != NULL)
   {
      List *local_declarations = procedure->local_decls;
      while(local_declarations != NULL)
      {
         GPDeclaration *declaration = local_declarations->value.declaration;
         if(declaration->decl_type == RULE_DECLARATION)
         {
           /* The rule name in the declaration is of the form 
            * "<proc_name>_<rule_name>". Disregard "<proc_name>_" for the 
            * comparison. */
            int length = strlen(declaration->value.rule->name) -
                         strlen(procedure->name) + 1;
            if(!strcmp(declaration->value.rule->name + length, name))
               return declaration->value.rule;
         }
         if(declaration->decl_type == PROCEDURE_DECLARATION)
         {
            /* Search for the rule declaration in the local declaration list of
             * the procedure and any local procedures. */
            GPRule *rule = findRuleDeclaration(local_declarations, name,  
                                               declaration->value.procedure);
            if(rule != NULL) return rule;
         }                           
         local_declarations = local_declarations->next;
      }
   }
   while(global_declarations != NULL)
   {
      GPDeclaration *declaration = global_declarations->value.declaration;
      if(declaration->decl_type == RULE_DECLARATION)
      {
          /* The rule name in the declaration node is of the form
           * "Main_<rule_name>". Disregard "Main_" for the comparison. */
          if(!strcmp(declaration->value.rule->name + 5, name))
             return declaration->value.rule;
      }
      global_declarations = global_declarations->next;
   }
   return NULL;
}   

GPProcedure *findProcedureDeclaration(List *declarations, string name,
                                      GPProcedure *excluded_procedure) 
{
   while(declarations != NULL)
   {
      GPDeclaration *declaration = declarations->value.declaration;
      if(declaration->decl_type == PROCEDURE_DECLARATION)
      {
         if(!strcmp(declaration->value.procedure->name, name) &&
            declaration->value.procedure != excluded_procedure)
            return declaration->value.procedure;
         /* Search for the procdure declaration in the local declaration list
          * of the procedure and any local procedures. */
         GPProcedure *procedure =
            findProcedureDeclaration(declaration->value.procedure->local_decls, 
                                     name, NULL);
         if(procedure != NULL) return procedure;
      }                           
      declarations = declarations->next;
   }
   return NULL;
}

void ruleScan(GPRule *rule, string scope)
{  
   /* Reverse the list of declaration types and the interface list. */
   if(rule->variables) rule->variables = reverse(rule->variables);
   if(rule->interface) rule->interface = reverse(rule->interface);

   List *variable_list = rule->variables;
   /* Variables to count how many times each type is encountered. These are
    * incremented, according to the list_type encountered, and a warning is
    * printed if any variable becomes greater than 1. Hence only the 
    * variables in the first declaration list for a type are added to the
    * symbol table; the rest are ignored. */ 
   int integer_count = 0, character_count = 0, string_count = 0, atom_count = 0, 
       list_count = 0;

   while(variable_list)
   {
      /* Reverse the list of variables */
      variable_list->value.variables = reverse(variable_list->value.variables);	   
      switch(variable_list->list_type) 
      {
         case INT_DECLARATIONS:
              checkDeclaration(rule, variable_list->value.variables, scope,
                               INT_S, integer_count++, "integer");  
	      break;

         case CHAR_DECLARATIONS:
              checkDeclaration(rule, variable_list->value.variables, scope,
                               CHAR_S, character_count++, "character");  
	      break;

         case STRING_DECLARATIONS:
              checkDeclaration(rule, variable_list->value.variables, scope,
                               STRING_S, string_count++, "string");  
              break;
   	
         case ATOM_DECLARATIONS:
              checkDeclaration(rule, variable_list->value.variables, scope,
                               ATOM_S, atom_count++, "atom"); 
	      break; 

	 case LIST_DECLARATIONS:
              checkDeclaration(rule, variable_list->value.variables, scope,
                               LIST_S, list_count++, "list"); 
	      break;  	 

	 default:
	      print_to_log("Error: Unexpected list type %d in AST node %d\n",
		           variable_list->list_type, variable_list->node_id);
              abort_compilation = true;
	      break;
      }
      variable_list = variable_list->next;
   }
   graphScan(rule, scope, rule->name, 'l');
   graphScan(rule, scope, rule->name, 'r');
   if(rule->interface) interfaceScan(rule->interface, scope, rule->name);
   if(rule->condition) conditionScan(rule->condition, scope, rule->name);
}   

void checkDeclaration(GPRule *rule, List *variables, string scope,
                      SymbolType type, int count, string type_name)
{
   if(count > 1) 
      print_error("Warning (%s): More than one %s list in the variable "
                  "declaration section. Only the first list is considered.\n",
                  rule->name, type_name);
   else 
   {
      int count = enterVariables(type, variables, scope, rule->name);
      rule->variable_count += count;
   }
}

int enterVariables(SymbolType type, List *variables, 
                   string scope, string rule_name)
{
   int variable_count;
   while(variables) 
   {
      variable_count++;
      string variable_name = strdup(variables->value.variable_name);	   
      GSList *symbol_list = g_hash_table_lookup(symbol_table, variable_name);
      /* symbol_list is preserved as a new symbol is prepended to it */
      GSList *iterator = symbol_list;

      bool add_variable = true;
      while(iterator) 
      {
         Symbol *current_var = (Symbol*)(iterator->data);

	 /* Print an error if there already exists a variable in the same rule
	  * and scope with the same name. */
         if(current_var->is_var && symbolInScope(current_var, scope, rule_name))
	 {	 
	    print_error("Warning (%s): Variable %s declared twice.\n", 
                        rule_name, variable_name);
	    add_variable = false;
            break;
	 }
         iterator = iterator->next;      
      }
      if(add_variable)
      {
         Symbol *var_symbol = makeSymbol(type, scope, rule_name, true, false,
                                         false, false);
         symbol_list = g_slist_prepend(symbol_list, var_symbol);  
         g_hash_table_replace(symbol_table, variable_name, symbol_list);
     }
     /* The malloc'd string variable_name is not used as a key to the symbol
      * table in the else case. It needs to be freed before the loop breaks. */
     else free(variable_name);
     /* Move to the next variable in the declaration list. */
     variables = variables->next;
   }
   return variable_count;
}  

void graphScan(GPRule *rule, string scope, string rule_name, char side)
{
   GPGraph *graph = NULL;
   if(side == 'l') graph = rule->lhs;
   else if(side == 'r') graph = rule->rhs;
   /* Variables to store the symbol types and the graph for semantic checking
    * and error reporting. */
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
   while(node_list)  
   {
      if(side == 'l') rule->left_nodes++;
      /* node_id is used as a key, so it is duplicated as node_id will be freed
       * by g_hash_table_insert. I do not want to also free the node->name in
       * the AST. */
      string node_id = strdup(node_list->value.node->name);
      symbol_list = g_hash_table_lookup(symbol_table, node_id);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;

      bool add_node = true;
      while(iterator) 
      {
         Symbol *current_node = (Symbol*)(iterator->data);
	 /* Print an error if there already exists a node in the same graph, 
	  * rule and scope with the same name. */
         if(current_node->type == node_type && 
            symbolInScope(current_node, scope, rule_name))
	 {
	     print_to_log("Warning (%s): Node ID %s not unique in the "
                          "%s.\n", rule_name, node_id, graph_type);  
	     add_node = false;
	     break;
	 }
         iterator = iterator->next;      
      }
      if(node_list->value.node->label->mark == DASHED)
      {
          print_to_log("Error (%s): Node %s in %s graph has invalid mark " 
	               "\"dashed\".\n", 
                       rule_name, node_id, graph_type);
          abort_compilation = true; 
      }
      if(add_node) 
      { 
         bool wildcard = node_list->value.node->label->mark == ANY;
         Symbol *node_symbol = makeSymbol(node_type, scope, rule_name, false,
                                          false, wildcard, false);
         symbol_list = g_slist_prepend(symbol_list, node_symbol);         
         g_hash_table_replace(symbol_table, node_id, symbol_list);         
      }      
      /* If the node is in the RHS and has an 'any' mark, the corresponding LHS node
       * must also have an 'any' mark. */
      if(side == 'r' && node_list->value.node->label->mark == ANY) 
      {
         /* The current node has just been prepended to the symbol list.
          * Hence the first entry in the symbol list is an RHS node. 
          * The corresponding LHS node is to be located. Therefore the search
          * starts at the second element in the symbol list. */
         iterator = symbol_list->next;
         while(iterator) 
         {
            Symbol *current_node = (Symbol*)(iterator->data);
	    /* Find a node in the same rule. If that node is in the LHS
             * and is not a wildcard (any mark) then report an error. */
            if(symbolInScope(current_node, scope, rule_name))	
  	    {
                if(current_node->type == LEFT_NODE_S && !current_node->wildcard) 
                {
	           print_to_log("Error (%s): RHS wildcard node %s has no "
                                "matching LHS wildcard.", rule_name, node_id);  
                   abort_compilation = true; 
                }
                /* Regardless of the outcome of the inner if statement, exit
                 * the loop as the single appropriate node has been located. */
                break;   
	    }
            iterator = iterator->next;      
         }
      }
      gpListScan(&(node_list->value.node->label->gp_list), scope, 
		   rule_name, side);
      node_list = node_list->next;   
      if(!add_node && node_id) free(node_id);  
   }   

   /* Reverse the edge list */
   graph->edges = reverse(graph->edges);
   List *edge_list = graph->edges;
   bool add_edge = true;
   while(edge_list)
   {
      if(side == 'l') rule->left_edges++;
      /* edge_id is used as a key, so it is duplicated as edge_id will be freed
       * by g_hash_table_insert. I do not also want to free the edge->name in
       * the AST. */
      string edge_id = strdup(edge_list->value.edge->name);
      string source_id = edge_list->value.edge->source;
      string target_id = edge_list->value.edge->target;

      symbol_list = g_hash_table_lookup(symbol_table, edge_id);
      /* symbol_list is preserved as a new symbol will be prepended to it */
      GSList *iterator = symbol_list;
      while(iterator) 
      {
         Symbol *current_edge = (Symbol*)(iterator->data);
	 /* Print an error if there already exists an edge in the same graph,
	  * rule and scope with the same name. */
         if(current_edge->type == edge_type &&
            symbolInScope(current_edge, scope, rule_name))
         {
	     print_to_log("Warning (%s): Edge ID %s not unique in the %s "
                          "graph.\n", rule_name, edge_id, graph_type);
	     add_edge = false;
	     break;
	 }
         iterator = iterator->next; 
      }
      if(add_edge) 
      {
         bool wildcard = edge_list->value.edge->label->mark == ANY; 
         bool bidirectional = edge_list->value.edge->bidirectional; 
         Symbol *edge_symbol = makeSymbol(edge_type, scope, rule_name, false,
                                          false, wildcard, bidirectional);
         symbol_list = g_slist_prepend(symbol_list, edge_symbol);   
         g_hash_table_replace(symbol_table, edge_id, symbol_list);
      }
      if(side == 'r' && edge_list->value.edge->label->mark == ANY) 
      {
         /* The current edge has just been prepended to the symbol list.
          * Hence the first entry in the symbol list is this RHS edge. 
          * The corresponding LHS edge is to be located, hence search
          * might as well start at the second element in the symbol list. */
         iterator = symbol_list->next;
         while(iterator) 
         {
            Symbol *current_edge = (Symbol*)(iterator->data);
	    /* Find an edge in the same rule. If that edge is in the LHS
             * and is not a wildcard (any mark) then report an error. */
            if(symbolInScope(current_edge, scope, rule_name))
  	    {
                if(current_edge->type == LEFT_EDGE_S && !current_edge->wildcard) 
                {
	           print_to_log("Error (%s): RHS wildcard edge %s has no "
                                "matching LHS wildcard.\n", 
                                rule_name, edge_id);  
                   abort_compilation = true;
                }
                /* Exit the loop as the single appropriate edge has been 
                 * located. */ 
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
       * the bidirectional edges encountered. */
      if(edge_list->value.edge->bidirectional) 
      {
         /* bidirectional_edges may be written to later, hence a copy is made
          * of the root pointer.
          */
         BiEdgeList *iterator = bidirectional_edges;

         bool add_bi_edge = true;
         bool matching_bi_edge = false;
 
         while(iterator) 
         {
            BiEdge list_edge = iterator->value;
            /* Find edges in the list with the same scope, rule, and 
             * incident node IDs as the current edge. Edge direction is 
             * irrelevant for this check. */
            if(!strcmp(list_edge.scope, scope) &&
               !strcmp(list_edge.containing_rule, rule_name) &&
               /* Either source = source and target = target... */
               ((!strcmp(list_edge.source, source_id) &&
                !strcmp(list_edge.target, target_id))   ||
               /*... or source = target and target = source */
               (!strcmp(list_edge.source, target_id) &&
                !strcmp(list_edge.target, source_id))) ) 
            {
               /* Semantic check (1) */
               if(side == 'r' && list_edge.graph == 'l') 
                  matching_bi_edge = true;
               /* Semantic check (2) */
               else 
               {
                  if(list_edge.graph == side)
                  {
                     print_to_log("Error (%s): Parallel bidirectional edges "
                                  "in %s.\n", rule_name, graph_type);
                     abort_compilation = true;
                     add_bi_edge = false;
                  }
               }
            }
            iterator = iterator->next;       
         }
         if(side == 'r' && !matching_bi_edge) 
         {
	    print_to_log("Error (%s): RHS bidirectional edge %s has no "
                         "matching LHS bidirectional edge.\n", 
                         rule_name, edge_id);
            abort_compilation = true; 
         }
         /* Only add an edge to the BE list if it is not a parallel edge. */
         if(add_bi_edge) addBiEdge(bidirectional_edges, scope, rule_name, side,
                                   source_id, target_id);
      }
      /* Verify source node exists in the graph. */
      symbol_list = g_hash_table_lookup(symbol_table, source_id);
      if(symbol_list == NULL) 
      {
	 print_to_log("Error (%s): Source node %s of edge %s does not "
                      "exist in %s graph.\n",
                      rule_name, source_id, edge_id, graph_type);     
         abort_compilation = true; 
      }
      else
      {
         /* Keep track of the symbol currently being looked at. */
         Symbol *current_sym = (Symbol*)(symbol_list->data);
                
         /* The while loop condition is true if the current symbol is not
          * the node in the same rule and graph as the current edge.
          * Hence the loop breaks if the appropriate node is found.
          * At each pass a check is made to see if the end of the list
          * has been reached. If not, check the next symbol in the symbol
          * list. Otherwise, print an error and break. */
         while(current_sym->type != node_type   ||
               strcmp(current_sym->scope,scope) ||
               strcmp(current_sym->containing_rule, rule_name))
         {   
            if(symbol_list->next == NULL) 
            {
               print_to_log("Error (%s): Source node %s of edge %s does "
                            "not exist in %s graph.\n", 
                            rule_name, source_id, edge_id, graph_type);     
               abort_compilation = true; 
               break;
            }
            else current_sym = (Symbol*)(symbol_list->next->data);             
         } 
      }
      /* Verify target node exists in the graph. */
      symbol_list = g_hash_table_lookup(symbol_table, target_id);    
       if(symbol_list == NULL) 
      {
	 print_to_log("Error (%s): Target node %s of edge %s does not "
                      "exist in %s graph.\n", 
                      rule_name, target_id, edge_id, graph_type);     
         abort_compilation = true; 
      } 
      else 
      {
         /* Keep track of the symbol currently being looked at. */
         Symbol *current_sym = (Symbol*)(symbol_list->data);

         /* The while loop condition is true if the current symbol is not
          * the node in the same rule and graph as the current edge.
          * Hence the loop breaks if the appropriate node is found.
          * At each pass a check is made to see if the end of the list
          * has been reached. If not, check the next symbol in the symbol
          * list. Otherwise, print an error and break. */               
         while(current_sym->type != node_type   ||
               strcmp(current_sym->scope,scope) ||
               strcmp(current_sym->containing_rule,rule_name))
         {
            if(symbol_list->next == NULL) 
            {
               print_to_log("Error (%s): Target node %s of edge %s does "
                            "not exist in %s graph.\n", 
                            rule_name, target_id, edge_id, graph_type);   
               abort_compilation = true; 
               break;
            }
            else current_sym = (Symbol*)(symbol_list->next->data);             
         }
      }	 
      gpListScan(&(edge_list->value.edge->label->gp_list), scope, 
		   rule_name, side);
      edge_list = edge_list->next;
      if(!add_edge && edge_id) free(edge_id);
   }
}

void interfaceScan(List *interface, string scope, string rule_name)
{
  /* Linked list to store node IDs encountered to check for uniqueness */
  GSList *interface_ids = NULL, *iterator = NULL;
  bool in_lhs, in_rhs;

  while(interface)
  {
     in_lhs = false, in_rhs = false;
     string current_node_id = interface->value.node_id;

     /* g_slist_insert_sorted inserts elements and maintains lexicographic
      * order of its elements with use of the function strcmp. This makes
      * checking for duplicate nodes easier. */
     interface_ids = g_slist_insert_sorted(interface_ids, current_node_id,
		                           (GCompareFunc)strcmp);
     GSList *node_list = g_hash_table_lookup(symbol_table, current_node_id);     
     while(node_list) 
     {
        Symbol *current_node = (Symbol*)node_list->data;     
	if(symbolInScope(current_node, scope, rule_name))
	{
	   if(current_node->type == LEFT_NODE_S) in_lhs = true;
	   if(current_node->type == RIGHT_NODE_S) in_rhs = true;
	}
	/* If both the LHS node and RHS node have been found, no need to look
	 * further down the symbol list. */         
	if(in_lhs && in_rhs) break;
	node_list = node_list->next;
     }
     if(!in_lhs)  
     {
        print_to_log("Error (%s): Interface node %s not in the LHS "
	   	     "graph.\n", rule_name, current_node_id);
        abort_compilation = true; 
     }
     if(!in_rhs) 
     {
        print_to_log("Error (%s): Interface node %s not in the RHS "
		     "graph.\n", rule_name, current_node_id);
        abort_compilation = true; 
     }
     interface = interface->next;   
  }
  /* Since interface_ids is sorted, each element in the list only needs to be 
   * compared to its successor. */
  for(iterator = interface_ids; iterator->next; iterator = iterator->next)
  {
     if(!strcmp(iterator->data,iterator->next->data))
        print_to_log("Warning (%s): Node %s occurs twice in interface list.\n",
                     rule_name, (char*)(iterator->data));
  }
  g_slist_free(interface_ids); 
}
        

void conditionScan(GPCondition * condition, string scope, string rule_name)
{
   switch(condition->exp_type) 
   {
      case INT_CHECK:
      case CHAR_CHECK:
      case STRING_CHECK:
      case ATOM_CHECK: 
      {
           bool in_rule = false; 
           GSList *var_list = 
              g_hash_table_lookup(symbol_table, condition->value.var);
	   /* Go through the list of symbols with the name in question
            * to check if any variables exist in this rule.
            */
           while(var_list != NULL) 
           {
 	      Symbol *current_var = (Symbol*)var_list->data;
              if(current_var->is_var &&
                 symbolInScope(current_var, scope, rule_name))
              {
                  in_rule = true;
                  break;
              }
              var_list = var_list->next;            
           }
           /* Not a critical error: if a variable in a type check predicate is 
            * not in the rule, the condition evaluates to false. */
           if(!in_rule) 
              print_error("Warning (%s): Variable %s in condition not "
                          "declared.\n", rule_name, condition->value.var);
           break;
      }
      /* For an edge predicate, the source and target node IDs must be present
       * in the LHS of the rule. The optional label argument is also scanned. */
      case EDGE_PRED:
      {
           bool in_lhs = false;
           GSList *node_list = 
              g_hash_table_lookup(symbol_table, condition->value.edge_pred.source);
           while(node_list != NULL)  
           {
              Symbol* current_node = (Symbol*)node_list->data;      
              if(current_node->type == LEFT_NODE_S &&
                 symbolInScope(current_node, scope, rule_name))
              {
                 in_lhs = true;
                 break;
              }
              node_list = node_list->next;
           }
           if(!in_lhs) 
           {
              print_error("Error (%s): Node %s in edge predicate not in LHS.\n",
                          rule_name, condition->value.edge_pred.source);
              abort_compilation = true;  
           }
           in_lhs = false;

           node_list = g_hash_table_lookup(symbol_table, 
                          condition->value.edge_pred.target);
           while(node_list != NULL) 
           {
		 Symbol* current_node = (Symbol*)node_list->data;      

         	 if(current_node->type == LEFT_NODE_S &&
                    symbolInScope(current_node, scope, rule_name)) 
                 {
                    in_lhs = true;
                    break;
                 }
                 node_list = node_list->next;
           }
           if(!in_lhs) 
           {
              print_error("Error (%s): Node %s in edge predicate not in LHS.\n",
                          rule_name, condition->value.edge_pred.target);
              abort_compilation = true;  
           }
           in_lhs = false;

	   /* Scan the label argument if it exists. */
           if(condition->value.edge_pred.label)
              gpListScan(&(condition->value.edge_pred.label->gp_list), 
		         scope, rule_name, 'c');
           break;
      }

      case EQUAL:
      case NOT_EQUAL:
           gpListScan(&(condition->value.list_cmp.left_list), scope, rule_name, 'c');
           gpListScan(&(condition->value.list_cmp.right_list), scope, rule_name, 'c');
           break;

      case GREATER:
      case GREATER_EQUAL:
      case LESS:
      case LESS_EQUAL:
           atomScan(condition->value.atom_cmp.left_exp, scope, rule_name, 'c',
                    true, false);
           atomScan(condition->value.atom_cmp.right_exp, scope, rule_name, 'c',
                    true, false);
           break;

      case BOOL_NOT:
           conditionScan(condition->value.not_exp, scope, rule_name);
           break;

      case BOOL_OR:
      case BOOL_AND:
           conditionScan(condition->value.bin_exp.left_exp, scope, rule_name);
           conditionScan(condition->value.bin_exp.right_exp, scope, rule_name);
           break;

      default:
	   print_to_log("Error: Unexpected condition type %d at AST node %d\n",
	                condition->exp_type, condition->node_id);
           abort_compilation = true; 
           break;
      }
}

/* Variables used by gpListScan and atomScan. */
static int list_var_count = 0;
static int string_var_count = 0;
static bool lhs_not_simple = false;

void gpListScan(List **gp_list, string scope, string rule_name, char location)
{
   if((*gp_list)->list_type == EMPTY_LIST) return;
   *gp_list = reverse(*gp_list);

   List *iterator = *gp_list;
   while(iterator) 
   {
      atomScan(iterator->value.atom, scope, rule_name, location, false, false);
      iterator = iterator->next; 
   } 
   if(list_var_count > 1) print_error("Error (%s): More than one list variable "
                                      "in LHS label.\n", rule_name);

   if(string_var_count > 1) print_error("Error (%s): More than one string variable "
                                        "in LHS string expression.\n", rule_name);

   if(lhs_not_simple) print_error("Error (%s): Non-simple expression in LHS "
                                      "label. \n", rule_name);

   if(list_var_count > 1 || string_var_count > 1 || lhs_not_simple) 
      abort_compilation = true;
   /* Reset variables. */
   list_var_count = 0;
   string_var_count = 0;
   lhs_not_simple = false;
}

/* Initially called with int_exp and string_exp set to false. Recursive calls
 * set one of these flags to true. Specifically, the atoms in degree operators,
 * the length operator and arithmetic expressions are called with int_exp true.
 * The atoms in concat expressions are called with string_exp true. */
void atomScan(GPAtom *atom, string scope, string rule_name, char location,
              bool int_exp, bool string_exp)
{
   switch(atom->exp_type) 
   {
      case INTEGER_CONSTANT:
           if(string_exp) 
           {
              print_error("Error (%s): Integer constant appears in a string "
                          "expression.\n", rule_name);
              abort_compilation = true;
           }             
           break;

      case STRING_CONSTANT:
           if(int_exp) 
           {
              print_error("Error (%s): String constant appears in an integer "
                          "expression.\n", rule_name);
	      abort_compilation = true;
           }
           break;

      case VARIABLE:
           variableScan(atom, scope, rule_name, location, int_exp, string_exp);
           break;

      case INDEGREE:
      case OUTDEGREE:
      {
           if(string_exp) 
           {
	      print_error("Error (%s): Degree operator appears in string "
                          "expression.\n", rule_name);
	      abort_compilation = true;
           }
           if(location == 'l') lhs_not_simple = true;
           /* If the degree operator is in a condition, its argument must exist
            * in the LHS graph. */
           if(location == 'c') 
           {
	      bool in_lhs = false;
	      GSList *node_list = 
		      g_hash_table_lookup(symbol_table, atom->value.node_id);
    
	      while(node_list != NULL) 
              {
		 Symbol *current_node = (Symbol*)node_list->data;     

		 if(current_node->type == LEFT_NODE_S &&
                    symbolInScope(current_node, scope, rule_name))
		 {
		    in_lhs = true;
		    break;
		 }
		 node_list = node_list->next;
	      }
	      if(!in_lhs) 
              {
		 print_error("Error (%s): Node %s in degree operator is not "
			     "in the LHS.\n", rule_name, atom->value.node_id);
                 abort_compilation = true;
              }
           }
           /* If the degree operator is in a right-label, its argument must exist
            * in the RHS graph.  */
           if(location == 'r') 
           {
              bool in_rhs = false;
	      GSList *node_list = 
		      g_hash_table_lookup(symbol_table, atom->value.node_id);
	      while(node_list != NULL) 
              {
		 Symbol *current_node = (Symbol*)node_list->data;     

		 if(current_node->type == RIGHT_NODE_S &&
                    symbolInScope(current_node, scope, rule_name))
                 {
		    in_rhs = true;
		    break;
		 }

		 node_list = node_list->next;
	      }
	      if(!in_rhs)  
              {
		 print_error("Error (%s): Node %s in degree operator is not "
			     "in the RHS.\n", rule_name, atom->value.node_id);
                 abort_compilation = true;
              }
           }
           break;
       }
       case LENGTH:
            if(string_exp) 
            {
               print_error("Error (%s): Length operator appears in string "
                           "expression.\n", rule_name);
               abort_compilation = true;
            }
            variableScan(atom, scope, rule_name, location, int_exp, string_exp);
            break;
           
       case NEG:
            if(string_exp) 
            {
               print_error("Error (%s): Arithmetic operator appears in string "
                           "expression.\n", rule_name);
               abort_compilation = true;
            }
            atomScan(atom->value.exp, scope, rule_name,
                          location, true, false);
            break;

       case ADD:
       case SUBTRACT:
       case MULTIPLY:
       case DIVIDE:
            if(string_exp) 
            {
               print_error("Error (%s): Arithmetic operator appears in string "
                           "expression.\n", rule_name);
               abort_compilation = true;
            }
            if(location == 'l') lhs_not_simple = true;
            atomScan(atom->value.bin_op.left_exp, scope,
                          rule_name, location, true, false);
            atomScan(atom->value.bin_op.right_exp, scope,
                          rule_name, location, true, false);       
            break;

       case CONCAT:     
            if(int_exp) 
            {
               print_error("Error (%s): String operator appears in integer "
                           "expression.\n", rule_name);
               abort_compilation = true;
            }
            atomScan(atom->value.bin_op.left_exp, scope,
                          rule_name, location, false, true);
            atomScan(atom->value.bin_op.right_exp, scope,
                          rule_name, location, false, true); 
	    if(string_var_count > 1) 
            {
	       print_error("Error (%s): More than one string variable in LHS "
		           "string expression.\n", rule_name);
	       string_var_count = 0;
            }
            break;

       default:
            print_to_log("Error: Unexpected atom type %d at AST node %d.\n",
		         atom->exp_type, atom->node_id);
            abort_compilation = true;
            break;                
    }
}               

void variableScan(GPAtom *atom, string scope, string rule_name, 
                  char location, bool int_exp, bool string_exp)
{
   /* var_list is pointed to the symbol_list of symbols with the same
    * identifier as the variable. */
   GSList *var_list = g_hash_table_lookup(symbol_table, atom->value.variable.name);      
   bool in_rule = false;
   while(var_list)
   {
      Symbol *current_var = (Symbol*)(var_list->data);
      /* Locate the variable with the appropriate scope and rule 
       * name. If it exists, there is only one, as duplicates are 
       * not entered into the symbol table. */
      if(current_var->is_var && symbolInScope(current_var, scope, rule_name))
      {
         if(location == 'l') current_var->in_lhs = true;   
         in_rule = true;
         /* Assign the variable's type to the AST node and increment the
          * string and list variable counters for every appropriate variable
          * in the LHS. */
         switch(current_var->type)
         {
            case INT_S:
                 atom->value.variable.type = INTEGER_VAR;
                 break;

            case CHAR_S:
                 atom->value.variable.type = CHARACTER_VAR;
                 break;
            
            case STRING_S:
                 atom->value.variable.type = STRING_VAR;
                 if(location == 'l') string_var_count++;
                 break;

            case ATOM_S:
                 atom->value.variable.type = ATOM_VAR;
                 if(location == 'l') string_var_count++;
                 break;

            case LIST_S:
                 atom->value.variable.type = LIST_VAR;
                 if(location == 'l') list_var_count++;
                 break;

            default:
                 print_to_log("Error (variableCheck): Unexpected symbol type "
                              "%d.\n", current_var->type);
                 break;
         }
         if(!in_rule) 
         {
            print_error("Error (%s): Variable %s in expression but not "
                        "declared.\n", rule_name, atom->value.variable.name);
            abort_compilation = true;
         }
         /* Other semantic errors are reported in the else clause: there is no
          * need to report these if the variable has not been declared. */
         else /* location == 'r' || location == 'c' */
         {
            /* Check if a RHS variable exists in the LHS */
            if(location == 'r' && !(current_var->in_lhs))
            {
               print_error("Error (%s): Variable %s in RHS but not in LHS.\n",
                           rule_name, atom->value.variable.name);
               abort_compilation = true;
            }
            /* Type checking */
            if(int_exp && current_var->type != INT_S) 
            {
               print_error("Error(%s): Variable %s occurs in an integer "
                           "expression but not declared as an integer.\n",
                           rule_name, atom->value.variable.name);
               abort_compilation = true;
            }
            if(string_exp && current_var->type != CHAR_S
                          && current_var->type != STRING_S )
            {
               print_error("Error(%s): Variable %s occurs in a string expression "
                           "but not declared as a string or character.\n",
                           rule_name, atom->value.variable.name);
               abort_compilation = true;
            }
         }
         /* We have found the variable in the rule with the appropriate
         * name. enterVariables ensures there is only one such variable
         * variable in the symbol list. There is no need to look further. */
         break;	
      }              
      var_list = var_list->next;
   } 
}
