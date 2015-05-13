#include "seman.h" 

GHashTable *symbol_table = NULL; 

bool analyseProgram(List *gp_program, bool debug, string program_name)
{
  /* Create a new GHashTable with strings as keys.
   * g_str_hash is glib's default string hashing function.
   * g_str_equal is glib's default function for comparing strings for hash
   * lookups.
   * free is the function called by glib to free keys during hash table
   * insertions and in the g_hash_table_destroy function.
   * The fourth argument is a function to free. I do this explicitly
   * with the function freeSymbolList defined in the symbol module. */
   symbol_table = g_hash_table_new_full(g_str_hash, g_str_equal, free, NULL); 

   bool abort = declarationScan(gp_program, "Main");
   if(abort) return false;
   abort = semanticCheck(gp_program, "Main");
   if(debug) 
   {
      printDotAST(gp_program, program_name, "_1");
      printSymbolTable(symbol_table, program_name);
   }
   if(symbol_table) 
   {
     g_hash_table_foreach(symbol_table, freeSymbolList, NULL);
     g_hash_table_destroy(symbol_table); 
   }
   return !abort;
}

bool declarationScan(List *ast, string scope)
{
   /* These variables are declared static so that their  are unchanged
    * on recursive calls. */
   static int main_count = 0;
   static bool name_clash = false;

   SymbolList *symbol_list = NULL;
   while(ast != NULL)
   {     
      switch(ast->declaration->decl_type) 
      {
         case MAIN_DECLARATION:
              main_count += 1;
              break; 	 
  
         case PROCEDURE_DECLARATION:
         {
              GPProcedure *procedure = ast->declaration->procedure;
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
  	      string old_rule_name = ast->declaration->rule->name;
              string rule_name = makeRuleIdentifier(old_rule_name, scope);
              ast->declaration->rule->name = rule_name;
              free(old_rule_name);
 
              symbol_list = g_hash_table_lookup(symbol_table, rule_name);      
	      SymbolList *iterator = symbol_list;
              bool add_rule = true;	      

              /* Report an error if two rules with the same name are declared
               * in the same scope. iterator points to the start of the symbol
               * list whose key is rule_name. */
              while(iterator != NULL)   
              {
                 if(iterator->type == RULE_S && !strcmp(iterator->scope, scope))
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
	         symbol_list = addSymbol(symbol_list, RULE_S, scope, NULL, false,
                                         false, false, false); 
                 /* If rule_name is not copied here, it would be freed twice: 
                  * once when the rule is freed, and again when the hash table
                  * keys are freed. */
                 string rule_name_copy = strdup(rule_name);
                 g_hash_table_replace(symbol_table, rule_name_copy, symbol_list);  
	      }
              break;
         }
         default: 
              print_to_log("Error: Unexpected node type %d at AST node %d\n\n", 
                           ast->declaration->decl_type, ast->declaration->id);
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

/* The return of semanticCheck. It is modified by many of the functions
 * called by semanticCheck, so I declare it as a static global variable. */
static bool abort_compilation = false; 

bool semanticCheck(List *declarations, string scope)
{
   while(declarations) 
   {
      GPDeclaration *current_declaration = declarations->declaration;
   
      switch(current_declaration->decl_type)
      {
         case MAIN_DECLARATION:
	      /* An empty main program does not comform to the grammar. The
	       * parser should catch it and report a syntax error, but there is
	       * no harm in checking here as well. */
              if(current_declaration->main_program)
		 commandScan(current_declaration->main_program,
                             scope, declarations, false);
	      else print_to_log("Error: Main procedure has no program. \n");
              break;

         case PROCEDURE_DECLARATION: 
	 {
              GPProcedure *procedure = current_declaration->procedure;
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
              ruleScan(current_declaration->rule, scope);
              break;  

         default: print_to_log("Error (semanticCheck): Unexpected declaration "
                               "type %d at AST node %d\n", 
                               current_declaration->decl_type,
                               current_declaration->id);
              break;
      } 
   declarations = declarations->next;  
   }
   /* The code following the if statement is only executed upon reaching the
    * end of the global declaration list. */
   if(!strcmp(scope, "Main"))
   {
      if(bidirectional_edges != NULL) freeBiEdgeList(bidirectional_edges);
   }
   return abort_compilation;
}   

void commandScan(GPCommand *command, string scope, List *declarations, bool in_loop)
{
   switch(command->command_type) 
   {
      case COMMAND_SEQUENCE: 
      {
           command->commands = reverse(command->commands);
           List *command_list = command->commands;

           while(command_list) 
           {
              commandScan(command_list->command, scope, declarations, in_loop);
              command_list = command_list->next;   
           }           
           break;
      }
      case RULE_CALL:
      {
           string name = command->rule_call.rule_name;
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
              command->rule_call.rule_name = strdup(rule->name);
              command->rule_call.rule = rule;
           }
           break;
      }
      case RULE_SET_CALL: 
      {
           command->rule_set = reverse(command->rule_set);
           List *rule_list = command->rule_set;
           while(rule_list)
           {
              string name = rule_list->rule_call.rule_name;
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
                 rule_list->rule_call.rule_name = strdup(rule->name);
                 rule_list->rule_call.rule = rule;
              }
              rule_list = rule_list->next;
           }           
           break;
      }
      case PROCEDURE_CALL:   
      {
           string name = command->proc_call.proc_name;
           GPProcedure *procedure = findProcedureDeclaration(declarations, name, NULL);
           if(procedure == NULL)
           {
              print_error("Error: Procedure %s called but not declared.\n", name);    
              abort_compilation = true;
           }
           else 
           {
              command->proc_call.procedure = procedure;
              commandScan(procedure->commands, name, declarations, in_loop);
           }
           break;
      }
      case IF_STATEMENT:
      case TRY_STATEMENT:
           commandScan(command->cond_branch.condition, scope, declarations, false);
           commandScan(command->cond_branch.then_command, scope, declarations, in_loop);
           commandScan(command->cond_branch.else_command, scope, declarations, in_loop);
           break;

      case ALAP_STATEMENT:
           commandScan(command->loop_stmt.loop_body, scope, declarations, true);
           break;

      case PROGRAM_OR:
           commandScan(command->or_stmt.left_command, scope, declarations, in_loop);
           commandScan(command->or_stmt.right_command, scope, declarations, in_loop);
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
                             "AST node %d\n", command->command_type, command->id);
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
         GPDeclaration *declaration = local_declarations->declaration;
         if(declaration->decl_type == RULE_DECLARATION)
         {
           /* The rule name in the declaration is of the form 
            * "<proc_name>_<rule_name>". Disregard "<proc_name>_" for the 
            * comparison. */
            int length = strlen(declaration->rule->name) -
                         strlen(procedure->name) + 1;
            if(!strcmp(declaration->rule->name + length, name))
               return declaration->rule;
         }
         if(declaration->decl_type == PROCEDURE_DECLARATION)
         {
            /* Search for the rule declaration in the local declaration list of
             * the procedure and any local procedures. */
            GPRule *rule = findRuleDeclaration(local_declarations, name,  
                                               declaration->procedure);
            if(rule != NULL) return rule;
         }                           
         local_declarations = local_declarations->next;
      }
   }
   while(global_declarations != NULL)
   {
      GPDeclaration *declaration = global_declarations->declaration;
      if(declaration->decl_type == RULE_DECLARATION)
      {
          /* The rule name in the declaration node is of the form
           * "Main_<rule_name>". Disregard "Main_" for the comparison. */
          if(!strcmp(declaration->rule->name + 5, name))
             return declaration->rule;
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
      GPDeclaration *declaration = declarations->declaration;
      if(declaration->decl_type == PROCEDURE_DECLARATION)
      {
         if(!strcmp(declaration->procedure->name, name) &&
            declaration->procedure != excluded_procedure)
            return declaration->procedure;
         /* Search for the procdure declaration in the local declaration list
          * of the procedure and any local procedures. */
         GPProcedure *procedure =
            findProcedureDeclaration(declaration->procedure->local_decls, name, NULL);
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
      variable_list->variables = reverse(variable_list->variables);	   
      switch(variable_list->list_type) 
      {
         case INT_DECLARATIONS:
              checkDeclaration(rule, variable_list->variables, scope,
                               INT_S, integer_count++, "integer");  
	      break;

         case CHAR_DECLARATIONS:
              checkDeclaration(rule, variable_list->variables, scope,
                               CHAR_S, character_count++, "character");  
	      break;

         case STRING_DECLARATIONS:
              checkDeclaration(rule, variable_list->variables, scope,
                               STRING_S, string_count++, "string");  
              break;
   	
         case ATOM_DECLARATIONS:
              checkDeclaration(rule, variable_list->variables, scope,
                               ATOM_S, atom_count++, "atom"); 
	      break; 

	 case LIST_DECLARATIONS:
              checkDeclaration(rule, variable_list->variables, scope,
                               LIST_S, list_count++, "list"); 
	      break;  	 

	 default:
	      print_to_log("Error: Unexpected list type %d in AST node %d\n",
		           variable_list->list_type, variable_list->id);
              abort_compilation = true;
	      break;
      }
      variable_list = variable_list->next;
   }
   graphScan(rule, rule->interface, scope, rule->name, 'l');
   graphScan(rule, rule->interface, scope, rule->name, 'r');
   if(rule->interface) interfaceScan(rule->interface, scope, rule->name);
   if(rule->condition) conditionScan(rule->condition, rule->interface, 
                                     scope, rule->name);
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

int enterVariables(SymbolType type, List *variables, string scope, string rule_name)
{
   int variable_count = 0;
   while(variables != NULL) 
   {
      variable_count++;
      string variable_name = strdup(variables->variable_name);	   
      SymbolList *symbol_list = g_hash_table_lookup(symbol_table, variable_name);
      SymbolList *iterator = symbol_list;

      bool add_variable = true;
      while(iterator) 
      {
	 /* Print an error if there already exists a variable in the same rule
	  * and scope with the same name. */
         if(iterator->is_var && symbolInScope(iterator, scope, rule_name))
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
         symbol_list = addSymbol(symbol_list, type, scope, rule_name, true,
                                 false, false, false);
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

void graphScan(GPRule *rule, List *interface, string scope, string rule_name, char side)
{
   GPGraph *graph = NULL;
   if(side == 'l') graph = rule->lhs;
   else if(side == 'r') graph = rule->rhs;
   /* Variables to store the symbol types and the graph for semantic checking
    * and error reporting. */
   SymbolType node_type, edge_type;
   string graph_type = NULL;
   SymbolList *symbol_list = NULL;
 
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
      string node_id = strdup(node_list->node->name);
      symbol_list = g_hash_table_lookup(symbol_table, node_id);
      SymbolList *symbol = symbol_list;

      bool add_node = true;
      while(symbol != NULL) 
      {
	 /* Print an error if there already exists a node in the same graph, 
	  * rule and scope with the same name. */
         if(symbol->type == node_type && symbolInScope(symbol, scope, rule_name))
	 {
	     print_to_log("Warning (%s): Node ID %s not unique in the "
                          "%s.\n", rule_name, node_id, graph_type);  
	     add_node = false;
	     break;
	 }
         symbol = symbol->next;      
      }
      if(node_list->node->label->mark == DASHED)
      {
          print_to_log("Error (%s): Node %s in %s graph has invalid mark " 
	               "\"dashed\".\n", rule_name, node_id, graph_type);
          abort_compilation = true; 
      }
      bool wildcard = node_list->node->label->mark == ANY;
      if(add_node) 
      { 
         symbol_list = addSymbol(symbol_list, node_type, scope, rule_name,
                                 false, false, wildcard, false);
         g_hash_table_replace(symbol_table, node_id, symbol_list);         
      }      
      /* Check that RHS wildcard nodes exist in the interface and that they
       * have a corresponding LHS wildcard. */
      if(wildcard && side == 'r')
      {
         bool in_interface = false;
         List *iterator = interface;
         while(iterator != NULL)  
         {
            if(!strcmp(node_id, iterator->node_id))
            {
               in_interface = true;
               break;
            }
            iterator = iterator->next;
         }
         if(!in_interface) 
         {
            print_error("Error (%s): Wildcard node %s in %s graph not in the "
                        "interface.\n", rule_name, node_id, graph_type);
            abort_compilation = true;  
         }
         else
         {
            symbol = symbol_list;
            while(symbol != NULL) 
            {
               /* Find a node in the same rule. If that node is in the LHS
                * and is not a wildcard then report an error. */
               if(symbolInScope(symbol, scope, rule_name))	
               {
                  if(symbol->type == LEFT_NODE_S && !(symbol->wildcard)) 
                  {
                     print_to_log("Error (%s): RHS wildcard node %s has no "
                                  "matching LHS wildcard.", rule_name, node_id);  
                     abort_compilation = true; 
                  }
                  /* Regardless of the outcome of the inner if statement, exit
                   * the loop as the single appropriate node has been located. */
                  break;
               }
               symbol = symbol->next;
            }
         }
      }
      gpListScan(&(node_list->node->label->gp_list), interface, scope, rule_name, side);
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
      string edge_id = strdup(edge_list->edge->name);
      string source_id = edge_list->edge->source;
      string target_id = edge_list->edge->target;

      symbol_list = g_hash_table_lookup(symbol_table, edge_id);
      SymbolList *symbol = symbol_list;
      while(symbol != NULL) 
      {
	 /* Print an error if there already exists an edge in the same graph,
	  * rule and scope with the same name. */
         if(symbol->type == edge_type && symbolInScope(symbol, scope, rule_name))
         {
	     print_to_log("Warning (%s): Edge ID %s not unique in the %s "
                          "graph.\n", rule_name, edge_id, graph_type);
	     add_edge = false;
	     break;
	 }
         symbol = symbol->next; 
      }
      bool wildcard = edge_list->edge->label->mark == ANY; 
      bool bidirectional = edge_list->edge->bidirectional; 
      if(add_edge) 
      {
         symbol_list = addSymbol(symbol_list, edge_type, scope, rule_name, 
                                 false, false, wildcard, bidirectional);
         g_hash_table_replace(symbol_table, edge_id, symbol_list);
      }
      /* Check that RHS wildcard edges exist in the interface and that they
       * have a corresponding LHS wildcard.
       * TODO: No interface checking yet. This would allow multiple RHS wildcard
       * edges and fewer LHS wildcard edges. I think. */
      if(wildcard && side == 'r')
      {
         symbol = symbol_list;
         while(symbol) 
         {
            /* Find an edge in the same rule. If that edge is in the LHS
             * and is not a wildcard then report an error. */
            if(symbolInScope(symbol, scope, rule_name))	
            {
               if(symbol->type == LEFT_EDGE_S && !(symbol->wildcard)) 
               {
                  print_to_log("Error (%s): RHS wildcard edge %s has no "
                               "matching LHS wildcard.", rule_name, edge_id);  
                  abort_compilation = true; 
               }
               /* Regardless of the outcome of the inner if statement, exit
                  * the loop as the single appropriate node has been located. */
               break;
            }
            symbol = symbol->next;
         }
      }
      /* Two semantic checks are made for bidirectional edges (BEs):
       * (1) A BE in the RHS must have a corresponding BE in the LHS.
       * (2) At most one BE is allowed between a pair of nodes. 
       *
       * Items of type struct BiEdgeList, defined in the header, are placed
       * in the global BiEdgeList *bidirectional_edges to keep track of all
       * the bidirectional edges encountered. */
      if(bidirectional) 
      {
         /* bidirectional_edges may be written to later, hence a copy is made
          * of the root pointer. */
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
               !strcmp(list_edge.rule_name, rule_name) &&
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
                         "matching LHS bidirectional edge.\n", rule_name, edge_id);
            abort_compilation = true; 
         }
         /* Only add an edge to the BE list if it is not a parallel edge. */
         if(add_bi_edge) 
            bidirectional_edges = addBiEdge(bidirectional_edges, scope, rule_name,
                                            side, source_id, target_id);
      }
      /* Verify source node exists in the graph. */
      symbol_list = g_hash_table_lookup(symbol_table, source_id);
      if(symbol_list == NULL) 
      {
	 print_to_log("Error (%s): Source node %s of edge %s does not "
                      "exist in %s graph.\n", rule_name, source_id, edge_id, 
                      graph_type);     
         abort_compilation = true; 
      }
      else
      {
         /* The while loop condition is true if the current symbol is not
          * the node in the same rule and graph as the current edge.
          * Hence the loop breaks if the appropriate node is found.
          * At each pass a check is made to see if the end of the list
          * has been reached. If not, check the next symbol in the symbol
          * list. Otherwise, print an error and break. */
         while(symbol_list->type != node_type    ||
               strcmp(symbol_list->scope, scope)  ||
               strcmp(symbol_list->rule_name, rule_name))
         {   
            if(symbol_list->next == NULL) 
            {
               print_to_log("Error (%s): Source node %s of edge %s does "
                            "not exist in %s graph.\n", 
                            rule_name, source_id, edge_id, graph_type);     
               abort_compilation = true; 
               break;
            }
            else symbol_list = symbol_list->next;             
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
         /* The while loop condition is true if the current symbol is not
          * the node in the same rule and graph as the current edge.
          * Hence the loop breaks if the appropriate node is found.
          * At each pass a check is made to see if the end of the list
          * has been reached. If not, check the next symbol in the symbol
          * list. Otherwise, print an error and break. */               
         while(symbol_list->type != node_type    ||
               strcmp(symbol_list->scope, scope) ||
               strcmp(symbol_list->rule_name, rule_name))
         {
            if(symbol_list->next == NULL) 
            {
               print_to_log("Error (%s): Target node %s of edge %s does "
                            "not exist in %s graph.\n", 
                            rule_name, target_id, edge_id, graph_type);   
               abort_compilation = true; 
               break;
            }
            else symbol_list = symbol_list->next;             
         }
      }	 
      gpListScan(&(edge_list->edge->label->gp_list), interface, scope, rule_name, side);
      edge_list = edge_list->next;
      if(!add_edge && edge_id) free(edge_id);
   }
}

void interfaceScan(List *interface, string scope, string rule_name)
{
   /* Linked list to store node IDs encountered to check for uniqueness */
   GSList *interface_ids = NULL, *iterator = NULL;
   bool in_lhs, in_rhs;

   while(interface != NULL)
   {
      in_lhs = false, in_rhs = false;
      /* g_slist_insert_sorted inserts elements and maintains lexicographic
       * order of its elements with use of the function strcmp. This makes
       * checking for duplicate nodes easier. */
      interface_ids = g_slist_insert_sorted(interface_ids, interface->node_id,
                                            (GCompareFunc)strcmp);
      SymbolList *node_symbol = g_hash_table_lookup(symbol_table, interface->node_id);     
      while(node_symbol) 
      {
         if(symbolInScope(node_symbol, scope, rule_name))
         {
            if(node_symbol->type == LEFT_NODE_S) in_lhs = true;
            if(node_symbol->type == RIGHT_NODE_S) in_rhs = true;
         }
         /* If both the LHS node and RHS node have been found, no need to look
          * further down the symbol list. */         
         if(in_lhs && in_rhs) break;
         node_symbol = node_symbol->next;
      }
      if(!in_lhs)  
      {
         print_to_log("Error (%s): Interface node %s not in the LHS "
                      "graph.\n", rule_name, interface->node_id);
         abort_compilation = true; 
      }
      if(!in_rhs) 
      {
         print_to_log("Error (%s): Interface node %s not in the RHS "
                      "graph.\n", rule_name, interface->node_id);
         abort_compilation = true; 
      }
      interface = interface->next;   
   }
   /* Since interface_ids is sorted, each element in the list only needs to be 
    * compared to its successor. */
   for(iterator = interface_ids; iterator->next; iterator = iterator->next)
   {
      if(!strcmp(iterator->data, iterator->next->data))
         print_to_log("Warning (%s): Node %s occurs twice in interface list.\n",
                      rule_name, (char*)(iterator->data));
   }
   g_slist_free(interface_ids); 
}
        

void conditionScan(GPCondition *condition, List *interface, string scope, 
                   string rule_name)
{
   switch(condition->type) 
   {
      case INT_CHECK:
      case CHAR_CHECK:
      case STRING_CHECK:
      case ATOM_CHECK: 
      {
           bool in_rule = false; 
           SymbolList *var_list = g_hash_table_lookup(symbol_table, condition->var);
	   /* Go through the list of symbols with the name in question
            * to check if any variables exist in this rule. */
           while(var_list != NULL) 
           {
              if(var_list->is_var && symbolInScope(var_list, scope, rule_name))
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
                          "declared.\n", rule_name, condition->var);
           break;
      }
      /* For an edge predicate, the source and target node IDs must be present
       * in the interface of the rule. The optional label argument is also scanned. */
      case EDGE_PRED:
      {
           bool in_interface = false;
           List *iterator = interface;
           while(iterator != NULL)  
           {
              if(!strcmp(condition->edge_pred.source, iterator->node_id))
              {
                 in_interface = true;
                 break;
              }
              iterator = iterator->next;
           }
           if(!in_interface) 
           {
              print_error("Error (%s): Node %s in edge predicate not in the "
                          "interface.\n", rule_name, condition->edge_pred.source);
              abort_compilation = true;  
           }
           in_interface = false;

           iterator = interface;
           while(iterator != NULL)  
           {
              if(!strcmp(condition->edge_pred.target, iterator->node_id))
              {
                 in_interface = true;
                 break;
              }
              iterator = iterator->next;
           }
           if(!in_interface) 
           {
              print_error("Error (%s): Node %s in edge predicate not in the "
                          "interface.\n", rule_name, condition->edge_pred.target);
              abort_compilation = true;  
           }
	   /* Scan the label argument if it exists. */
           if(condition->edge_pred.label)
              gpListScan(&(condition->edge_pred.label->gp_list), interface, 
                         scope, rule_name, 'c');
           break;
      }

      case EQUAL:
      case NOT_EQUAL:
           gpListScan(&(condition->list_cmp.left_list), interface, scope, rule_name, 'c');
           gpListScan(&(condition->list_cmp.right_list), interface, scope, rule_name, 'c');
           break;

      case GREATER:
      case GREATER_EQUAL:
      case LESS:
      case LESS_EQUAL:
           atomScan(condition->atom_cmp.left_exp, interface, scope, rule_name, 'c', true, false);
           atomScan(condition->atom_cmp.right_exp, interface, scope, rule_name, 'c', true, false);
           break;

      case BOOL_NOT:
           conditionScan(condition->not_exp, interface, scope, rule_name);
           break;

      case BOOL_OR:
      case BOOL_AND:
           conditionScan(condition->bin_exp.left_exp, interface, scope, rule_name);
           conditionScan(condition->bin_exp.right_exp, interface, scope, rule_name);
           break;

      default:
	   print_to_log("Error: Unexpected condition type %d at AST node %d\n",
	                condition->type, condition->id);
           abort_compilation = true; 
           break;
      }
}

/* Variables used by gpListScan and atomScan. */
static int list_var_count = 0;
static int string_var_count = 0;
static bool lhs_not_simple = false;

void gpListScan(List **gp_list, List *interface, string scope, string rule_name, 
                char location)
{
   *gp_list = reverse(*gp_list);

   List *iterator = *gp_list;
   while(iterator) 
   {
      atomScan(iterator->atom, interface, scope, rule_name, location, false, false);
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
void atomScan(GPAtom *atom, List *interface, string scope, string rule_name, 
              char location, bool int_exp, bool string_exp)
{
   switch(atom->type) 
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
           variableScan(atom, scope, rule_name, location, int_exp, string_exp, false);
           break;

       case LENGTH:
            if(string_exp) 
            {
               print_error("Error (%s): Length operator appears in string "
                           "expression.\n", rule_name);
               abort_compilation = true;
            }
            if(location == 'l') lhs_not_simple = true;
            variableScan(atom, scope, rule_name, location, int_exp, string_exp, true);
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
           /* If the degree operator is in a condition or a RHS-label, its argument 
            * must exist in the interface. */
           else /* location == 'c' || location == 'r' */
           {
              bool in_interface = false;
              List *iterator = interface;
              while(iterator != NULL)  
              {
                 if(!strcmp(atom->node_id, iterator->node_id))
                 {
                    in_interface = true;
                    break;
                 }
                 iterator = iterator->next;
              }
              if(!in_interface) 
              {
                 print_error("Error (%s): Node %s in degree operator not in the "
                             "interface.\n", rule_name, atom->node_id);
                 abort_compilation = true;  
              }
           }
           break;
       }
       case NEG:
            if(string_exp) 
            {
               print_error("Error (%s): Arithmetic operator appears in string "
                           "expression.\n", rule_name);
               abort_compilation = true;
            }
            atomScan(atom->neg_exp, interface, scope, rule_name, location, true, false);
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
            atomScan(atom->bin_op.left_exp, interface, scope, rule_name, 
                     location, true, false);
            atomScan(atom->bin_op.right_exp, interface, scope, rule_name,
                     location, true, false);       
            break;

       case CONCAT:     
            if(int_exp) 
            {
               print_error("Error (%s): String operator appears in integer "
                           "expression.\n", rule_name);
               abort_compilation = true;
            }
            atomScan(atom->bin_op.left_exp, interface, scope, rule_name,
                     location, false, true);
            atomScan(atom->bin_op.right_exp, interface, scope, rule_name, 
                     location, false, true); 
	    if(string_var_count > 1) 
            {
	       print_error("Error (%s): More than one string variable in LHS "
		           "string expression.\n", rule_name);
	       string_var_count = 0;
            }
            break;

       default:
            print_to_log("Error: Unexpected atom type %d at AST node %d.\n",
		         atom->type, atom->id);
            abort_compilation = true;
            break;                
    }
}               

void variableScan(GPAtom *atom, string scope, string rule_name, 
                  char location, bool int_exp, bool string_exp, bool length)
{
   /* var_list is pointed to the symbol_list of symbols with the same
    * identifier as the variable. */
   SymbolList *var_list = g_hash_table_lookup(symbol_table, atom->variable.name);      
   bool in_rule = false;
   while(var_list)
   {
      /* Locate the variable with the appropriate scope and rule name. If it 
       * exists, there is only one, as duplicates are not entered into the 
       * symbol table. */
      if(var_list->is_var && symbolInScope(var_list, scope, rule_name)) in_rule = true;
      else 
      {
         var_list = var_list->next;
         continue;
      }
      switch(var_list->type)
      {
         case INT_S:
              atom->variable.type = INTEGER_VAR;
              if(location == 'l') var_list->in_lhs = true;
              if(length) 
              {
                 print_error("Error (%s): Integer variable %s in length "
                             "operator.\n", rule_name, atom->variable.name);
                 abort_compilation = true;
              }
              break;

         case CHAR_S:
              atom->variable.type = CHARACTER_VAR;
              if(location == 'l') var_list->in_lhs = true;
              if(length) 
              {
                 print_error("Error (%s): Character variable %s in length "
                             "operator.\n", rule_name, atom->variable.name);
                 abort_compilation = true;
              }
              break;
         
         case STRING_S:
              atom->variable.type = STRING_VAR;
              if(location == 'l') 
              {
                 var_list->in_lhs = true;
                 string_var_count++;
              }
              break;

         case ATOM_S:
              atom->variable.type = ATOM_VAR;
              if(location == 'l') 
              {
                 var_list->in_lhs = true;
                 string_var_count++;
              }
              break;

         case LIST_S:
              atom->variable.type = LIST_VAR;
              if(location == 'l') 
              {
                 var_list->in_lhs = true;
                 list_var_count++;
              }
              break;

         default:
              print_to_log("Error (variableCheck): Unexpected symbol type "
                            "%d.\n", var_list->type);
              break;
      }
      /* Check if a RHS variable exists in the LHS */
      if(location == 'r' && !(var_list->in_lhs))
      {
         print_error("Error (%s): Variable %s in RHS but not in LHS.\n",
                     rule_name, atom->variable.name);
         abort_compilation = true;
      }
      /* Type checking */
      if(int_exp && var_list->type != INT_S && !length) 
      {
         print_error("Error (%s): Variable %s occurs in an integer "
                     "expression but not declared as an integer.\n",
                     rule_name, atom->variable.name);
         abort_compilation = true;
      }
      if(string_exp && var_list->type != CHAR_S && var_list->type != STRING_S)
      {
         print_error("Error (%s): Variable %s occurs in a string expression "
                     "but not declared as a string or character.\n",
                     rule_name, atom->variable.name);
         abort_compilation = true;
      }
      /* We have found the variable in the rule with the appropriate
       * name. enterVariables ensures there is only one such variable
       * variable in the symbol list. There is no need to look further. */
      if(in_rule) break;	
      var_list = var_list->next;
   }

   if(!in_rule) 
   {
      print_error("Error (%s): Variable %s in expression but not declared.\n",
                  rule_name, atom->variable.name);
      abort_compilation = true;
   } 
}
