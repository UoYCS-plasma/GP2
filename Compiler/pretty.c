#include "pretty.h" 

/* printSymbolTable uses g_hash_table_foreach to print the symbol table.
 * g_hash_table_for_each iterates over every key and in the
 * table, calling the function passed to it (printSymbolList) on
 * each  */
static FILE *symbol_table_file;

void printSymbolTable(GHashTable *table, string program_name) 
{
   int length = strlen(program_name) + 4;
   char file_name [length];
   strcpy(file_name, program_name);
   strncat(file_name, ".tab", 4);

   symbol_table_file = fopen(file_name, "w");
   if(symbol_table_file == NULL) {
      perror("symbols.tab");
      exit(1);
   }
   print_to_symtab_file("Symbol Table\n\n");	
   g_hash_table_foreach(table, printSymbolList, NULL);
   fclose(symbol_table_file);
}

void printSymbolList(gpointer key, gpointer value, gpointer user_data)
{
    SymbolList *symbol = NULL;
    /* The for loop iterates over  which is a GSList of Symbols. 
     * Typecasting is required to access the Symbol structs because a GSList's
     * data field is a gpointer, equivalent to a void pointer. */
    for(symbol = (SymbolList*)value; symbol != NULL; symbol = symbol->next)
    {
	/* Not all symbols have a containing rule */    
	if(symbol->rule_name == NULL)
        {
	   print_to_symtab_file("Name: %s\nType: %d\nScope: %s\n",
	                       (string)key, symbol->type, symbol->scope);
	   if(symbol->is_var) print_to_symtab_file("Variable\n");
	   if(symbol->in_lhs) print_to_symtab_file("In LHS\n");
           if(symbol->wildcard) print_to_symtab_file("Wildcard\n");
           if(symbol->bidirectional) print_to_symtab_file("Bidirectional\n");
	   print_to_symtab_file("\n");
	}	
	else 
        {	
           print_to_symtab_file("Name: %s\nType: %d\nScope: %s\nContaining "
                                "Rule: %s\n", (string)key, symbol->type, 
                                symbol->scope, symbol->rule_name);
       	   if(symbol->is_var) print_to_symtab_file("Variable\n");
	   if(symbol->in_lhs) print_to_symtab_file("In LHS\n");
           if(symbol->wildcard) print_to_symtab_file("Wildcard\n");
           if(symbol->bidirectional) print_to_symtab_file("Bidirectional\n");
	   print_to_symtab_file("\n");
	}
    }
}
       
/* Dot syntax for nodes:
 * <id>[shape=<node_shape>,label=<node_label>]
 *
 * Nodes generated from struct List have shape box. Nodes representing
 * NULL to mark the end of AST lists has shape plaintext (no border). 
 * The node pointing to the first Global Declaration node of the AST
 * and error nodes are also plaintext with the labels ROOT and ERROR
 * respectively. All other nodes have the default shape (ellipse). 
 * Nodes are labelled with their AST node type and any attributes.
 *
 * Dot syntax for edges:
 * <source_id> -> <target_id>[label=<edge_label>]
 *
 * Edges are labelled with the name of the corresponding pointer in the
 * struct definition. For example, a List node will have outgoing
 * edges labelled ' and 'next'. */ 

static unsigned int next_id = 1;

void printDotAST(List *const gp_ast, string file_name, string suffix)
{
     int length = strlen(file_name) + strlen(suffix) + 4; 
     char dot_file_name[length];
     strcpy(dot_file_name, file_name);
     strncat(dot_file_name, suffix, strlen(suffix));
     strncat(dot_file_name, ".dot", 4);
     FILE *dot_file = fopen(dot_file_name, "w");
     if(dot_file == NULL) {
	perror(dot_file_name);
	exit(1);
     }	
     print_to_dot_file("digraph g { \n");
     /* Print the entry point of the AST. node1 is the first 
      * node created by printList. */
     print_to_dot_file("node0[shape=plaintext,label=\"ROOT\"]\n");
     print_to_dot_file("node0->node1\n");

     next_id = 1;   
     printASTList(gp_ast,dot_file);
     print_to_dot_file("}\n\n");
     fclose(dot_file);
}

void printDotHostGraph(GPGraph *const host_graph_ast, string file_name)
{
     int dot_length = strlen(file_name) + 5; 
     char dot_file_name[dot_length];
     strcpy(dot_file_name, file_name);
     strncat(dot_file_name, ".dot", 4);
     FILE *dot_file = fopen(dot_file_name, "w");
     
     if(dot_file == NULL) {
	perror(dot_file_name);
	exit(1);
     }	
     print_to_dot_file("digraph g { \n");

     /* Print the entry point of the AST. node1 will be the first 
      * node created by printGraph. */
     print_to_dot_file("node0[shape=plaintext,label=\"ROOT\"]\n");
     print_to_dot_file("node0->node1\n");

     next_id = 1;
     printASTGraph(host_graph_ast,dot_file);
     print_to_dot_file("}\n\n");
     fclose(dot_file);
}

/* printList is a recursive function which prints the nodes and edges
 * of its AST argument to the .dot file created by printDotAST.
 *
 * Unique node names are generated with the global variable next_id. 
 * A new AST node is reached whenever a printing function is called through the 
 * prettyPrint macros. Hence the first operation of each printing function is 
 * the assignment of next_id to the id of the AST node being examined.
 * next_id is then incremented in preparation for printing a new node in
 * the .dot file.
 *
 * These functions make frequent use of the macros prettyPrint, prettyPrintList
 * LOCATION_ARGS, defined in the header file. Function calls with odd arguments, 
 * such as printListNode, are function macros written to reduce some of the 
 * clutter.
 */

void printASTList(List *const list, FILE *dot_file)
{
   switch(list->list_type)
   {
     case GLOBAL_DECLARATIONS:
          list->id = next_id;
          next_id += 1;
          printListNode(Global \\n Declarations);
          prettyPrint(list->declaration, Declaration);
          prettyPrintList(list->next,list, next);
          break;	

     case LOCAL_DECLARATIONS:
          list->id = next_id;
          next_id += 1;
          printListNode(Local \\n Declarations);
	  prettyPrint(list->declaration, Declaration);
          prettyPrintList(list->next,list,next);
          break;	

     case COMMANDS:
          list->id = next_id;
          next_id += 1;
          printListNode(Commands);
          prettyPrint(list->command, Command);
          prettyPrintList(list->next,list,next);
          break;	

     case RULES:
          list->id = next_id;
          next_id += 1;

          if(list->rule_call.rule_name != NULL)
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Rule Call \\n Name: %s\"]\n", list->id, list->id,
                               LOCATION_ARGS(list->location), list->rule_call.rule_name);
          else 
          {
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Rule \\n Name: UNDEFINED\"]\n", list->id, list->id, 
                               LOCATION_ARGS(list->location));
             print_to_log("Error: Undefined rule name at AST node %d", 
                          list->id);
          }
          prettyPrint(list->rule_call.rule, Rule);
          if(list->rule_call.rule != NULL)
             print_to_dot_file("node%d->node%d[label=\"rule\"]\n", 
                               list->id, list->rule_call.rule->id);
          prettyPrintList(list->next, list, next);
          break;
     
     case INT_DECLARATIONS:
          list->id = next_id;
          next_id += 1;
          printListNode(Integer \\n Declarations);
          prettyPrint(list->variables, List);
          prettyPrintList(list->next,list,next);
          break;

     case CHAR_DECLARATIONS:
          list->id = next_id;
          next_id += 1;
          printListNode(Character \\n Declarations);
          prettyPrint(list->variables, List);
          prettyPrintList(list->next,list,next);

          break;

     case STRING_DECLARATIONS:
          list->id = next_id;
          next_id += 1;
          printListNode(String \\n Declarations);
          prettyPrint(list->variables, List);
          prettyPrintList(list->next,list,next);
           
          break;
     
     case ATOM_DECLARATIONS:
          list->id = next_id;
          next_id += 1;
          printListNode(Atom \\n Declarations);
          prettyPrint(list->variables, List);
          prettyPrintList(list->next,list,next);

          break;
     
     case LIST_DECLARATIONS:
          list->id = next_id;
          next_id += 1;
          printListNode(List \\n Declarations);
          prettyPrint(list->variables, List);
          prettyPrintList(list->next,list,next);

          break;
     
     case VARIABLE_LIST:
          list->id = next_id;
          next_id += 1;

          if(list->variable_name != NULL)
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Variable \\n Name: %s\"]\n", list->id, list->id, 
                               LOCATION_ARGS(list->location), list->variable_name);
          else 
          {
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Variable \\n Name: UNDEFINED\"]\n",
                               list->id, list->id, 
                               LOCATION_ARGS(list->location)); 
             print_to_log("Error: Undefined variable name at AST node %d", 
                           list->id);
          }
          prettyPrintList(list->next,list,next);
          break;
     
     case INTERFACE_LIST:
          list->id = next_id;
          next_id += 1; 
          if(list->node_id != NULL)
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Interface \\n Node: %s\"]\n", list->id, list->id, 
                               LOCATION_ARGS(list->location), list->node_id);
          else 
          {
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Node \\n Name: UNDEFINED\"]\n", list->id, list->id, 
                               LOCATION_ARGS(list->location));
             print_to_log("Error (printASTList): Undefined node name at "
                           "AST node %d", list->id);
          }
          prettyPrintList(list->next,list,next);
          break;
     
     case NODE_LIST:
          list->id = next_id;
          next_id += 1; 
          printListNode(Nodes);
          prettyPrint(list->node, Node);
          prettyPrintList(list->next,list,next);
          break;
     
     case EDGE_LIST:
          list->id = next_id;
          next_id += 1; 
          printListNode(Edges);
          prettyPrint(list->edge, Edge);
          prettyPrintList(list->next,list,next);
          break;

     case GP_LIST:
          list->id = next_id;
          next_id += 1; 
          printListNode(GP List);
          prettyPrint(list->atom, Atom);
          prettyPrintList(list->next,list,next);
          break;

     default: print_to_log("Error (printASTList): Unexpected type: %d\n",
                           (int)list->list_type); 
              break;	 
     }
}


void printASTDeclaration(GPDeclaration * const decl, FILE *dot_file)
{
   switch(decl->decl_type) 
   {
      case MAIN_DECLARATION:
           decl->id = next_id;
           next_id += 1;
           printDeclarationNode(Main, main \\n program, next_id);
           prettyPrint(decl->main_program, Command);
           break;

      case PROCEDURE_DECLARATION:
           decl->id = next_id;
           next_id += 1;
           printDeclarationNode(Procedure \\n Declaration, proc, next_id);
           prettyPrint(decl->procedure, Procedure);
           break;

      case RULE_DECLARATION:
           decl->id = next_id;
           next_id += 1;
           prettyPrint(decl->rule, Rule);
           printDeclarationNode(Rule \\n Declaration, rule, decl->rule->id);
           break;

      default: print_to_log("Error (printASTDeclaration): Unexpected type: "
                            "%d\n", (int)decl->decl_type);
               break;
   }
}


void printASTCommand(GPCommand *const command, FILE *dot_file)
{
  switch(command->command_type)
  {
      case COMMAND_SEQUENCE:	
           command->id = next_id;
           next_id += 1;

           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                             "Command \\n Sequence\"]\n", 
                             command->id, command->id, 
                             LOCATION_ARGS(command->location));

           print_to_dot_file("node%d->node%d[label=\"commands\"]\n",  
                             command->id, next_id); 

           prettyPrint(command->commands, List);
           break;

      case RULE_CALL:
           command->id = next_id;
           next_id += 1;

           if(command->rule_call.rule_name != NULL)
              print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                                "Rule Call \\n Name: %s\"]\n", command->id,
                                command->id, LOCATION_ARGS(command->location), 
                                command->rule_call.rule_name);
           else 
           {
               print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                                 "Rule \\n Name: UNDEFINED\"]\n", command->id,
                                 command->id, LOCATION_ARGS(command->location));
               print_to_log("Error (printASTCommand): Undefined rule name "
                            "at AST node %d", command->id);
           }
           prettyPrint(command->rule_call.rule, Rule);
           if(command->rule_call.rule)
              print_to_dot_file("node%d->node%d[label=\"rule\"]\n",
                                command->id, command->rule_call.rule->id);
           break;

      case RULE_SET_CALL:
           command->id = next_id;
           next_id += 1;

           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                             "Rule Set Call\"]\n", command->id, command->id,
                             LOCATION_ARGS(command->location));
           print_to_dot_file("node%d->node%d[label=\"rule set\"]\n",  
                             command->id, next_id); 
           prettyPrint(command->rule_set, List);
           break;

      case PROCEDURE_CALL:
           command->id = next_id;
           next_id += 1;

           if(command->proc_call.proc_name != NULL)
              print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                                "Procedure Call \\n Name: %s\"]\n",
                                command->id, command->id,
                                LOCATION_ARGS(command->location), 
                                command->proc_call.proc_name);
           else 
           {
               print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                                 "Procedure \\n Name: UNDEFINED\"]\n",
                                 command->id, command->id,
                                 LOCATION_ARGS(command->location));
               print_to_log("Error (printASTCommand): Undefined procedure "
                           "ame at AST node %d", command->id);
           }
           if(command->proc_call.procedure != NULL)
              print_to_dot_file("node%d->node%d[label=\"procedure\"]\n",
                                command->id, command->proc_call.procedure->id);
           break;

      case IF_STATEMENT:
           command->id = next_id;
           next_id += 1;
           printConditionalNode(If Statement);
           break;

      case TRY_STATEMENT:
           command->id = next_id;
           next_id += 1;
           printConditionalNode(Try Statement);
           break;

      case ALAP_STATEMENT:
           command->id = next_id;
           next_id += 1;

           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                             "ALAP Statement\\n Restore Point = %d\\n"
                             "Roll Back = %d\\n Stop Recording = %d\"]\n", 
                             command->id, command->id, 
                             LOCATION_ARGS(command->location),
                             command->loop_stmt.restore_point,
                             command->loop_stmt.roll_back,
                             command->loop_stmt.stop_recording);

           print_to_dot_file("node%d->node%d[label=\"loop \\n body\"]\n",  
                             command->id, next_id); 

           prettyPrint(command->loop_stmt.loop_body, Command);
           break;

      case PROGRAM_OR:
           command->id = next_id;
           next_id += 1;

           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                             "OR Statement\"]\n", command->id, command->id,
                             LOCATION_ARGS(command->location));

           print_to_dot_file("node%d->node%d[label=\"left \\n statement\"]\n",  
                             command->id, next_id);  
           prettyPrint(command->or_stmt.left_command, Command);             

           print_to_dot_file("node%d->node%d[label=\"right \\n statement\"]\n",  
                             command->id, next_id);  
           prettyPrint(command->or_stmt.right_command, Command);
           break;

      case SKIP_STATEMENT:
           command->id = next_id;
           next_id += 1;
           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n skip\"]\n", 
                             command->id, command->id,
                             LOCATION_ARGS(command->location));
           break;

      case FAIL_STATEMENT:
           command->id = next_id;
           next_id += 1;
           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n fail\"]\n", 
                             command->id, command->id, 
                             LOCATION_ARGS(command->location));
           break;
      
      case BREAK_STATEMENT:
           command->id = next_id;
           next_id += 1;
           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n break\"]\n", 
                             command->id, command->id, 
                             LOCATION_ARGS(command->location));
           break;
      
      default: print_to_log("Error (printASTCommand): Unexpected type: "
                           "%d\n", (int)command->command_type); 
               break;
      }
}


void printASTCondition(GPCondition *const cond, FILE *dot_file)
{
   switch(cond->type) 
   {
      case INT_CHECK:
           cond->id = next_id;
           next_id += 1;
           printTypeCheckNode(int check, INT_CHECK);
           break;

      case CHAR_CHECK:
           cond->id = next_id;
           next_id += 1;
           printTypeCheckNode(char check, CHAR_CHECK);
           break;

      case STRING_CHECK:
           cond->id = next_id;
           next_id += 1;
           printTypeCheckNode(string check, STRING_CHECK);
           break;

      case ATOM_CHECK:
           cond->id = next_id;
           next_id += 1;
           printTypeCheckNode(atom check, ATOM_CHECK);
           break;

      case EDGE_PRED:
           cond->id = next_id;
           next_id += 1;
           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\nEdge Test\\n",
                             cond->id, cond->id, LOCATION_ARGS(cond->location));

           if(cond->edge_pred.source != NULL)
              print_to_dot_file("Source: %s \\n ", cond->edge_pred.source);
           else 
           {
              print_to_log("Error (printASTCondition): Undefined node at "
                           "AST node %d", cond->id);
              print_to_dot_file("Source: ERROR \\n ");
           }

           if(cond->edge_pred.target != NULL)
               print_to_dot_file("Target: %s\"]\n ", cond->edge_pred.target);
           else 
           {
              print_to_log("Error (printASTCondition): Undefined node at "
                           "AST node %d", cond->id);
              print_to_dot_file("Target: ERROR \"]\n");
           }

           if(cond->edge_pred.label) 
           {
              print_to_dot_file("node%d->node%d[label=\"label \\n argument\"]\n",  
                                cond->id, next_id);
              prettyPrint(cond->edge_pred.label, Label);
           }
           else
           {
              print_to_dot_file("node%d[shape=plaintext,label=\"%d NULL\"]\n", 
                                next_id, next_id);  
              print_to_dot_file("node%d->node%d[label=\"label \\n argument\"]"          
                                "\n", cond->id, next_id);                     
              next_id += 1;       
           }
           break;

      case EQUAL:
           cond->id = next_id;
           next_id += 1;
           printListEqualityNode(=);
           break;	

      case NOT_EQUAL:
           cond->id = next_id;
           next_id += 1;
           printListEqualityNode(!=);
           break;
      
      case GREATER:
           cond->id = next_id;
           next_id += 1;
           printRelationalNode(>);
           break;

      case GREATER_EQUAL:
           cond->id = next_id;
           next_id += 1;
           printRelationalNode(>=);
           break;

      case LESS:
           cond->id = next_id;
           next_id += 1;
           printRelationalNode(<);
           break;

      case LESS_EQUAL:
           cond->id = next_id;
           next_id += 1;
           printRelationalNode(>=);
           break;	  

      case BOOL_NOT:
           cond->id = next_id;
           next_id += 1;
           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\nNOT\"]\n", 
                             cond->id, cond->id, LOCATION_ARGS(cond->location));

           print_to_dot_file("node%d->node%d[label=\"not exp\"]\n",  
                             cond->id, next_id);

           prettyPrint(cond->not_exp, Condition);
           break;

      case BOOL_OR:
           cond->id = next_id;
           next_id += 1;
           printBinaryBooleanNode(OR);
           break;

      case BOOL_AND:
           cond->id = next_id;
           next_id += 1;
           printBinaryBooleanNode(AND);
           break;

      default: print_to_log("Error (printASTCondition): Unexpected Type: "
                            "%d\n", (int)cond->type); 
              break;
    }
 }


 void printASTAtom(GPAtom * const atom, FILE *dot_file)
{
   switch(atom->type) 
   {
      case VARIABLE:
           atom->id = next_id;
           next_id += 1;

           if(atom->variable.name != NULL)
              print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                                "Variable: %s \\n \nType: %d\"]\n", 
                                atom->id, atom->id, LOCATION_ARGS(atom->location),
                                atom->variable.name, atom->variable.type);
           else 
           {
              print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d"
                                "\\nVariable: \\n UNDEFINED\"]\n",
                                atom->id, atom->id, LOCATION_ARGS(atom->location));
              print_to_log("Error (printASTAtom): Undefined variable name "
                           "at AST node %d", atom->id);
           }
           break;

     case INTEGER_CONSTANT:
          atom->id = next_id;
          next_id += 1;

          print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                            "Number: %d\"]\n", atom->id, atom->id, 
                            LOCATION_ARGS(atom->location), atom->number);
          break;

     case STRING_CONSTANT:
          atom->id = next_id;
          next_id += 1;

          if(atom->string != NULL)
             print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                               "String: %s\"]\n", atom->id, atom->id,
                               LOCATION_ARGS(atom->location), atom->string);
          else
             print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                               "Empty String\"]\n", atom->id, atom->id, 
                               LOCATION_ARGS(atom->location));
          break;

     case INDEGREE:
          atom->id = next_id;
          next_id += 1;
          printDegreeOperatorNode(indegree, INDEGREE);
          break;

     case OUTDEGREE:
          atom->id = next_id;
          next_id += 1;
          printDegreeOperatorNode(outdegree, OUTDEGREE);
          break;

     case LENGTH:
          atom->id = next_id;
          next_id += 1;

          if(atom->variable.name != NULL)
             print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                               "Length: %s \\n \nType: %d\"]\n", 
                               atom->id, atom->id, LOCATION_ARGS(atom->location),
                               atom->variable.name, atom->variable.type);
          else 
          {
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d"
                               "\\nLength: \\n UNDEFINED\"]\n",
                               atom->id, atom->id, LOCATION_ARGS(atom->location));
             print_to_log("Error (printASTAtom): Undefined variable name "
                          "at AST node %d", atom->id);
          }
          break;

     case NEG:
          atom->id = next_id;
          next_id += 1;

          print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\nMINUS\"]\n", 
                            atom->id, atom->id, LOCATION_ARGS(atom->location));

          print_to_dot_file("node%d->node%d[label=\"exp\"]\n", atom->id, next_id);   
          prettyPrint(atom->neg_exp, Atom);
          break;

     case ADD:
          atom->id = next_id;
          next_id += 1;
          printBinaryOperatorNode(+);
          break;

     case SUBTRACT:
          atom->id = next_id;
          next_id += 1;
          printBinaryOperatorNode(-);
          break;

     case MULTIPLY:
          atom->id = next_id;
          next_id += 1;
          printBinaryOperatorNode(*);
          break;

     case DIVIDE:
          atom->id = next_id;
          next_id += 1;
          printBinaryOperatorNode(/);
          break;

     case CONCAT:
          atom->id = next_id;
          next_id += 1;
          printBinaryOperatorNode(.);
          break;

     default: print_to_log("Error (printAtomicExp): Unexpected Atomic "
                           "Expression Type: %d\n", (int)atom->type);
              break;
     }
}


void printASTProcedure(GPProcedure *const proc, FILE *dot_file)
{
   proc->id = next_id;
   next_id += 1;

   if(proc->name != NULL)
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Procedure \\n Name: %s\"]\n", proc->id, proc->id, 
                        LOCATION_ARGS(proc->location), proc->name);
    else
    {
        print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                         "Procedure \\n Name: UNDEFINED\"]\n",
                         proc->id, proc->id, LOCATION_ARGS(proc->location));

       print_to_log("Error (printASTProcedure): Undefined procedure name at "
                    "AST node %d", proc->id);
    }

    prettyPrintList(proc->local_decls, proc, decls);
    print_to_dot_file("node%d->node%d[label=\"cmd seq\"]\n", 
                      proc->id, next_id); 
    prettyPrint(proc->commands, Command);
}

void printASTRule(GPRule *const rule, FILE *dot_file)
{
   /* Rule nodes have more than one parent node. Checking the rule_id's value
    * ensures the AST for the rule is printed only once. */
   if(rule->id > 0) return;
   rule->id = next_id;
   next_id += 1;

   if(rule->name != NULL)
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Rule \\n Name: %s \\n Left Nodes: %d\\n" 
                        "Left Edges: %d \\n Variables: %d \\n Empty LHS: %d\\n"
                        "Predicate: %d\"]\n", rule->id, rule->id,
                        LOCATION_ARGS(rule->location), rule->name,
                        rule->left_nodes, rule->left_edges, rule->variable_count,
                        rule->empty_lhs, rule->is_predicate);
   else 
   {
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Rule \\n Name: UNDEFINED\"]\n", 
                        rule->id, rule->id, LOCATION_ARGS(rule->location));
      print_to_log("Error (printASTRule): Undefined rule name at AST node "
                    "%d", rule->id);       
   }

   prettyPrintList(rule->variables, rule, variables);
   print_to_dot_file("node%d->node%d[label=\"lhs\"]\n", rule->id, next_id); 
   prettyPrint(rule->lhs, Graph);
   print_to_dot_file("node%d->node%d[label=\"rhs\"]\n", rule->id, next_id); 
   prettyPrint(rule->rhs, Graph);
   prettyPrintList(rule->interface, rule, interface);

   /* Same code as the prettyPrintList macro, except this fragment needs to
   * call printCondition instead of printList.
   */
   if(rule->condition == NULL) 
   {                                         
      print_to_dot_file("node%d[shape=plaintext,label=\"%d NULL\"]\n", 
                        next_id, next_id);                            
      print_to_dot_file("node%d->node%d[label=\"condition\"]\n",            
                        rule->id, next_id);                           
      next_id += 1;                                              
   }							          
   else 
   {                                                            
      print_to_dot_file("node%d->node%d[label=\"condition\"]\n",            
                         rule->id, next_id);                           
      printASTCondition(rule->condition, dot_file);                                        
   }  
}

void printASTGraph(GPGraph *const graph, FILE *dot_file)
{
   graph->id = next_id;
   next_id += 1;

   print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n Graph\"]\n",
                     graph->id, graph->id, LOCATION_ARGS(graph->location));
   prettyPrintList(graph->nodes, graph, nodes);
   prettyPrintList(graph->edges, graph, edges);
}

void printASTNode(GPNode * const node, FILE *dot_file)
{
   node->id = next_id;
   next_id += 1;
   if(node->name != NULL)
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Node \\n Name: %s", node->id, node->id, 
                        LOCATION_ARGS(node->location), node->name);
   else 
   {
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Node \\n Name: UNDEFINED", node->id, node->id, 
                        LOCATION_ARGS(node->location));
      print_to_log("Error (printASTNode): Undefined node name at AST node "
                   "%d", node->id);
   }
   if(node->root == true) 
        print_to_dot_file("\\n Root\"]\n"); 
   else print_to_dot_file("\"]\n");	
   
   print_to_dot_file("node%d->node%d[label=\"label\"]\n", node->id, next_id); 
   prettyPrint(node->label, Label);
}

void printASTEdge(GPEdge * const edge, FILE *dot_file)
{
   edge->id = next_id;
   next_id += 1;
   if(edge->name != NULL)
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Edge \\n Name: %s\\n ", edge->id, edge->id, 
                        LOCATION_ARGS(edge->location), edge->name);
   else 
   {
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Edge \\n Name: UNDEFINED", edge->id, edge->id, 
                        LOCATION_ARGS(edge->location));
      print_to_log("Error (printASTEdge): Undefined edge name at AST node "
                   "%d", edge->id);
   }
   if(edge->bidirectional == true)  print_to_dot_file("\\n Bidirectional\\n"); 
   else print_to_dot_file("\\n");	

   if(edge->source != NULL) print_to_dot_file("Source: %s \\n ", edge->source);
   else 
   {
      print_to_log("Error (printASTEdge): Undefined edge source at AST "
                   "node %d", edge->id);
      print_to_dot_file("Source: UNDEFINED \\n ");
   }
   if(edge->target != NULL) print_to_dot_file("Target: %s\"]\n", edge->target);
   else 
   {
      print_to_log("Error (printASTEdge): Undefined edge target at AST "
                   "node %d", edge->id);
      print_to_dot_file("Target: UNDEFINED \"]\n");
   }
   print_to_dot_file("node%d->node%d[label=\"label\"]\n", edge->id, next_id); 
   prettyPrint(edge->label, Label);
}

void printASTLabel(GPLabel * const label, FILE *dot_file)
{
   label->id = next_id;
   next_id += 1;
   print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n Label \\n Mark: ", 
                     label->id, label->id, LOCATION_ARGS(label->location));
   switch(label->mark) 
   {
      case NONE: 	 print_to_dot_file("No mark\"]\n"); break;
      case RED:          print_to_dot_file("Red\"]\n"); break;
      case GREEN: 	 print_to_dot_file("Green\"]\n"); break;
      case BLUE: 	 print_to_dot_file("Blue\"]\n"); break;
      case GREY: 	 print_to_dot_file("Grey\"]\n"); break;
      case DASHED: 	 print_to_dot_file("Dashed\"]\n"); break;
      case ANY:	         print_to_dot_file("Any\"]\n"); break;
      default: 
      {
           print_to_dot_file("Unexpected mark: %d\"]\n", (int)label->mark);
           print_to_log("Error (printASTLabel): Unexpected mark %d at AST "
                        "node %d", (int)label->mark, label->id);
           break;
      }
   }
   prettyPrintList(label->gp_list, label, gp_list);
}

