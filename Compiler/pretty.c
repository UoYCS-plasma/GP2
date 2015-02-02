/* ///////////////////////////////////////////////////////////////////////////

  =================================
  pretty.c - Chris Bak (18/09/2013)
  =================================

/////////////////////////////////////////////////////////////////////////// */

#include "pretty.h" 

/* printSymbolTable uses g_hash_table_foreach to print the symbol table.
 * g_hash_table_for_each iterates over every key and value in the
 * table, calling the function passed to it (printSymbolList) on
 * each value. */
static FILE *symbol_table_file;

void printSymbolTable(GHashTable *table, string const file_name) 
{
   int length = strlen(file_name) + 4; 
   char symbol_table_file_name[length];
   strcpy(symbol_table_file_name, file_name);
   strncat(symbol_table_file_name, ".tab", 4);
   symbol_table_file = fopen(symbol_table_file_name, "w");
     
   if(symbol_table_file == NULL) {
      perror(symbol_table_file_name);
      exit(1);
   }

   print_to_symtab_file("Symbol Table\n\n");	
   g_hash_table_foreach(table, printSymbolList, NULL);
   fclose(symbol_table_file);
}

void printSymbolList(gpointer key, gpointer value, gpointer user_data)
{
    GSList *current_name = NULL;
    /* The for loop iterates over value, which is a GSList of Symbols. 
     * Typecasting is required to access the Symbol structs because a GSList's
     * data field is a gpointer, equivalent to a void pointer. */
    for(current_name = value; current_name; current_name = current_name->next)
    {
        Symbol *current_sym = (Symbol*)(current_name->data);
	/* Not all symbols have a containing rule */    
	if(current_sym->containing_rule == NULL)
        {
	   print_to_symtab_file("Name: %s\nType: %d\nScope: %s\n",
	           (string)key, current_sym->type, current_sym->scope);
	   if(current_sym->is_var) print_to_symtab_file("Variable\n");
	   if(current_sym->in_lhs) print_to_symtab_file("In LHS\n");
           if(current_sym->wildcard) print_to_symtab_file("Wildcard\n");
           if(current_sym->bidirectional) print_to_symtab_file("Bidirectional\n");
	   print_to_symtab_file("\n");
	}	
	else 
        {	
           print_to_symtab_file("Name: %s\nType: %d\nScope: %s\n"
                   "Containing Rule: %s\n", (string)key, 
                   current_sym->type, current_sym->scope, 
                   current_sym->containing_rule);
       	   if(current_sym->is_var) print_to_symtab_file("Variable\n");
	   if(current_sym->in_lhs) print_to_symtab_file("In LHS\n");
           if(current_sym->wildcard) print_to_symtab_file("Wildcard\n");
           if(current_sym->bidirectional) print_to_symtab_file("Bidirectional\n");
	   print_to_symtab_file("\n");
	}
    }
}
       
/* Dot syntax for nodes:
 * <node_id>[shape=<node_shape>,label=<node_label>]
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
 * edges labelled 'value' and 'next'. */ 

static unsigned int next_node_id = 1;

void printDotAST(List *const gp_ast, string file_name)
{
     int length = strlen(file_name) + 5; 
     char dot_file_name[length];
     strcpy(dot_file_name, file_name);
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

     next_node_id = 1;   
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

     next_node_id = 1;
     printASTGraph(host_graph_ast,dot_file);
     print_to_dot_file("}\n\n");
     fclose(dot_file);
}

/* printList is a recursive function which prints the nodes and edges
 * of its AST argument to the .dot file created by printDotAST.
 *
 * Unique node names are generated with the global variable next_node_id. 
 * A new AST node is reached whenever a printing function is called through the 
 * prettyPrint macros. Hence the first operation of each printing function is 
 * the assignment of next_node_id to the node_id of the AST node being examined.
 * next_node_id is then incremented in preparation for printing a new node in
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

          list->node_id = next_node_id;
          next_node_id += 1;
          printListNode(Global \\n Declarations);
          prettyPrint(list->value.declaration, Declaration);
          prettyPrintList(list->next,list, next);

          break;	

     case LOCAL_DECLARATIONS:

          list->node_id = next_node_id;
          next_node_id += 1;
          printListNode(Local \\n Declarations);
	  prettyPrint(list->value.declaration, Declaration);
          prettyPrintList(list->next,list,next);

          break;	

     case COMMANDS:

          list->node_id = next_node_id;
          next_node_id += 1;
          printListNode(Commands);
          prettyPrint(list->value.command, Statement);
          prettyPrintList(list->next,list,next);

          break;	

     case RULES:

          list->node_id = next_node_id;
          next_node_id += 1;

          if(list->value.rule_name != NULL)
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Rule \\n Name: %s\"]\n", 
                               list->node_id, list->node_id,
                               LOCATION_ARGS(list->location),
                               list->value.rule_name);
          else 
          {
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Rule \\n Name: UNDEFINED\"]\n", 
                               list->node_id, list->node_id, 
                               LOCATION_ARGS(list->location));
             print_to_log("Error: Undefined rule name at AST node %d", 
                          list->node_id);
          }

          prettyPrintList(list->next,list,next);

          break;
     
     case INT_DECLARATIONS:

          list->node_id = next_node_id;
          next_node_id += 1;
          printListNode(Integer \\n Declarations);
          prettyPrint(list->value.variables, List);
          prettyPrintList(list->next,list,next);

          break;

     case CHAR_DECLARATIONS:

          list->node_id = next_node_id;
          next_node_id += 1;
          printListNode(Character \\n Declarations);
          prettyPrint(list->value.variables, List);
          prettyPrintList(list->next,list,next);

          break;

     case STRING_DECLARATIONS:

          list->node_id = next_node_id;
          next_node_id += 1;
          printListNode(String \\n Declarations);
          prettyPrint(list->value.variables, List);
          prettyPrintList(list->next,list,next);
           
          break;
     
     case ATOM_DECLARATIONS:

          list->node_id = next_node_id;
          next_node_id += 1;
          printListNode(Atom \\n Declarations);
          prettyPrint(list->value.variables, List);
          prettyPrintList(list->next,list,next);

          break;
     
     case LIST_DECLARATIONS:

          list->node_id = next_node_id;
          next_node_id += 1;
          printListNode(List \\n Declarations);
          prettyPrint(list->value.variables, List);
          prettyPrintList(list->next,list,next);

          break;
     
     case VARIABLE_LIST:

          list->node_id = next_node_id;
          next_node_id += 1;

          if(list->value.variable_name != NULL)
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Variable \\n Name: %s\"]\n", 
                               list->node_id, list->node_id, 
                               LOCATION_ARGS(list->location),
                               list->value.variable_name);
          else 
          {
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Variable \\n Name: UNDEFINED\"]\n",
                               list->node_id, list->node_id, 
                               LOCATION_ARGS(list->location)); 
             print_to_log("Error: Undefined variable name at AST node %d", 
                           list->node_id);
          }

          prettyPrintList(list->next,list,next);

          break;
     
     case INTERFACE_LIST:

          list->node_id = next_node_id;
          next_node_id += 1; 
          if(list->value.node_id != NULL)
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Interface \\n Node: %s\"]\n", 
                               list->node_id, list->node_id, 
                               LOCATION_ARGS(list->location),
                               list->value.node_id);
          else 
          {
             print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                               "Node \\n Name: UNDEFINED\"]\n", 
                               list->node_id, list->node_id, 
                               LOCATION_ARGS(list->location));
             print_to_log("Error (printASTList): Undefined node name at "
                           "AST node %d", list->node_id);
          }
          prettyPrintList(list->next,list,next);

          break;
     
     case NODE_LIST:

          list->node_id = next_node_id;
          next_node_id += 1; 
          printListNode(Nodes);
          prettyPrint(list->value.node, Node);
          prettyPrintList(list->next,list,next);

          break;
     
     case EDGE_LIST:

          list->node_id = next_node_id;
          next_node_id += 1; 
          printListNode(Edges);
          prettyPrint(list->value.edge, Edge);
          prettyPrintList(list->next,list,next);

          break;

     case GP_LIST:

          list->node_id = next_node_id;
          next_node_id += 1; 
          printListNode(GP List);
          prettyPrint(list->value.atom, Atom);
          prettyPrintList(list->next,list,next);

          break;
     
     case EMPTY_LIST:
             
          list->node_id = next_node_id;
          next_node_id += 1;
          print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n" 
                            "Empty List\"]\n", 
                            list->node_id, list->node_id,
                            LOCATION_ARGS(list->location));
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
      
           decl->node_id = next_node_id;
           next_node_id += 1;
           printDeclarationNode(Main, main \\n program);
           prettyPrint(decl->value.main_program, Statement);

           break;

      case PROCEDURE_DECLARATION:

           decl->node_id = next_node_id;
           next_node_id += 1;
           printDeclarationNode(Procedure \\n Declaration, proc);
           prettyPrint(decl->value.procedure, Procedure);

           break;

      case RULE_DECLARATION:

           decl->node_id = next_node_id;
           next_node_id += 1;
           printDeclarationNode(Rule \\n Declaration, rule);
           prettyPrint(decl->value.rule, Rule);

           break;

      default: print_to_log("Error (printASTDeclaration): Unexpected type: "
                            "%d\n", (int)decl->decl_type);
               break;
   }
}


void printASTStatement(GPStatement *const stmt, FILE *dot_file)
{
  switch(stmt->statement_type)
  {
      case COMMAND_SEQUENCE:	

           stmt->node_id = next_node_id;
           next_node_id += 1;

           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                             "Command \\n Sequence\"]\n", 
                             stmt->node_id, stmt->node_id, 
                             LOCATION_ARGS(stmt->location));

           print_to_dot_file("node%d->node%d[label=\"cmd_seq\"]\n",  
                             stmt->node_id, next_node_id); 

           prettyPrint(stmt->value.cmd_seq, List);

           break;

      case RULE_CALL:

           stmt->node_id = next_node_id;
           next_node_id += 1;

           if(stmt->value.rule_name != NULL)
              print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                                "Rule Call \\n Name: %s\"]\n",
                                stmt->node_id, stmt->node_id, 
                                LOCATION_ARGS(stmt->location), 
                                stmt->value.rule_name);
           else 
           {
               print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                                 "Rule \\n Name: UNDEFINED\"]\n",  
                                 stmt->node_id, stmt->node_id, 
                                 LOCATION_ARGS(stmt->location));
               print_to_log("Error (printASTStatement): Undefined rule name "
                           "at AST node %d", stmt->node_id);
            }

            break;

      case RULE_SET_CALL:

           stmt->node_id = next_node_id;
           next_node_id += 1;

           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                             "Rule Set Call\"]\n", 
                             stmt->node_id, stmt->node_id,
                             LOCATION_ARGS(stmt->location));

           print_to_dot_file("node%d->node%d[label=\"rule set\"]\n",  
                             stmt->node_id, next_node_id); 

           prettyPrint(stmt->value.rule_set, List);

           break;

      case PROCEDURE_CALL:

           stmt->node_id = next_node_id;
           next_node_id += 1;

           if(stmt->value.proc_name != NULL)
              print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                                "Procedure Call \\n Name: %s\"]\n",
                                stmt->node_id, stmt->node_id,
                                LOCATION_ARGS(stmt->location), 
                                stmt->value.proc_name);
           else 
           {
               print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                                 "Procedure \\n Name: UNDEFINED\"]\n",
                                 stmt->node_id, stmt->node_id,
                                 LOCATION_ARGS(stmt->location));
               print_to_log("Error (printASTStatement): Undefined procedure "
                           "ame at AST node %d", stmt->node_id);
           }

           break;

      case IF_STATEMENT:

           stmt->node_id = next_node_id;
           next_node_id += 1;

           printConditionalNode(If Statement);
            
           break;

      case TRY_STATEMENT:

           stmt->node_id = next_node_id;
           next_node_id += 1;

           printConditionalNode(Try Statement);
         
           break;

      case ALAP_STATEMENT:

           stmt->node_id = next_node_id;
           next_node_id += 1;

           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                             "ALAP Statement\"]\n", 
                             stmt->node_id, stmt->node_id,
                             LOCATION_ARGS(stmt->location));

           print_to_dot_file("node%d->node%d[label=\"loop \\n statement\"]\n",  
                             stmt->node_id, next_node_id); 

           prettyPrint(stmt->value.loop_stmt, Statement);
            
           break;

      case PROGRAM_OR:

           stmt->node_id = next_node_id;
           next_node_id += 1;

           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                             "OR Statement\"]\n", 
                             stmt->node_id, stmt->node_id,
                             LOCATION_ARGS(stmt->location));

           print_to_dot_file("node%d->node%d[label=\"left \\n statement\"]\n",  
                             stmt->node_id, next_node_id);  

           prettyPrint(stmt->value.or_stmt.left_stmt, Statement);             

           print_to_dot_file("node%d->node%d[label=\"right \\n statement\"]\n",  
                             stmt->node_id, next_node_id);  

           prettyPrint(stmt->value.or_stmt.right_stmt, Statement);

           break;

      case SKIP_STATEMENT:

           stmt->node_id = next_node_id;
           next_node_id += 1;
           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n skip\"]\n", 
                             stmt->node_id, stmt->node_id,
                             LOCATION_ARGS(stmt->location));
           break;

      case FAIL_STATEMENT:

           stmt->node_id = next_node_id;
           next_node_id += 1;
           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n fail\"]\n", 
                             stmt->node_id, stmt->node_id, 
                             LOCATION_ARGS(stmt->location));
           break;
      
      default: print_to_log("Error (printASTStatement): Unexpected type: "
                           "%d\n", (int)stmt->statement_type); 
               break;
      }
}


void printASTCondition(GPCondExp *const cond, FILE *dot_file)
{
   switch(cond->exp_type) 
   {
      case INT_CHECK:
 
           cond->node_id = next_node_id;
           next_node_id += 1;
           printTypeCheckNode(int check, INT_CHECK);

           break;

      case CHAR_CHECK:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printTypeCheckNode(char check, CHAR_CHECK);

           break;

      case STRING_CHECK:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printTypeCheckNode(string check, STRING_CHECK);

           break;

      case ATOM_CHECK:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printTypeCheckNode(atom check, ATOM_CHECK);

           break;

      case EDGE_PRED:

           cond->node_id = next_node_id;
           next_node_id += 1;
           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\nEdge Test\\n",
                             cond->node_id, cond->node_id, 
                             LOCATION_ARGS(cond->location));

           if(cond->value.edge_pred.source != NULL)
              print_to_dot_file("Source: %s \\n ", cond->value.edge_pred.source);
           else 
           {
              print_to_log("Error (printASTCondition): Undefined node at "
                           "AST node %d", cond->node_id);
              print_to_dot_file("Source: ERROR \\n ");
           }

           if(cond->value.edge_pred.target != NULL)
               print_to_dot_file("Target: %s\"]\n ", cond->value.edge_pred.target);
           else 
           {
              print_to_log("Error (printASTCondition): Undefined node at "
                           "AST node %d", cond->node_id);
              print_to_dot_file("Target: ERROR \"]\n");
           }

           if(cond->value.edge_pred.label) 
           {
              print_to_dot_file("node%d->node%d[label=\"label \\n argument\"]\n",  
                                cond->node_id, next_node_id);
              prettyPrint(cond->value.edge_pred.label, Label);
           }
           else
           {
              print_to_dot_file("node%d[shape=plaintext,label=\"%d NULL\"]\n", 
                                next_node_id, next_node_id);  
              print_to_dot_file("node%d->node%d[label=\"label \\n argument\"]"          
                                "\n", cond->node_id, next_node_id);                     
              next_node_id += 1;       
           }
           break;

      case EQUAL:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printListEqualityNode(=);
     
           break;	

      case NOT_EQUAL:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printListEqualityNode(!=);
     
           break;
      
      case GREATER:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printRelationalNode(>);

           break;

      case GREATER_EQUAL:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printRelationalNode(>=);

           break;

      case LESS:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printRelationalNode(<);

           break;

      case LESS_EQUAL:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printRelationalNode(>=);

           break;	  

      case BOOL_NOT:

           cond->node_id = next_node_id;
           next_node_id += 1;
           print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\nNOT\"]\n", 
                             cond->node_id, cond->node_id,
                             LOCATION_ARGS(cond->location));

           print_to_dot_file("node%d->node%d[label=\"not exp\"]\n",  
                             cond->node_id, next_node_id);

           prettyPrint(cond->value.not_exp, Condition);

           break;

      case BOOL_OR:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printBinaryBooleanNode(OR);
  
           break;

      case BOOL_AND:

           cond->node_id = next_node_id;
           next_node_id += 1;
           printBinaryBooleanNode(AND);

           break;

      default: print_to_log("Error (printASTCondition): Unexpected Type: "
                            "%d\n", (int)cond->exp_type); 
              break;
    }
 }


 void printASTAtom(GPAtomicExp * const atom, FILE *dot_file)
{
   switch(atom->exp_type) 
   {
      case VARIABLE:

           atom->node_id = next_node_id;
           next_node_id += 1;

           if(atom->value.name != NULL)
              print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                                "Variable: %s\"]\n", 
                                atom->node_id, atom->node_id, 
                                LOCATION_ARGS(atom->location),
                                atom->value.name);
           else 
           {
              print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d"
                                "\\nVariable: \\n UNDEFINED\"]\n",
                                atom->node_id, atom->node_id, 
                                LOCATION_ARGS(atom->location));
              print_to_log("Error (printASTAtom): Undefined variable name "
                           "at AST node %d", atom->node_id);
           }

           break;

     case INTEGER_CONSTANT:

          atom->node_id = next_node_id;
          next_node_id += 1;

          print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                            "Number: %d\"]\n", 
                            atom->node_id, atom->node_id, 
                            LOCATION_ARGS(atom->location), 
                            atom->value.number);
          break;

     case CHARACTER_CONSTANT:

          atom->node_id = next_node_id;
          next_node_id += 1;

          if(atom->value.string != NULL)
             print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                               "Character: %s\"]\n", 
                               atom->node_id, atom->node_id,
                               LOCATION_ARGS(atom->location), 
                               atom->value.string);
          else 
          {
             print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                               "UNDEFINED\"]\n",
                               atom->node_id, atom->node_id, 
                               LOCATION_ARGS(atom->location));
             print_to_log("Error (printASTAtom): Empty character at AST "
                          "node %d\n", atom->node_id);
          }

          break;
       
     case STRING_CONSTANT:

          atom->node_id = next_node_id;
          next_node_id += 1;

          if(atom->value.string != NULL)
             print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                               "String: %s\"]\n",  
                               atom->node_id, atom->node_id,
                               LOCATION_ARGS(atom->location), 
                               atom->value.string);
          else
             print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                               "Empty String\"]\n", 
                               atom->node_id, atom->node_id, 
                               LOCATION_ARGS(atom->location));
          break;

     case INDEGREE:
     
          atom->node_id = next_node_id;
          next_node_id += 1;
          printDegreeOperatorNode(indegree, INDEGREE);

          break;

     case OUTDEGREE:

          atom->node_id = next_node_id;
          next_node_id += 1;
          printDegreeOperatorNode(outdegree, OUTDEGREE);

          break;

     case LIST_LENGTH:

          atom->node_id = next_node_id;
          next_node_id += 1;

          if(atom->value.list_arg) {
             print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                               "List \\n Length\"]\n", 
                               atom->node_id, atom->node_id, 
                               LOCATION_ARGS(atom->location));
             print_to_dot_file("node%d->node%d[label=\"arg\"]\n", 
                               atom->node_id, next_node_id);
             prettyPrint(atom->value.list_arg, List);
          }
          else 
          {
             print_to_dot_file("node%d[shape=plaintext,label=\"%dNULL\"]\n",
                               next_node_id, next_node_id);  
             print_to_dot_file("node%d->node%d[label=\"arg\"]\n",          
                               atom->node_id, next_node_id);                     
             next_node_id += 1;       
          }
          break;

     case STRING_LENGTH:

          atom->node_id = next_node_id;
          next_node_id += 1;

          if(atom->value.str_arg)
          {
             print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                               "String \\n Length\"]\n", 
                               atom->node_id, atom->node_id, 
                               LOCATION_ARGS(atom->location));
             print_to_dot_file("node%d->node%d[label=\"arg\"]\n", 
                     atom->node_id, next_node_id);
             prettyPrint(atom->value.str_arg, Atom);
          }
          else
          {
             print_to_dot_file("node%d[shape=plaintext,label=\"%dNULL\"]\n", 
                               next_node_id, next_node_id);  
             print_to_dot_file("node%d->node%d[label=\"arg\"]\n",          
                               atom->node_id, next_node_id);                     
             next_node_id += 1;       
          }
          break;

     case NEG:

          atom->node_id = next_node_id;
          next_node_id += 1;

          print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\nMINUS\"]\n", 
                            atom->node_id, atom->node_id,
                            LOCATION_ARGS(atom->location));

          print_to_dot_file("node%d->node%d[label=\"exp\"]\n",          
                    atom->node_id, next_node_id);   
          prettyPrint(atom->value.exp, Atom);

          break;

     case ADD:

          atom->node_id = next_node_id;
          next_node_id += 1;
          printBinaryOperatorNode(+);

          break;

     case SUBTRACT:

          atom->node_id = next_node_id;
          next_node_id += 1;
          printBinaryOperatorNode(-);

          break;


     case MULTIPLY:

          atom->node_id = next_node_id;
          next_node_id += 1;
          printBinaryOperatorNode(*);

          break;

     case DIVIDE:

          atom->node_id = next_node_id;
          next_node_id += 1;
          printBinaryOperatorNode(/);

          break;

     case CONCAT:

          atom->node_id = next_node_id;
          next_node_id += 1;
          printBinaryOperatorNode(.);

          break;

     default: print_to_log("Error (printAtomicExp): Unexpected Atomic "
                           "Expression Type: %d\n", (int)atom->exp_type);
              break;
     }
}


void printASTProcedure(GPProcedure *const proc, FILE *dot_file)
{
   proc->node_id = next_node_id;
   next_node_id += 1;

   if(proc->name != NULL)
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Procedure \\n Name: %s\"]\n",
                        proc->node_id, proc->node_id, 
                        LOCATION_ARGS(proc->location), proc->name);
    else
    {
        print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                         "Procedure \\n Name: UNDEFINED\"]\n",
                         proc->node_id, proc->node_id, 
                         LOCATION_ARGS(proc->location));

       print_to_log("Error (printASTProcedure): Undefined procedure name at "
                    "AST node %d", proc->node_id);
    }

    prettyPrintList(proc->local_decls, proc, decls);
    print_to_dot_file("node%d->node%d[label=\"cmd seq\"]\n", 
                      proc->node_id, next_node_id); 
    prettyPrint(proc->cmd_seq, Statement);
}

void printASTRule(GPRule *const rule, FILE *dot_file)
{
   rule->node_id = next_node_id;
   next_node_id += 1;

   if(rule->name != NULL)
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Rule \\n Name: %s\"]\n", 
                        rule->node_id, rule->node_id,
                        LOCATION_ARGS(rule->location), rule->name);
   else 
   {
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Rule \\n Name: UNDEFINED\"]\n", 
                        rule->node_id, rule->node_id, 
                        LOCATION_ARGS(rule->location));
      print_to_log("Error (printASTRule): Undefined rule name at AST node "
                    "%d", rule->node_id);       
   }

   prettyPrintList(rule->variables, rule, variables);
   print_to_dot_file("node%d->node%d[label=\"lhs\"]\n", 
                     rule->node_id, next_node_id); 
   prettyPrint(rule->lhs, Graph);
   print_to_dot_file("node%d->node%d[label=\"rhs\"]\n", 
                     rule->node_id, next_node_id); 
   prettyPrint(rule->rhs, Graph);
   prettyPrintList(rule->interface, rule, interface);

   /* Same code as the prettyPrintList macro, except this fragment needs to
   * call printCondition instead of printList.
   */
   if(rule->condition == NULL) 
   {                                         
      print_to_dot_file("node%d[shape=plaintext,label=\"%d NULL\"]\n", 
                        next_node_id, next_node_id);                            
      print_to_dot_file("node%d->node%d[label=\"condition\"]\n",            
                        rule->node_id, next_node_id);                           
      next_node_id += 1;                                              
   }							          
   else 
   {                                                            
      print_to_dot_file("node%d->node%d[label=\"condition\"]\n",            
                         rule->node_id, next_node_id);                           
      printASTCondition(rule->condition, dot_file);                                        
   }  
}

void printASTGraph(GPGraph *const graph, FILE *dot_file)
{
   graph->node_id = next_node_id;
   next_node_id += 1;

   print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n Graph\"]\n",
                     graph->node_id, graph->node_id, 
                     LOCATION_ARGS(graph->location));
   /* print_to_dot_file("node%d->node%d[label=\"position\"]\n", 
                     graph->node_id, next_node_id); 

    * prettyPrint(graph->position, Position); */
   prettyPrintList(graph->nodes, graph, nodes);
   prettyPrintList(graph->edges, graph, edges);
}

void printASTNode(GPNode * const node, FILE *dot_file)
{
   node->node_id = next_node_id;
   next_node_id += 1;

   if(node->name != NULL)
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Node \\n Name: %s", 
                        node->node_id, node->node_id, 
                        LOCATION_ARGS(node->location), node->name);
   else 
   {
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Node \\n Name: UNDEFINED", 
                        node->node_id, node->node_id, 
                        LOCATION_ARGS(node->location));
      print_to_log("Error (printASTNode): Undefined node name at AST node "
                   "%d", node->node_id);
   }
   if(node->root == true) 
        print_to_dot_file("\\n Root\"]\n"); 
   else print_to_dot_file("\"]\n");	
   
   print_to_dot_file("node%d->node%d[label=\"label\"]\n", 
                     node->node_id, next_node_id); 
   prettyPrint(node->label, Label);

   /* print_to_dot_file("node%d->node%d[label=\"position\"]\n", 
                     node->node_id, next_node_id); 
    * prettyPrint(node->position, Position); */
}

void printASTEdge(GPEdge * const edge, FILE *dot_file)
{
   edge->node_id = next_node_id;
   next_node_id += 1;

   if(edge->name != NULL)
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Edge \\n Name: %s\\n ", 
                        edge->node_id, edge->node_id, 
                        LOCATION_ARGS(edge->location), edge->name);
   else 
   {
      print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Edge \\n Name: UNDEFINED", 
                        edge->node_id, edge->node_id, 
                        LOCATION_ARGS(edge->location));
      print_to_log("Error (printASTEdge): Undefined edge name at AST node "
                   "%d", edge->node_id);
   }

   if(edge->bidirectional == true) 
        print_to_dot_file("\\n Bidirectional\\n"); 
   else print_to_dot_file("\\n");	

   if(edge->source != NULL)
      print_to_dot_file("Source: %s \\n ", 
                        edge->source);
   else 
   {
      print_to_log("Error (printASTEdge): Undefined edge source at AST "
                   "node %d", edge->node_id);
      print_to_dot_file("Source: UNDEFINED \\n ");
   }

   if(edge->target != NULL)
      print_to_dot_file("Target: %s\"]\n", 
                        edge->target);
   else 
   {
      print_to_log("Error (printASTEdge): Undefined edge target at AST "
                   "node %d", edge->node_id);
      print_to_dot_file("Target: UNDEFINED \"]\n");
   }

   print_to_dot_file("node%d->node%d[label=\"label\"]\n", 
                     edge->node_id, next_node_id); 
   prettyPrint(edge->label, Label);
}


/* void printASTPosition(GPPos * const pos, FILE *dot_file)
{
     pos->node_id = next_node_id;
     next_node_id += 1;

     print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                       "Position \\n x: %d \\n y: %d\"]\n", 
                       pos->node_id, pos->node_id,
                       LOCATION_ARGS(pos->location), pos->x, pos->y);
} */


void printASTLabel(GPLabel * const label, FILE *dot_file)
{
   label->node_id = next_node_id;
   next_node_id += 1;

   print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n Label \\n Mark: ", 
                     label->node_id, label->node_id, 
                     LOCATION_ARGS(label->location));

   switch (label->mark) 
   {
      case (RED):	 print_to_dot_file("Red\"]\n"); break;
      case (GREEN): 	 print_to_dot_file("Green\"]\n"); break;
      case (BLUE): 	 print_to_dot_file("Blue\"]\n"); break;
      case (GREY): 	 print_to_dot_file("Grey\"]\n"); break;
      case (DASHED): 	 print_to_dot_file("Dashed\"]\n"); break;
      case (ANY):	 print_to_dot_file("Any\"]\n"); break;
      case (NONE): 	 print_to_dot_file("No mark\"]\n"); break;

      default: 
      {
           print_to_dot_file("Unexpected mark: %d\"]\n", (int)label->mark);
           print_to_log("Error (printASTLabel): Unexpected mark %d at AST "
                        "node %d", (int)label->mark, label->node_id);
           break;
      }
   }

   print_to_dot_file("node%d->node%d[label=\"gp list\"]\n",  
                     label->node_id, next_node_id); 
   prettyPrint(label->gp_list, List);
}



