/*//////////////////////////////////////////////////////////////////////////// 

                           pretty.c       
                              
              Pretty printers for AST and symbol table

                  Created on 18/9/2013 by Chris Bak 

/////////////////////////////////////////////////////////////////////////// */ 

#include "pretty.h" /* function prototypes */
#include "seman.h" /* struct Symbol */
#include <stdio.h> /* fprintf, fopen, fclose */
#include <string.h> /* strlen, strcpy, strcat */
#include <glib.h> /* GHashTable and GSList */ 

/* print_symbol_table uses glib's hash table iterator to print the table.
 * table is the hash table to print.
 * print_symbol is the function called for each value (GSList) encountered.
 * The third argument is passed to the print_symbol, but this is not needed here.
 */

static FILE *symbol_table_file;

int print_symbol_table(GHashTable *table, char *file_name) 
{
   /* Assumes input file has no extension, but will be .gpp in the future. */

   /* The length of the new file name is the length of the old file name
    * plus 4 for ".dot" plus 1 for the terminating null character.  
    */

   int length = strlen(file_name) + 4; 
   char symbol_table_file_name[length];
   strcpy(symbol_table_file_name, file_name);
   strncat(symbol_table_file_name, ".tab", 4);
   symbol_table_file = fopen(symbol_table_file_name, "w");
     
   if(symbol_table_file == NULL) {
      perror(symbol_table_file_name);
      return 1;
   }

   fprintf(symbol_table_file,"Symbol Table\n\n");	
   g_hash_table_foreach(table, print_symbol, NULL);

   fclose(symbol_table_file);

   return 0;
}

/* Auxiliary function used by print_symbol_table. It takes a key and
 * value provided by g_hash_table_foreach 
 * user_data is a pointer from the caller, not necessary in this case.
 */

void print_symbol(gpointer key, gpointer value, gpointer user_data)
{
    GSList *current_name = NULL;

    /* iterates over value, which is a GSList of Symbols 
     * typecasting is required to access the Symbol structs as a GSList's
     * data field is a gpointer, equivalent to a void pointer.
     */
    for(current_name = value; current_name!=NULL; 
	current_name = current_name->next) {

        Symbol *current_sym = (Symbol*)(current_name->data);
	
	/* Not all symbols have a containing rule */    
	if(current_sym->containing_rule == NULL) {
	   fprintf(symbol_table_file,"Name: %s\nType: %s\nScope: %s\n",
	           (char*)key, current_sym->type, current_sym->scope);
	   if(current_sym->is_var) fprintf(symbol_table_file,"Variable\n");
	   if(current_sym->in_lhs) fprintf(symbol_table_file,"In LHS\n");
	   fprintf(symbol_table_file,"\n");
	}	
	else {	
           fprintf(symbol_table_file,"Name: %s\nType: %s\nScope: %s\n"
                   "Containing Rule: %s\n", (char*)key, current_sym->type, 
                   current_sym->scope, current_sym->containing_rule);
       	   if(current_sym->is_var) fprintf(symbol_table_file,"Variable\n");
	   if(current_sym->in_lhs) fprintf(symbol_table_file,"In LHS\n");
	   fprintf(symbol_table_file,"\n");
	}
    }
}
       

/* print_dot_ast takes as arguments a pointer to the root of the AST and the 
 * name of the GP source file. It creates a new file <source_name>.dot,
 * writes to it a graph declaration in the dot syntax for graph specification, 
 * and calls print_list to write the node and edge declarations.
 *
 * Dot syntax for nodes:
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
 * edges labelled 'value' and 'next'. 
 */ 


static unsigned int next_node_id = 1;
static FILE *dot_file; 

int print_dot_ast(List *const gp_ast, char* file_name)
{
 
     /* Assumes input file has no extension, but will be .gpp in the future. */

     /* The length of the new file name is the length of the old file name
      * plus 4 for ".dot" plus 1 for the terminating null character.  
      */

     int length = strlen(file_name) + 5; 
     char dot_file_name[length];
     strcpy(dot_file_name, file_name);
     strncat(dot_file_name, ".dot", 4);
     dot_file = fopen(dot_file_name, "w");
     
     if(dot_file == NULL) {
	perror(dot_file_name);
	return 1;
     }	

     fprintf(dot_file,"digraph g { \n");

     /* Print the entry point of the AST. node1 will be the first 
      * node created by print_list. */

     fprintf(dot_file,"node0[shape=plaintext,label=\"ROOT\"]\n");
     fprintf(dot_file,"node0->node1\n");

     next_node_id = 1;
     print_list(gp_ast);

     fprintf(dot_file,"}\n\n");

     fclose(dot_file);

     return 0;
}

int print_dot_host_graph(GPGraph *const host_graph_ast, char* file_name)
{
 
     /* Assumes input file has no extension, but will be .gpg in the future. */

     /* The length of the new file name is the length of the old file name
      * plus 4 for ".dot" plus 1 for the terminating null character.      
      */

     int dot_length = strlen(file_name) + 5; 
     char dot_file_name[dot_length];
     strcpy(dot_file_name, file_name);
     strncat(dot_file_name, ".dot", 4);
     dot_file = fopen(dot_file_name, "w");
     
     if(dot_file == NULL) {
	perror(dot_file_name);
	return 1;
     }	

     fprintf(dot_file,"digraph g { \n");

     /* Print the entry point of the AST. node1 will be the first 
      * node created by print_list. */

     fprintf(dot_file,"node0[shape=plaintext,label=\"ROOT\"]\n");
     fprintf(dot_file,"node0->node1\n");

     next_node_id = 1;
     print_graph(host_graph_ast);

     fprintf(dot_file,"}\n\n");

     fclose(dot_file);

     return 0;
}



/* print_list is a recursive function that prints the nodes and edges
 * of its AST argument to the .dot file created by print_dot_ast.
 *
 * Unique node names are generated with the global variable next_node_id. 
 * A new AST node is reached whenever a print_X function is called through the 
 * pretty_print macros. Hence each print_X function will assign next_node_id
 * to the node_id of the current AST node and increment next_node_id.
 *
 * These functions make frequent use of the macros pretty_print,
 * pretty_print_list and LOCATION_ARGS. They are defined and described in the 
 * header file.
 */

void print_list(List * const list)
{

     switch(list->list_type) {

	case GLOBAL_DECLARATIONS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "Global \\n Declarations\"]\n", list->node_id, 
                     list->node_id, LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.declaration, declaration);
             pretty_print_list(list->next,list,next);

	     break;	


	case LOCAL_DECLARATIONS:

	     list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "Local \\n Declarations\"]\n", list->node_id, 
                     list->node_id, LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  		

	     pretty_print(list->value.declaration, declaration);
             pretty_print_list(list->next,list,next);

	     break;	


	case COMMANDS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "Commands\"]\n", list->node_id, list->node_id,
                     LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.command, statement);
             pretty_print_list(list->next,list,next);

	     break;	


	case RULES:

             list->node_id = next_node_id;
             next_node_id += 1;

	     if(list->value.rule_name != NULL)
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Rule \\n Name: %s\"]\n", list->node_id, list->node_id,
		        LOCATION_ARGS(list->location), list->value.rule_name);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Rule \\n Name: UNDEFINED\"]\n", list->node_id,
                        list->node_id, LOCATION_ARGS(list->location));
                fprintf(log_file,"Error: Undefined rule name at AST node %d", 
                        list->node_id);
             }

             pretty_print_list(list->next,list,next);

	     break;
	

	case INT_DECLARATIONS:
 
             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "Integer \\n Declarations\"]\n", list->node_id, 
                     list->node_id, LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.variables, list);
             pretty_print_list(list->next,list,next);

	     break;
	

	case STRING_DECLARATIONS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "String \\n Declarations\"]\n", list->node_id, 
                     list->node_id, LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.variables, list);
             pretty_print_list(list->next,list,next);
	     
	     break;
	

	case ATOM_DECLARATIONS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "Atom \\n Declarations\"]\n", list->node_id, 
                     list->node_id, LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.variables, list);
             pretty_print_list(list->next,list,next);

	     break;
	

	case LIST_DECLARATIONS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "List \\n Declarations\"]\n", list->node_id,
                     list->node_id, LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.variables, list);
             pretty_print_list(list->next,list,next);

	     break;
	

	case VARIABLE_LIST:

             list->node_id = next_node_id;
             next_node_id += 1;

	     if(list->value.variable_name != NULL)
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Variable \\n Name: %s\"]\n", list->node_id, 
                        list->node_id, LOCATION_ARGS(list->location),
			list->value.variable_name);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Variable \\n Name: UNDEFINED\"]\n", list->node_id,
                        list->node_id, LOCATION_ARGS(list->location)); 
                fprintf(log_file,"Error: Undefined variable name at AST node %d", 
                        list->node_id);
             }

             pretty_print_list(list->next,list,next);

	     break;
	

	case INTERFACE_LIST:

             list->node_id = next_node_id;
             next_node_id += 1; 

	     if(list->value.node_id != NULL)
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Interface \\n Node: %s\"]\n", list->node_id, 
                        list->node_id, LOCATION_ARGS(list->location),
			list->value.node_id);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Node \\n Name: UNDEFINED\"]\n", list->node_id,
                        list->node_id, LOCATION_ARGS(list->location));
                fprintf(log_file,"Error: Undefined node name at AST node %d", 
                        list->node_id);
             }

             pretty_print_list(list->next,list,next);

	     break;
	

	case NODE_LIST:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "Nodes\"]\n", list->node_id, list->node_id,
                     LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.node, node);
             pretty_print_list(list->next,list,next);

	     break;
	

	case EDGE_LIST:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "Edges\"]\n", list->node_id, list->node_id,
                     LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.edge, edge);
             pretty_print_list(list->next,list,next);

	     break;


	case GP_LIST:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "GP List\"]\n", list->node_id, list->node_id,
                     LOCATION_ARGS(list->location));

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id); 

	     pretty_print(list->value.atom, atom);
             pretty_print_list(list->next,list,next);

	     break;
	

	default: fprintf(log_file,"Unexpected List Type: %d\n",
                         (int)list->list_type); 
                 break;	 

	}
}



void print_declaration(GPDeclaration * const decl)
{
     switch(decl->decl_type) {

	case MAIN_DECLARATION:

             decl->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(decl->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "Main\"]\n", decl->node_id, decl->node_id,
                     LOCATION_ARGS(decl->location));

             fprintf(dot_file,"node%d->node%d[label=\"main \\n program\"]\n",  
                     decl->node_id, next_node_id); 

	     pretty_print(decl->value.main_program, statement);

	     break;

	case PROCEDURE_DECLARATION:

             decl->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(decl->location); */

             fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "Procedure \\n Declaration\"]\n", decl->node_id, 
                     decl->node_id, LOCATION_ARGS(decl->location));

             fprintf(dot_file,"node%d->node%d[label=\"proc\"]\n",  
                     decl->node_id, next_node_id); 

	     pretty_print(decl->value.procedure, procedure);

	     break;

	case RULE_DECLARATION:

             decl->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(decl->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "Rule \\n Declaration\"]\n", decl->node_id, 
                     decl->node_id, LOCATION_ARGS(decl->location));

             fprintf(dot_file,"node%d->node%d[label=\"rule\"]\n",  
                     decl->node_id, next_node_id); 

	     pretty_print(decl->value.rule, rule);

	     break;

	default: fprintf(log_file,"Unexpected Declaration Type: %d\n",
                         (int)decl->decl_type); 
                 break;

	}
}



void print_statement(GPStatement * const stmt)
{
     switch(stmt->statement_type) {

	case COMMAND_SEQUENCE:	

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

             fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "Command \\n Sequence\"]\n", stmt->node_id,  
                     stmt->node_id, LOCATION_ARGS(stmt->location));

             fprintf(dot_file,"node%d->node%d[label=\"cmd_seq\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.cmd_seq, list);

	     break;

	case RULE_CALL:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     if(stmt->value.rule_name != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Rule Call \\n Name: %s\"]\n",
                        stmt->node_id, stmt->node_id, 
                        LOCATION_ARGS(stmt->location), stmt->value.rule_name);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Rule \\n Name: UNDEFINED\"]\n", stmt->node_id, 
                        stmt->node_id, LOCATION_ARGS(stmt->location));
                fprintf(log_file,"Error: Undefined rule name at AST node %d", 
                        stmt->node_id);
             }

	     break;

	case RULE_SET_CALL:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                    "Rule Set Call\"]\n", stmt->node_id, stmt->node_id,
                     LOCATION_ARGS(stmt->location));

             fprintf(dot_file,"node%d->node%d[label=\"rule set\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.rule_set, list);

	     break;

	case PROCEDURE_CALL:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     if(stmt->value.proc_name != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Procedure Call \\n Name: %s\"]\n",
                        stmt->node_id, stmt->node_id,
                        LOCATION_ARGS(stmt->location), stmt->value.proc_name);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Procedure \\n Name: UNDEFINED\"]\n",
                        stmt->node_id, stmt->node_id,
                        LOCATION_ARGS(stmt->location));
                fprintf(log_file,"Error: Undefined procedure name at AST node %d", 
                        stmt->node_id);
             }

	     break;

	case IF_STATEMENT:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "If Statement\"]\n", stmt->node_id, stmt->node_id,
                     LOCATION_ARGS(stmt->location));

             fprintf(dot_file,"node%d->node%d[label=\"condition\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.cond_branch.condition, statement);

	    
             fprintf(dot_file,"node%d->node%d[label=\"then\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.cond_branch.then_stmt, statement);

             
             fprintf(dot_file,"node%d->node%d[label=\"else\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.cond_branch.else_stmt, statement);
             
	     break;

	case TRY_STATEMENT:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "Try Statement\"]\n", stmt->node_id, stmt->node_id,
                     LOCATION_ARGS(stmt->location));

             fprintf(dot_file,"node%d->node%d[label=\"condition\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.cond_branch.condition, statement);

	    
             fprintf(dot_file,"node%d->node%d[label=\"then\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.cond_branch.then_stmt, statement);

             
             fprintf(dot_file,"node%d->node%d[label=\"else\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.cond_branch.else_stmt, statement);
	    
	     break;

	case ALAP_STATEMENT:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "ALAP Statement\"]\n", stmt->node_id, stmt->node_id,
                     LOCATION_ARGS(stmt->location));

             fprintf(dot_file,"node%d->node%d[label=\"loop \\n statement\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.loop_stmt, statement);
             
	     break;

	case PROGRAM_OR:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "OR Statement\"]\n", stmt->node_id, stmt->node_id,
                     LOCATION_ARGS(stmt->location));

             fprintf(dot_file,"node%d->node%d[label=\"left \\n statement\"]\n",  
                     stmt->node_id, next_node_id);  

	     pretty_print(stmt->value.or_stmt.left_stmt, statement);             
 
             fprintf(dot_file,"node%d->node%d[label=\"right \\n statement\"]\n",  
                     stmt->node_id, next_node_id);  

	     pretty_print(stmt->value.or_stmt.right_stmt, statement);

	     break;

	case SKIP_STATEMENT:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n skip\"]\n", 
                     stmt->node_id, stmt->node_id,
                     LOCATION_ARGS(stmt->location));

	     break;

	case FAIL_STATEMENT:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n fail\"]\n", 
                     stmt->node_id, stmt->node_id, 
                     LOCATION_ARGS(stmt->location));

	     break;
	
	default: fprintf(log_file,"Unexpected Statement Type: %d\n",
                         (int)stmt->statement_type); 
                 break;

	}
}



void print_condition(GPCondExp * const cond)
{
     switch(cond->exp_type) {

	case INT_CHECK:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

             if(cond->value.var != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "int check \\n Variable: %s\"]\n", cond->node_id, 
                        cond->node_id, LOCATION_ARGS(cond->location),
                        cond->value.var);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Variable: \\n UNDEFINED\"]\n",
                        cond->node_id, cond->node_id,
                        LOCATION_ARGS(cond->location));
                fprintf(log_file,"Error: Undefined variable name at AST node %d", 
                        cond->node_id);
             }

             break;


	case CHAR_CHECK:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

	     if(cond->value.var != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "char check \\n Variable: %s\"]\n",
                        cond->node_id, cond->node_id, 
                        LOCATION_ARGS(cond->location), cond->value.var);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Variable: \\n UNDEFINED\"]\n",
                        cond->node_id, cond->node_id,
                        LOCATION_ARGS(cond->location));
                fprintf(log_file,"Error: Undefined variable name at AST node %d", 
                        cond->node_id);
             }

             break;


	case STRING_CHECK:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

	     if(cond->value.var != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "string check \\n Variable: %s\"]\n",
                        cond->node_id, cond->node_id, 
                        LOCATION_ARGS(cond->location), cond->value.var);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Variable: \\n UNDEFINED\"]\n",
                        cond->node_id, cond->node_id,
                        LOCATION_ARGS(cond->location));
                fprintf(log_file,"Error: Undefined variable name at AST node %d", 
                        cond->node_id);
             }

             break;

	case ATOM_CHECK:

	     cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

	     if(cond->value.var != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "atom check \\n Variable: %s\"]\n",
                        cond->node_id, cond->node_id, 
                        LOCATION_ARGS(cond->location), cond->value.var);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Variable: \\n UNDEFINED\"]\n",
                        cond->node_id, cond->node_id,
                        LOCATION_ARGS(cond->location));
                fprintf(log_file,"Error: Undefined variable name at AST node %d", 
                        cond->node_id);
             }

             break;

	case EDGE_PRED:

	     cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */
        
	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "Edge Test \\n ", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             if(cond->value.edge_pred.source != NULL)
                 fprintf(dot_file,"Source: %s \\n ", cond->value.edge_pred.source);
             else {
                 fprintf(log_file,"Error: Undefined node at AST node %d", 
                         cond->node_id);
                 fprintf(dot_file,"Source: ERROR \\n ");
             }

             if(cond->value.edge_pred.target != NULL)
                 fprintf(dot_file,"Target: %s\"]\n ", cond->value.edge_pred.target);
             else {
                 fprintf(log_file,"Error: Undefined node at AST node %d", 
                         cond->node_id);
                 fprintf(dot_file,"Target: ERROR \"]\n");
             }

             if(cond->value.edge_pred.label) {
                fprintf(dot_file,"node%d->node%d[label=\"label \\n argument\"]\n",  
                        cond->node_id, next_node_id);
	        pretty_print(cond->value.edge_pred.label, label);
             }
             else {
                fprintf(dot_file,"node%d[shape=plaintext,label=\"%d NULL\"]\n", 
                        next_node_id, next_node_id);  
                fprintf(dot_file,"node%d->node%d[label=\"label \\n argument\"]"          
                        "\n", cond->node_id, next_node_id);                     
                next_node_id += 1;       
             }
                                         
             break;


	case EQUAL:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "=\"]\n", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             fprintf(dot_file,"node%d->node%d[label=\"left list\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.list_cmp.left_list, list);

	     fprintf(dot_file,"node%d->node%d[label=\"right list\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.list_cmp.right_list, list);
	
	     break;	


	case NOT_EQUAL:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "!=\"]\n", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             fprintf(dot_file,"node%d->node%d[label=\"left list\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.list_cmp.left_list, list);

	     fprintf(dot_file,"node%d->node%d[label=\"right list\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.list_cmp.right_list, list);
	
	     break;
	

	case GREATER:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     ">\"]\n", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.atom_cmp.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.atom_cmp.right_exp, atom);

	     break;
	

	case GREATER_EQUAL:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     ">=\"]\n", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.atom_cmp.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.atom_cmp.right_exp, atom);

	     break;
	

	case LESS:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "<\"]\n", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.atom_cmp.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.atom_cmp.right_exp, atom);

	     break;
	

	case LESS_EQUAL:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                     "<=\"]\n", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.atom_cmp.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.atom_cmp.right_exp, atom);

	     break;	  


	case BOOL_NOT:

	     cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "NOT\"]\n", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             fprintf(dot_file,"node%d->node%d[label=\"not exp\"]\n",  
                     cond->node_id, next_node_id);

	     pretty_print(cond->value.not_exp, condition);

	     break;

	case BOOL_OR:

	     cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "OR\"]\n", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",  
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.bin_exp.left_exp, condition);

             fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",  
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.bin_exp.right_exp, condition);

	     break;

	case BOOL_AND:

	     cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "AND\"]\n", cond->node_id, cond->node_id,
                     LOCATION_ARGS(cond->location));

             fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",  
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.bin_exp.left_exp, condition);

             fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",  
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.bin_exp.right_exp, condition);

	     break;

	default: fprintf(log_file,"Unexpected Condition Type: %d\n",
                         (int)cond->exp_type); 
                 break;

	}
}



void print_atom(GPAtomicExp * const atom)
{
     switch(atom->exp_type) {

	case EMPTY_LIST:
		
	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */	

             fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n" 
                     "EMPTY\"]\n", atom->node_id, atom->node_id,
                     LOCATION_ARGS(atom->location));

             break;

	case VARIABLE:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */	

             if(atom->value.name != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Variable: %s\"]\n", atom->node_id, atom->node_id, 
                        LOCATION_ARGS(atom->location), atom->value.name);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "Variable: \\n UNDEFINED\"]\n", atom->node_id, 
                         atom->node_id, LOCATION_ARGS(atom->location));
                fprintf(log_file,"Error: Undefined variable name at AST node %d", 
                        atom->node_id);
             }

             break;

	case INT_CONSTANT:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "Number: %d\"]\n", atom->node_id, atom->node_id, 
                     LOCATION_ARGS(atom->location), atom->value.number);

             break;


	case CHARACTER_CONSTANT:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             if(atom->value.name != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Character: %s\"]\n", atom->node_id, atom->node_id,
                        LOCATION_ARGS(atom->location), atom->value.string);
             else {
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "Character: UNDEFINED\"]\n", atom->node_id, 
                        atom->node_id, LOCATION_ARGS(atom->location));
                fprintf(log_file,"Error: Undefined string at AST node %d", 
                          atom->node_id);
             }

             break;

          
	case STRING_CONSTANT:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             if(atom->value.name != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "String: %s\"]\n", atom->node_id, atom->node_id,
                        LOCATION_ARGS(atom->location), atom->value.string);
             else {
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "String: UNDEFINED\"]\n", atom->node_id, 
                        atom->node_id, LOCATION_ARGS(atom->location));
                fprintf(log_file,"Error: Undefined string at AST node %d", 
                          atom->node_id);
             }

             break;

	case INDEGREE:
	
	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             if(atom->value.name != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "indegree(%s)\"]\n", atom->node_id, atom->node_id, 
                        LOCATION_ARGS(atom->location), atom->value.node_id);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "indegree: \\n UNDEFINED\"]\n", atom->node_id, 
                        atom->node_id, LOCATION_ARGS(atom->location));
                fprintf(log_file,"Error: Undefined node name at AST node %d", 
                        atom->node_id);
             }


	     break;
 
        case OUTDEGREE:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             if(atom->value.name != NULL)
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "outdegree(%s)\"]\n", atom->node_id, atom->node_id, 
                        LOCATION_ARGS(atom->location), atom->value.node_id);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"
                        "outdegree: \\n UNDEFINED\"]\n", atom->node_id, 
                        atom->node_id, LOCATION_ARGS(atom->location));
                fprintf(log_file,"Error: Undefined node name at AST node %d", 
                        atom->node_id);
             }


             break;

	case LIST_LENGTH:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */
 
             if(atom->value.list_arg) {
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "List \\n Length\"]\n", atom->node_id, 
                        atom->node_id, LOCATION_ARGS(atom->location));
                fprintf(dot_file,"node%d->node%d[label=\"arg\"]\n", 
                        atom->node_id, next_node_id);
	        pretty_print(atom->value.list_arg, list);
             }
             else {
                fprintf(dot_file,"node%d[shape=plaintext,label=\"%dNULL\"]\n", 
                        next_node_id, next_node_id);  
                fprintf(dot_file,"node%d->node%d[label=\"arg\"]\n",          
                        atom->node_id, next_node_id);                     
                next_node_id += 1;       
             }

		
             break;

	case STRING_LENGTH:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             if(atom->value.str_arg) {
                fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                        "String \\n Length\"]\n", atom->node_id, 
                        atom->node_id, LOCATION_ARGS(atom->location));
                fprintf(dot_file,"node%d->node%d[label=\"arg\"]\n", 
                        atom->node_id, next_node_id);
	        pretty_print(atom->value.str_arg, atom);
             }
             else {
                fprintf(dot_file,"node%d[shape=plaintext,label=\"%dNULL\"]\n", 
                        next_node_id, next_node_id);  
                fprintf(dot_file,"node%d->node%d[label=\"arg\"]\n",          
                        atom->node_id, next_node_id);                     
                next_node_id += 1;       
             }

             break;


	case NEG:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "MINUS\"]\n", atom->node_id, atom->node_id,
                     LOCATION_ARGS(atom->location));

             fprintf(dot_file,"node%d->node%d[label=\"exp\"]\n",          
                        atom->node_id, next_node_id);   
	     pretty_print(atom->value.exp, atom);

             break;

	case ADD:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "+\"]\n", atom->node_id, atom->node_id,
                     LOCATION_ARGS(atom->location));

	     fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;

	case SUBTRACT:

	     atom->node_id = next_node_id;
             next_node_id += 1;

             /* print_location(atom->location); */

             fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "-\"]\n", atom->node_id, atom->node_id,
		     LOCATION_ARGS(atom->location));

	     fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;


	case MULTIPLY:

	     atom->node_id = next_node_id;
             next_node_id += 1;
 
             /* print_location(atom->location); */

             fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "*\"]\n", atom->node_id, atom->node_id,
		     LOCATION_ARGS(atom->location));

	     fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;

	case DIVIDE:

	     atom->node_id = next_node_id;
             next_node_id += 1;
 
             /* print_location(atom->location); */

             fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     "/\"]\n", atom->node_id, atom->node_id,
		     LOCATION_ARGS(atom->location));

	     fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;

	case CONCAT:

	     atom->node_id = next_node_id;
             next_node_id += 1;

             /* print_location(atom->location); */

             fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                     ".\"]\n", atom->node_id, atom->node_id,
                     LOCATION_ARGS(atom->location));

	     fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;

	default: fprintf(log_file,"Unexpected Atomic Expression Type: %d\n",
                         (int)atom->exp_type); 
                 break;

	}
}



void print_procedure(GPProcedure * const proc)
{
     proc->node_id = next_node_id;
     next_node_id += 1;

     /* print_location(proc->location); */

     if(proc->name != NULL)
        fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
               "Procedure \\n Name: %s\"]\n",
               proc->node_id, proc->node_id, 
               LOCATION_ARGS(proc->location), proc->name);
     else {
        fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                "Procedure \\n Name: UNDEFINED\"]\n",
                proc->node_id, proc->node_id, LOCATION_ARGS(proc->location));

        fprintf(log_file,"Error: Undefined procedure name at AST node %d", 
                  proc->node_id);
     }

     pretty_print_list(proc->local_decls, proc, decls);
   
     fprintf(dot_file,"node%d->node%d[label=\"cmd seq\"]\n", proc->node_id,
             next_node_id); 
     pretty_print(proc->cmd_seq, statement);
}



void print_rule(GPRule * const rule)
{
     rule->node_id = next_node_id;
     next_node_id += 1;

     /* print_location(rule->location); */

     if(rule->name != NULL)
        fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                "Rule \\n Name: %s \\n ", rule->node_id, rule->node_id,
                LOCATION_ARGS(rule->location), rule->name);
     else {
        fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                "Rule \\n Name: UNDEFINED\"]\n", rule->node_id, 
                rule->node_id, LOCATION_ARGS(rule->location));
        fprintf(log_file,"Error: Undefined rule name at AST node %d", 
                rule->node_id);       
     }

     if(rule->injective == true) 
          fprintf(dot_file,"Injective\"]\n"); 
     else fprintf(dot_file,"Non-injective\"]\n");	

     pretty_print_list(rule->variables, rule, variables);

     fprintf(dot_file,"node%d->node%d[label=\"lhs\"]\n", 
             rule->node_id, next_node_id); 
     pretty_print(rule->lhs, graph);

     fprintf(dot_file,"node%d->node%d[label=\"rhs\"]\n", 
             rule->node_id, next_node_id); 
     pretty_print(rule->rhs, graph);

     pretty_print_list(rule->interface, rule, interface);

     
     /* Same code as the pretty_print_list macro, except this fragment needs to
      * call print_condition instead of print_list.
      */
     if(rule->condition == NULL) {                                         
        fprintf(dot_file,"node%d[shape=plaintext,label=\"%d NULL\"]\n", 
                next_node_id, next_node_id);                            
        fprintf(dot_file,"node%d->node%d[label=\"condition\"]\n",            
                rule->node_id, next_node_id);                           
        next_node_id += 1;                                              
     }							          
     else {                                                            
        fprintf(dot_file,"node%d->node%d[label=\"condition\"]\n",            
                rule->node_id, next_node_id);                           
        print_condition(rule->condition);                                        
     }  
}



void print_graph(GPGraph * const graph)
{
     graph->node_id = next_node_id;
     next_node_id += 1;

     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n Graph\"]\n",
             graph->node_id, graph->node_id, LOCATION_ARGS(graph->location));

     fprintf(dot_file,"node%d->node%d[label=\"position\"]\n", 
             graph->node_id, next_node_id); 

     pretty_print(graph->position, position);

     pretty_print_list(graph->nodes, graph, nodes);

     pretty_print_list(graph->edges, graph, edges);
}



void print_node(GPNode * const node)
{
     node->node_id = next_node_id;
     next_node_id += 1;

     /* print_location(node->location); */

     if(node->name != NULL)
        fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
               "Node \\n Name: %s", node->node_id, node->node_id, 
                LOCATION_ARGS(node->location), node->name);
     else {
        fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                "Node \\n Name: UNDEFINED \\n", node->node_id,
                node->node_id, LOCATION_ARGS(node->location));
        fprintf(log_file,"Error: Undefined node name at AST node %d", 
                node->node_id);

     }

     if(node->root == true) 
          fprintf(dot_file," \\n Root\"]\n"); 
     else fprintf(dot_file,"\"]\n");	
     
     fprintf(dot_file,"node%d->node%d[label=\"label\"]\n", 
             node->node_id, next_node_id); 
     pretty_print(node->label, label);

     fprintf(dot_file,"node%d->node%d[label=\"position\"]\n", 
             node->node_id, next_node_id); 
     pretty_print(node->position, position);
}



void print_edge(GPEdge * const edge)
{
     edge->node_id = next_node_id;
     next_node_id += 1;

     /* print_location(edge->location); */

     if(edge->name != NULL)
        fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                "Edge \\n Name: %s \\n ", edge->node_id, edge->node_id, 
                LOCATION_ARGS(edge->location), edge->name);
     else {
        fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
                "Edge \\n Name: UNDEFINED \\n ", edge->node_id,
                edge->node_id, LOCATION_ARGS(edge->location));
        fprintf(log_file,"Error: Undefined edge name at AST node %d", 
                edge->node_id);

     }

     if(edge->source != NULL)
        fprintf(dot_file,"Source: %s \\n ", edge->source);
     else {
        fprintf(log_file,"Error: Undefined edge source at AST node %d", 
                edge->node_id);
        fprintf(dot_file,"Source: UNDEFINED \\n ");
     }

     if(edge->target != NULL)
        fprintf(dot_file,"Target: %s\"]\n", edge->target);
     else {
        fprintf(log_file,"Error: Undefined edge target at AST node %d", 
                edge->node_id);
        fprintf(dot_file,"Target: UNDEFINED \"]\n");
     }

     fprintf(dot_file,"node%d->node%d[label=\"label\"]\n", edge->node_id, 
             next_node_id); 
     pretty_print(edge->label, label);
}


void print_position(GPPos * const pos)
{
     pos->node_id = next_node_id;
     next_node_id += 1;

     /* print_location(pos->location); */

     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n"
             "Position \\n x: %d \\n y: %d\"]\n", pos->node_id, pos->node_id,
             LOCATION_ARGS(pos->location), pos->x, pos->y);
}


void print_label(GPLabel * const label)
{
     label->node_id = next_node_id;
     next_node_id += 1;

     /* print_location(label->location); */

     fprintf(dot_file,"node%d[label=\"%d\\n%d.%d-%d.%d\\n Label \\n Mark: ", 
             label->node_id, label->node_id, LOCATION_ARGS(label->location));

     switch (label->mark) {

        case (RED):	 fprintf(dot_file,"Red\"]\n"); break;
        case (GREEN): 	 fprintf(dot_file,"Green\"]\n"); break;
        case (BLUE): 	 fprintf(dot_file,"Blue\"]\n"); break;
        case (GREY): 	 fprintf(dot_file,"Grey\"]\n"); break;
        case (DASHED): 	 fprintf(dot_file,"Dashed\"]\n"); break;
        case (NONE): 	 fprintf(dot_file,"No mark\"]\n"); break;

        default: fprintf(log_file,"Error: Unexpected \\n GPLabel mark: %d\"]\n", 
                         (int)label->mark); 
                 break;
     }

     fprintf(dot_file,"node%d->node%d[label=\"gp list\"]\n", label->node_id,
             next_node_id); 
     pretty_print(label->gp_list, list);
}



