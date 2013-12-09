/*//////////////////////////////////////////////////////////////////////////// 

                           pretty.c       
                              
              Pretty printers for AST and symbol table

                  Created on 18/9/2013 by Chris Bak 

/////////////////////////////////////////////////////////////////////////// */ 
 
#include <stdio.h> /* fprintf, fopen, fclose */
#include <string.h> /* strlen, strcpy, strcat */
#include <glib.h> /* GHashTable and GSList */ 
#include "pretty.h" /* function prototypes */
#include "seman.h" /* struct Symbol */

void print_location(YYLTYPE const loc)
{
     printf("%d.%d-%d.%d\n", loc.first_line, loc.first_column, loc.last_line, loc.last_column);
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
	   printf("Name: %s\nType: %s\nScope: %s\n", (char*)key, 
	          current_sym->type, current_sym->scope);
	   if(current_sym->is_var) printf("Variable\n");
	   if(current_sym->in_lhs) printf("In LHS\n");
	   printf("\n");
	}	
	else {	
           printf("Name: %s\nType: %s\nScope: %s\nContaining Rule: %s\n", 
		  (char*)key, current_sym->type, current_sym->scope,
	           current_sym->containing_rule);
       	   if(current_sym->is_var) printf("Variable\n");
	   if(current_sym->in_lhs) printf("In LHS\n");
	   printf("\n");
	}
    }
}
       
/* print_symbol_table uses glib's hash table iterator to print the table.
 * table is the hash table to print.
 * print_symbol is the function called for each value (GSList) encountered.
 * The third argument is passed to the print_symbol, but this is not needed here.
 */

void print_symbol_table(GHashTable *table) 
{
   printf("\n\n# Symbol Table #\n\n");	
   g_hash_table_foreach(table, print_symbol, NULL);
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
FILE *dot_file; 

int print_dot_ast(List *const gp_ast, char* file_name)
{
 
     /* Assumes input file has no extension, but will be .gpx in the future. */

     /* The length of the new file name is the length of the old file name
      * plus 4 for ".dot" plus 1 for the terminating null character.         */

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
     print_list(gp_ast);

     fprintf(dot_file,"}\n\n");

     fclose(dot_file);

     return 1;
}

/* Two macros used in the printing functions are defined below. 
 *
 * The first handles cases where a NULL pointer is unexpected and hence 
 * an error.
 *
 * The second handles cases where a NULL pointer is expected. These are
 * almost always struct Lists, hence it calls print_list without an extra 
 * argument to specify the print function.
 */


/* pretty_print is shorthand for a function that calls the appropriate print
 * function if the first argument is not a null pointer. 
 *
 * POINTER_ARG is a pointer member of the current structure pointing to
 * an AST node we wish to print.
 *
 * TYPE corresponds to the print_ functions in this file. For example, calling
 * pretty_print with second argument 'list' will call print_list on POINTER_ARG
 * if POINTER_ARG is not NULL. Otherwise an error node is created in the 
 * appropriate place and an error message is printed to stderr. 
 *
 * Be aware that pretty_print does not write an edge to point to any nodes
 * written by pretty_print. The edges must be explicitly written directly 
 * before calling this macro.
 */ 

#define pretty_print(POINTER_ARG,TYPE)                                      \
  do { 									    \
       if(POINTER_ARG != NULL) print_ ## TYPE (POINTER_ARG);                \
       else {                                                               \
         fprintf(dot_file,"node%d[shape=plaintext,label=\"%d ERROR\"]\n",   \
                 next_node_id, next_node_id);                               \
         fprintf(stderr, "Error: Unexpected NULL pointer at AST node %d\n", \
                 next_node_id);                                             \
       }            							    \
     }                                                                      \
  while (0)


/* pretty_print_list is used to process members of AST structs that point
 * to a struct List. It should only be used when a NULL pointer is valid in
 * the GP AST. For example, this macro is called to print the node 
 * and edge lists of a graph, which may be NULL, but not for the list
 * component of a label, which should not point to NULL. Use print_list
 * or pretty_print for the latter cases.
 * 
 * The NODE_TYPE parameter is the name of the argument of the calling function:
 * the name of the structure that stores the node_id.
 *
 * If POINTER_ARG is NULL, a NULL node is written to the .dot file and an edge
 * is created from the current node to the NULL node with the label EDGE_LABEL. 
 * Note that stringification is used to print EDGE_LABEL (#EDGE_LABEL is 
 * converted to the parameter enclosed by double quotes)
 *
 * Otherwise an edge is written with label EDGE_LABEL, pointing from the 
 * current node to the node that will be created by the print_list call 
 * immediately following this edge creation. The use of the global node counter
 * next_node_id ensures that the edge points to the correct node.
 */

#define pretty_print_list(POINTER_ARG, NODE_TYPE, EDGE_LABEL)             \
   do {                                                                   \
        if(POINTER_ARG == NULL) {                                         \
          fprintf(dot_file,"node%d[shape=plaintext,label=\"%d NULL\"]\n", \
                  next_node_id, next_node_id);                            \
          fprintf(dot_file,"node%d->node%d[label=\"" #EDGE_LABEL "\"]\n", \
                  NODE_TYPE->node_id, next_node_id);                      \
          next_node_id += 1;                                              \
        }							          \
        else {                                                            \
          fprintf(dot_file,"node%d->node%d[label=\"" #EDGE_LABEL "\"]\n", \
                  NODE_TYPE->node_id, next_node_id);                      \
          print_list(POINTER_ARG);                                        \
        }                                                                 \
      } 			                                          \
    while (0)

/* The use of do while loops is a C trick to enable these macros to be called
 * with a terminating semicolon as per an actual function call. This would not
 * be possible with a normal code block as they are terminated by a right brace.
 */


/* print_list is a recursive function that prints the nodes and edges
 * of its AST argument to the .dot file created by print_dot_ast.
 *
 * Unique node names are generated with the global variable next_node_id. 
 * A new AST node is reached whenever a print_X function is called through the 
 * pretty_print macros. Hence each print_X function will assign next_node_id
 * to the node_id of the current AST node and increment next_node_id.
 */

void print_list(List * const list)
{

     switch(list->list_type) {

	case GLOBAL_DECLARATIONS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d Global \\n " 
		     "Declarations\"]\n", list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.declaration, declaration);
             pretty_print_list(list->next,list,next);

	     break;	


	case LOCAL_DECLARATIONS:

	     list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d Local \\n " 
	             "Declarations\"]\n", list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  		

	     pretty_print(list->value.declaration, declaration);
             pretty_print_list(list->next,list,next);

	     break;	


	case COMMANDS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d Commands\"]\n",
                     list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.command, statement);
             pretty_print_list(list->next,list,next);

	     break;	


	case RULES:

             list->node_id = next_node_id;
             next_node_id += 1;

	     if(list->value.rule_name != NULL)
                fprintf(dot_file,"node%d[shape=box,label=\"%d Rule \\n " 
		        "Name: %s\"]\n",  list->node_id, list->node_id,
		        list->value.rule_name);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"Rule \\n " 
		        "Name: UNDEFINED\"]\n", list->node_id);
                fprintf(stderr,"Error: Undefined rule name at AST node %d", 
                        list->node_id);
             }

             pretty_print_list(list->next,list,next);

	     break;
	

	case INT_DECLARATIONS:
 
             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d Integer \\n " 
		     "Declarations\"]\n", list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.variables, list);
             pretty_print_list(list->next,list,next);

	     break;
	

	case STRING_DECLARATIONS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d String \\n " 
	             "Declarations\"]\n", list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.variables, list);
             pretty_print_list(list->next,list,next);
	     
	     break;
	

	case ATOM_DECLARATIONS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d Atom \\n " 
		     "Declarations\"]\n", list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.variables, list);
             pretty_print_list(list->next,list,next);

	     break;
	

	case LIST_DECLARATIONS:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d List \\n " 
		     "Declarations\"]\n", list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.variables, list);
             pretty_print_list(list->next,list,next);

	     break;
	

	case VARIABLE_LIST:

             list->node_id = next_node_id;
             next_node_id += 1;

	     if(list->value.variable_name != NULL)
                fprintf(dot_file,"node%d[shape=box,label=\"%d Variable \\n " 
		        "Name: %s\"]\n", list->node_id, list->node_id, 
			list->value.variable_name);
             else fprintf(stderr,"Error: Undefined variable name at AST node %d", 
                          list->node_id);

             pretty_print_list(list->next,list,next);

	     break;
	

	case INTERFACE_LIST:

             list->node_id = next_node_id;
             next_node_id += 1; 

	     if(list->value.node_id != NULL)
                fprintf(dot_file,"node%d[shape=box,label=\"%d Interface \\n " 
			"Node: %s\"]\n", list->node_id, list->node_id, 
			list->value.node_id);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"Rule \\n " 
			"Name: UNDEFINED\"]\n", list->node_id);
                fprintf(stderr,"Error: Undefined rule name at AST node %d", 
                        list->node_id);
             }

             pretty_print_list(list->next,list,next);

	     break;
	

	case NODE_LIST:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d Nodes\"]\n",
                     list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.node, node);
             pretty_print_list(list->next,list,next);

	     break;
	

	case EDGE_LIST:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d Edges\"]\n",
                     list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id);  

	     pretty_print(list->value.edge, edge);
             pretty_print_list(list->next,list,next);

	     break;

	
	

	case GP_LIST:

             list->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d GP List\"]\n",
                     list->node_id, list->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"value\"]\n",  
                     list->node_id, next_node_id); 

	     pretty_print(list->value.atom, atom);
             pretty_print_list(list->next,list,next);

	     break;
	

	default: fprintf(stderr,"Unexpected List Type: %d\n",
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

	     fprintf(dot_file,"node%d[label=\"%d Main\"]\n",
                     decl->node_id, decl->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"main \\n program\"]\n",  
                     decl->node_id, next_node_id); 

	     pretty_print(decl->value.main_program, statement);

	     break;

	case PROCEDURE_DECLARATION:

             decl->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(decl->location); */

             fprintf(dot_file,"node%d[label=\"%d Procedure \\n Declaration\"]\n",
                     decl->node_id, decl->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"proc\"]\n",  
                     decl->node_id, next_node_id); 

	     pretty_print(decl->value.procedure, procedure);

	     break;

	case RULE_DECLARATION:

             decl->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(decl->location); */

	     fprintf(dot_file,"node%d[label=\"%d Rule \\n Declaration\"]\n",
                     decl->node_id, decl->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"rule\"]\n",  
                     decl->node_id, next_node_id); 

	     pretty_print(decl->value.rule, rule);

	     break;

	default: fprintf(stderr,"Unexpected Declaration Type: %d\n",
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

             fprintf(dot_file,"node%d[label=\"%d Command \\n Sequence\"]\n", 
                     stmt->node_id, stmt->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"cmd_seq\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.cmd_seq, list);

	     break;

	case RULE_CALL:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     if(stmt->value.rule_name != NULL)
                fprintf(dot_file,"node%d[label=\"%d Rule Call \\n Name: %s\"]\n",
                        stmt->node_id, stmt->node_id, stmt->value.rule_name);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d Rule \\n Name: UNDEFINED\"]\n",
                        stmt->node_id, stmt->node_id);
                fprintf(stderr,"Error: Undefined rule name at AST node %d", 
                        stmt->node_id);
             }

	     break;

	case RULE_SET_CALL:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d Rule Set Call\"]\n", 
                     stmt->node_id, stmt->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"rule set\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.rule_set, list);

	     break;

	case PROCEDURE_CALL:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     if(stmt->value.proc_name != NULL)
                fprintf(dot_file,"node%d[label=\"%d Procedure Call \\n Name: %s\"]\n",
                        stmt->node_id, stmt->node_id, stmt->value.proc_name);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d Procedure \\n Name: UNDEFINED\"]\n",
                        stmt->node_id, stmt->node_id);
                fprintf(stderr,"Error: Undefined procedure name at AST node %d", 
                        stmt->node_id);
             }

	     break;

	case IF_STATEMENT:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d If Statement\"]\n", 
                     stmt->node_id, stmt->node_id);

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

	     fprintf(dot_file,"node%d[label=\"%d Try Statement\"]\n", 
                     stmt->node_id, stmt->node_id);

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

	     fprintf(dot_file,"node%d[label=\"%d ALAP Statement\"]\n", 
                     stmt->node_id, stmt->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"loop \\n statement\"]\n",  
                     stmt->node_id, next_node_id); 

	     pretty_print(stmt->value.loop_stmt, statement);
             
	     break;

	case PROGRAM_OR:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d OR Statement\"]\n", 
                     stmt->node_id, stmt->node_id);

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

	     fprintf(dot_file,"node%d[label=\"%d skip\"]\n", 
                     stmt->node_id, stmt->node_id);

	     break;

	case FAIL_STATEMENT:

             stmt->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(stmt->location); */

	     fprintf(dot_file,"node%d[label=\"%d fail\"]\n", 
                     stmt->node_id, stmt->node_id);

	     break;
	
	default: fprintf(stderr,"Unexpected Statement Type: %d\n",
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
                fprintf(dot_file,"node%d[label=\"%d int check \\n Variable: %s\"]\n",
                        cond->node_id, cond->node_id, cond->value.var);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d Variable: \\n UNDEFINED\"]\n",
                        cond->node_id, cond->node_id);
                fprintf(stderr,"Error: Undefined variable name at AST node %d", 
                        cond->node_id);
             }

             break;

	case STRING_CHECK:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

	     if(cond->value.var != NULL)
                fprintf(dot_file,"node%d[label=\"%d string check \\n Variable: %s\"]\n",
                        cond->node_id, cond->node_id, cond->value.var);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d Variable: \\n UNDEFINED\"]\n",
                        cond->node_id, cond->node_id);
                fprintf(stderr,"Error: Undefined variable name at AST node %d", 
                        cond->node_id);
             }

             break;

	case ATOM_CHECK:

	     cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

	     if(cond->value.var != NULL)
                fprintf(dot_file,"node%d[label=\"%d atom check \\n Variable: %s\"]\n",
                        cond->node_id, cond->node_id, cond->value.var);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d Variable: \\n UNDEFINED\"]\n",
                        cond->node_id, cond->node_id);
                fprintf(stderr,"Error: Undefined variable name at AST node %d", 
                        cond->node_id);
             }

             break;

	case EDGE_PRED:

	     cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */
        
	     fprintf(dot_file,"node%d[label=\"%d Edge Test \\n ",
	             cond->node_id, cond->node_id);

             if(cond->value.edge_pred.source != NULL)
                 fprintf(dot_file,"Source: %s \\n ", cond->value.edge_pred.source);
             else {
                 fprintf(stderr,"Error: Undefined node at AST node %d", 
                         cond->node_id);
                 fprintf(dot_file,"Source: ERROR \\n ");
             }

             if(cond->value.edge_pred.target != NULL)
                 fprintf(dot_file,"Target: %s\"]\n ", cond->value.edge_pred.target);
             else {
                 fprintf(stderr,"Error: Undefined node at AST node %d", 
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
                fprintf(dot_file,"node%d->node%d[label=\"label \\n argument\"]\n",          
                        cond->node_id, next_node_id);                     
                next_node_id += 1;       
             }
                                         
             break;


	case EQUAL:

             cond->node_id = next_node_id;
             next_node_id += 1;

	     fprintf(dot_file,"node%d[shape=box,label=\"%d =\"]\n",
                     cond->node_id, cond->node_id);

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

	     fprintf(dot_file,"node%d[shape=box,label=\"%d !=\"]\n",
                     cond->node_id, cond->node_id);

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

	     fprintf(dot_file,"node%d[shape=box,label=\"%d >\"]\n",
                     cond->node_id, cond->node_id);

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

	     fprintf(dot_file,"node%d[shape=box,label=\"%d >=\"]\n",
                     cond->node_id, cond->node_id);

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

	     fprintf(dot_file,"node%d[shape=box,label=\"%d <\"]\n",
                     cond->node_id, cond->node_id);

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

	     fprintf(dot_file,"node%d[shape=box,label=\"%d <=\"]\n",
                     cond->node_id, cond->node_id);

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

	     fprintf(dot_file,"node%d[label=\"%d NOT\"]\n", 
                     cond->node_id, cond->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"not exp\"]\n",  
                     cond->node_id, next_node_id);

	     pretty_print(cond->value.not_exp, condition);

	     break;

	case BOOL_OR:

	     cond->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(cond->location); */

	     fprintf(dot_file,"node%d[label=\"%d OR\"]\n", 
                     cond->node_id, cond->node_id);

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

	     fprintf(dot_file,"node%d[label=\"%d AND\"]\n", 
                     cond->node_id, cond->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",  
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.bin_exp.left_exp, condition);

             fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",  
                     cond->node_id, next_node_id); 
	     pretty_print(cond->value.bin_exp.right_exp, condition);

	     break;

	default: fprintf(stderr,"Unexpected Condition Type: %d\n",
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

             fprintf(dot_file,"node%d[label=\"%d EMPTY\"]\n", 
                     atom->node_id, atom->node_id);

             break;

	case VARIABLE:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */	

             if(atom->value.name != NULL)
                fprintf(dot_file,"node%d[label=\"%d Variable: %s\"]\n",
                        atom->node_id, atom->node_id, atom->value.name);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d Variable: \\n UNDEFINED\"]\n",
                        atom->node_id, atom->node_id);
                fprintf(stderr,"Error: Undefined variable name at AST node %d", 
                        atom->node_id);
             }

             break;

	case INT_CONSTANT:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             fprintf(dot_file,"node%d[label=\"%d Number: %d\"]\n",
                     atom->node_id, atom->node_id, atom->value.number);

             break;
          
	case STRING_CONSTANT:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             if(atom->value.name != NULL)
                fprintf(dot_file,"node%d[label=\"%d String: %s\"]\n",
                        atom->node_id, atom->node_id, atom->value.string);
             else {
                fprintf(dot_file,"node%d[label=\"%d String: UNDEFINED\"]\n",
                        atom->node_id, atom->node_id);
                fprintf(stderr,"Error: Undefined string at AST node %d", 
                          atom->node_id);
             }

             break;

	case INDEGREE:
	
	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             if(atom->value.name != NULL)
                fprintf(dot_file,"node%d[label=\"%d indegree(%s)\"]\n",
                       atom->node_id, atom->node_id, atom->value.node_id);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d indegree: \\n UNDEFINED\"]\n",
                        atom->node_id, atom->node_id);
                fprintf(stderr,"Error: Undefined node name at AST node %d", 
                        atom->node_id);
             }


	     break;
 
        case OUTDEGREE:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             if(atom->value.name != NULL)
                fprintf(dot_file,"node%d[label=\"%d outdegree(%s)\"]\n",
                        atom->node_id, atom->node_id, atom->value.node_id);
             else {
                fprintf(dot_file,"node%d[shape=box,label=\"%d outdegree: \\n UNDEFINED\"]\n",
                        atom->node_id, atom->node_id);
                fprintf(stderr,"Error: Undefined node name at AST node %d", 
                        atom->node_id);
             }


             break;

	case LIST_LENGTH:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */
 
             if(atom->value.list_arg) {
                fprintf(dot_file,"node%d[label=\"%d List \\n Length\"]\n", 
                        atom->node_id, atom->node_id);
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
                fprintf(dot_file,"node%d[label=\"%d String \\n Length\"]\n", 
                        atom->node_id, atom->node_id);
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

	     fprintf(dot_file,"node%d[label=\"%d MINUS\"]\n", 
                     atom->node_id, atom->node_id);

             fprintf(dot_file,"node%d->node%d[label=\"exp\"]\n",          
                        atom->node_id, next_node_id);   
	     pretty_print(atom->value.exp, atom);

             break;

	case ADD:

	     atom->node_id = next_node_id;
             next_node_id += 1;

	     /* print_location(atom->location); */

             fprintf(dot_file,"node%d[label=\"%d +\"]\n", 
                     atom->node_id, atom->node_id);

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

             fprintf(dot_file,"node%d[label=\"%d -\"]\n", 
                     atom->node_id, atom->node_id);

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

             fprintf(dot_file,"node%d[label=\"%d *\"]\n", 
                     atom->node_id, atom->node_id);

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

             fprintf(dot_file,"node%d[label=\"%d /\"]\n", 
                     atom->node_id, atom->node_id);

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

             fprintf(dot_file,"node%d[label=\"%d .\"]\n", 
                     atom->node_id, atom->node_id);

	     fprintf(dot_file,"node%d->node%d[label=\"left exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     fprintf(dot_file,"node%d->node%d[label=\"right exp\"]\n",          
                     atom->node_id, next_node_id); 
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;

	default: fprintf(stderr,"Unexpected Atomic Expression Type: %d\n",
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
        fprintf(dot_file,"node%d[label=\"%d Procedure \\n Name: %s\"]\n",
               proc->node_id, proc->node_id, proc->name);
     else {
        fprintf(dot_file,"node%d[label=\"%d Procedure \\n Name: UNDEFINED\"]\n",
                proc->node_id, proc->node_id);
        fprintf(stderr,"Error: Undefined procedure name at AST node %d", 
                  proc->node_id);
     }

     pretty_print_list(proc->local_decls, proc, decls);
   
     fprintf(dot_file,"node%d->node%d[label=\"cmd seq\"]\n", proc->node_id, next_node_id); 
     pretty_print(proc->cmd_seq, statement);
}



void print_rule(GPRule * const rule)
{
     rule->node_id = next_node_id;
     next_node_id += 1;

     /* print_location(rule->location); */

     if(rule->name != NULL)
        fprintf(dot_file,"node%d[label=\"%d Rule \\n Name: %s \\n ",
                rule->node_id, rule->node_id, rule->name);
     else {
        fprintf(dot_file,"node%d[label=\"%d Rule \\n Name: UNDEFINED\"]\n",
                rule->node_id, rule->node_id);
        fprintf(stderr,"Error: Undefined rule name at AST node %d", 
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

     /* print_location(graph->location); */

     fprintf(dot_file,"node%d[label=\"%d Graph\"]\n", 
             graph->node_id, graph->node_id);

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
        fprintf(dot_file,"node%d[label=\"%d Node \\n Name: %s",
               node->node_id, node->node_id, node->name);
     else {
        fprintf(stderr,"Error: Undefined node name at AST node %d", 
                node->node_id);
        fprintf(dot_file,"node%d[label=\"Node \\n Name: ERROR \\n ",
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
        fprintf(dot_file,"node%d[label=\"%d Edge \\n Name: %s \\n ",
               edge->node_id, edge->node_id, edge->name);
     else {
        fprintf(stderr,"Error: Undefined edge name at AST node %d", 
                edge->node_id);
        fprintf(dot_file,"node%d[label=\"Edge \\n Name: ERROR \\n ",
                edge->node_id);
     }

     if(edge->source != NULL)
        fprintf(dot_file,"Source: %s \\n ", edge->source);
     else {
        fprintf(stderr,"Error: Undefined edge source at AST node %d", 
                edge->node_id);
        fprintf(dot_file,"Source: ERROR \\n ");
     }

     if(edge->target != NULL)
        fprintf(dot_file,"Target: %s\"]\n", edge->target);
     else {
        fprintf(stderr,"Error: Undefined edge target at AST node %d", 
                edge->node_id);
        fprintf(dot_file,"Target: ERROR\"]\n");
     }

     fprintf(dot_file,"node%d->node%d[label=\"label\"]\n", edge->node_id, next_node_id); 
     pretty_print(edge->label, label);
}


void print_position(GPPos * const pos)
{
     pos->node_id = next_node_id;
     next_node_id += 1;

     /* print_location(pos->location); */

     fprintf(dot_file,"node%d[label=\"%d Position \\n x: %d \\n y: %d\"]\n",
             pos->node_id, pos->node_id, pos->x, pos->y);
}


void print_label(GPLabel * const label)
{
     label->node_id = next_node_id;
     next_node_id += 1;

     /* print_location(label->location); */

     fprintf(dot_file,"node%d[label=\"%d Label \\n Mark: ", 
             label->node_id, label->node_id);

     switch (label->mark) {

        case (RED):	 fprintf(dot_file,"Red\"]\n"); break;
        case (GREEN): 	 fprintf(dot_file,"Green\"]\n"); break;
        case (BLUE): 	 fprintf(dot_file,"Blue\"]\n"); break;
        case (GREY): 	 fprintf(dot_file,"Grey\"]\n"); break;
        case (DASHED): 	 fprintf(dot_file,"Dashed\"]\n"); break;
        case (NONE): 	 fprintf(dot_file,"No mark\"]\n"); break;

        default: fprintf(stderr,"Error: Unexpected \\n GPLabel mark: %d\"]\n", 
                         (int)label->mark); 
                 break;
     }

     fprintf(dot_file,"node%d->node%d[label=\"gp list\"]\n", label->node_id, next_node_id); 
     pretty_print(label->gp_list, list);
}



