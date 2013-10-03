/*//////////////////////////////////////////////////////////////////////////// 

                           pretty.c       
                              
              Pretty printers for AST and symbol table

                  Created on 18/9/2013 by Chris Bak 

/////////////////////////////////////////////////////////////////////////// */ 
 
#include <stdio.h> /* printf */
#include "pretty.h" /* Function prototypes */

/* The macro pretty_print is shorthand for a function that calls the 
 * appropriate print function if the first argument is not a null pointer.
 *
 * POINTER_ARG is a pointer member of the current structure.
 *
 * TYPE corresponds to the print_ functions in this file. For example, calling
 * pretty_print with second argument 'list' will call print_list on POINTER_ARG
 * if POINTER_ARG is not NULL.
 */ 

#define pretty_print(POINTER_ARG,TYPE) if(POINTER_ARG != NULL) print_ ## TYPE (POINTER_ARG)


void print_location(YYLTYPE const loc)
{
     printf("%d.%d-%d.%d\n", loc.first_line, loc.first_column, loc.last_line, loc.last_column);
}

/* print_list is a recursive function that pretty prints an AST.
 * Argument: a pointer to an AST node of type struct List.
 *
 * It prints the node type of its argument, followed by a depth-first
 * print of its children which are determined by the node type. 
 *
 * The function also outputs additional semantic information in the AST:
 * rule names, procedure names, variable names, integer values, constant
 * values, node names, edge names, root node flag and injective matching
 * flag.
 *
 *
 * Similar pretty printing functions are defined for each AST struct.
 */ 

void print_list(List const * const list)
{
     switch(list->list_type) {

	case GLOBAL_DECLARATIONS:

	     printf("List node: Global Declaration\n\n");

	     pretty_print(list->value.decl, declaration);
             pretty_print(list->next, list);

	     break;	


	case LOCAL_DECLARATIONS:

	     printf("List node: Local Declaration\n\n");

	     pretty_print(list->value.decl, declaration);
             pretty_print(list->next, list);

	     break;	


	case COMMANDS:

	     printf("List node: Command\n\n");

	     pretty_print(list->value.command, statement);
             pretty_print(list->next, list);

	     break;	


	case RULES:

             if(list->value.rule_name)
                printf("List node: Rule Set\nName: %s\n\n", list->value.rule_name);

             pretty_print(list->next, list);

	     break;
	

	case INT_DECLARATIONS:

	     printf("List node: Integer Variable\n\n");

	     pretty_print(list->value.vars, list);
             pretty_print(list->next, list);

	     break;
	

	case STRING_DECLARATIONS:

	     printf("List node: String Variable\n\n");

	     pretty_print(list->value.vars, list);
             pretty_print(list->next, list);
	     
	     break;
	

	case ATOM_DECLARATIONS:

	     printf("List node: Atom Variable\n\n");

	     pretty_print(list->value.vars, list);
             pretty_print(list->next, list);

	     break;
	

	case LIST_DECLARATIONS:

	     printf("List node: List Variable\n\n");

	     pretty_print(list->value.vars, list);
             pretty_print(list->next, list);

	     break;
	

	case VARIABLE_LIST:

             if(list->value.var)
                printf("List node: Variable\nName: %s\n\n", list->value.var);

             pretty_print(list->next, list);

	     break;
	

	case INTERFACE_LIST:

	     printf("List node: Interface\n\n");

	     pretty_print(list->value.node_pair, node_pair);
             pretty_print(list->next, list);

	     break;
	

	case NODE_LIST:

	     printf("List node: Node\n\n");

	     pretty_print(list->value.node, node);
             pretty_print(list->next, list);

	     break;
	

	case EDGE_LIST:

	     printf("List node: Edge\n\n");

	     pretty_print(list->value.edge, edge);
             pretty_print(list->next, list);

	     break;

	
	case EQUAL:

	     printf("List node: =\n\n");

	     pretty_print(list->value.rel_exp, list);
             pretty_print(list->next, list);
	
	     break;	


	case NOT_EQUAL:

	     printf("List node: !=\n\n");

	     pretty_print(list->value.rel_exp, list);
             pretty_print(list->next, list);
	
	     break;
	

	case GREATER:

	     printf("List node: >\n\n");

	     pretty_print(list->value.rel_exp, list);
             pretty_print(list->next, list);

	     break;
	

	case GREATER_EQUAL:

	     printf("List node: >=\n\n");

	     pretty_print(list->value.rel_exp, list);
             pretty_print(list->next, list);

	     break;
	

	case LESS:

	     printf("List node: <\n\n");

	     pretty_print(list->value.rel_exp, list);
             pretty_print(list->next, list);

	     break;
	

	case LESS_EQUAL:

	     printf("List node: <=\n\n");

	     pretty_print(list->value.rel_exp, list);
             pretty_print(list->next, list);

	     break;
	

	case GP_LIST:

	     printf("List node: GP List\n\n");

	     pretty_print(list->value.atom, atom);
             pretty_print(list->next, list);

	     break;
	

	default: printf("Unexpected value.\n"); break;

	}
}



void print_declaration(GPDeclaration const * const decl)
{
     switch(decl->decl_type) {

	case MAIN_DECLARATION:

	     print_location(decl->location);

	     printf("Main\n\n");

	     pretty_print(decl->value.main_program, statement);

	     break;

	case PROCEDURE_DECLARATION:

	     print_location(decl->location);

             printf("Procedure Declaration\n\n");

	     pretty_print(decl->value.proc, procedure);

	     break;

	case RULE_DECLARATION:

	     print_location(decl->location);

	     printf("Rule Declaration\n\n");

	     pretty_print(decl->value.rule, rule);

	     break;

	default: printf("Unexpected value.\n"); break;

	}
}



void print_statement(GPStatement const * const stmt)
{
     switch(stmt->statement_type) {

	case COMMAND_SEQUENCE:	

	     print_location(stmt->location);

             printf("CommandSequence\n\n");

	     pretty_print(stmt->value.cmd_seq, list);

	     break;

	case RULE_CALL:

	     print_location(stmt->location);

	     printf("Rule Call\n");

             if(stmt->value.rule_name) printf("Name: %s\n\n", stmt->value.rule_name);

	     break;

	case RULE_SET_CALL:

	     print_location(stmt->location);

	     printf("Rule Set Call\n\n");

	     pretty_print(stmt->value.rule_set, list);

	     break;

	case PROCEDURE_CALL:

	     print_location(stmt->location);

	     printf("Procedure Call\n\n");

             if(stmt->value.proc_name) printf("Name: %s\n\n", stmt->value.proc_name);

	     break;

	case IF_STATEMENT:

	     print_location(stmt->location);

             printf("If\n\n");
	     pretty_print(stmt->value.cond_branch.condition, statement);
	    
             printf("Then\n\n");
	     pretty_print(stmt->value.cond_branch.then_stmt, statement);
             
             printf("Else\n\n");
	     pretty_print(stmt->value.cond_branch.else_stmt, statement);
             
	     break;

	case TRY_STATEMENT:

	     print_location(stmt->location);

             printf("Try\n\n");
	     pretty_print(stmt->value.cond_branch.condition, statement);
	    
             printf("Then\n\n");
	     pretty_print(stmt->value.cond_branch.then_stmt, statement);
             
             printf("Else\n\n");
	     pretty_print(stmt->value.cond_branch.else_stmt, statement);
	    
	     break;

	case ALAP_STATEMENT:

	     print_location(stmt->location);

	     printf("ALAP\n\n");
	     pretty_print(stmt->value.cond_branch.condition, statement);
             
	     break;

	case PROGRAM_OR:

	     print_location(stmt->location);

             printf("Or\n\n");
 
             printf("First Argument:\n\n");
	     pretty_print(stmt->value.or_stmt.right_stmt, statement);
             
 
             printf("Second Argument:\n\n");
	     pretty_print(stmt->value.or_stmt.right_stmt, statement);

	     break;

	case SKIP_STATEMENT:

	     print_location(stmt->location);

	     printf("skip\n\n");

	     break;

	case FAIL_STATEMENT:

	     print_location(stmt->location);

	     printf("fail\n\n");

	     break;
	
	default: printf("Unexpected value.\n"); break;

	}
}



void print_condition(GPCondExp const * const cond)
{
     switch(cond->exp_type) {

	case INT_CHECK:

	     print_location(cond->location);

	     if(cond->value.var) printf("int(%s)\n\n", cond->value.var);

             break;

	case STRING_CHECK:

	     print_location(cond->location);

	     if(cond->value.var) printf("string(%s)\n\n", cond->value.var);

             break;

	case ATOM_CHECK:

	     print_location(cond->location);

	     if(cond->value.var) printf("atom(%s)\n\n", cond->value.var);

             break;

	case EDGE_PRED:

	     print_location(cond->location);
        
             if(cond->value.edge_pred.source && cond->value.edge_pred.target)
	        printf("edge(%s, %s)\n", cond->value.edge_pred.source, 
                       cond->value.edge_pred.target);

             if(cond->value.edge_pred.label) {
                 printf("Label argument:\n");
	         print_label(cond->value.edge_pred.label);
             }
             else printf("No label argument.\n\n");

             break;

	case REL_EXP:

	     print_location(cond->location);

	     pretty_print(cond->value.rel_exp, list);
	     
             break;

	case BOOL_NOT:

	     print_location(cond->location);

	     printf("NOT\n\n");	

	     pretty_print(cond->value.not_exp, condition);

	     break;

	case BOOL_OR:

	     print_location(cond->location);

             printf("OR\n\n");
    
             printf("First Argument:\n\n");
	     pretty_print(cond->value.bin_exp.left_exp, condition);

             printf("Second Argument:\n\n");      
	     pretty_print(cond->value.bin_exp.right_exp, condition);

	     break;

	case BOOL_AND:

	     print_location(cond->location);

             printf("AND\n\n");

             printf("First Argument:\n\n");
	     pretty_print(cond->value.bin_exp.left_exp, condition);

             printf("Second Argument:\n\n");      
	     pretty_print(cond->value.bin_exp.right_exp, condition);

	     break;

	default: printf("Unexpected value\n"); break;

	}
}



void print_atom(GPAtomicExp const * const atom)
{
     switch(atom->exp_type) {

	case VARIABLE:

	     print_location(atom->location);	
	
             printf("Variable\n");

	     if(atom->value.name) printf("Name: %s\n\n",atom->value.name);

             break;

	case INT_CONSTANT:

	     print_location(atom->location);

             printf("Number\n");

	     printf("Value: %d\n\n", atom->value.num);

             break;
          
	case STRING_CONSTANT:

	     print_location(atom->location);

             printf("String\n");

	     if(atom->value.str) printf("Value: %s\n\n", atom->value.str);

             break;

	case INDEGREE:

	     print_location(atom->location);

             printf("Indegree\n");

	     if(atom->value.node_id) printf("Node: %s\n\n", atom->value.node_id);

	     break;
 
        case OUTDEGREE:

	     print_location(atom->location);

             printf("Outdegree\n");

	     if(atom->value.node_id) printf("Node: %s\n\n", atom->value.node_id);

             break;

	case LIST_LENGTH:

	     print_location(atom->location);
 
             if(atom->value.list_arg) {
                printf("Length:\n\n");
	        print_list(atom->value.list_arg);
             }
             else printf("Error: No list argument.\n\n");
		
             break;

	case STRING_LENGTH:

	     print_location(atom->location);

             if(atom->value.list_arg) {
                printf("Length:\n\n");
	        print_list(atom->value.list_arg);
             }
             else printf("Error: No string argument.\n\n");

             break;

	case NEG:

	     print_location(atom->location);

             printf("Minus\n\n");
	     pretty_print(atom->value.exp, atom);

             break;

	case ADD:

	     print_location(atom->location);

             printf("+\n\n");

	     printf("First argument\n\n");
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     printf("Second argument\n\n");
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;

	case SUBTRACT:

             print_location(atom->location);

             printf("-\n\n");

	     printf("First argument\n\n");
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     printf("Second argument\n\n");
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;


	case MULTIPLY:
 
             print_location(atom->location);

             printf("*\n\n");

	     printf("First argument\n\n");
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     printf("Second argument\n\n");
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;

	case DIVIDE:
 
             print_location(atom->location);

             printf("/\n\n");

	     printf("First argument\n\n");
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     printf("Second argument\n\n");
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;

	case CONCAT:

             print_location(atom->location);

             printf(".\n\n");

	     printf("First argument\n\n");
	     pretty_print(atom->value.bin_op.left_exp, atom);

	     printf("Second argument\n\n");
	     pretty_print(atom->value.bin_op.right_exp, atom);

             break;

	default: printf("Unexpected value.\n"); break;

	}
}



void print_procedure(GPProcedure const * const proc)
{
     print_location(proc->location);

     printf("Procedure\n");

     printf("Name: %s\n\n", proc->name);		

     pretty_print(proc->local_decls, list);

     pretty_print(proc->cmd_seq, statement);
}



void print_rule(GPRule const * const rule)
{
     print_location(rule->location);

     printf("Rule\n");

     if(rule->injective == true) 
          printf("Injective Matching\n"); 
     else printf("Non-injective Matching\n");

     printf("Name: %s\n", rule->name);		

     pretty_print(rule->variables, list);
     pretty_print(rule->lhs, graph);
     pretty_print(rule->rhs, graph);
     pretty_print(rule->interface, list);
     pretty_print(rule->condition, condition);
}



void print_graph(GPGraph const * const graph)
{
     print_location(graph->location);

     printf("Graph\n");

     pretty_print(graph->position, position);

     pretty_print(graph->nodes, list);
     pretty_print(graph->edges, list);
}



void print_node_pair(GPNodePair const * const node_pair)
{
     printf("Node Pair\n");
 
     print_location(node_pair->location);

     if(node_pair->left_node) printf("Left: %s\n", node_pair->left_node);
     if(node_pair->right_node) printf("Right: %s\n", node_pair->right_node);
}



void print_node(GPNode const * const node)
{
     print_location(node->location);

     if(node->name) printf("Node\nName: %s\n", node->name);

     if(node->root == true) printf("Root Node\n");  
     
     pretty_print(node->label, label);

     pretty_print(node->position, position);
}



void print_edge(GPEdge const * const edge)
{
     print_location(edge->location);

     printf("Edge\n");

     if(edge->name) printf("Name: %s\n", edge->name);
     if(edge->source) printf("Source: %s\n", edge->source);
     if(edge->target) printf("Target: %s\n", edge->target);

     pretty_print(edge->label, label);
}


void print_position(GPPos const * const pos)
{
     print_location(pos->location);

     printf("Position\nx coordinate: %d\ny coordinate: %d\n\n", pos->x, pos->y);
}


void print_label(GPLabel const * const label)
{
     print_location(label->location);

     printf("Label\nMark: ");

     switch (label->mark) {
        case (RED):	 printf("Red\n\n"); break;
        case (GREEN): 	 printf("Green\n\n"); break;
        case (BLUE): 	 printf("Blue\n\n"); break;
        case (GREY): 	 printf("Grey\n\n"); break;
        case (DASHED): 	 printf("Dashed\n\n"); break;
        case (NONE): 	 printf("No mark\n\n"); break;
        default: 	 printf("Unexpected value\n\n"); break;
     }

     pretty_print(label->gp_list, list);
}



