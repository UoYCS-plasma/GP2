/* ///////////////////////////////////////////////////////////////////////////////////////////////// */

/*              pretty.c       
*                                
* Pretty printers for AST and symbol table
*
* Created on 18/9/2013 by Chris Bak 
* 
* ///////////////////////////////////////////////////////////////////////////////////////////////// */

#include <stdio.h>
#include "pretty.h"

void print_location(YYLTYPE loc)
{
     printf("%d.%d-%d.%d\n", loc.first_line, loc.first_column, loc.last_line, loc.last_column);
}


void print_ast(List *list)
{
     switch(list->list_type) {

	case GLOBAL_DECLS:
	     printf("List node: Global Declaration\n\n");
	     print_declaration(list->value.decl);
             if (list->next) {
                print_ast(list->next);	
                break;	
             }
	     break;	


	case LOCAL_DECLS:
	     printf("List node: Local Declaration\n\n");
	     print_declaration(list->value.decl);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;	


	case COMMANDS:
	     printf("List node: Command Sequence\n\n");
	     print_statement(list->value.command);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;	


	case RULES:
             printf("List node: Rule Set\nName: %s\n\n", list->value.rule_name);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case INT_DECLS:
	     printf("List node: Integer Variables\n");
	     print_ast(list->value.vars);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case STRING_DECLS:
	     printf("List node: String Variables\n");
	     print_ast(list->value.vars);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case ATOM_DECLS:
	     printf("List node: Atom Variables\n");
	     print_ast(list->value.vars);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case LIST_DECLS:
	     printf("List node: List Variables\n");
	     print_ast(list->value.vars);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case VARIABLES:
	     printf("List node: Variable\nName: %s\n\n", list->value.var);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case INTERFACE_LIST:
	     printf("List node: Interface\n\n");
	     print_node_pair(list->value.node_pair);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case NODE_LIST:
	     printf("List node: Node\n\n");
	     print_node(list->value.node);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case EDGE_LIST:
	     printf("List node: Edge\n\n");
	     print_edge(list->value.edge);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case NOT_EQUAL:
	     printf("!=\n\n");
	     print_ast(list->value.rel_exp);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case GREATER:
	     printf(">\n\n");
	     print_ast(list->value.rel_exp);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case GREATER_EQUAL:
	     printf(">=\n\n");
	     print_ast(list->value.rel_exp);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case LESS:
	     printf("<\n\n");
	     print_ast(list->value.rel_exp);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }
	     break;
	

	case LESS_EQUAL:
	     printf("<\n\n");
	     print_ast(list->value.rel_exp);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	case GP_LIST:
	     printf("List node: GP List\n\n");
	     print_atom(list->value.atom);
	     if (list->next) {
                print_ast(list->next);	
                break;	
             }	
	     break;
	

	default: printf("Unexpected value.\n"); break;
	}
}



void print_declaration(GPDeclaration *decl)
{
     switch(decl->decl_type) {

	case MAIN_DECL:
	     print_location(decl->location);
	     printf("Main\n\n");
	     print_statement(decl->value.main_program);
	     break;

	case PROCEDURE_DECL:
	     print_location(decl->location);
	     print_procedure(decl->value.proc);
	     break;

	case RULE_DECL:
	     print_location(decl->location);
	     print_rule(decl->value.rule);
	     break;

	default: printf("Unexpected value.\n"); break;
	}
}



void print_statement(GPStatement *stmt)
{
     switch(stmt->statement_type) {

	case COMMAND_SEQUENCE:	
	     print_location(stmt->location);
	     if (stmt->value.cmd_seq) {
                print_ast(stmt->value.cmd_seq);	
                break;	
             }
	     break;

	case RULE_CALL:
	     print_location(stmt->location);
	     printf("Rule Call\n");
             printf("Name: %s\n\n", stmt->value.rule_name);
	     break;

	case RULE_SET_CALL:
	     print_location(stmt->location);
	     printf("Rule Set Call\n");
	     if (stmt->value.rule_set) {
                print_ast(stmt->value.rule_set);
                break;
             }
	     break;

	case PROCEDURE_CALL:
	     print_location(stmt->location);
	     printf("Procedure Call\n");
             printf("Name: %s\n\n", stmt->value.proc_name);
	     break;

	case IF_STMT:
	     print_location(stmt->location);
	     printf("If\n\n");
	     print_statement(stmt->value.cond_branch.condition);
	     printf("Then\n\n");
	     print_statement(stmt->value.cond_branch.then_stmt);
	     printf("Else\n\n");
	     print_statement(stmt->value.cond_branch.else_stmt);
	     break;

	case TRY_STMT:
	     print_location(stmt->location);
	     printf("Try\n\n");
	     print_statement(stmt->value.cond_branch.condition);
	     printf("Then\n\n");
	     print_statement(stmt->value.cond_branch.then_stmt);
	     printf("Else\n\n");
	     print_statement(stmt->value.cond_branch.else_stmt);
	     break;

	case ALAP_STMT:
	     print_location(stmt->location);
             printf("ALAP\n\n");
	     print_statement(stmt->value.loop_stmt);	     
	     break;

	case PROGRAM_OR:
	     print_location(stmt->location);
             printf("Or\n\n");
             printf("First Argument:\n\n");
	     print_statement(stmt->value.or_stmt.left_stmt);
             printf("Second Argument:\n\n");
	     print_statement(stmt->value.or_stmt.right_stmt);
	     break;

	case SKIP_STMT:
	     print_location(stmt->location);
	     printf("Skip\n\n");
	     break;

	case FAIL_STMT:
	     print_location(stmt->location);
	     printf("Fail\n\n");
	     break;
	
	default: printf("Unexpected value.\n"); break;
	}
}



void print_condition(GPCondExp *cond)
{
     switch(cond->exp_type) {

	case INT_CHECK:
	     print_location(cond->location);
	     printf("int( )");
             break;

	case STRING_CHECK:
	     print_location(cond->location);
	     printf("string( )");
             break;

	case ATOM_CHECK:
	     print_location(cond->location);
	     printf("atom( )");
             break;

	case EDGE_PRED:
	     print_location(cond->location);
	     printf("edge(%s, %s)\n", cond->value.edge_pred.source, 
                    cond->value.edge_pred.target);
             if (cond->value.edge_pred.label) {
                 printf("Label argument:\n");
	         print_label(cond->value.edge_pred.label);
             }
             else printf("No label argument.\n\n");
             break;

	case REL_EXP:
	     print_location(cond->location);
	     if (cond->value.rel_exp) {
                 print_ast(cond->value.rel_exp);
		 break;
	     }
             break;

	case BOOL_NOT:
	     print_location(cond->location);
	     printf("NOT\n\n");	
	     if(cond->value.not_exp) print_condition(cond->value.not_exp);
	     break;

	case BOOL_OR:
	     print_location(cond->location);
             printf("OR\n\n");
             printf("First Argument:\n\n");
	     if(cond->value.bin_exp.left_exp) print_condition(cond->value.bin_exp.left_exp);
	     printf("Second Argument:\n\n");      
	     if(cond->value.bin_exp.right_exp) print_condition(cond->value.bin_exp.right_exp);
	     break;

	case BOOL_AND:
	     print_location(cond->location);
             printf("AND\n\n");
	     printf("First Argument:\n\n");
	     if(cond->value.bin_exp.left_exp) print_condition(cond->value.bin_exp.left_exp);
	     printf("Second Argument:\n\n");
	     if(cond->value.bin_exp.right_exp) print_condition(cond->value.bin_exp.right_exp);
	     break;

	default: printf("Unexpected value\n"); break;
	}
}



void print_atom(GPAtomicExp *atom)
{
     switch(atom->exp_type) {

	case VARIABLE:
	     print_location(atom->location);		
             printf("Variable\n\n");
	     printf("Name: %s",atom->value.var);
             break;

	case INT_CONSTANT:
	     print_location(atom->location);
             printf("Number\n");
	     printf("Value: %d\n\n", atom->value.num);
             break;
          
	case STRING_CONSTANT:
	     print_location(atom->location);
             printf("String\n");
	     printf("Value: %s\n\n", atom->value.str);
             break;

	case INDEGREE:
	     print_location(atom->location);
             printf("Indegree\n");
	     printf("Node: %s\n\n", atom->value.node_id);
	     break;
 
        case OUTDEGREE:
	     print_location(atom->location);
             printf("Outdegree\n");
	     printf("Node: %s\n\n", atom->value.node_id);
             break;

	case LIST_LENGTH:
	     print_location(atom->location);
             printf("Length:\n\n");
	     if (atom->value.list_arg) {
		 print_ast(atom->value.list_arg);
		 break;
	     }		
             break;

	case STRING_LENGTH:
	     print_location(atom->location);
             printf("Length:\n");
	     print_atom(atom->value.str_arg);
             break;

	case NEG:
	     print_location(atom->location);
             printf("Unary Minus\n\n");
	     print_atom(atom->value.exp);
             break;

	case ADD:
	     print_location(atom->location);
             printf("+\n\n");
	     printf("First argument\n\n");
             print_atom(atom->value.bin_op.left_exp);
	     printf("Second argument\n\n");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	case SUBTRACT:
             printf("-\n\n");
	     printf("First argument\n\n");
             print_atom(atom->value.bin_op.left_exp);
	     printf("Second argument\n\n");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	case MULTIPLY:
             printf("*\n\n");
	     printf("First argument\n\n");
             print_atom(atom->value.bin_op.left_exp);
	     printf("Second argument\n\n");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	case DIVIDE:
             printf("/\n\n");
	     printf("First argument\n\n");
             print_atom(atom->value.bin_op.left_exp);
	     printf("Second argument\n\n");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	case CONCAT:
             printf(".\n\n");
	     printf("First argument\n\n");
             print_atom(atom->value.bin_op.left_exp);
	     printf("Second argument\n\n");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	default: printf("Unexpected value.\n"); break;
	}
}



void print_procedure(GPProcedure *proc)
{
     print_location(proc->location);

     printf("Procedure\n");

     printf("Name: %s\n", proc->name);		

     if (proc->local_decls) print_ast(proc->local_decls);

     print_statement(proc->cmd_seq);
}



void print_rule(GPRule *rule)
{
     print_location(rule->location);

     printf("Rule\n");

     if(rule->injective) 
          printf("Injective Matching\n"); 
     else printf("Non-injective Matching\n");

     printf("Name: %s\n", rule->name);		

     if (rule->variables) print_ast(rule->variables);
     print_graph(rule->lhs);
     print_graph(rule->rhs);
     if (rule->interface) print_ast(rule->interface);
     if (rule->condition) print_condition(rule->condition);
}



void print_graph(GPGraph *graph)
{
     print_location(graph->location);

     printf("Graph\n");

     print_pos(graph->position);
     if (graph->nodes) print_ast(graph->nodes);
     if (graph->edges) print_ast(graph->edges);
}



void print_node_pair(GPNodePair *node_pair)
{
     printf("Node Pair\n");
 
     print_location(node_pair->location);

     printf("Left: %s\n", node_pair->left_node);
     printf("Right: %s\n", node_pair->right_node);
}



void print_node(GPNode *node)
{
     print_location(node->location);

     printf("Node\nName: %s\n", node->name);

     if(node->root) printf("Root Node\n"); 
 
     print_label(node->label);

     print_pos(node->position);
}



void print_edge(GPEdge *edge)
{
     print_location(edge->location);

     printf("Edge\n");

     printf("Name: %s\n", edge->name);
     printf("Source: %s\n", edge->name);
     printf("Target: %s\n", edge->name);

     print_label(edge->label);
}



void print_label(GPLabel *label)
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

     if (label->gp_list) print_ast(label->gp_list);
}



void print_pos(GPPos *pos)
{
     print_location(pos->location);

     printf("Position\n");

     printf("x coordinate: %d\ny coordinate: %d\n\n", pos->x, pos->y);
}
