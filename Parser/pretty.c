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
     printf("Location: %s: %d.%d-%d.%d\n", file_name, loc.first_line, 
            loc.first_column, loc.last_line, loc.last_column);
}


void print_ast(List *list)
{
     switch(list->list_type) {

	case GLOBAL_DECLS:
	     printf("Global Declaration\n");
	     print_location(list->location);
	     print_declaration(list->value.decl);
             print_ast(list->next);		
	     break;	


	case LOCAL_DECLS:
	     printf("Local Declaration\n");
	     print_location(list->location);
	     print_declaration(list->value.decl);
	     print_ast(list->next);	
	     break;	


	case COMMANDS:
	     printf("Command Sequence\n");
	     print_location(list->location);
	     print_statement(list->value.command);
	     print_ast(list->next);	
	     break;	


	case RULES:
	     printf("Rule Set Call\n");
	     print_location(list->location);
	     print_ast(list->next);	
	     break;
	

	case INT_DECLS:
	     printf("Integer Variables\n");
	     print_location(list->location);
	     print_ast(list->value.vars);
	     print_ast(list->next);	
	     break;
	

	case STRING_DECLS:
	     printf("String Variables\n");
	     print_location(list->location);
	     print_ast(list->value.vars);
	     print_ast(list->next);	
	     break;
	

	case ATOM_DECLS:
	     printf("Atom Variables\n");
	     print_location(list->location);
	     print_ast(list->value.vars);
	     print_ast(list->next);	
	     break;
	

	case LIST_DECLS:
	     printf("List Variables\n");
	     print_location(list->location);
	     print_ast(list->value.vars);
	     print_ast(list->next);	
	     break;
	

	case VARIABLES:
	     print_location(list->location);
	     print_ast(list->next);	
	     break;
	

	case INTERFACE_LIST:
	     printf("Interface\n");
	     print_location(list->location);
	     print_node_pair(list->value.node_pair);
	     print_ast(list->next);	
	     break;
	

	case NODE_LIST:
	     printf("Node\n");
	     print_location(list->location);
	     print_node(list->value.node);
	     print_ast(list->next);	
	     break;
	

	case EDGE_LIST:
	     printf("Edge\n");
	     print_location(list->location);
	     print_edge(list->value.edge);
	     print_ast(list->next);	
	     break;
	

	case NOT_EQUAL:
	     printf("!=\n");
	     print_location(list->location);
	     print_ast(list->value.rel_exp);
	     print_ast(list->next);	
	     break;
	

	case GREATER:
	     printf(">\n");
	     print_location(list->location);
	     print_ast(list->value.rel_exp);
	     print_ast(list->next);	
	     break;
	

	case GREATER_EQUAL:
	     printf(">=\n");
	     print_location(list->location);
	     print_ast(list->value.rel_exp);
	     print_ast(list->next);	
	     break;
	

	case LESS:
	     printf("<\n");
	     print_location(list->location);
	     print_ast(list->value.rel_exp);
	     print_ast(list->next);	
	     break;
	

	case LESS_EQUAL:
	     printf("<\n");
	     print_location(list->location);
	     print_ast(list->value.rel_exp);
	     print_ast(list->next);	
	     break;
	

	case GP_LIST:
	     printf("List\n");
	     print_location(list->location);
	     print_atom(list->value.atom);
	     print_ast(list->next);	
	     break;
	

	default: printf("Unexpected value.\n"); break;
	}
}



void print_declaration(GPDeclaration *decl)
{
     switch(decl->decl_type) {

	case MAIN_DECL:
	     print_location(decl->location);
	     printf("Main: ");
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
	     print_ast(stmt->value.cmd_seq);
	     break;

	case RULE_CALL:
	     print_location(stmt->location);
	     printf("Rule call");
	     break;

	case RULE_SET_CALL:
	     print_location(stmt->location);
	     print_ast(stmt->value.rule_set);
	     break;

	case PROCEDURE_CALL:
	     print_location(stmt->location);
	     printf("Procedure call");
	     break;

	case IF_STMT:
	     print_location(stmt->location);
	     printf("if ");
	     print_statement(stmt->value.cond_branch.condition);
	     printf(" then ");
	     print_statement(stmt->value.cond_branch.then_stmt);
	     printf(" else ");
	     print_statement(stmt->value.cond_branch.else_stmt);
	     break;

	case TRY_STMT:
	     print_location(stmt->location);
	     printf("try ");
	     print_statement(stmt->value.cond_branch.condition);
	     printf(" then ");
	     print_statement(stmt->value.cond_branch.then_stmt);
	     printf(" else ");
	     print_statement(stmt->value.cond_branch.else_stmt);
	     break;

	case ALAP_STMT:
	     print_location(stmt->location);
	     print_statement(stmt->value.loop_stmt);
	     printf("!");
	     break;

	case PROGRAM_OR:
	     print_location(stmt->location);
	     print_statement(stmt->value.or_stmt.left_stmt);
	     printf(" or ");
	     print_statement(stmt->value.or_stmt.right_stmt);
	     break;

	case SKIP_STMT:
	     print_location(stmt->location);
	     printf("skip");
	     break;

	case FAIL_STMT:
	     print_location(stmt->location);
	     printf("fail");
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
	     printf("edge(%s, %s, label)\n", cond->value.edge_pred.source, 
                    cond->value.edge_pred.target);
             printf("Label argument: ");
	     print_label(cond->value.edge_pred.label);
             break;

	case REL_EXP:
	     print_location(cond->location);
	     print_ast(cond->value.rel_exp);
             break;

	case BOOL_NOT:
	     print_location(cond->location);
	     printf("NOT ");	
	     print_condition(cond->value.not_exp);
	     break;

	case BOOL_OR:
	     print_location(cond->location);
	     print_condition(cond->value.bin_exp.left_exp);
	     printf(" OR ");
	     print_condition(cond->value.bin_exp.left_exp);
	     break;

	case BOOL_AND:
	     print_location(cond->location);
	     print_condition(cond->value.bin_exp.left_exp);
	     printf(" AND ");
	     print_condition(cond->value.bin_exp.left_exp);
	     break;

	default: printf("Unexpected value.\n"); break;
	}
}



void print_atom(GPAtomicExp *atom)
{
     switch(atom->exp_type) {

	case VARIABLE:
	     print_location(atom->location);		
             printf("Variable\n");
             break;

	case INT_CONSTANT:
	     print_location(atom->location);
             printf("Number: %d\n", atom->value.num);
             break;
          
	case STRING_CONSTANT:
	     print_location(atom->location);
             printf("String: %s\n", atom->value.str);
             break;

	case INDEGREE:
	     print_location(atom->location);
             printf("Indegree: %s\n", atom->value.node_id);
             break;
 
        case OUTDEGREE:
	     print_location(atom->location);
             printf("Outdegree: %s\n", atom->value.node_id);
             break;

	case LIST_LENGTH:
	     print_location(atom->location);
             printf("Length:\n");
	     print_ast(atom->value.list_arg);
             break;

	case STRING_LENGTH:
	     print_location(atom->location);
             printf("Length:\n");
	     print_atom(atom->value.str_arg);
             break;

	case NEG:
	     print_location(atom->location);
             printf("-");
	     print_atom(atom->value.exp);
             break;

	case ADD:
	     print_location(atom->location);
             print_atom(atom->value.bin_op.left_exp);
             printf("+");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	case SUBTRACT:
	     print_location(atom->location);
             print_atom(atom->value.bin_op.left_exp);
             printf("-");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	case MULTIPLY:
	     print_location(atom->location);
             print_atom(atom->value.bin_op.left_exp);
             printf("*");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	case DIVIDE:
	     print_location(atom->location);
             print_atom(atom->value.bin_op.left_exp);
             printf("/");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	case CONCAT:
	     print_location(atom->location);
             print_atom(atom->value.bin_op.left_exp);
             printf(".");
	     print_atom(atom->value.bin_op.right_exp);
             break;

	default: printf("Unexpected value.\n"); break;
	}
}



void print_procedure(GPProcedure *proc)
{
     printf("Procedure\n");
 
     print_location(proc->location);

     printf("Name: %s\n", proc->name);		

     print_ast(proc->local_decls);
     print_statement(proc->cmd_seq);
}



void print_rule(GPRule *rule)
{
     printf("Rule\n");
 
     print_location(rule->location);

     if(rule->injective) 
          printf("Injective Matching\n"); 
     else printf("Non-injective Matching\n");

     printf("Name: %s\n", rule->name);		

     print_ast(rule->variables);
     print_graph(rule->lhs);
     print_graph(rule->rhs);
     print_ast(rule->interface);
     print_condition(rule->condition);
}



void print_graph(GPGraph *graph)
{
     printf("Graph\n");
 
     print_location(graph->location);

     print_pos(graph->position);
     print_ast(graph->nodes);
     print_ast(graph->edges);
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
     printf("Node\n");
 
     print_location(node->location);

     printf("Name: %s\n", node->name);

     if(node->root) printf("Root Node\n"); 

     print_label(node->label);
     print_pos(node->position);
}



void print_edge(GPEdge *edge)
{
     printf("Edge\n");
 
     print_location(edge->location);

     printf("Name: %s\n", edge->name);
     printf("Source: %s\n", edge->name);
     printf("Target: %s\n", edge->name);
}



void print_label(GPLabel *label)
{
     printf("Label\n");
 
     print_location(label->location);

     switch (label->mark) {
        case (RED):	 printf("red\n"); break;
        case (GREEN): 	 printf("green\n"); break;
        case (BLUE): 	 printf("blue\n"); break;
        case (GREY): 	 printf("grey\n"); break;
        case (DASHED): 	 printf("dashed\n"); break;
        case (NONE): 	 printf("no mark\n"); break;
        default: 	 printf("Unexpected value.\n"); break;
     }

     print_ast(label->gp_list);
}



void print_pos(GPPos *pos)
{
     printf("Position\n");
 
     print_location(pos->location);

     printf("x coordinate: %d \ny coordinate: %d", pos->x, pos->y);
}
