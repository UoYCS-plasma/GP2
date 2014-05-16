/*////////////////////////////////////////////////////////////////////////////

                                  ast.c              
                         
              Contains AST constructor definitions and functions to
                                free the AST.

                     Created on 22/7/2013 by Chris Bak 

/////////////////////////////////////////////////////////////////////////// */ 

#include "ast.h" /* AST struct definitions */
#include <stdio.h> /* printf */
#include <stdlib.h> /* malloc */
#include <string.h> /* strdup */

/* Most of the functions in this file create new AST nodes. They are called
 * from the Bison parser (gpparser.y) which provides the appropriate arguments
 * from the semantic values of rules it reduces. The functions just assign
 * the pointers to the corresponding structure fields. 
 *
 * Strings, such as rule names and variable names, are dynamically allocated 
 * with strdup. This is because the pointer passed to the function is freed in 
 * gpparser.y immediately after the constructor call, hence a new allocation is 
 * required to prevent a double free.
 */ 

/* The constructor functions for AST nodes of type struct List. */

List *addDecl (list_t list_type, YYLTYPE location, GPDeclaration *declaration,
	       List *next)
{ 
    List *new_decl = malloc(sizeof(List));
    
    if(new_decl == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_decl->list_type = list_type; 
    /* list_type: GLOBAL_DECLARATIONS, LOCAL_DECLARATIONS */
    new_decl->location = location;
    new_decl->value.declaration = declaration;
    new_decl->next = next;

    return new_decl;
}

List *addCommand (YYLTYPE location, GPStatement *command, List *next)
{ 
    List *new_command = malloc(sizeof(List));
    
    if(new_command == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_command->list_type = COMMANDS;
    new_command->location = location;
    new_command->value.command = command;
    new_command->next = next;

    return new_command;
}

List *addRule (YYLTYPE location, char *rule_name, List *next)
{ 
    List *new_rule = malloc(sizeof(List));
    
    if(new_rule == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_rule->list_type = RULES;
    new_rule->location = location;
    new_rule->value.rule_name = strdup(rule_name);
    new_rule->next = next;

    return new_rule;
}

List *addVariableDecl (list_t list_type, YYLTYPE location, List *variables,
	               List *next)
{ 
    List *new_var_decl = malloc(sizeof(List));
    
    if(new_var_decl == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_var_decl->list_type = list_type; 
    /* list_type: INT_DECLARATIONS, STRING_DECLARATIONS, ATOM_DECLARATIONS,
     * LIST_DECLARATIONS */
    new_var_decl->location = location;
    new_var_decl->value.variables = variables;
    new_var_decl->next = next;

    return new_var_decl;
}

List *addVariable (YYLTYPE location, char *variable_name, List *next)
{ 
    List *new_var = malloc(sizeof(List));
    
    if(new_var == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_var->list_type = VARIABLE_LIST;
    new_var->location = location;
    new_var->value.variable_name = strdup(variable_name);
    new_var->next = next;

    return new_var;
}

List *addNodeID (YYLTYPE location, char *node_id, List *next)
{ 
    List *new_pair = malloc(sizeof(List));
    
    if(new_pair == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_pair->list_type = INTERFACE_LIST;
    new_pair->location = location;
    new_pair->value.node_id = strdup(node_id);
    new_pair->next = next;

    return new_pair;
}

List *addNode (YYLTYPE location, GPNode *node, List *next)
{
     List *new_node = malloc(sizeof(List));
     
     if(new_node == NULL) {
	fprintf(log_file,"Memory exhausted during AST construction.\n");
        exit(0);
     }

     new_node->list_type = NODE_LIST;
     new_node->location = location;
     new_node->value.node = node;
     new_node->next = next;

     return new_node;
}
      
List *addEdge (YYLTYPE location, GPEdge *edge, List *next)
{
     List *new_edge = malloc(sizeof(List));
     
     if(new_edge == NULL) {
	fprintf(log_file,"Memory exhausted during AST construction.\n");
        exit(0);
     }

     new_edge->list_type = EDGE_LIST;
     new_edge->location = location;
     new_edge->value.edge = edge;
     new_edge->next = next;

     return new_edge;
}

List *addAtom (YYLTYPE location, GPAtomicExp *atom, List *next)
{
    List *new_atom = malloc(sizeof(List));
   
    if(new_atom == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_atom->list_type = GP_LIST;
    new_atom->location = location;
    new_atom->value.atom = atom;
    new_atom->next = next;

    return new_atom;
}

List *addEmptyList (YYLTYPE location)
{
     List *new_empty = malloc(sizeof(List));

     if(new_empty == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     new_empty->list_type = EMPTY_LIST;
     new_empty->location = location;
     new_empty->next = NULL;

     return new_empty;
}


/* The constructor functions for AST nodes of type struct GPDeclaration. */

GPDeclaration *newMainDecl (YYLTYPE location, GPStatement *main_program)
{
    GPDeclaration *new_main = malloc(sizeof(GPDeclaration));
   
    if(new_main == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_main->decl_type = MAIN_DECLARATION;
    new_main->location = location;
    new_main->value.main_program = main_program;

    return new_main;
}

GPDeclaration *newProcedureDecl (YYLTYPE location, GPProcedure *procedure)
{
    GPDeclaration *new_proc = malloc(sizeof(GPDeclaration));
   
    if(new_proc == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_proc->decl_type = PROCEDURE_DECLARATION;
    new_proc->location = location;
    new_proc->value.procedure = procedure;

    return new_proc;
}

GPDeclaration *newRuleDecl (YYLTYPE location, GPRule *rule)
{
    GPDeclaration *new_rule = malloc(sizeof(GPDeclaration));
   
    if(new_rule == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_rule->decl_type = RULE_DECLARATION;
    new_rule->location = location;
    new_rule->value.rule = rule;

    return new_rule;
}


/* The constructor functions for AST nodes of type struct GPStatement. */

GPStatement *newCommandSequence(YYLTYPE location, List *cmd_seq)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = COMMAND_SEQUENCE;
    stmt->location = location;
    stmt->value.cmd_seq = cmd_seq;

    return stmt;
}

GPStatement *newRuleCall(YYLTYPE location, char *rule_name)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = RULE_CALL;
    stmt->location = location;
    stmt->value.rule_name = strdup(rule_name);

    return stmt;
}

GPStatement *newRuleSetCall(YYLTYPE location, List *rule_set)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = RULE_SET_CALL;
    stmt->location = location;
    stmt->value.rule_set = rule_set;

    return stmt;
}

GPStatement *newProcCall(YYLTYPE location, char *proc_name)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = PROCEDURE_CALL;
    stmt->location = location;
    stmt->value.proc_name = strdup(proc_name);

    return stmt;
}

GPStatement *newCondBranch(stmt_t statement_type, YYLTYPE location, 
	      GPStatement *condition, GPStatement *then_stmt, GPStatement *else_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = statement_type; 
    /* statement_type: IF_STATEMENT, TRY_STATEMENT */
    stmt->location = location;
    stmt->value.cond_branch.condition = condition;
    stmt->value.cond_branch.then_stmt = then_stmt;
    stmt->value.cond_branch.else_stmt = else_stmt;

    return stmt;
}

GPStatement *newAlap(YYLTYPE location, GPStatement *loop_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = ALAP_STATEMENT;
    stmt->location = location;
    stmt->value.loop_stmt = loop_stmt;

    return stmt;
}

GPStatement *newOrStmt(YYLTYPE location, GPStatement *left_stmt, 
	      GPStatement *right_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = PROGRAM_OR;
    stmt->location = location;
    stmt->value.or_stmt.left_stmt = left_stmt;
    stmt->value.or_stmt.right_stmt = right_stmt;

    return stmt;
}

GPStatement *newSkip(YYLTYPE location)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = SKIP_STATEMENT;
    stmt->location = location;

    return stmt;
}

GPStatement *newFail(YYLTYPE location)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = FAIL_STATEMENT;
    stmt->location = location;

    return stmt;
}


/* The constructor functions for AST nodes of type struct GPCondExp. */

GPCondExp *newSubtypePred (condexp_t exp_type, YYLTYPE location, char *var)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = exp_type; 
     /* exp_type: INT_CHECK, STRING_CHECK, ATOM_CHECK */
     cond->location = location;
     cond->value.var = strdup(var);

     return cond;
}

GPCondExp *newEdgePred (YYLTYPE location, char *source, char *target, 
	    GPLabel *label)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = EDGE_PRED;
     cond->location = location;
     cond->value.edge_pred.source = strdup(source);
     cond->value.edge_pred.target = strdup(target);
     cond->value.edge_pred.label = label;

     return cond;
}

GPCondExp *newListComparison (condexp_t exp_type, YYLTYPE location,
	    List *left_list, List *right_list)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = exp_type; 
     /* exp_type: EQUAL, NOT_EQUAL */
     cond->location = location;
     cond->value.list_cmp.left_list = left_list;
     cond->value.list_cmp.right_list = right_list;

     return cond;
}


GPCondExp *newAtomComparison (condexp_t exp_type, YYLTYPE location,
	    GPAtomicExp *left_exp, GPAtomicExp *right_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = exp_type; 
     /* exp_type: GREATER, GREATER_EQUAL, LESS, LESS_EQUAL */
     cond->location = location;
     cond->value.atom_cmp.left_exp = left_exp;
     cond->value.atom_cmp.right_exp = right_exp;

     return cond;
}


GPCondExp *newNotExp (YYLTYPE location, GPCondExp *not_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = BOOL_NOT;
     cond->location = location;
     cond->value.not_exp = not_exp;

     return cond;
}

GPCondExp *newBinaryExp (condexp_t exp_type, YYLTYPE location, 
	    GPCondExp *left_exp, GPCondExp *right_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = exp_type; 
     /* exp_type: OR, AND */
     cond->location = location;
     cond->value.bin_exp.left_exp = left_exp;
     cond->value.bin_exp.right_exp = right_exp;

     return cond;
}



/* The constructor functions for AST nodes of type struct GPAtomicExp. */


GPAtomicExp *newVariable (YYLTYPE location, char *name)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = VARIABLE;
     atom->location = location;
     atom->value.name = strdup(name);

     return atom;
}

GPAtomicExp *newNumber (YYLTYPE location, int number)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = INT_CONSTANT;
     atom->location = location;
     atom->value.number = number;

     return atom;
}


GPAtomicExp *newCharacter (YYLTYPE location, char *character)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));

     if(atom == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = CHARACTER_CONSTANT;
     atom->location = location;
     if(character) atom->value.string = strdup(character);

     return atom;
}



GPAtomicExp *newString (YYLTYPE location, char *string)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = STRING_CONSTANT;
     atom->location = location;
     if(string) atom->value.string = strdup(string);

     return atom;
}

GPAtomicExp *newDegreeOp (atomexp_t exp_type, YYLTYPE location, char *node_id)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = exp_type; 
     /* exp_type: INDEGREE, OUTDEGREE */
     atom->location = location;
     atom->value.node_id = strdup(node_id);

     return atom;
}

GPAtomicExp *newListLength (YYLTYPE location, List *list_arg)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = LIST_LENGTH;
     atom->location = location;
     atom->value.list_arg = list_arg;

     return atom;
}

GPAtomicExp *newStringLength (YYLTYPE location, GPAtomicExp *str_arg)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = STRING_LENGTH;
     atom->location = location;
     atom->value.str_arg = str_arg;

     return atom;
}

GPAtomicExp *newNegExp (YYLTYPE location, GPAtomicExp *exp)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = NEG;
     atom->location = location;
     atom->value.exp = exp;

     return atom;
}

GPAtomicExp *newBinaryOp (atomexp_t exp_type, YYLTYPE location, GPAtomicExp *left_exp, GPAtomicExp *right_exp)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(log_file,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = exp_type; 
     /* exp_type: ADD, SUBTRACT, MULTIPLE, DIVIDE, CONCAT */
     atom->location = location;
     atom->value.bin_op.left_exp = left_exp;
     atom->value.bin_op.right_exp = right_exp;

     return atom;
}


/* The constructor functions for the remaining AST node types. */

GPProcedure *newProcedure(YYLTYPE location, char *name, List *local_decls, GPStatement *cmd_seq)
{
    GPProcedure *proc = malloc(sizeof(GPProcedure));
    
    if(proc == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    proc->node_type = PROCEDURE;
    proc->location = location;
    proc->name = strdup(name);
    proc->local_decls = local_decls;
    proc->cmd_seq = cmd_seq;

    return proc;
}
 
GPRule *newRule(YYLTYPE location, bool injective, char *name, List *variables,
	        GPGraph *lhs, GPGraph *rhs, List *interface, 
		GPCondExp *condition)
{
    GPRule *rule = malloc(sizeof(GPRule));
    
    if(rule == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    rule->node_type = RULE;
    rule->location = location;
    rule->injective = injective;
    rule->name = strdup(name);
    rule->variables = variables;
    rule->lhs = lhs;
    rule->rhs = rhs;
    rule->interface = interface;
    rule->condition = condition;

    return rule;
}    


GPGraph *newGraph (YYLTYPE location, GPPos *position, List *nodes, 
                   List *edges)
{
    GPGraph *graph = malloc(sizeof(GPGraph));
    
    if(graph == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    graph->node_type = GRAPH;
    graph->location = location;
    graph->position = position; 
    graph->nodes = nodes;
    graph->edges = edges;

    return graph;
}


GPNode *newNode (YYLTYPE location, bool root, char *name, GPLabel *label, GPPos *position)
{
    GPNode *node = malloc(sizeof(GPNode));
    
    if(node == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    node->node_type = NODE;
    node->location = location;
    node->root = root;
    node->name = strdup(name);
    node->label = label;
    node->position = position;

    return node;
}

GPEdge *newEdge (YYLTYPE location, char *name, char *source, char *target, GPLabel *label)
{
    GPEdge *edge = malloc(sizeof(GPEdge));
    
    if(edge == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    edge->node_type = EDGE;
    edge->location = location;
    edge->name = strdup(name);
    edge->source = strdup(source);
    edge->target = strdup(target);
    edge->label = label;

    return edge;
}

GPPos *newPosition (YYLTYPE location, int x, int y)
{
    GPPos *pos = malloc(sizeof(GPPos));
    
    if(pos == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    pos->node_type = POSITION;
    pos->location = location;
    pos->x = x;
    pos->y = y;

    return pos;
}

GPLabel *newLabel (YYLTYPE location, mark_t mark, List *gp_list)
{
    GPLabel *label = malloc(sizeof(GPLabel));
    
    if(label == NULL) {
      fprintf(log_file,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    label->node_type = LABEL;
    label->location = location;
    label->mark = mark;
    label->gp_list = gp_list;

    return label;
}

/* These functions perform a depth-first walk of the AST. They take a pointer
 * to an AST node as their argument. They first free any malloc'd strings 
 * (identifiers and string constants) if present, then they free any
 * substructures, and finally they free themselves. 
 */

void free_ast(List *ast) 
{
   switch(ast->list_type) {

	case GLOBAL_DECLARATIONS:

        case LOCAL_DECLARATIONS:

	     if(ast->value.declaration) 
               free_declaration(ast->value.declaration);

	     break;	


	case COMMANDS:

             if(ast->value.command) free_statement(ast->value.command);

	     break;	


	case RULES:

             if(ast->value.rule_name) free(ast->value.rule_name);

	     break;
	

	case INT_DECLARATIONS:
              
	case STRING_DECLARATIONS:

	case ATOM_DECLARATIONS:

	case LIST_DECLARATIONS:

             if(ast->value.variables) free_ast(ast->value.variables);

	     break;
	

	case VARIABLE_LIST:

             if(ast->value.variable_name) free(ast->value.variable_name);

	     break;
	

	case INTERFACE_LIST:

             if(ast->value.node_id) free(ast->value.node_id);

	     break;
	

	case NODE_LIST:

             if(ast->value.node) free_node(ast->value.node);

	     break;
	

	case EDGE_LIST:

             if(ast->value.edge) free_edge(ast->value.edge);

	     break;


	case GP_LIST:

             if(ast->value.atom) free_atomic_exp(ast->value.atom);

	     break;

	
	case EMPTY_LIST:

             break;


	default: fprintf(log_file,"Unexpected List Type: %d\n",
                         (int)ast->list_type); 
                 break;	 

	}

   if(ast->next) free_ast(ast->next);
   free(ast);
}

void free_declaration(GPDeclaration *decl)
{
     switch(decl->decl_type) {

	case MAIN_DECLARATION:

             if(decl->value.main_program) 
               free_statement(decl->value.main_program);

	     break;


	case PROCEDURE_DECLARATION:

             if(decl->value.procedure) free_procedure(decl->value.procedure);

	     break;


	case RULE_DECLARATION:

             if(decl->value.rule) free_rule(decl->value.rule);

	     break;


	default: fprintf(log_file,"Unexpected Declaration Type: %d\n",
                         (int)decl->decl_type); 
                 break;

	}

   free(decl);
}

void free_statement(GPStatement *stmt)
{
     switch(stmt->statement_type) {

	case COMMAND_SEQUENCE:	

             if(stmt->value.cmd_seq) free_ast(stmt->value.cmd_seq);

	     break;


	case RULE_CALL:

             if(stmt->value.rule_name) free(stmt->value.rule_name);

	     break;


	case RULE_SET_CALL:

             if(stmt->value.rule_set) free_ast(stmt->value.rule_set);

	     break;


	case PROCEDURE_CALL:

             if(stmt->value.proc_name) free(stmt->value.proc_name);

	     break;


	case IF_STATEMENT:

        case TRY_STATEMENT:

             if(stmt->value.cond_branch.condition) 
               free_statement(stmt->value.cond_branch.condition);
             if(stmt->value.cond_branch.then_stmt) 
               free_statement(stmt->value.cond_branch.then_stmt);
	     if(stmt->value.cond_branch.else_stmt) 
               free_statement(stmt->value.cond_branch.else_stmt);

	     break;


	case ALAP_STATEMENT:

	     if(stmt->value.loop_stmt) free_statement(stmt->value.loop_stmt);
             
	     break;


	case PROGRAM_OR:

             if(stmt->value.or_stmt.left_stmt) 
               free_statement(stmt->value.or_stmt.left_stmt);
             if(stmt->value.or_stmt.right_stmt) 
               free_statement(stmt->value.or_stmt.right_stmt);

	     break;


	case SKIP_STATEMENT:

 	case FAIL_STATEMENT:

	     break;

	
	default: fprintf(log_file,"Unexpected Statement Type: %d\n",
                         (int)stmt->statement_type); 
                 break;

	}

   free(stmt);
}

void free_condition(GPCondExp *cond)
{
     switch(cond->exp_type) {

	case INT_CHECK:

	case STRING_CHECK:

	case ATOM_CHECK:

             if(cond->value.var) free(cond->value.var);

             break;


	case EDGE_PRED:

	     if(cond->value.edge_pred.source)
               free(cond->value.edge_pred.source);
	     if(cond->value.edge_pred.target)
               free(cond->value.edge_pred.target);
	     if(cond->value.edge_pred.label)
               free_label(cond->value.edge_pred.label);
                                         
             break;


	case EQUAL:

	case NOT_EQUAL:

             if(cond->value.list_cmp.left_list) 
               free_ast(cond->value.list_cmp.left_list);
             if(cond->value.list_cmp.right_list)  
               free_ast(cond->value.list_cmp.right_list);

	     break;
	

	case GREATER:

	case GREATER_EQUAL:

	case LESS:

	case LESS_EQUAL:

             if(cond->value.atom_cmp.left_exp)
               free_atomic_exp(cond->value.atom_cmp.left_exp);
             if(cond->value.atom_cmp.right_exp) 
               free_atomic_exp(cond->value.atom_cmp.right_exp);

	     break;	  


	case BOOL_NOT:

	     if(cond->value.not_exp) free_condition(cond->value.not_exp);

	     break;


	case BOOL_OR:

	case BOOL_AND:

	     if(cond->value.bin_exp.left_exp)
               free_condition(cond->value.bin_exp.left_exp);
	     if(cond->value.bin_exp.right_exp) 
               free_condition(cond->value.bin_exp.right_exp);

	     break;


	default: fprintf(log_file,"Unexpected Condition Type: %d\n",
                         (int)cond->exp_type); 
                 break;

	}
   free(cond);
}

void free_atomic_exp(GPAtomicExp *atom)
{
     switch(atom->exp_type) {


	case VARIABLE:

	     if(atom->value.name)
               free(atom->value.name);

             break;


	case INT_CONSTANT:

             break;


        case CHARACTER_CONSTANT:
          
	case STRING_CONSTANT:

	     if(atom->value.string)
               free(atom->value.string);

             break;


	case INDEGREE:
 
        case OUTDEGREE:

	     if(atom->value.node_id) 
               free(atom->value.node_id);

             break;


	case LIST_LENGTH:

	     if(atom->value.list_arg)
               free_ast(atom->value.list_arg);
		
             break;


	case STRING_LENGTH:

	     if(atom->value.str_arg)
               free_atomic_exp(atom->value.str_arg);

             break;

	case NEG:

	     if(atom->value.exp) free_atomic_exp(atom->value.exp);

             break;


	case ADD:

	case SUBTRACT:

	case MULTIPLY:

	case DIVIDE:

	case CONCAT:

	     if(atom->value.bin_op.left_exp)
                free_atomic_exp(atom->value.bin_op.left_exp);
	     if(atom->value.bin_op.right_exp)  
                free_atomic_exp(atom->value.bin_op.right_exp);

             break;


	default: fprintf(log_file,"Unexpected Atomic Expression Type: %d\n",
                         (int)atom->exp_type); 
                 break;

	}

   free(atom);
}

void free_procedure(GPProcedure *proc)
{
   if(proc->name) free(proc->name);
   if(proc->local_decls) free_ast(proc->local_decls);
   if(proc->cmd_seq) free_statement(proc->cmd_seq);
   free(proc);
}

void free_rule(GPRule *rule)
{
   if(rule->name) free(rule->name);
   if(rule->variables) free_ast(rule->variables);
   if(rule->lhs) free_graph(rule->lhs);  
   if(rule->rhs) free_graph(rule->rhs);
   if(rule->interface) free_ast(rule->interface);
   if(rule->condition) free_condition(rule->condition);
   free(rule);
}

void free_graph(GPGraph *graph)
{
   if(graph->position) free(graph->position);
   if(graph->nodes) free_ast(graph->nodes);
   if(graph->edges) free_ast(graph->edges);
   free(graph);
}

void free_node(GPNode *node)
{
   if(node->name) free(node->name);
   if(node->label) free_label(node->label);
   if(node->position) free(node->position);
   free(node);
}

void free_edge(GPEdge *edge)
{
   if(edge->name) free(edge->name);
   if(edge->source) free(edge->source);
   if(edge->target) free(edge->target);
   if(edge->label) free_label(edge->label);
   free(edge);
}

void free_label(GPLabel *label)
{
   if(label->gp_list) free_ast(label->gp_list);
   free(label);
}
   


