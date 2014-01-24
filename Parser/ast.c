/*////////////////////////////////////////////////////////////////////////////

                                  ast.c              
                         
                   Contains AST constructor definitions. 

                     Created on 22/7/2013 by Chris Bak 

/////////////////////////////////////////////////////////////////////////// */ 

#include "ast.h" /* AST struct definitions */
#include <stdio.h> /* printf */
#include <stdlib.h> /* malloc */

/* The constructor functions for AST nodes of type struct List. */

List *addDecl (list_t list_type, YYLTYPE location, GPDeclaration *declaration,
	       List *next)
{ 
    List *new_decl = malloc(sizeof(List));
    
    if(new_decl == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_rule->list_type = RULES;
    new_rule->location = location;
    new_rule->value.rule_name = rule_name;
    new_rule->next = next;

    return new_rule;
}

List *addVariableDecl (list_t list_type, YYLTYPE location, List *variables,
	               List *next)
{ 
    List *new_var_decl = malloc(sizeof(List));
    
    if(new_var_decl == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_var->list_type = VARIABLE_LIST;
    new_var->location = location;
    new_var->value.variable_name = variable_name;
    new_var->next = next;

    return new_var;
}

List *addNodeID (YYLTYPE location, char *node_id, List *next)
{ 
    List *new_pair = malloc(sizeof(List));
    
    if(new_pair == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_pair->list_type = INTERFACE_LIST;
    new_pair->location = location;
    new_pair->value.node_id = node_id;
    new_pair->next = next;

    return new_pair;
}

List *addNode (YYLTYPE location, GPNode *node, List *next)
{
     List *new_node = malloc(sizeof(List));
     
     if(new_node == NULL) {
	fprintf(stderr,"Memory exhausted during AST construction.\n");
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
	fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_atom->list_type = GP_LIST;
    new_atom->location = location;
    new_atom->value.atom = atom;
    new_atom->next = next;

    return new_atom;
}


/* The constructor functions for AST nodes of type struct GPDeclaration. */

GPDeclaration *newMainDecl (YYLTYPE location, GPStatement *main_program)
{
    GPDeclaration *new_main = malloc(sizeof(GPDeclaration));
   
    if(new_main == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = RULE_CALL;
    stmt->location = location;
    stmt->value.rule_name = rule_name;

    return stmt;
}

GPStatement *newRuleSetCall(YYLTYPE location, List *rule_set)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = PROCEDURE_CALL;
    stmt->location = location;
    stmt->value.proc_name = proc_name;

    return stmt;
}

GPStatement *newCondBranch(stmt_t statement_type, YYLTYPE location, 
	      GPStatement *condition, GPStatement *then_stmt, GPStatement *else_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
       fprintf(stderr,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = exp_type; 
     /* exp_type: INT_CHECK, STRING_CHECK, ATOM_CHECK */
     cond->location = location;
     cond->value.var = var;

     return cond;
}

GPCondExp *newEdgePred (YYLTYPE location, char *source, char *target, 
	    GPLabel *label)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       fprintf(stderr,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = EDGE_PRED;
     cond->location = location;
     cond->value.edge_pred.source = source;
     cond->value.edge_pred.target = target;
     cond->value.edge_pred.label = label;

     return cond;
}

GPCondExp *newListComparison (condexp_t exp_type, YYLTYPE location,
	    List *left_list, List *right_list)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       fprintf(stderr,"Memory exhausted during AST construction.\n");
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
       fprintf(stderr,"Memory exhausted during AST construction.\n");
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
       fprintf(stderr,"Memory exhausted during AST construction.\n");
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
       fprintf(stderr,"Memory exhausted during AST construction.\n");
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

GPAtomicExp *newEmpty (YYLTYPE location)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));

     if(atom == NULL) {
       fprintf(stderr,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = EMPTY_LIST;
     atom->location = location;

     return atom;
}


GPAtomicExp *newVariable (YYLTYPE location, char *name)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(stderr,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = VARIABLE;
     atom->location = location;
     atom->value.name = name;

     return atom;
}

GPAtomicExp *newNumber (YYLTYPE location, int number)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(stderr,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = INT_CONSTANT;
     atom->location = location;
     atom->value.number = number;

     return atom;
}


GPAtomicExp *newString (YYLTYPE location, char *string)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(stderr,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = STRING_CONSTANT;
     atom->location = location;
     atom->value.string = string;

     return atom;
}

GPAtomicExp *newDegreeOp (atomexp_t exp_type, YYLTYPE location, char *node_id)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(stderr,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = exp_type; 
     /* exp_type: INDEGREE, OUTDEGREE */
     atom->location = location;
     atom->value.node_id = node_id;

     return atom;
}

GPAtomicExp *newListLength (YYLTYPE location, List *list_arg)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(stderr,"Memory exhausted during AST construction.\n");
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
       fprintf(stderr,"Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = STRING_LENGTH; 
     atom->location = location;
     atom->value.str_arg = str_arg;

     return atom;
}

GPAtomicExp *newNegExp (YYLTYPE location, struct GPAtomicExp *exp)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       fprintf(stderr,"Memory exhausted during AST construction.\n");
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
       fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    proc->node_type = PROCEDURE;
    proc->location = location;
    proc->name = name;
    proc->local_decls = local_decls;
    proc->cmd_seq = cmd_seq;

    return proc;
}
 
GPRule *newRule(YYLTYPE location, bool injective, char *name, List *variables, GPGraph *lhs, GPGraph *rhs, List *interface, GPCondExp *condition)
{
    GPRule *rule = malloc(sizeof(GPRule));
    
    if(rule == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    rule->node_type = RULE;
    rule->location = location;
    rule->injective = injective;
    rule->name = name;
    rule->variables = variables;
    rule->lhs = lhs;
    rule->rhs = rhs;
    rule->interface = interface;
    rule->condition = condition;

    return rule;
}    


GPGraph *newGraph (YYLTYPE location, GPPos *position, List *nodes, List *edges)
{
    GPGraph *graph = malloc(sizeof(GPGraph));
    
    if(graph == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    node->node_type = NODE;
    node->location = location;
    node->root = root;
    node->name = name;
    node->label = label;
    node->position = position;

    return node;
}

GPEdge *newEdge (YYLTYPE location, char *name, char *source, char *target, GPLabel *label)
{
    GPEdge *edge = malloc(sizeof(GPEdge));
    
    if(edge == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    edge->node_type = EDGE;
    edge->location = location;
    edge->name = name;
    edge->source = source;
    edge->target = target;
    edge->label = label;

    return edge;
}

GPPos *newPosition (YYLTYPE location, int x, int y)
{
    GPPos *pos = malloc(sizeof(GPPos));
    
    if(pos == NULL) {
      fprintf(stderr,"Memory exhausted during AST construction.\n");
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
      fprintf(stderr,"Memory exhausted during AST construction.\n");
      exit(0);
    }

    label->node_type = LABEL;
    label->location = location;
    label->mark = mark;
    label->gp_list = gp_list;

    return label;
}

