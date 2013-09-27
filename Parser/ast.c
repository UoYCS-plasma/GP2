/* ///////////////////////////////////////////////////////////////////////////////////////////////// */

/*              ast.c                                       
* Contains AST constructor definitions. 
*
* Created on 22/7/2013 by Chris Bak 
* 
* ///////////////////////////////////////////////////////////////////////////////////////////////// */

#include <stdio.h>
#include <stdlib.h>
#include "ast.h"

/* constructors for struct List */

List *addDecl (list_t list_type, YYLTYPE location, GPDeclaration *decl, List *next)
{ 
    List *new_decl = malloc(sizeof(List));
    
    if(!new_decl) {
      yyerror("insufficient space");
      exit(0);
    }
    new_decl->list_type = list_type;
    new_decl->location = location;
    new_decl->value.decl = decl;
    new_decl->next = next;

    return new_decl;
}

List *addCommand (YYLTYPE location, GPStatement *command, List *next)
{ 
    List *new_command = malloc(sizeof(List));
    
    if(!new_command) {
      yyerror("insufficient space");
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
    
    if(!new_rule) {
      yyerror("insufficient space");
      exit(0);
    }
    new_rule->list_type = RULES;
    new_rule->location = location;
    new_rule->value.rule_name = rule_name;
    new_rule->next = next;

    return new_rule;
}

List *addVariableDecl (list_t list_type, YYLTYPE location, List *vars, List *next)
{ 
    List *new_var_decl = malloc(sizeof(List));
    
    if(!new_var_decl) {
      yyerror("insufficient space");
      exit(0);
    }
    new_var_decl->list_type = list_type;
    new_var_decl->location = location;
    new_var_decl->value.vars = vars;
    new_var_decl->next = next;

    return new_var_decl;
}

List *addVariable (YYLTYPE location, char *var, List *next)
{ 
    List *new_var = malloc(sizeof(List));
    
    if(!new_var) {
      yyerror("insufficient space");
      exit(0);
    }
    new_var->list_type = VARIABLES;
    new_var->location = location;
    new_var->value.var = var;
    new_var->next = next;

    return new_var;
}

List *addNodePair (YYLTYPE location, GPNodePair *node_pair, List *next)
{ 
    List *new_pair = malloc(sizeof(List));
    
    if(!new_pair) {
      yyerror("insufficient space");
      exit(0);
    }
    new_pair->list_type = INTERFACE_LIST;
    new_pair->location = location;
    new_pair->value.node_pair = node_pair;
    new_pair->next = next;

    return new_pair;
}

List *addNode (YYLTYPE location, GPNode *node, List *next)
{
     List *new_node = malloc(sizeof(List));
     
     if(!new_node) {
	yyerror("insufficient space");
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
     
     if(!new_edge) {
	yyerror("insufficient space");
        exit(0);
     }

     new_edge->list_type = EDGE_LIST;
     new_edge->location = location;
     new_edge->value.edge = edge;
     new_edge->next = next;

     return new_edge;
}

List *addRelationalExp (list_t list_type, YYLTYPE location, List *rel_exp, List *next)
{
    List *new_rel_exp = malloc(sizeof(List));
   
    if(!new_rel_exp) {
      yyerror("insufficient space");
      exit(0);
    }
    new_rel_exp->list_type = list_type;
    new_rel_exp->location = location;
    new_rel_exp->value.rel_exp = rel_exp;
    new_rel_exp->next = next;

    return new_rel_exp;
}

List *addAtom (YYLTYPE location, GPAtomicExp *atom, List *next)
{
    List *new_atom = malloc(sizeof(List));
   
    if(!new_atom) {
      yyerror("insufficient space");
      exit(0);
    }
    new_atom->list_type = GP_LIST;
    new_atom->location = location;
    new_atom->value.atom = atom;
    new_atom->next = next;

    return new_atom;
}


/* constructors for struct GPDeclaration */

GPDeclaration *newMainDecl (YYLTYPE location, GPStatement *main_program)
{
    GPDeclaration *new_main = malloc(sizeof(GPDeclaration));
   
    if(!new_main) {
      yyerror("insufficient space");
      exit(0);
    }
    new_main->decl_type = MAIN_DECL;
    new_main->location = location;
    new_main->value.main_program = main_program;

    return new_main;
}

GPDeclaration *newProcedureDecl (YYLTYPE location, GPProcedure *proc)
{
    GPDeclaration *new_proc = malloc(sizeof(GPDeclaration));
   
    if(!new_proc) {
      yyerror("insufficient space");
      exit(0);
    }
    new_proc->decl_type = PROCEDURE_DECL;
    new_proc->location = location;
    new_proc->value.proc = proc;

    return new_proc;
}

GPDeclaration *newRuleDecl (YYLTYPE location, GPRule *rule)
{
    GPDeclaration *new_rule = malloc(sizeof(GPDeclaration));
   
    if(!new_rule) {
      yyerror("insufficient space");
      exit(0);
    }
    new_rule->decl_type = RULE_DECL;
    new_rule->location = location;
    new_rule->value.rule = rule;

    return new_rule;
}


/* constructors for struct GPStatement */

GPStatement *newCommandSequence(YYLTYPE location, List *cmd_seq)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(!stmt) {
      yyerror("insufficient space");
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
   
    if(!stmt) {
      yyerror("insufficient space");
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
   
    if(!stmt) {
      yyerror("insufficient space");
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
   
    if(!stmt) {
      yyerror("insufficient space");
      exit(0);
    }
    stmt->statement_type = PROCEDURE_CALL;
    stmt->location = location;
    stmt->value.proc_name = proc_name;

    return stmt;
}

GPStatement *newCondBranch(stmt_t statement_type, YYLTYPE location, GPStatement *condition, GPStatement *then_stmt, GPStatement *else_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(!stmt) {
      yyerror("insufficient space");
      exit(0);
    }
    stmt->statement_type = statement_type; /* IF_STMT, TRY_STMT */
    stmt->location = location;
    stmt->value.cond_branch.condition = condition;
    stmt->value.cond_branch.then_stmt = then_stmt;
    stmt->value.cond_branch.else_stmt = else_stmt;

    return stmt;
}

GPStatement *newAlap(YYLTYPE location, GPStatement *loop_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(!stmt) {
      yyerror("insufficient space");
      exit(0);
    }
    stmt->statement_type = ALAP_STMT;
    stmt->location = location;
    stmt->value.loop_stmt = loop_stmt;

    return stmt;
}

GPStatement *newOrStmt(YYLTYPE location, GPStatement *left_stmt, GPStatement *right_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(!stmt) {
      yyerror("insufficient space");
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
   
    if(!stmt) {
      yyerror("insufficient space");
      exit(0);
    }
    stmt->statement_type = SKIP_STMT;
    stmt->location = location;

    return stmt;
}

GPStatement *newFail(YYLTYPE location)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(!stmt) {
      yyerror("insufficient space");
      exit(0);
    }
    stmt->statement_type = FAIL_STMT;
    stmt->location = location;

    return stmt;
}


/* constructors for struct GPCondExp */

GPCondExp *newSubtypePred (condexp_t exp_type, YYLTYPE location, char *var)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(!cond) {
       yyerror("insufficient space");
       exit(0);
     }
     cond->exp_type = exp_type; /* INT_CHECK, STRING_CHECK, ATOM_CHECK */
     cond->location = location;
     cond->value.var = var;

     return cond;
}

GPCondExp *newEdgePred (YYLTYPE location, char *source, char *target, GPLabel *label)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(!cond) {
       yyerror("insufficient space");
       exit(0);
     }
     cond->exp_type = EDGE_PRED;
     cond->location = location;
     cond->value.edge_pred.source = source;
     cond->value.edge_pred.target = target;
     cond->value.edge_pred.label = label;

     return cond;
}

GPCondExp *newRelationalExp (YYLTYPE location, List *rel_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(!cond) {
       yyerror("insufficient space");
       exit(0);
     }
     cond->exp_type = REL_EXP; 
     cond->location = location;
     cond->value.rel_exp = rel_exp;

     return cond;
}


GPCondExp *newNotExp (YYLTYPE location, GPCondExp *not_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(!cond) {
       yyerror("insufficient space");
       exit(0);
     }
     cond->exp_type = BOOL_NOT;
     cond->location = location;
     cond->value.not_exp = not_exp;

     return cond;
}

GPCondExp *newBinaryExp (condexp_t exp_type, YYLTYPE location, GPCondExp *left_exp, GPCondExp *right_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(!cond) {
       yyerror("insufficient space");
       exit(0);
     }
     cond->exp_type = exp_type; /* OR, AND */ 
     cond->location = location;
     cond->value.bin_exp.left_exp = left_exp;
     cond->value.bin_exp.right_exp = right_exp;

     return cond;
}

/* constructors for struct GPAtomicExp */

GPAtomicExp *newVariable (YYLTYPE location, char *name)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(!atom) {
       yyerror("insufficient space");
       exit(0);
     }
     atom->exp_type = VARIABLE;
     atom->location = location;
     atom->value.var = name;

     return atom;
}

GPAtomicExp *newNumber (YYLTYPE location, int num)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(!atom) {
       yyerror("insufficient space");
       exit(0);
     }
     atom->exp_type = INT_CONSTANT;
     atom->location = location;
     atom->value.num = num;

     return atom;
}


GPAtomicExp *newString (YYLTYPE location, char *str)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(!atom) {
       yyerror("insufficient space");
       exit(0);
     }
     atom->exp_type = STRING_CONSTANT;
     atom->location = location;
     atom->value.str = str;

     return atom;
}

GPAtomicExp *newDegreeOp (atomexp_t exp_type, YYLTYPE location, char *node_id)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(!atom) {
       yyerror("insufficient space");
       exit(0);
     }
     atom->exp_type = exp_type; /*INDEGREE, OUTDEGREE */
     atom->location = location;
     atom->value.node_id = node_id;

     return atom;
}

GPAtomicExp *newListLength (YYLTYPE location, List *list_arg)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(!atom) {
       yyerror("insufficient space");
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
 
     if(!atom) {
       yyerror("insufficient space");
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
 
     if(!atom) {
       yyerror("insufficient space");
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
 
     if(!atom) {
       yyerror("insufficient space");
       exit(0);
     }
     atom->exp_type = exp_type; /* ADD, SUBTRACT, MULTIPLE, DIVIDE, CONCAT */ 
     atom->location = location;
     atom->value.bin_op.left_exp = left_exp;
     atom->value.bin_op.right_exp = right_exp;

     return atom;
}


/* constructors for other AST node structs */

GPProcedure *newProcedure(YYLTYPE location, char *name, List *local_decls, GPStatement *cmd_seq)
{
    GPProcedure *proc = malloc(sizeof(GPProcedure));
    
    if(!proc) {
      yyerror("insufficient space");
      exit(0);
    }
    proc->node_type = PROCEDURE;
    proc->location = location;
    proc->name = name;
    proc->local_decls = local_decls;
    proc->cmd_seq = cmd_seq;

    return proc;
}
 
GPRule *newRule(YYLTYPE location, int injective, char *name, List *variables, GPGraph *lhs, GPGraph *rhs, List *interface, GPCondExp *condition)
{
    GPRule *rule = malloc(sizeof(GPProcedure));
    
    if(!rule) {
      yyerror("insufficient space");
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

GPNodePair *newNodePair (YYLTYPE location, char *left_node, char *right_node)
{
    GPNodePair *node_pair = malloc(sizeof(GPEdge));
    
    if(!node_pair) {
      yyerror("insufficient space");
      exit(0);
    }
    node_pair->node_type = NODE_PAIR;
    node_pair->location = location;
    node_pair->left_node = left_node;
    node_pair->right_node = right_node;

    return node_pair;
}


GPGraph *newGraph (YYLTYPE location, GPPos *position, List *nodes, List *edges)
{
    GPGraph *graph = malloc(sizeof(GPGraph));
    
    if(!graph) {
      yyerror("insufficient space");
      exit(0);
    }
    graph->node_type = GRAPH;
    graph->location = location;
    graph->position = position; 
    graph->nodes = nodes;
    graph->edges = edges;

    return graph;
}


GPNode *newNode (YYLTYPE location, int root, char *name, GPLabel *label, GPPos *position)
{
    GPNode *node = malloc(sizeof(GPNode));
    
    if(!node) {
      yyerror("insufficient space");
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
    
    if(!edge) {
      yyerror("insufficient space");
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
    
    if(!pos) {
      yyerror("insufficient space");
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
    
    if(!label) {
      yyerror("insufficient space");
      exit(0);
    }
    label->node_type = LABEL;
    label->location = location;
    label->mark = mark;
    label->gp_list = gp_list;

    return label;
}

