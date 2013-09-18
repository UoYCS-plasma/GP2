/* ////////////////////////////////////////////////////////////////////////////////////// */

/*                                     pretty.h                               
 * 
 * Created on 18/9/13 by Chris Bak 
 *
/* /////////////////////////////////////////////////////////////////////////////////////// */

#include "ast.h"

void print_location(YYLTYPE loc);
void ast_pretty_print(List *gp_program);
void print_list(List *list);
void print_declaration(GPDeclaration *decl);
void print_statement(GPStatement *stmt);
void print_condition(GPCondExp *cond);
void print_atom(GPAtomicExp *atom);
void print_procedure(GPProcedure *proc);
void print_rule(GPRule *rule);
void print_graph(GPGraph *graph);
void print_node_pair(GPNodePair *node_pair);
void print_node(GPNode *node);
void print_edge(GPEdge *edge);
void print_label(GPLabel *label);
void print_pos(GPPos *pos);
