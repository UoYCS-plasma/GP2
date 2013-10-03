/*////////////////////////////////////////////////////////////////////////////

                                pretty.h                               

                      Created on 18/9/13 by Chris Bak 

////////////////////////////////////////////////////////////////////////////*/

#include "ast.h" // AST structure declarations.

void print_location(YYLTYPE const loc);
void print_list(List const * const list);
void print_declaration(GPDeclaration const * const decl);
void print_statement(GPStatement const * const stmt);
void print_condition(GPCondExp const * const cond);
void print_atom(GPAtomicExp const * const atom);
void print_procedure(GPProcedure const * const proc);
void print_rule(GPRule const * const rule);
void print_graph(GPGraph const * const graph);
void print_node_pair(GPNodePair const * const node_pair);
void print_node(GPNode const * const node);
void print_edge(GPEdge const * const edge);
void print_label(GPLabel const * const label);
void print_position(GPPos const * const pos);
