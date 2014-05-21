/*////////////////////////////////////////////////////////////////////////////

                                pretty.h                               

                      Created on 18/9/13 by Chris Bak 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_PRETTY_H
#define INC_PRETTY_H

/* prettyPrint is a macro that calls the appropriate print provided the first
 * argument is not a null poin ter. 
 *
 * POINTER_ARG is a member of the current structure pointing to an AST node 
 * that we wish to print.
 *
 * TYPE corresponds to the print_ functions in this file. For example, calling
 * prettyPrint with second argument 'list' will call printList on POINTER_ARG
 * if POINTER_ARG is not NULL. Otherwise an error node is created in the 
 * appropriate place and an error message is printed to stderr. 
 *
 * Be aware that prettyPrint does not write an edge to point to any nodes
 * written by prettyPrint. The edges must be explicitly written directly 
 * before calling this macro. This is done in the .c file.
 */ 

#define prettyPrint(POINTER_ARG,TYPE)                                        \
  do { 									      \
       if(POINTER_ARG != NULL) print ## TYPE (POINTER_ARG);                  \
       else {                                                                 \
         fprintf(dot_file,"node%d[shape=plaintext,label=\"%d ERROR\"]\n",     \
                 next_node_id, next_node_id);                                 \
         fprintf(log_file, "Error: Unexpected NULL pointer at AST node %d\n", \
                 next_node_id);                                               \
       }            							      \
     }                                                                        \
  while (0)


/* prettyPrintList is used to process members of AST structs that point
 * to a struct List. It should only be used when a NULL pointer is valid in
 * the GP AST. For example, this macro is called to print the node 
 * and edge lists of a graph, which may be NULL, but not for the list
 * component of a label, which should not point to NULL. Use printList
 * or prettyPrint for the latter cases.
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
 * current node to the node that will be created by the printList call 
 * immediately following this edge creation. The use of the global node counter
 * next_node_id ensures that the edge points to the correct node.
 */

#define prettyPrintList(POINTER_ARG, NODE_TYPE, EDGE_LABEL)             \
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
          printList(POINTER_ARG);                                        \
        }                                                                 \
      } 			                                          \
    while (0)

/* The use of do while loops is a C trick to enable these macros to be called
 * with a terminating semicolon as per an actual function call. This would not
 * be possible with a normal code block as they are terminated by a right brace.
 */

/* LOCATION_ARGS(LOC) is shorthand for the components of the location structure
 * that are required for calls to fprintf. LOC is a variable of type struct 
 * YYLTYPE, which occurs in the location field of every AST node struct.
 */

#define LOCATION_ARGS(LOC)    \
   LOC.first_line, LOC.first_column, LOC.last_line, LOC.last_column

#include "ast.h" /* AST structure declarations */
#include <glib.h> /* GHashTable, gpointer */

int printSymbolTable(GHashTable *table, char *file_name);
void printSymbol(gpointer key, gpointer value, gpointer user_data);
int printDotAST(List *const gp_ast, char* file_name);
int printDotHostGraph(GPGraph *const host_graph_ast, char* file_name);
void printList(List * const list);
void printDeclaration(GPDeclaration * const decl);
void printStatement(GPStatement * const stmt);
void printCondition(GPCondExp * const cond);
void printAtom(GPAtomicExp * const atom);
void printProcedure(GPProcedure * const proc);
void printRule(GPRule * const rule);
void printGraph(GPGraph * const graph);
void printNode(GPNode * const node);
void printEdge(GPEdge * const edge);
void printLabel(GPLabel * const label);
void printPosition(GPPos * const pos);

#endif /* INC_PRETTY_H */
