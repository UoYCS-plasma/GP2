/*////////////////////////////////////////////////////////////////////////////

                                pretty.h                               

                      Created on 18/9/13 by Chris Bak 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_PRETTY_H
#define INC_PRETTY_H

#define print_to_dot_file(text, ...)                  \
  do { fprintf(dot_file, text, ## __VA_ARGS__); }     \
  while(0) 

#define print_to_symtab_file(text, ...)                        \
  do { fprintf(symbol_table_file, text, ## __VA_ARGS__); }     \
  while(0) 


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

#define prettyPrint(POINTER_ARG,TYPE)                                         \
  do { 									      \
       if(POINTER_ARG != NULL)                                                \
         print ## TYPE (POINTER_ARG, dot_file, next_node_id);                 \
       else {                                                                 \
         print_to_dot_file("node%d[shape=plaintext,label=\"%d ERROR\"]\n",    \
                 next_node_id, next_node_id);                                 \
         print_to_log("Error: Unexpected NULL pointer at AST node %d\n",      \
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

#define prettyPrintList(POINTER_ARG, NODE_TYPE, EDGE_LABEL)                 \
   do {                                                                     \
        if(POINTER_ARG == NULL) {                                           \
          print_to_dot_file("node%d[shape=plaintext,label=\"%d NULL\"]\n",  \
                  next_node_id, next_node_id);                              \
          print_to_dot_file("node%d->node%d[label=\"" #EDGE_LABEL "\"]\n",  \
                  NODE_TYPE->node_id, next_node_id);                        \
          next_node_id += 1;                                                \
        }							            \
        else {                                                              \
          print_to_dot_file("node%d->node%d[label=\"" #EDGE_LABEL "\"]\n",  \
                  NODE_TYPE->node_id, next_node_id);                        \
          printList(POINTER_ARG, dot_file, next_node_id);                   \
        }                                                                   \
      } 			                                            \
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


/* printSymbolTable creates the file <program>.tab, where <program> is the
 * name of the GP2 program file, and pretty prints the symbol table to that 
 * file. 
 *
 * Argument 1: The symbol table.
 * Argument 2: The name of the GP2 program file.
 */

int printSymbolTable(GHashTable *table, string const file_name);

/* Auxiliary function called by printSymbolTable. It iterates over
 * a symbol list, pretty printing each symbol in the list.
 */

void printSymbolList(gpointer key, gpointer value, gpointer user_data);

/* printDotAST creates a new file <source_name>.dot. It generates
 * a DOT text file that can be used to draw a picture of the AST via
 * graphviz. 
 * 
 * Argument 1: A pointer to the AST to be pretty printed.
 * Argument 2: The name of the GP2 program file.
 */

int printDotAST(List * const gp_ast, string file_name);

/* Identical to printDotAST, except it calls printGraph instead of
 * printList.
 *
 * Argument 1: A pointer to the AST of the host graph to be pretty printed.
 * Argument 2: The name of the GP2 host graph file.
 *
 */

int printDotHostGraph(GPGraph * const host_graph_ast, string file_name);

/* printList is called by printDotAST. It walks through the AST, outputting
 * lines to dot_file and recursively calling printing functions at the
 * appropriate places. 
 * 
 * Argument 1: A pointer to a List in the AST.
 * Argument 2: The .dot file created by printDotAST.
 * Argument 3: A variable to keep track of the current node number. Used
               to uniquely identify nodes in the output.
 *
 */

void printList(List * const list, FILE *dot_file, unsigned int next_node_id);
void printDeclaration(GPDeclaration * const decl, FILE *dot_file, unsigned int next_node_id);
void printStatement(GPStatement * const stmt, FILE *dot_file, unsigned int next_node_id);
void printCondition(GPCondExp * const cond, FILE *dot_file, unsigned int next_node_id);
void printAtom(GPAtomicExp * const atom, FILE *dot_file, unsigned int next_node_id);
void printProcedure(GPProcedure * const proc, FILE *dot_file, unsigned int next_node_id);
void printRule(GPRule * const rule, FILE *dot_file, unsigned int next_node_id);
void printGraph(GPGraph * const graph, FILE *dot_file, unsigned int next_node_id);
void printNode(GPNode * const node, FILE *dot_file, unsigned int next_node_id);
void printEdge(GPEdge * const edge, FILE *dot_file, unsigned int next_node_id);
void printLabel(GPLabel * const label, FILE *dot_file, unsigned int next_node_id);
void printPosition(GPPos * const pos, FILE *dot_file, unsigned int next_node_id);

#endif /* INC_PRETTY_H */
