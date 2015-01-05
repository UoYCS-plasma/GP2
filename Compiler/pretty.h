/* ///////////////////////////////////////////////////////////////////////////

  ======================
  Pretty Printing Module
  ======================

  Module for pretty printing the abstract syntax tree and the symbol table.
  Contains several macros to free the source file from clutter and prototypes
  for printing functions.                         
                      
/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_PRETTY_H
#define INC_PRETTY_H

#include <glib.h>
#include "ast.h"
#include "error.h"
#include "globals.h"
#include "seman.h" 

#define print_to_dot_file(text, ...)                  \
  do { fprintf(dot_file, text, ## __VA_ARGS__); }     \
  while(0) 

#define print_to_symtab_file(text, ...)                        \
  do { fprintf(symbol_table_file, text, ## __VA_ARGS__); }     \
  while(0) 


/* prettyPrint is a macro that calls the appropriate print provided the first
 * argument is not a null pointer. 
 *
 * POINTER_ARG is a member of the current structure pointing to an AST node 
 * that is to be printed.
 *
 * TYPE corresponds to the print functions in this file. For example, calling
 * prettyPrint with second argument 'List' will call printList on POINTER_ARG
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
         printAST ## TYPE (POINTER_ARG, dot_file);                            \
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
          printASTList(POINTER_ARG, dot_file);                              \
        }                                                                   \
      } 			                                            \
    while (0)

/* LOCATION_ARGS(LOC) is shorthand for the components of the location structure
 * that are required for calls to fprintf. LOC is a variable of type struct 
 * YYLTYPE, which occurs in the location field of every AST node struct.
 */

#define LOCATION_ARGS(LOC)    \
   LOC.first_line, LOC.first_column, LOC.last_line, LOC.last_column

/* printSymbolTable creates the file <program>.tab, where <program> is the
 * name of the GP2 program file, and pretty prints the symbol table to that 
 * file. 
 *
 * Argument 1: The symbol table.
 * Argument 2: The name of the GP2 program file.
 */

int printSymbolTable(GHashTable *table, string const file_name);


/* printSymbolList is an auxiliary function called by printSymbolTable. It
 * iterates over a symbol list, pretty printing each symbol in the list.
 */

void printSymbolList(gpointer key, gpointer value, gpointer user_data);


/* symbolTypeToString is a helper function of printSymbolList. It returns the
 * corresponding to its argument.
 */

string symbolTypeToString(SymbolType type);


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

void printASTList(List * const list, FILE *dot_file);
void printASTDeclaration(GPDeclaration * const decl, FILE *dot_file);
void printASTStatement(GPStatement * const stmt, FILE *dot_file);
void printASTCondition(GPCondExp * const cond, FILE *dot_file);
void printASTAtom(GPAtomicExp * const atom, FILE *dot_file);
void printASTProcedure(GPProcedure * const proc, FILE *dot_file);
void printASTRule(GPRule * const rule, FILE *dot_file);
void printASTGraph(GPGraph * const graph, FILE *dot_file);
void printASTNode(GPNode * const node, FILE *dot_file);
void printASTEdge(GPEdge * const edge, FILE *dot_file);
void printASTLabel(GPLabel * const label, FILE *dot_file);
/* void printASTPosition(GPPos * const pos, FILE *dot_file); */

#endif /* INC_PRETTY_H */
