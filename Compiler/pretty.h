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
#define prettyPrint(POINTER_ARG,TYPE)                                               \
  do { 									            \
       if(POINTER_ARG != NULL)                                                      \
         printAST ## TYPE (POINTER_ARG, dot_file);                                  \
       else {                                                                       \
         print_to_dot_file("node%d[shape=plaintext,label=\"%d ERROR\"]\n",          \
                           next_id, next_id);                                       \
         print_to_log("Error: Unexpected NULL pointer at AST node %d\n", next_id);  \
       }            							            \
     }                                                                              \
  while (0)

/* prettyPrintList is used to process members of AST structs that point
 * to a struct List. It should only be used when a NULL pointer is valid in
 * the GP AST. For example, this macro is called to print the node 
 * and edge lists of a graph, which may be NULL, but not for the list
 * component of a label, which should not point to NULL. Use printList
 * or prettyPrint for the latter cases.
 * 
 * The NODE_TYPE parameter is the name of the argument of the calling function:
 * the name of the structure that stores the id.
 *
 * If POINTER_ARG is NULL, a NULL node is written to the .dot file and an edge
 * is created from the current node to the NULL node with the label EDGE_LABEL. 
 *
 * Otherwise an edge is written with label EDGE_LABEL, pointing from the 
 * current node to the node that will be created by the printList call 
 * immediately following this edge creation. The use of the global node counter
 * next_id ensures that the edge points to the correct node.
 */
#define prettyPrintList(POINTER_ARG, NODE_TYPE, EDGE_LABEL)                 \
   do {                                                                     \
        if(POINTER_ARG == NULL) {                                           \
          print_to_dot_file("node%d[shape=plaintext,label=\"%d NULL\"]\n",  \
                            next_id, next_id);                              \
          print_to_dot_file("node%d->node%d[label=\"" #EDGE_LABEL "\"]\n",  \
                            NODE_TYPE->id, next_id);                        \
          next_id += 1;                                                     \
        }							            \
        else {                                                              \
          print_to_dot_file("node%d->node%d[label=\"" #EDGE_LABEL "\"]\n",  \
                            NODE_TYPE->id, next_id);                        \
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

#define printListNode(NODE_LABEL)                                           \
  do                                                                        \
  {                                                                         \
     print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"       \
                       #NODE_LABEL "\"]\n", list->id, list->id,             \
                       LOCATION_ARGS(list->location));                      \
     print_to_dot_file("node%d->node%d[label=\"\"]\n", list->id, next_id);  \
  }                                                                         \
  while(0)              

#define printDeclarationNode(NODE_LABEL_1, NODE_LABEL_2, NEXT_ID)       \
  do                                                                    \
  {                                                                     \
     print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"             \
                       #NODE_LABEL_1 "\"]\n", decl->id, decl->id,       \
                       LOCATION_ARGS(decl->location));                  \
     print_to_dot_file("node%d->node%d[label=\"" #NODE_LABEL_2 "\"]\n", \
                       decl->id, NEXT_ID);                              \
  }                                                                     \
  while(0)     

#define printConditionalNode(NODE_LABEL)                                  \
  do                                                                      \
  {                                                                       \
     print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"               \
                       #NODE_LABEL "\\n Restore Point = %d\\n"            \
                       "Roll Back = %d\"]\n", command->id, command->id,   \
                       LOCATION_ARGS(command->location),                  \
                       command->cond_branch.restore_point,                \
                       command->cond_branch.roll_back);                   \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"condition\"]\n",           \
                       command->id, next_id);                             \
     prettyPrint(command->cond_branch.condition, Command);                \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"then\"]\n",                \
                       command->id, next_id);                             \
     prettyPrint(command->cond_branch.then_command, Command);             \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"else\"]\n",                \
                       command->id, next_id);                             \
     prettyPrint(command->cond_branch.else_command, Command);             \
  }                                                                       \
  while(0)     

#define printTypeCheckNode(NODE_LABEL_1, NODE_LABEL_2)                               \
  do                                                                                 \
  {                                                                                  \
     if(cond->var != NULL)                                                           \
        print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"                       \
                          #NODE_LABEL_1 "\\n Variable: %s\"]\n", cond->id, cond->id, \
                          LOCATION_ARGS(cond->location), cond->var);                 \
     else                                                                            \
     {                                                                               \
        print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"                       \
                          "Variable: \\n UNDEFINED\"]\n", cond->id, cond->id,        \
                          LOCATION_ARGS(cond->location));                            \
        print_to_dot_file("Error (printASTCondition." #NODE_LABEL_2 "): "            \
                          "Undefined name at AST node %d", cond->id);                \
     }                                                                               \
  }                                                                                  \
  while(0)                                                                           \

#define printListEqualityNode(NODE_LABEL)                                 \
  do                                                                      \
  {                                                                       \
     print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"               \
                       #NODE_LABEL "\"]\n", cond->id, cond->id,           \
                       LOCATION_ARGS(cond->location));                    \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"left list\"]\n",           \
                       cond->id, next_id);                                \
     prettyPrint(cond->list_cmp.left_list, List);                         \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"right list\"]\n",          \
                       cond->id, next_id);                                \
     prettyPrint(cond->list_cmp.right_list, List);                        \
  }                                                                       \
  while(0)  

#define printRelationalNode(NODE_LABEL)                                   \
  do                                                                      \
  {                                                                       \
     print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"               \
                       #NODE_LABEL "\"]\n", cond->id, cond->id,           \
                       LOCATION_ARGS(cond->location));                    \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"left exp\"]\n",            \
                       cond->id, next_id);                                \
     prettyPrint(cond->atom_cmp.left_exp, Atom);                          \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"right exp\"]\n",           \
                       cond->id, next_id);                                \
     prettyPrint(cond->atom_cmp.right_exp, Atom);                         \
  }                                                                       \
  while(0)  

#define printBinaryBooleanNode(NODE_LABEL)                                \
  do                                                                      \
  {                                                                       \
     print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"               \
                       #NODE_LABEL "\"]\n", cond->id, cond->id,           \
                       LOCATION_ARGS(cond->location));                    \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"left exp\"]\n",            \
                       cond->id, next_id);                                \
     prettyPrint(cond->bin_exp.left_exp, Condition);                      \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"right exp\"]\n",           \
                       cond->id, next_id);                                \
     prettyPrint(cond->bin_exp.right_exp, Condition);                     \
  }                                                                       \
  while(0)  

#define printBinaryOperatorNode(NODE_LABEL)                               \
  do                                                                      \
  {                                                                       \
     print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n"               \
                       #NODE_LABEL "\"]\n", atom->id, atom->id,           \
                       LOCATION_ARGS(atom->location));                    \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"left exp\"]\n",            \
                       atom->id, next_id);                                \
     prettyPrint(atom->bin_op.left_exp, Atom);                            \
                                                                          \
     print_to_dot_file("node%d->node%d[label=\"right exp\"]\n",           \
                       atom->id, next_id);                                \
     prettyPrint(atom->bin_op.right_exp, Atom);                           \
  }                                                                       \
  while(0)  

#define printDegreeOperatorNode(NODE_LABEL_1, NODE_LABEL_2)                 \
  do                                                                        \
  {                                                                         \
     if(atom->node_id != NULL)                                              \
     print_to_dot_file("node%d[label=\"%d\\n%d.%d-%d.%d\\n" #NODE_LABEL_1   \
                       "(%s)\"]\n", atom->id, atom->id,                     \
                       LOCATION_ARGS(atom->location), atom->node_id);       \
     else                                                                   \
     {                                                                      \
        print_to_dot_file("node%d[shape=box,label=\"%d\\n%d.%d-%d.%d\\n"    \
                          #NODE_LABEL_1": \\n UNDEFINED\"]\n",              \
                          atom->id, atom->id,                               \
                          LOCATION_ARGS(atom->location));                   \
        print_to_log("Error (printASTAtom." #NODE_LABEL_2 "): Undefined "   \
                     "node name at AST node %d", atom->id);                 \
     }                                                                      \
  }                                                                         \
  while(0)  

/* Write the symbol table to the file <program_name>.tab.
 * printSymbolList is called on each hash table entry. */
void printSymbolTable(GHashTable *table, string program_name);
void printSymbolList(gpointer key, gpointer , gpointer user_data);

/* Creates the file <file_name><suffix>.dot to which is printed a DOT
 * representation of the passed AST for visualisation. printDotAST is initially
 * called to print the program AST, while printASTGraph is called to print the 
 * host graph AST. */
void printDotAST(List *const gp_ast, string file_name, string suffix);

/* AST nodes for rules and procedures can have more than one parent node.
 * To avoid these nodes being printed twice, their printing functions only
 * write to the file if their ID is the initial value (0). 
 * However, if the AST is printed more than once during program execution, 
 * the AST nodes won't have their initial IDs after the first print. Without 
 * resetting the IDs of rule and procedure nodes, they won't be printed at all
 * on subsequent AST prints. Note that this is only a problem for AST nodes
 * with more than one parent node.
 * This function is called before printASTList to ensure that all rule and procedure
 * nodes have ID 0 which results in the correct printing of the AST. */
void resetRuleAndProcedureIds(List *list);

void printDotHostGraph(GPGraph *const host_graph_ast, string file_name);
void printASTList(List * const list, FILE *dot_file);
void printASTDeclaration(GPDeclaration * const decl, FILE *dot_file);
void printASTCommand(GPCommand * const stmt, FILE *dot_file);
void printASTCondition(GPCondition * const cond, FILE *dot_file);
void printASTAtom(GPAtom * const atom, FILE *dot_file);
void printASTProcedure(GPProcedure * const proc, FILE *dot_file);
void printASTRule(GPRule * const rule, FILE *dot_file);
void printASTGraph(GPGraph * const graph, FILE *dot_file);
void printASTNode(GPNode * const node, FILE *dot_file);
void printASTEdge(GPEdge * const edge, FILE *dot_file);
void printASTLabel(GPLabel * const label, FILE *dot_file);

#endif /* INC_PRETTY_H */
