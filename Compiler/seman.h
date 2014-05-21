/* ////////////////////////////////////////////////////////////////////////////

                                seman.h                               
 
  This contains the interface for the symbol table and the declarations of
  the semantic analysis functions.
 

                   Created on 24/10/13 by Chris Bak 

//////////////////////////////////////////////////////////////////////////// */

#ifndef INC_SEMAN_H
#define INC_SEMAN_H

#include "ast.h" /* struct List */
#include <glib.h> /* hashtable */
#include <stdbool.h>

extern GHashTable *gp_symbol_table; /* Defined in main.c */

typedef struct Symbol {
  char *type; /* rule, procedure, integer, character, string, atom, list,
               * left_node, left_edge, right_node, right_edge */
  char *scope; /* The procedure in which the symbol is visible. 
		* "Global" scope is seen by all procedures. */   
  char *containing_rule; /* for variables, nodes and edges */
  bool is_var; /* set to true if this symbol represents a GP variable. */
  bool in_lhs; /* set to true if a variable matching the symbol is found
                * in the LHS of a rule */
  bool wildcard; /* set to true if a node or edge has the cyan mark. */
} Symbol;

struct List *reverse (struct List * listHead);
void reverseGraphAST (GPGraph *graph); 
void freeSymbolList(gpointer key, gpointer value, gpointer data);
bool declarationScan(const List *ast, GHashTable *table, char *scope);
bool semanticCheck(List *declarations, GHashTable *table, char *scope);
void statementScan(GPStatement *statement, GHashTable *table, char *scope);
void validateCall(char *name, GHashTable *table, char *scope, 
                   char *call_type);
void ruleScan(GPRule *rule, GHashTable *table, char *scope);
void enterVariables(char *type, List *variables, GHashTable *table, 
		     char *scope, char *rule_name);
void graphScan(GPGraph *graph, GHashTable *table, char *scope, 
                char *rule_name, char side);
void interfaceScan(List *interface, GHashTable *table, char *scope,
                    char *rule_name);
void conditionScan(GPCondExp *condition, GHashTable *table, char *scope,
                    char *rule_name);
void gpListScan(List **gp_list, GHashTable *table, char *scope,
                  char *rule_name, char location);
void atomicExpScan(GPAtomicExp *atom_exp, GHashTable *table, char *scope,
                    char *rule_name, char location, bool int_check,
		    bool string_check);

#endif /* INC_SEMAN_H */
