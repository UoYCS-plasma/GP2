/* ////////////////////////////////////////////////////////////////////////////

                                seman.h                               
 
  This contains the interface for the symbol table and the declarations of
  the semantic analysis functions.
 

                   Created on 24/10/13 by Chris Bak 

//////////////////////////////////////////////////////////////////////////// */

#include <glib.h> /* hashtable and linked list functions */
#include <stdbool.h>

extern GHashTable *gp_symbol_table;

/* enum of possible symbol types */

typedef enum {PROC_SYMBOL=0, RULE_SYMBOL, VAR_SYMBOL, LEFT_NODE_SYMBOL,
 LEFT_EDGE_SYMBOL, RIGHT_NODE_SYMBOL, RIGHT_EDGE_SYMBOL} symbol_type_t;

typedef struct Symbol {
  char *type;
  char *scope; /* the procedure in which the symbol is visible. 
		* "Global" scope is seen by all procedures. */   
  char *containing_rule; /* for variables, nodes and edges */
  /* bit field for variable context flags */
  struct { 
    unsigned int is_var : 1;	  
    unsigned int int_exp : 1;
    unsigned int string_exp : 1;
    unsigned int right_label : 1; 
  } context;
} Symbol;

void declaration_scan(const List *ast, GHashTable *table, char *current_scope);
int semantic_check(List *ast, GHashTable *table, char *scope);
void validate_call(char *name, GHashTable *table, char *scope);
void statement_scan(GPStatement *statement, GHashTable *table, char *scope);
void rule_scan(GPRule *rule, GHashTable *table, char *scope);
void enter_variables(char *type, List *variables, GHashTable *table, 
		     char *scope, char *rule);
