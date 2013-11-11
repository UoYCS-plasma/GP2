/* ////////////////////////////////////////////////////////////////////////////

                                seman.h                               
 
  This contains the interface for the symbol table and the declarations of
  the semantic analysis functions.
 

                   Created on 24/10/13 by Chris Bak 

//////////////////////////////////////////////////////////////////////////// */

#include <glib.h> /* hashtable and linked list functions */

extern GHashTable *gp_symbol_table;

/* enum of possible symbol types */

typedef enum {PROC_SYMBOL=0, RULE_SYMBOL, VAR_SYMBOL, LEFT_NODE_SYMBOL,
 LEFT_EDGE_SYMBOL, RIGHT_NODE_SYMBOL, RIGHT_EDGE_SYMBOL} symbol_type_t;

typedef struct Symbol {
  char *type;
  char *scope;
} Symbol;

void declaration_scan(const List *ast, GHashTable *table, char *current_scope);
