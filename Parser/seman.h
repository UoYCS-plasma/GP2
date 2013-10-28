/* ////////////////////////////////////////////////////////////////////////////

                                seman.h                               
 
  This contains the interface for the symbol table and the declarations of
  the semantic analysis functions.
 

                   Created on 24/10/13 by Chris Bak 

//////////////////////////////////////////////////////////////////////////// */

/* enum of possible symbol types */

typedef enum {PROCEDURE=0, RULE, VARIABLE, LEFT_NODE, LEFT_EDGE, RIGHT_NODE,
              RIGHT_EDGE} symbol_type_t;

typedef struct Symbol {
  symbol_type_t type;
  char *scope;
} Symbol;

