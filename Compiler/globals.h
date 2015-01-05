/* ///////////////////////////////////////////////////////////////////////////

  ==================
  Global Header File
  ==================

  Includes C's standard libraries and contains global variables and structures. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GLOBALS_H
#define INC_GLOBALS_H

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

typedef char* string;

/* Bison uses a global variable yylloc of type YYLTYPE to keep track of the 
 * locations of tokens and nonterminals. The scanner will set these values upon
 * reading each token. This is the standard YYLTYPE definition but I define it
 * here so it is seen by every file.
 */
typedef struct YYLTYPE {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;

# define YYLTYPE_IS_DECLARED 1 /* tells the parser that YYLTYPE is defined here */

extern FILE *yyin; /* Created by Bison. */

/* Declarations for functions and variables defined in gplexer.l */
extern int yylineno; 
extern string yytext; 

extern struct Stack *rule_stack;

/* Declarations for functions and variables defined in gpparser.y */
int yyparse(void);
extern int yydebug;
extern struct List *gp_program; 
extern struct GPGraph *ast_host_graph;

/* Abstract data type for GP2's marks. */
typedef enum {NONE = 0, RED, GREEN, BLUE, GREY, DASHED, ANY} MarkType; 

/* Abstract data type for conditions. */
typedef enum {INT_CHECK = 0, CHAR_CHECK, STRING_CHECK, ATOM_CHECK, EDGE_PRED,
              EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, 
	      BOOL_NOT, BOOL_OR, BOOL_AND } CondExpType;

/* Abstract data type for atomic expressions. */
typedef enum {EMPTY = 0, VARIABLE, INTEGER_CONSTANT, CHARACTER_CONSTANT,
              STRING_CONSTANT, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH,
              NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomExpType;

#endif /* INC_GLOBALS_H */
