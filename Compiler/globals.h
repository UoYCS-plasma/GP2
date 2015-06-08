/* ///////////////////////////////////////////////////////////////////////////

  ==================
  Global Header File
  ==================

  Includes C's standard libraries and contains global variables and structures. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GLOBALS_H
#define INC_GLOBALS_H

/* Toggle tracing of the Bison parser. The trace is printed to stderr. */
#undef PARSER_TRACE 

/* Toggle debugging of the compiler through AST printing before and after 
 * static analysis, and through printing of the symbol table. The output files
 * are placed in the same directory as the input program with filenames
 * <program>_ast_1.dot, <program>_ast_2.dot, and <program>.tab. */
#define DEBUG_PROGRAM

/* Toggle label class indexing for host graphs. This sets a flag in the host
 * graph data structure and influences parts of the rule matching code
 * generator.*/
#define LABEL_CLASS_INDEXING

/* Convenience macros for the code generating modules that write to C header
 * and C source files. The source file pointer in each module is named "file"
 * to avoid any potential confusion with sources in graphs. */
#define printToHeader(code, ...)	       \
  do { fprintf(header, code, ##__VA_ARGS__); } \
  while(0) 

#define printToFile(code, ...)	               \
  do { fprintf(file, code, ##__VA_ARGS__); }   \
  while(0) 

/* A wrapper to a call to fprintf which indents the line written
 * to the file with <indent> number of spaces. */
#define printToFileIndented(code, indent, ...)	        	 \
  do { fprintf(file, "%*s" code, indent, " ", ##__VA_ARGS__); }  \
  while(0) 

#define PTH printToHeader
#define PTF printToFile
#define PTFI printToFileIndented

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 
#include <sys/stat.h>
#include <sys/types.h>

typedef char* string;

/* Bison uses a global variable yylloc of type YYLTYPE to keep track of the 
 * locations of tokens and nonterminals. The scanner will set these values upon
 * reading each token. This is the standard YYLTYPE definition but I define it
 * here so it is visible to the AST module. */
typedef struct YYLTYPE {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;

# define YYLTYPE_IS_DECLARED 1 /* Tells Bison that YYLTYPE is defined here. */

/* GP 2's variable types. */
typedef enum {INTEGER_VAR = 0, CHARACTER_VAR, STRING_VAR, ATOM_VAR, LIST_VAR} GPType;

typedef enum {NONE = 0, RED, GREEN, BLUE, GREY, DASHED, ANY} MarkType; 

typedef enum {INT_CHECK = 0, CHAR_CHECK, STRING_CHECK, ATOM_CHECK, EDGE_PRED,
              EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, 
	      BOOL_NOT, BOOL_OR, BOOL_AND } ConditionType;

typedef enum {INTEGER_CONSTANT = 0, STRING_CONSTANT, VARIABLE, LENGTH, INDEGREE,
              OUTDEGREE, NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomType;

#endif /* INC_GLOBALS_H */
