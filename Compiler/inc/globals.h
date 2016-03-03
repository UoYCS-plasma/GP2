/* ///////////////////////////////////////////////////////////////////////////

  Copyright 2015-2016 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>.

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
 * <program>.dot and <program>.tab. */
#undef DEBUG_PROGRAM

/* If defined, lists are stored in a hash table at runtime as a single point of
 * reference. Otherwise, nodes and edges point to their own copies of their list. */
#define LIST_HASHING

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

#define print_to_log(error_message, ...)                    \
  do { fprintf(log_file, error_message, ##__VA_ARGS__); }   \
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
#include <unistd.h>

typedef char* string;

extern FILE *log_file;
extern bool graph_copying;

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
