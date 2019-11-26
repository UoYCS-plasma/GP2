/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison interface for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* "%code requires" blocks.  */

/* Line 2068 of yacc.c  */
#line 26 "gpparser.y"

#include "ast.h"



/* Line 2068 of yacc.c  */
#line 44 "parser.h"

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     MAIN = 258,
     IF = 259,
     TRY = 260,
     THEN = 261,
     ELSE = 262,
     SKIP = 263,
     FAIL = 264,
     BREAK = 265,
     WHERE = 266,
     EDGETEST = 267,
     INDEG = 268,
     OUTDEG = 269,
     _LENGTH = 270,
     INT = 271,
     CHARACTER = 272,
     STRING = 273,
     ATOM = 274,
     LIST = 275,
     INTERFACE = 276,
     _EMPTY = 277,
     INJECTIVE = 278,
     MARK = 279,
     ANY_MARK = 280,
     ARROW = 281,
     NEQ = 282,
     GTEQ = 283,
     LTEQ = 284,
     NUM = 285,
     DNUM = 286,
     STR = 287,
     PROCID = 288,
     ID = 289,
     ROOT = 290,
     BIDIRECTIONAL = 291,
     GP_GRAPH = 292,
     GP_RULE = 293,
     GP_PROGRAM = 294,
     OR = 295,
     AND = 296,
     NOT = 297,
     UMINUS = 298
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 2068 of yacc.c  */
#line 71 "gpparser.y"
  
  int num;   /* value of NUM token. */
  double dnum; /* value of DNUM token. */
  char *str; /* value of STRING and CHAR tokens. */
  char *id;  /* value of PROCID and ID tokens. */
  int mark;  /* enum MarkTypes, value of MARK token. */



/* Line 2068 of yacc.c  */
#line 105 "gpparser.y"
  
  struct List *list; 
  struct GPDeclaration *decl;
  struct GPCommand *command;
  struct GPProcedure *proc;
  struct GPRule *rule;
  struct GPGraph *graph;
  struct GPNode *node;
  struct GPEdge *edge;
  struct GPPos *pos;
  struct GPCondition *cond_exp;
  struct GPLabel *label;  
  struct GPAtom *atom_exp;

  int list_type; /* enum ListType */
  int check_type; /* enum CondExpType */



/* Line 2068 of yacc.c  */
#line 135 "parser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYLTYPE yylloc;

/* "%code provides" blocks.  */

/* Line 2068 of yacc.c  */
#line 31 "gpparser.y"

extern GPGraph *ast_host_graph; 
extern GPRule *gp_rule; 
extern List *gp_program; 
extern int yylineno;
extern string yytext;
extern FILE *yyin;
extern bool syntax_error;



/* Line 2068 of yacc.c  */
#line 175 "parser.h"
