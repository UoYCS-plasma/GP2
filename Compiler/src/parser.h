/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

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

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 42 "parser.y" /* yacc.c:1909  */

#include "ast.h"

#line 48 "parser.h" /* yacc.c:1909  */

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
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
    GP_PROGRAM = 292,
    GP_GRAPH = 293,
    GP_RULE = 294,
    OR = 295,
    AND = 296,
    NOT = 297,
    UMINUS = 298
  };
#endif
/* Tokens.  */
#define MAIN 258
#define IF 259
#define TRY 260
#define THEN 261
#define ELSE 262
#define SKIP 263
#define FAIL 264
#define BREAK 265
#define WHERE 266
#define EDGETEST 267
#define INDEG 268
#define OUTDEG 269
#define _LENGTH 270
#define INT 271
#define CHARACTER 272
#define STRING 273
#define ATOM 274
#define LIST 275
#define INTERFACE 276
#define _EMPTY 277
#define INJECTIVE 278
#define MARK 279
#define ANY_MARK 280
#define ARROW 281
#define NEQ 282
#define GTEQ 283
#define LTEQ 284
#define NUM 285
#define DNUM 286
#define STR 287
#define PROCID 288
#define ID 289
#define ROOT 290
#define BIDIRECTIONAL 291
#define GP_PROGRAM 292
#define GP_GRAPH 293
#define GP_RULE 294
#define OR 295
#define AND 296
#define NOT 297
#define UMINUS 298

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 77 "parser.y" /* yacc.c:1909  */
  
  int num;   /* value of NUM token. */
  double dnum; /* value of DNUM token. */
  char *str; /* value of STRING and CHAR tokens. */
  char *id;  /* value of PROCID and ID tokens. */
  int mark;  /* enum MarkTypes, value of MARK token. */
#line 110 "parser.y" /* yacc.c:1909  */
  
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

#line 171 "parser.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;
int yyparse (void);
/* "%code provides" blocks.  */
#line 47 "parser.y" /* yacc.c:1909  */

extern List *gp_program; 
extern int yylineno;
extern string yytext;
extern FILE *yyin;
extern bool syntax_error;

#line 206 "parser.h" /* yacc.c:1909  */

#endif /* !YY_YY_PARSER_H_INCLUDED  */
