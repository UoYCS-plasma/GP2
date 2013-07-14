/* //////////////////////////////////////////////////////////////////////////////////////////// */
/*
*						gpparser.y 					
* 
* This is the parser for GP2, written in Bison. In combination with Flex, it performs syntax 
* checking, creates a symbol table and generates an Abstract Syntax Tree for the input GP2 program.
*
* Created on 28/5/13 by Chris Bak 
* 
* Host Graphs must be parsed separately due to separate file entry and different start symbol,
* but the graph grammar of this parser can be used. I will focus on just GP programs for the
* time being.
* 
* 11/7/13: Removed Block '!' from Command, added '!' productions lower down so that 'rule!' 
* statements in conditional branches aren't required to be bracketed.
*
/* /////////////////////////////////////////////////////////////////////////////////////////// */


%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "gpparser.h"
void yyerror(char *errormsg, ...);
%}

%code requires /* place this code before YYLTYPE is defined in the generated parser */
{ 

char *filename; /* current filename for the lexer */

/* The code below redefines Bison's location structure and macro to include filenames. 
   Only the lines dealing with filenames are new; the rest is the same as Bison's definitions */

/* Bison uses a global variable yylloc of type YYLTYPE to keep track of the locations of 
   tokens and nonterminals. The scanner will set these values upon reading each token. */

typedef struct YYLTYPE {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
  char *filename;	/* new */
} YYLTYPE;

# define YYLTYPE_IS_DECLARED 1 /* tells the parser that YYLTYPE is defined here */

/* YYLLOC_DEFAULT copies location information from the RHS of a rule to the LHS symbol 
   'Current' when the parser reduces a rule (before action code is executed). The location
   of the LHS symbol will start at the first character of the first RHS symbol and will
   finish at the last character of the last RHS symbol. N is the number of symbols
   on the RHS. Bison's YYRHSLOC macro returns the location of a particular RHS symbol. */ 

# define YYLLOC_DEFAULT(Current, Rhs, N)						 \
    do											 \
      if (N)										 \
       {										 \
         (Current).first_line	= YYRHSLOC (Rhs, 1).first_line;				 \
         (Current).first_column = YYRHSLOC (Rhs, 1).first_column;			 \
         (Current).last_line	= YYRHSLOC (Rhs, N).last_line;				 \
         (Current).last_column	= YYRHSLOC (Rhs, N).last_column;			 \
         (Current).filename	= YYRHSLOC (Rhs, 1).filename;	/* new */		 \
       }										 \
     else										 \
       {  /* empty RHS */								 \
         (Current).first_line = (Current).last_line = YYRHSLOC (Rhs, 0).last_line;	 \
         (Current).first_column = (Current).last_column = YYRHSLOC (Rhs, 0).last_column; \
         (Current).filename = YYRHSLOC (Rhs, 0).filename;	/* new */		 \
       }										 \
    while (0)
}   


/* declare tokens */
/* single-character tokens do not need to be explicitly declared */

%locations /* generates code to process locations. Automatically enabled when '@n' tokens used 
              in grammar */

%token MAIN IF TRY THEN ELSE SKIP FAIL                           /* Program text keywords */
%token WHERE EDGE TRUE FALSE INDEG OUTDEG                        /* Schema condition keywords */
%token INT STRING ATOM LIST 	                                 /* Type keywords */
%token RED GREEN BLUE GREY DASHED				 /* Colour keywords */
%token INTERFACE EMPTY INJECTIVE				 /* Other keywords */
%token ARROW					                 /* Arrow */
%token NE GTE LTE			                         /* Boolean operators */
%token NUM STR MACID ID ROOT                                     /* Numbers, strings, identifiers, root node */

%left '+' '-' OR	/* lowest precedence level, left associative */
%left '*' '/' AND
%left UMINUS NOT	
%left '.'	/* highest precedence level. UMINUS represents unary '-' */

%union {
  int num;    /* A NUM token contains a number */
  char *str;  /* STRING and ID tokens contain a string */
} 

%start Program	/* Program is the start symbol of the grammar */

%%

 /* Grammar for textual GP2 programs */

Program: /* empty */
       | Program Declaration

Declaration: MainDecl
     	   | MacroDecl
           | RuleDecl

MainDecl: MAIN '=' ComSeq

MacroDecl: MacroID '=' ComSeq 
         | MacroID '=' ComSeq '[' ProcList ']' 

ProcList: /* empty */
        | ProcList RuleDecl
	| ProcList MacroDecl

ComSeq: Command
      | ComSeq ';' Command

Command: Block
       | Block '!'
       | Block OR Block
       | IF Block THEN Block 
       | IF Block THEN Block ELSE Block
       | TRY Block
       | TRY Block THEN Block
       | TRY Block THEN Block ELSE Block

 /* Perhaps create new NTs IfStatement and TryStatement? */

Block: '(' ComSeq ')'
     | SimpleCommand

SimpleCommand: RuleSetCall
	     | RuleSetCall '!'
	     | MacroCall
             | MacroCall '!'
             | SKIP
             | FAIL

RuleSetCall: RuleID 
	   | '{' IDList '}'

IDList: /* empty */ 
      | RuleID
      | IDList ',' RuleID

MacroCall: MacroID

 /* Grammar for GP2 conditional rule schemata */

RuleDecl: RuleID '(' ParamList ';' ')' Graphs Inter CondDecl INJECTIVE '=' Bool 

ParamList: /* empty */
	 | VarList ':' Type 
	 | ParamList ';' VarList ':' Type 

VarList: Variable 
       | VarList ',' Variable

Inter: INTERFACE '{' NodePairList '}'

NodePairList: /* empty */
	    | '(' NodeID ',' NodeID ')'
            | NodePairList ',' '(' NodeID ',' NodeID ')'

Bool: TRUE | FALSE

Type: INT | STRING | ATOM | LIST

 /* Grammar for GP2 graphs */

Graphs: '[' LHS ']' ARROW '[' RHS ']'

LHS: Graph
RHS: Graph

Graph: Position '|' NodeList '|' EdgeList

NodeList: /* empty */
        | '(' NodeID RootNode ',' Label ',' Position ')'
	| NodeList ',' '(' NodeID RootNode ',' Label ',' Position ')'

EdgeList: /* empty */
	| '(' EdgeID ',' NodeID ',' NodeID ',' Label ')'
	| EdgeList ',' '(' EdgeID ',' NodeID ',' NodeID ',' Label ')'

RootNode: /* empty */
	| ROOT

Position: '(' NUM ',' NUM ')'

 /* Grammar for GP2 conditions */

CondDecl: /* empty */
        | WHERE Condition

Condition: Subtype '(' Variable ')'   
         | EDGE '(' NodeID ',' NodeID LabelArg ')'	/*LabelArg NT is for optional Label argument */
         | RelList
         | NOT Condition
         | Condition OR Condition  
         | Condition AND Condition
	 | '(' Condition ')'

Subtype: INT | STRING | ATOM

LabelArg: /* empty */	
       | ',' Label

RelList: List RelOp List
       | RelList RelOp List

RelOp: '=' | NE | '>' | GTE | '<' | LTE
 
 /* Grammar for GP2 Labels */

Label: List 
     | List '#' Mark

List: EMPTY
    | AtomExp 
    | List ':' AtomExp

Mark: RED | BLUE | GREEN | GREY | DASHED

AtomExp: Variable
       | NUM 
       | INDEG '(' NodeID ')'
       | OUTDEG '(' NodeID ')'
       | '-' AtomExp %prec UMINUS	/* Use the precedence of UMINUS for this rule */
       | '(' AtomExp ')'
       | AtomExp '+' AtomExp
       | AtomExp '-' AtomExp
       | AtomExp '*' AtomExp
       | AtomExp '/' AtomExp		/* Ambiguity resolved by explicit precedences */
       | STR
       | AtomExp '.' AtomExp

 /* Identifiers */

MacroID: MACID
RuleID: ID
NodeID: ID
EdgeID: ID
Variable: ID

%%

int main(int argc, char** argv) {
       
  extern FILE *yyin; 
 
  if(argc > 1 && !strcmp(argv[1], "-d")) {
    yydebug = 1; argc--; argv++;	/* for producing the debugging file gpparser.output */
  }

  if(argc != 2) {
    fprintf(stderr, "ERROR: filename required\n");
    return 1;
  }

  if(!(yyin = fopen(argv[1], "r"))) {	/* Flex scans from yyin which is assigned to input file. */
     perror(argv[1]);
     yylineno = 1;	/* reset yylineno */
     return 1;
  }

  filename = argv[1];
  printf("Processing %s\n", filename);

  if(!yyparse())
    printf("GP2 parse succeeded\n");
  else
    printf("GP2 parse failed\n");
 
  fclose(yyin);
}

/* default bison error function, uses the location stored in yylloc */

void yyerror(char *errormsg, ...)
{
   va_list args;
   va_start(args, errormsg);

   if(yylloc.first_line)
     fprintf(stderr, "%s:%d.%d-%d.%d: error at '%s': ", yylloc.filename, yylloc.first_line,
       yylloc.first_column, yylloc.last_line, yylloc.last_column, yytext);
     vfprintf(stderr, errormsg, args);
     fprintf(stderr, "\n");
}

/* alternate error function with a location as its first argument */

void lyyerror(YYLTYPE loc, char *errormsg, ...)
{
   va_list args;
   va_start(args, errormsg);

   if(loc.first_line)
     fprintf(stderr, "%s:%d.%d-%d.%d: error at '%s': ", loc.filename, loc.first_line,
       loc.first_column, loc.last_line, loc.last_column, yytext);
     vfprintf(stderr, errormsg, args);
     fprintf(stderr, "\n");
}



