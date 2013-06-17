/* //////////////////////////////////////////////////////////////////////////////////////////// */
/*
*						gpparser.y
* 						Version 1.0
* 
* This is the parser for GP2, written in Bison. In combination with Flex, it  performs syntax 
* checking, creates a symbol table and generates an Abstract Syntax Tree for the input GP2 program.
*
* v1.0 Basic parser that interacts with the scanner to print something to the screen.
*
* Created on 28/5/13 by Chris Bak 
* 
* Host Graphs must be parsed separately due to separate file entry and different start symbol,
* but the graph grammar of this parser can be used. I will focus on just GP programs for the
* time being.
* 
* 17/6/13: Changed MacroDecl production order: ComSeq ProcList -> ProcList ComSeq
*          Added '@' before MacroCall 
*          Changed RuleSetCall to '[' IDList ']'
/* /////////////////////////////////////////////////////////////////////////////////////////// */

%{
# define YYDEBUG 1	/* for producing the debugging file gpparser.output */
# include <stdio.h>
# include <stdlib.h>
# include "gpparser.h"
%}

/* declare tokens */
/* single-character tokens do not need to be explicitly declared */

%token MAIN IF TRY THEN ELSE SKIP FAIL                           /* Program text keywords */
%token WHERE EDGE TRUE FALSE INDEG OUTDEG                        /* Schema condition keywords */
%token INT STRING ATOM LIST 	                                 /* Type keywords */
%token INTERFACE EMPTY INJECTIVE				 /* Other keywords */
%token ARROW					                 /* Arrow */
%token NE GTE LTE			                         /* Boolean operators */
%token NUM STR ID ROOT                                           /* Numbers, strings, identifiers, root node */
%token END 							 /* End of file */

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
         | MacroID '=' '{' ProcList ComSeq '}' 

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
	     | MacroCall
             | SKIP
             | FAIL

RuleSetCall: RuleID 
	   | '[' IDList ']'

IDList: /* empty */ 
      | RuleID
      | IDList ',' RuleID

MacroCall: '@' MacroID

 /* Grammar for GP2 conditional rule schemata */

RuleDecl: RuleID '(' ParamList ')' '=' '{' Graphs Inter WHERE CondDecl INJECTIVE '=' Bool '}'

ParamList: /* empty */
	 | ParamList ';' VarList ':' Type 

VarList: /* empty */
       | VarList ',' Variable

Inter: INTERFACE '=' '{' NodePairList '}'

NodePairList: /* empty */
            | NodePairList ',' '(' NodeID ',' NodeID ')'

Bool: TRUE | FALSE

Type: INT | STRING | ATOM | LIST

 /* Grammar for GP2 graphs */

Graphs: '{' LHS '}' ARROW '{' RHS '}'

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

CondDecl: Subtype '(' Variable ')'          
	| List '=' List
        | List NE List
        | AtomExp RelOp AtomExp
        | EDGE '(' NodeID ',' NodeID ListArg ')'	/*ListArg NT is for optional List argument */
        | NOT CondDecl 
        | CondDecl OR CondDecl   
        | CondDecl AND CondDecl		/* Amibiguity resolved by explicit precedences */

Subtype: INT | STRING | ATOM

ListArg: /* empty */	
       | ',' List

RelOp: '>' | GTE | '<' | LTE
 
 /* Grammar for GP2 Labels */

Label: List 
     | List '#'

List: EMPTY
    | AtomExp 
    | List ':' AtomExp

AtomExp: Variable
       | NUM 
       | INDEG '(' NodeID ')'
       | OUTDEG '(' NodeID ')'
       | '-' AtomExp %prec UMINUS	/* Use the precedence of UMINUS for this rule */
       | '(' AtomExp ')'
       | AtomExp '+' AtomExp
       | AtomExp '-' AtomExp
       | AtomExp '*' AtomExp
       | AtomExp '/' AtomExp
       | STR
       | AtomExp '.' AtomExp

 /* Identifiers */

MacroID: ID
RuleID: ID
NodeID: ID
EdgeID: ID
Variable: ID

%%

int main(int argc, char** argv) {
       
  if(argc != 2) {
    fprintf(stderr, "ERROR: filename required\n");
    return 1;
  }

  if(!(yyin = fopen(argv[1], "r"))) {	/* Flex scans from yyin which is assigned to input file. */
     perror(argv[1]);
     yylineno = 1;	/* reset yylineno */
     return 1;
  }

  curfilename = argv[1];
  printf("Processing %s\n\n", curfilename);

  yyparse();
}


int yyerror (char const *s)	/* basic error function for parser */
{
  fprintf (stderr, "%s\n", s);
}



