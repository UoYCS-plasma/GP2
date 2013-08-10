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

%left '+' '-' AND	/* lowest precedence level, left associative */
%left '*' '/' OR
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

MacroDecl: MacroID '=' ComSeq /* new GPMacroDecl with empty second branch */
         | MacroID '=' '[' ProcList ']' ComSeq /* new GPMacroDecl */ 

ProcList: /* empty */
        | ProcList RuleDecl /* new AST */
	| ProcList MacroDecl /* new AST */

ComSeq: Command /* $$ = $1 */ 
      | Command ';' ComSeq  /* new AST. Explain right recursion. */

Command: Block /* $$ = $1 */
       | IF Block THEN Block /* new GPCond, third arg empty */
       | IF Block THEN Block ELSE Block
       | TRY Block /* new GPCond, null pointers for two arguments, or maybe pointers to skip */
       | TRY Block THEN Block
       | TRY Block THEN Block ELSE Block

 /* Perhaps create new NTs IfStatement and TryStatement? */

Block: '(' ComSeq ')' /* $$ = $2 */
     | '(' ComSeq ')' '!' /* new GPloop $2 */
     | SimpleCommand /* $$ = $1 */
     | Block OR Block /* new  AST */

SimpleCommand: RuleSetCall /* $$ = $1 */
	     | RuleSetCall '!' /* new GPloop $1 */
	     | MacroCall /* $$ = $1 */
             | MacroCall '!' /* new GPloop $1 */
             | SKIP
             | FAIL

RuleSetCall: RuleID /* $$ = $1 */
	   | '{' IDList '}' /* new GPChoice */

IDList: /* empty */ 
      | RuleID /* $$ = $1 */
      | RuleID ',' IDList /* new AST */

MacroCall: MacroID

 /* Grammar for GP2 conditional rule schemata */

RuleDecl: RuleID '(' ParamList ')' Graphs Inter CondDecl INJECTIVE '=' Bool /* new GPRule - injective bool is attribute, ruleID is pointer to symbol table */

ParamList: /* empty */
	 | VarList ':' Type /* new AST type [Type]List where [Type] depends on $3. Or maybe a generic VarList will suffice as symtable will contain type information */
	 | ParamList ';' VarList ':' Type 

VarList: Variable /* $$ = $1 or maybe nothing */
       | VarList ',' Variable /* new AST type VarList */

Inter: INTERFACE '{' NodePairList '}'

 /* Like NodeList, split NodePairList up into NodePair */

NodePairList: /* empty */
	    | '(' NodeID ',' NodeID ')'
            | NodePairList ',' '(' NodeID ',' NodeID ')'

Bool: TRUE | FALSE

Type: INT | STRING | ATOM | LIST

 /* Grammar for GP2 graphs */

Graphs: '[' LHS ']' ARROW '[' RHS ']' /* new AST $2 $6 */

LHS: Graph /* $$ = $1, possibly a LHS flag */
RHS: Graph /* $$ = $1, possible a RHS flag */

Graph: Position '|' NodeList '|' EdgeList /* new GPGraph */

 /* for NodeList and EdgeList, create new NTs for each individual
    item i.e. Node: '(' NodeID ... and then do
    NodeList: empty | Node | NodeList ',' Node. This will make the AST construction possible. */

NodeList: /* empty */ 
        | '(' NodeID RootNode ',' Label ',' Position ')'
	| NodeList ',' '(' NodeID RootNode ',' Label ',' Position ')'

EdgeList: /* empty */
	| '(' EdgeID ',' NodeID ',' NodeID ',' Label ')'
	| EdgeList ',' '(' EdgeID ',' NodeID ',' NodeID ',' Label ')'

RootNode: /* empty */
	| ROOT /* switch root flag on */

Position: '(' NUM ',' NUM ')' /* new GPPos $2 $4 */

 /* Grammar for GP2 conditions */

CondDecl: /* empty */
        | WHERE Condition

Condition: Subtype '(' Variable ')' /* use the subtype flag from Subtype production to create a new node of the appropriate type, with a symbol table pointer to Variable. Or maybe do nothing with the Subtype production and deal with this as if it were, say, STRING '(' Variable ')' */
         | EDGE '(' NodeID ',' NodeID LabelArg ')'	/*LabelArg NT is for optional Label argument */
         | RelList
         | NOT Condition /* new AST with one branch */
         | Condition OR Condition  /* new AST */
         | Condition AND Condition /* new AST */
	 | '(' Condition ')' /* $$ = $2 */

Subtype: INT | STRING | ATOM /* set a subtype flag to whichever keyworld is reduced. Somehow. */

LabelArg: /* empty */	
       | ',' Label

RelList: List RelOp List /* new AST with type RelOp */
       | RelList RelOp List /* as above. Is there a way to make this into a sequence of AND ASTs instead of a list of RelOps? MAYBE. */

RelOp: '=' | NE | '>' | GTE | '<' | LTE /* record the operator somehow */

 
 /* Grammar for GP2 Labels */

Label: List /* $$ = $1 */
     | List '#' Mark /* new AST with type Label, left points to List, right to Mark */

List: EMPTY
    | AtomExp /* when this is reduced, AtomExp ($1) points to the AST of the most recently parsed AtomExp. Make List point to this with $$ = $1. */
    | AtomExp ':' List /* new AST, left branch List, right branch AtomExp. Explain right recursion. */

Mark: RED | BLUE | GREEN | GREY | DASHED

AtomExp: Variable /* new GPVarExp */
       | NUM /* new GPnum */
       | INDEG '(' NodeID ')' /* new GPdegree */
       | OUTDEG '(' NodeID ')' /* new GPdegree */
       | '-' AtomExp %prec UMINUS	/* Use the precedence of UMINUS for this rule. Change value of AST pointed to by AtomExp to  0 - yylval. Context: AtomExp must be an integer expression */ 
       | '(' AtomExp ')' /* probably $$ = $2 */
       | AtomExp '+' AtomExp /* new AST for arithops. Context: AtomExps must be integer expressions */
       | AtomExp '-' AtomExp
       | AtomExp '*' AtomExp
       | AtomExp '/' AtomExp		/* Ambiguity resolved by explicit precedences */
       | STR /* new GPstring */
       | AtomExp '.' AtomExp /* new AST. Context: Both AtomExps need to be strings */

 /* Identifiers */

 /* symtable contains scoping information for rules/macros */
MacroID: MACID /* pointers to symtable for these rules */
RuleID: ID
NodeID: ID
EdgeID: ID
Variable: ID

%%

/* add a token printing procedure here for debugging */

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



