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
/* /////////////////////////////////////////////////////////////////////////////////////////// */


%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h> /* for error functions */
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
%left UMINUS NOT	/* UMINUS represents unary '-' */
%left '.'	/* highest precedence level */

/* %type to set types of NTs */

%union {  /* defines possible values of nonterminals and tokens */
  int num;    /* value of a NUM token */
  char *str;  /* value of STRING tokens */
  char *id;   /* value of MACID and ID tokens */
  rel_t rel;  /* value of REL token */
  mark_t mark; /* value of a MARK token */
  type_t type; /* value of TYPE and SUBTYPE tokens */
  symbol *name; /* pointer to symbol table */
  AST *tree; /* pointer to an AST */
} 

%start Program	/* Program is the start symbol of the grammar */

%%

 /* Grammar for textual GP2 programs */

Program: /* empty */
       | Program Declaration /* new AST. */ 

Declaration: MainDecl /* $$ = $1 */
     	   | MacroDecl
           | RuleDecl

MainDecl: MAIN '=' ComSeq

MacroDecl: MacroID '=' ComSeq /* new GPMacroDecl with empty second branch */
         | MacroID '=' '[' ProcList ']' ComSeq /* new GPMacroDecl */ 

ProcList: /* empty */
        | ProcList RuleDecl /* new AST */
	| ProcList MacroDecl /* new AST */

ComSeq: Command /* $$ = $1 */ 
      | ComSeq ';' Command  /* new AST */

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
      | IDList ',' RuleID /* new AST */

MacroCall: MacroID

 /* Grammar for GP2 conditional rule schemata */

RuleDecl: RuleID '(' ParamList ')' Graphs Inter CondDecl INJECTIVE '=' Bool /* new GPRule - injective bool is attribute, ruleID is pointer to symbol table */

ParamList: /* empty */
	 | VarList ':' Type /* new AST type [Type]List where [Type] depends on $3. Or maybe a generic VarList will suffice as symtable will contain type information */
	 | ParamList ';' VarList ':' Type /* new AST [Type]List as above*/

 /* some post-processing could be done to sort out the parameter list AST here as the AST generated
    in this manner is a bit strange. Need to remove the intermediate ASTs created from the reduction
    of the second VarList production so that their parents point directly to the variables. This may be achieved during AST construction with some pointer manipulation */

VarList: Variable /* $$ = $1 */
       | VarList ',' Variable /* new AST type VarList */

Inter: INTERFACE '{' NodePairList '}'

NodePairList: /* empty */
	    | NodePair
            | NodePairList ',' NodePair

NodePair: '(' NodeID ',' NodeID ')'

Bool: TRUE | FALSE

Type: INT | STRING | ATOM | LIST /* keep track of the specific type somehow for VarList : Type */

 /* Grammar for GP2 graphs */

Graphs: '[' LHS ']' ARROW '[' RHS ']' /* new AST $2 $6 */

LHS: Graph /* $$ = $1, possibly a LHS flag */
RHS: Graph /* $$ = $1, possible a RHS flag */

Graph: Position '|' NodeList '|' EdgeList /* new GPGraph */

NodeList: /* empty */
        | Node
        | NodeList ',' Node	

Node:  '(' NodeID RootNode ',' Label ',' Position ')'

EdgeList: /* empty */
	| Edge
        | Edge ',' EdgeList

Edge: '(' EdgeID ',' NodeID ',' NodeID ',' Label ')'

RootNode: /* empty */
	| ROOT /* switch root flag on */

Position: '(' NUM ',' NUM ')' /* new GPPos $2 $4 */

 /* Grammar for GP2 conditions */

CondDecl: /* empty */
        | WHERE Condition

Condition: Subtype '(' Variable ')' /* new GPTypeCheck node */
         | EDGE '(' NodeID ',' NodeID LabelArg ')'	/*LabelArg NT is for optional Label argument */
         | CmpList
         | NOT Condition /* new AST with one branch */
         | Condition OR Condition  /* new AST */
         | Condition AND Condition /* new AST */
	 | '(' Condition ')' /* $$ = $2 */

Subtype: INT | STRING | ATOM /* type_t values  */

LabelArg: /* empty */	
 	| ',' Label

CmpList: List CMP List /* new AST with type $2 */
       | RelList CMP List /* as above. Is there a way to make this into a sequence of AND ASTs instead of a list of RelOps? MAYBE. */


  /* Grammar for GP2 Labels */

Label: List 				{ $$ = newLabel(yylloc, NONE , $1); }
     | List '#' MARK			{ $$ = newLabel(yylloc, $3, $1); } 

List: EMPTY                             { $$ = 0; }
    | AtomExp				{ $$ = newAtom(yylloc, $1, 0 /* null pointer */); } 
    | List ':' AtomExp			{ $$ = newAtom(yylloc, $3, $1); }

AtomExp: Variable			{ $$ = newVariable(yylloc, $1); }
       | NUM 				{ $$ = newNumber(yylloc, $1); }
       | STR 				{ $$ = newString(yylloc, $1); }
       | INDEG '(' NodeID ')' 		{ $$ = newDegreeOp(INDEGREE, yylloc, $3); }
       | OUTDEG '(' NodeID ')' 		{ $$ = newDegreeOp(OUTDEGREE, yylloc, $3); }
       | LLEN '(' List ')' 		{ $$ = newListLength(yylloc, $3); }
       | SLEN '(' AtomExp ')' 		{ $$ = newStringLength(yylloc, $3); }
       | '-' AtomExp %prec UMINUS 	{ $2 = $2*(-1); $$ = $2; } 
       | '(' AtomExp ')' 		{ $$ = $2; }
       | AtomExp '+' AtomExp 		{ $$ = newBinaryOp(ADD, yylloc, $1, $3);  }
       | AtomExp '-' AtomExp 		{ $$ = newBinaryOp(SUBTRACT, yylloc, $1, $3); }
       | AtomExp '*' AtomExp 		{ $$ = newBinaryOp(MULTIPLY, yylloc, $1, $3); }
       | AtomExp '/' AtomExp 		{ $$ = newBinaryOp(DIVIDE, yylloc, $1, $3); }
	/* Ambiguity resolved by explicit precedences */
       | AtomExp '.' AtomExp 		{ $$ = newBinaryOp(CONCAT, yylloc, $1, $3); }

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

  if(!(yyin = fopen(argv[1], "r"))) {	/* Flex scans from yyin. */
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

/* default bison error function, implicitly uses the location stored in yylloc */

void yyerror(char *errormsg, ...)
{
   va_list args; /* variables of type va_list stores variable length argument list */
   va_start(args, errormsg); /* macro to initalise args to retrieve arguments after char *errormsg */

   if(yylloc.first_line)
     fprintf(stderr, "%s:%d.%d-%d.%d: error at '%s': ", yylloc.filename, yylloc.first_line,
       yylloc.first_column, yylloc.last_line, yylloc.last_column, yytext);
     vfprintf(stderr, errormsg, args);
     fprintf(stderr, "\n");

   va_end(args);
}

/* alternate error function for an explicit location */

void lyyerror(YYLTYPE loc, char *errormsg, ...)
{
   va_list args;
   va_start(args, errormsg);

   if(loc.first_line)
     fprintf(stderr, "%s:%d.%d-%d.%d: error at '%s': ", loc.filename, loc.first_line,
       loc.first_column, loc.last_line, loc.last_column, yytext);
     vfprintf(stderr, errormsg, args);
     fprintf(stderr, "\n");

   va_end(args);
}



