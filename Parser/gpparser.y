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
%token NEQ GTEQ LTEQ			                         /* Boolean operators */
%token NUM STR PROCID ID ROOT                                    /* Numbers, strings, identifiers, root node */

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

Program: Declaration	                
       | Program Declaration /* new AST. */ 

Declaration: MainDecl 			{ $$ = addGlobalDecl(MAIN_DECL, yylloc, $1, NULL); }
     	   | ProcDecl			{ $$ = addGlobalDecl(PROCEDURE_DECL, yylloc, $1, NULL); }
           | RuleDecl			{ $$ = addGlobalDecl(RULE_DECL, yylloc, $1, NULL); }

MainDecl: MAIN '=' ComSeq		{ $$ = newMain(yylloc, $3); }

ProcDecl: ProcID '=' ComSeq 		{ $$ = newProcedure(yylloc, $1, NULL, $3); }
        | ProcID '=' '[' ProcList ']' ComSeq 
					{ $$ = newProcedure(yylloc, $1, $4, $6); }

ProcList: /* empty */			{ $$ = NULL; }
        | ProcList RuleDecl             { $$ = addLocalDecl(yylloc, $2, $1); }
	| ProcList ProcDecl 		{ $$ = addLocalDecl(yylloc, $2, $1); }

ComSeq: Command 			{ $$ = addCommand(yylloc, $1, NULL; }
      | ComSeq ';' Command  		{ $$ = addCommand(yylloc, $3, $1); }

Command: Block 				/* default $$ = $1 */
       | IF Block THEN Block      	{ $$ = newCondBranch(IF, yylloc, $2, $4, newSkip(@6)); }
       | IF Block THEN Block ELSE Block { $$ = newCondBranch(IF, yylloc, $2, $4, $6); }
       | TRY Block 			{ $$ = newCondBranch(TRY, yylloc, $2, newSkip(@4), newSkip(@6)); }
       | TRY Block THEN Block		{ $$ = newCondBranch(TRY, yylloc, $2, $4, newSkip(@6)); }
       | TRY Block THEN Block ELSE Block 
					{ $$ = newCondBranch(TRY, yylloc, $2, $4, $6); }

Block: '(' ComSeq ')' 	                { $$ = $2; }
     | '(' ComSeq ')' '!' 		{ $$ = newAlap(yylloc, $2); } 
     | SimpleCommand 			/* default $$ = $1 */
     | Block OR Block 			{ $$ = newOrStmt(yylloc, $1, $3); }

SimpleCommand: RuleSetCall 	        { $$ = newRuleSetCall(yylloc, $1); }
	     | RuleSetCall '!' 		{ $$ = newAlap(yylloc, $1); }
	     | ProcCall 		{ $$ = newProcCall(yylloc, $1); }
             | ProcCall '!' 		{ $$ = newAlap(yylloc, $1); }
             | SKIP			{ $$ = newSkip(yylloc); }
             | FAIL			{ $$ = newFail(yylloc); }

RuleSetCall: RuleID                     /* default $$ = $1 */
	   | '{' IDList '}'		{ $$ = $2; } 

IDList: RuleID 				{ $$ = addRule(yylloc, $1, NULL); }
      | IDList ',' RuleID 		{ $$ = addRule(yylloc, $3, $1); } 

ProcCall: ProcID

 /* Grammar for GP2 conditional rule schemata */

RuleDecl: RuleID '(' VarDecls ')' '[' Graph ']' ARROW '[' Graph ']' Inter CondDecl INJECTIVE '=' Bool
					{ $$ = newRule(yylloc, injective_flag /*to be defined */, $1, $3, $6, $10, $12, $13); }

VarDecls: 				{ $$ = NULL; }
	 | VarList ':' Type		{ $$ = newVariableDecl($3, yylloc, $1, NULL); }  
	 | VarDecls ';' VarList ':' Type 
					{ $$ = newVariableDecl($5, yylloc, $3, $1); }

 /* some post-processing could be done to sort out the parameter list AST here as the AST generated
    in this manner is a bit strange. Need to remove the intermediate ASTs created from the reduction
    of the second VarList production so that their parents point directly to the variables. This may be achieved during AST construction with some pointer manipulation. 8/8/13: I suspect the above is nonsense. */

VarList: Variable 			{ $$ = addVariable(yylloc, $1, NULL); }
       | VarList ',' Variable          	{ $$ = addVariable(yylloc, $3, $1); }

Inter: INTERFACE '{' NodePairList '}'   { $$ = $3; }

NodePairList: /* empty */		{ $$ = NULL; }
	    | NodePair			{ $$ = addNodePair(yylloc, $1, NULL); }
            | NodePairList ',' NodePair { $$ = addNodePair(yylloc, $3, $1);   }

NodePair: '(' NodeID ',' NodeID ')'   	{ $$ = newNodePair(yylloc, $2, $4); }

Bool: TRUE | FALSE /* set injective flag */

Type: INT				{ $$ = INT_DECL; } 
    | STRING                            { $$ = STRING_DECL; }
    | ATOM 	                        { $$ = ATOM_DECL; }
    | LIST				{ $$ = LIST_DECL; }

 /* Grammar for GP2 graphs */

Graph: Position '|' NodeList '|' EdgeList 
     					{ $$ = newGraph($1, $3, $5); }

NodeList: /* empty */			{ $$ = NULL; }
        | Node				{ $$ = addNode(yylloc, $1, NULL); }
        | NodeList ',' Node		{ $$ = addNode(yylloc, $3, $1); }

Node: '(' NodeID RootNode ',' Label ',' Position ')'
    					{ $$ = newNode(yylloc, rootflag /*to be defined */, $2, $5, $7); }

EdgeList: /* empty */			{ $$ = NULL; }
	| Edge				{ $$ = addEdge(yylloc, $1, NULL); }
        | EdgeList ',' Edge		{ $$ = addEdge(yylloc, $3, $1); }

Edge: '(' EdgeID ',' NodeID ',' NodeID ',' Label ')'
					{ $$ = newEdge(yylloc, $2, $4, $6, $8); }
RootNode: /* empty */
	| ROOT /* switch root flag on */

Position: '(' NUM ',' NUM ')' 		{ $$ = newPosition(yylloc, ($2)->value.num, ($4)->value.num); }

 /* Grammar for GP2 conditions */

CondDecl: /* empty */                   { $$ = NULL; }
        | WHERE Condition		{ $$ = $2; }

Condition: Subtype '(' Variable ')' 	{ $$ = newSubtypePred($1, yylloc, $3); }
         | EDGE '(' NodeID ',' NodeID LabelArg ')' 
					{ $$ = newEdgePred(yylloc, $3, $5, $6); }	
         | RelExp 			/* default $$ = $1 */
         | NOT Condition	        { $$ = newNotExp(yylloc, $2); }
         | Condition OR Condition  	{ $$ = newBinaryExp(OR, yylloc, $1, $3); }
         | Condition AND Condition      { $$ = newBinaryExp(AND, yylloc, $1, $3); }
	 | '(' Condition ')' 		{ $$ = $2; }

Subtype: INT				{ $$ = INT_CHECK; } 
       | STRING                         { $$ = STRING_CHECK; }
       | ATOM 	                        { $$ = ATOM_CHECK; }

LabelArg: /* empty */			{ $$ = NULL; }
 	| ',' Label			{ $$ = $2; }

RelExp: List 				/* default $$ = $1 */
      | RelExp '=' RelExp		{ $$ = newRelationalExp(EQUAL, yylloc, $1,$3); }
      | RelExp NEQ RelExp		{ $$ = newRelationalExp(NOT_EQUAL, yylloc, $1,$3); }
      | RelExp '>' RelExp		{ $$ = newRelationalExp(GREATER, yylloc, $1,$3); }
      | RelExp GEQ RelExp		{ $$ = newRelationalExp(GREATER_EQUAL, yylloc, $1,$3); }
      | RelExp '<' RelExp		{ $$ = newRelationalExp(LESS, yylloc, $1,$3); }
      | RelExp LEQ RelExp		{ $$ = newRelationalExp(LESS_EQUAL, yylloc, $1,$3); }

  /* Grammar for GP2 Labels */

Label: List 				{ $$ = newLabel(yylloc, NONE , $1); }
     | List '#' MARK			{ $$ = newLabel(yylloc, $3, $1); } 

List: EMPTY                             { $$ = NULL; }
    | AtomExp				{ $$ = addAtom(yylloc, $1, NULL); } 
    | List ':' AtomExp			{ $$ = addAtom(yylloc, $3, $1); }

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
ProcID: PROCID /* pointers to symtable for these rules */
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



