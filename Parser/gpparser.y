/*//////////////////////////////////////////////////////////////////////////////////////////// 
//
//				      gpparser.y 					
//
//  This is the Bison parser for GP2. In combination with Flex, it performs syntax 
//  checking and generates an Abstract Syntax Tree for its input GP2 program.
//
//                           Created on 28/5/13 by Chris Bak 
//
/////////////////////////////////////////////////////////////////////////////////////////// */ 


%{
#include <stdio.h>  /* printf, fprintf, fopen */
#include <stdlib.h> /* malloc, free */
#include <stdarg.h> /* va_start, va_list, va_end */
#include "ast.h" /* mark_t, list_t, cond_exp_t, AST constructors */

List *gp_program = NULL; /* This will point to the root of the AST. */

char *file_name = NULL; 

/* Flags used in the AST construction. */
  bool is_root = false;
  bool is_injective = false;

%}

/* declare tokens */

%locations /* Generates code to process locations of symbols in the source file. */

%union {  
  int num;   /* value of NUM token. */
  char *str; /* value of STRING tokens. */
  char *id;  /* value of MACID and ID tokens. */
  int mark;  /* enum mark_t, value of MARK token. */
}

/* Single-character tokens do not need to be explicitly declared. */

%token MAIN IF TRY THEN ELSE SKIP FAIL                          
%token WHERE EDGETEST TRUE FALSE 		               
%token INDEG OUTDEG LLEN SLEN					
%token INT STRING ATOM LIST 	                               
%token INTERFACE EMPTY INJECTIVE 	
%token <mark> MARK			                        
%token ARROW					                
%token NEQ GTEQ LTEQ			                       
%token <num> NUM 
%token <str> STR 
%token <id> PROCID ID           				
%token ROOT							


%left '+' '-' AND	/* lowest precedence level, left associative */
%left '*' '/' OR
%left UMINUS NOT	/* UMINUS represents unary '-' */
%left '.'		/* highest precedence level */

%union {  
  struct List *list; 
  struct GPDeclaration *decl;
  struct GPStatement *stmt;
  struct GPProcedure *proc;
  struct GPRule *rule;
  struct GPNodePair *node_pair;
  struct GPGraph *graph;
  struct GPNode *node;
  struct GPEdge *edge;
  struct GPPos *pos;
  struct GPCondExp *cond_exp;
  struct GPLabel *label;  
  struct GPAtomicExp *atom_exp;

  int list_type; /* enum list_t */
  int check_type; /* enum cond_exp_t */
} 

%type <list> GPProgram Program ProcList ComSeq RuleSetCall IDList VarDecls
             VarList Inter NodePairList NodeList EdgeList List
%type <decl> Declaration
%type <stmt> MainDecl Command Block SimpleCommand 
%type <proc> ProcDecl
%type <rule> RuleDecl
%type <node_pair> NodePair
%type <graph> Graph
%type <node> Node
%type <edge> Edge
%type <pos> Position
%type <cond_exp> CondDecl Condition 
%type <label> LabelArg Label
%type <atom_exp> AtomExp
%type <list_type> Type  
%type <check_type> Subtype
%type <id> NodeID EdgeID ProcID RuleID Variable

%start GPProgram 

%%

/* Grammar for GP2 Program Text. */

GPProgram: Program			{ gp_program = $1; }

Program: Declaration	      		{ $$ = addDecl(GLOBAL_DECLARATIONS, yylloc, $1, NULL); }  
       | Program Declaration            { $$ = addDecl(GLOBAL_DECLARATIONS, yylloc, $2, $1); }  

Declaration: MainDecl 			{ $$ = newMainDecl(yylloc, $1); }
     	   | ProcDecl			{ $$ = newProcedureDecl(yylloc, $1); }
           | RuleDecl			{ $$ = newRuleDecl(yylloc, $1); }

MainDecl: MAIN '=' ComSeq		{ $$ = newCommandSequence(yylloc,$3); }

ProcDecl: ProcID '=' ComSeq 		{ $$ = newProcedure(yylloc, $1, NULL, 
                                               newCommandSequence(yylloc,$3)); }
        | ProcID '=' '[' ProcList ']' ComSeq 
					{ $$ = newProcedure(yylloc, $1, $4, 
                                               newCommandSequence(yylloc,$6)); }

ProcList: /* empty */			{ $$ = NULL; }
        | ProcList RuleDecl             { $$ = addDecl(LOCAL_DECLARATIONS, yylloc, 
                                               newRuleDecl(yylloc, $2), $1); }
	| ProcList ProcDecl 		{ $$ = addDecl(LOCAL_DECLARATIONS, yylloc,
                                               newProcedureDecl(yylloc, $2), $1); }

ComSeq: Command 			{ $$ = addCommand(yylloc, $1, NULL); }
      | ComSeq ';' Command  		{ $$ = addCommand(yylloc, $3, $1); }

Command: Block 				/* default $$ = $1 */ 
       | IF Block THEN Block      	{ $$ = newCondBranch(IF_STATEMENT, yylloc,
                                               $2, $4, newSkip(yylloc)); }
       | IF Block THEN Block ELSE Block { $$ = newCondBranch(IF_STATEMENT, yylloc,
                                               $2, $4, $6); }
       | TRY Block 			{ $$ = newCondBranch(TRY_STATEMENT, yylloc,
                                               $2, newSkip(yylloc), newSkip(yylloc)); }
       | TRY Block THEN Block		{ $$ = newCondBranch(TRY_STATEMENT, yylloc,
                                               $2, $4, newSkip(yylloc)); }
       | TRY Block THEN Block ELSE Block { $$ = newCondBranch(TRY_STATEMENT, yylloc,
                                                $2, $4, $6); }

Block: '(' ComSeq ')' 	                { $$ = newCommandSequence(yylloc,$2); }
     | '(' ComSeq ')' '!' 		{ $$ = newAlap(yylloc, newCommandSequence(yylloc,$2)); } 
     | SimpleCommand 			/* default $$ = $1 */ 
     | SimpleCommand  '!'		{ $$ = newAlap(yylloc, $1); }
     | Block OR Block 			{ $$ = newOrStmt(yylloc, $1, $3); }
     | SKIP				{ $$ = newSkip(yylloc); }
     | FAIL				{ $$ = newFail(yylloc); }

SimpleCommand: RuleSetCall 	        { $$ = newRuleSetCall(yylloc, $1); }
             | RuleID                   { $$ = newRuleCall(yylloc, $1); }
	     | ProcID	 		{ $$ = newProcCall(yylloc, $1); }

RuleSetCall: '{' IDList '}'		{ $$ = $2; } 

IDList: RuleID				{ $$ = addRule(yylloc, $1, NULL); }
      | IDList ',' RuleID 		{ $$ = addRule(yylloc, $3, $1); } 


/* Grammar for GP2 Rule Definitions. */

RuleDecl: RuleID '(' VarDecls ')' '[' Graph ']' ARROW '[' Graph ']' Inter CondDecl INJECTIVE '=' Bool
					{ $$ = newRule(yylloc, is_injective,
 					        $1, $3, $6, $10, $12, $13); }
					  
VarDecls: /* empty */ 			{ $$ = NULL; }
	| VarList ':' Type		{ $$ = addVariableDecl($3, yylloc, $1, NULL); }  
	| VarDecls ';' VarList ':' Type { $$ = addVariableDecl($5, yylloc, $3, $1); }

VarList: Variable 			{ $$ = addVariable(yylloc, $1, NULL); }
       | VarList ',' Variable          	{ $$ = addVariable(yylloc, $3, $1); }

Inter: INTERFACE '{' NodePairList '}'   { $$ = $3; }

NodePairList: /* empty */ 		{ $$ = NULL; }
	    | NodePair			{ $$ = addNodePair(yylloc, $1, NULL); }
            | NodePairList ',' NodePair { $$ = addNodePair(yylloc, $3, $1);   }

NodePair: '(' NodeID ',' NodeID ')'   	{ $$ = newNodePair(yylloc, $2, $4); }

Bool: TRUE 				{ is_injective = true; }
    | FALSE				{ is_injective = false; }

Type: INT				{ $$ = INT_DECLARATIONS; } 
    | STRING                            { $$ = STRING_DECLARATIONS; }
    | ATOM 	                        { $$ = ATOM_DECLARATIONS; }
    | LIST				{ $$ = LIST_DECLARATIONS; }


/* Grammar for GP2 Graph Definitions. */

Graph: Position '|' NodeList '|' EdgeList 
     					{ $$ = newGraph(yylloc, $1, $3, $5); }

NodeList: /* empty */ 			{ $$ = NULL; }
        | Node				{ $$ = addNode(yylloc, $1, NULL); }
        | NodeList ',' Node		{ $$ = addNode(yylloc, $3, $1); }

Node: '(' NodeID RootNode ',' Label ',' Position ')'
    					{ $$ = newNode(yylloc, is_root, $2, $5, $7); 
 					  is_root = false; } /* Resets the root node flag */	    

EdgeList: /* empty */ 			{ $$ = NULL; }
	| Edge				{ $$ = addEdge(yylloc, $1, NULL); }
        | EdgeList ',' Edge		{ $$ = addEdge(yylloc, $3, $1); }

Edge: '(' EdgeID ',' NodeID ',' NodeID ',' Label ')'
					{ $$ = newEdge(yylloc, $2, $4, $6, $8); }
RootNode: /* empty */ 
	| ROOT 				{ is_root = true; }

Position: '(' NUM ',' NUM ')' 		{ $$ = newPosition(yylloc, $2, $4); }


/* Grammar for GP2 Conditions. */

CondDecl: /* empty */                   { $$ = NULL; }
        | WHERE Condition		{ $$ = $2; }

Condition: Subtype '(' Variable ')' 	{ $$ = newSubtypePred($1, yylloc, $3); }
         | EDGETEST '(' NodeID ',' NodeID LabelArg ')' 
					{ $$ = newEdgePred(yylloc, $3, $5, $6); }	
	 | List '=' List		{ $$ = newListComparison(EQUAL, yylloc, $1, $3); }
	 | List NEQ List		{ $$ = newListComparison(NOT_EQUAL, yylloc, $1, $3); }
	 | AtomExp '>' AtomExp          { $$ = newAtomComparison(GREATER, yylloc, $1, $3); }    
	 | AtomExp GTEQ AtomExp         { $$ = newAtomComparison(GREATER_EQUAL, yylloc, $1, $3); }    
	 | AtomExp '<' AtomExp          { $$ = newAtomComparison(LESS, yylloc, $1, $3); }    
	 | AtomExp LTEQ AtomExp         { $$ = newAtomComparison(LESS_EQUAL, yylloc, $1, $3); }    
         | NOT Condition	        { $$ = newNotExp(yylloc, $2); }
         | Condition OR Condition  	{ $$ = newBinaryExp(BOOL_OR, yylloc, $1, $3); }
         | Condition AND Condition      { $$ = newBinaryExp(BOOL_AND, yylloc, $1, $3); }
	 | '(' Condition ')' 		{ $$ = $2; }

Subtype: INT				{ $$ = INT_CHECK; } 
       | STRING                         { $$ = STRING_CHECK; }
       | ATOM 	                        { $$ = ATOM_CHECK; }

LabelArg: /* empty */ 			{ $$ = NULL; }
 	| ',' Label			{ $$ = $2; }

/* Grammar for GP2 Labels */

Label: List 				{ $$ = newLabel(yylloc, NONE, $1); }
     | List '#' MARK			{ $$ = newLabel(yylloc, $3, $1); } 

List: AtomExp				{ $$ = addAtom(yylloc, $1, NULL); } 
    | List ':' AtomExp			{ $$ = addAtom(yylloc, $3, $1); }

AtomExp: EMPTY				{ $$ = newEmpty(yylloc); }
       | Variable			{ $$ = newVariable(yylloc, $1); }
       | NUM 				{ $$ = newNumber(yylloc, $1); }
       | STR 				{ $$ = newString(yylloc, $1); }
       | INDEG '(' NodeID ')' 		{ $$ = newDegreeOp(INDEGREE, yylloc, $3); }
       | OUTDEG '(' NodeID ')' 		{ $$ = newDegreeOp(OUTDEGREE, yylloc, $3); }
       | LLEN '(' List ')' 		{ $$ = newListLength(yylloc, $3); }
       | SLEN '(' AtomExp ')' 		{ $$ = newStringLength(yylloc, $3); }
       | '-' AtomExp %prec UMINUS 	{ $$ = newNegExp(yylloc, $2); } 
       | '(' AtomExp ')' 		{ $$ = $2; }
       | AtomExp '+' AtomExp 		{ $$ = newBinaryOp(ADD, yylloc, $1, $3);  }
       | AtomExp '-' AtomExp 		{ $$ = newBinaryOp(SUBTRACT, yylloc, $1, $3); }
       | AtomExp '*' AtomExp 		{ $$ = newBinaryOp(MULTIPLY, yylloc, $1, $3); }
       | AtomExp '/' AtomExp 		{ $$ = newBinaryOp(DIVIDE, yylloc, $1, $3); }
	 /* Ambiguity resolved by explicit precedences */
       | AtomExp '.' AtomExp 		{ $$ = newBinaryOp(CONCAT, yylloc, $1, $3); }

/* GP2 Identifiers */

ProcID: PROCID 				/* default $$ = $1 */ 
RuleID: ID		         	/* default $$ = $1 */ 
NodeID: ID				/* default $$ = $1 */ 
EdgeID: ID				/* default $$ = $1 */ 
Variable: ID		  		/* default $$ = $1 */ 

%%

/* The default bison error function which implicitly uses the location stored in yylloc. */

int yyerror(char *error_message)
{
   if(yylloc.first_line)
     fprintf(stderr, "%s:%d.%d-%d.%d: error at '%s': %s\n", file_name, yylloc.first_line,
       yylloc.first_column, yylloc.last_line, yylloc.last_column, yytext, error_message);

   return 0;
}

/* Alternate error function with an explicit location as its argument. */

void print_error(YYLTYPE location, char *error_message, ...)
{
   va_list args;
   va_start(args, error_message);

   if(location.first_line)
     fprintf(stderr, "%s:%d.%d-%d.%d: error at '%s': ", file_name, location.first_line,
       location.first_column, location.last_line, location.last_column, yytext);
     vfprintf(stderr, error_message, args);
     fprintf(stderr, "\n");

   va_end(args);
}
        
