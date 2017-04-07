/* ////////////////////////////////////////////////////////////////////////////

  ==========
  GP2 Parser				
  ==========

  The Bison grammar for GP2 programs and rules. The generated parser builds an
  abstract syntax tree representation of a GP 2 program. There is also a host
  graph parser for syntactic analysis of host graphs.
  
  =============
  Update Policy
  =============
  Changes to the GP 2 grammar as defined in this file must be mirrored in the
  file GP2-editor/src/developer/translate/gpparser.y in order to maintain
  consistency with the graphical editor.

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

//////////////////////////////////////////////////////////////////////////// */

/* The names of the generated C files. */
%defines "parser.h"
%output "parser.c"

/* Code placed at the top of parser.h. ast.h is included here so that the 
 * types of gp_program and ast_host_graph are known when these variables
 * are declared in parser.h. */
%code requires {
#include "ast.h"
}

/* Declarations of global variables placed at the bottom of parser.h. */ 
 %code provides {
extern List *gp_program; 
extern int host_nodes, host_edges;
extern int yylineno;
extern string yytext;
extern FILE *yyin;
extern bool syntax_error;
}

/* Code placed in parser.c. */
%{
#include "common.h"
#include "gp2enums.h"

int yylex(void);

void yyerror(const char *error_message);
void report_warning(const char *error_message);

/* Flags used in the AST construction. */
bool is_root = false;
bool is_bidir = false;

/* Pointers to data structures constructed by the parser. */
struct List *gp_program = NULL; 
int host_nodes = 0, host_edges = 0;

bool syntax_error = false;
%}

%locations /* Generates code to process locations of symbols in the source file. */

%union {  
  int num;   /* value of NUM token. */
  double dnum; /* value of DNUM token. */
  char *str; /* value of STRING and CHAR tokens. */
  char *id;  /* value of PROCID and ID tokens. */
  int mark;  /* enum MarkTypes, value of MARK token. */
}

/* Single character tokens do not need to be explicitly declared. */
%token MAIN IF TRY THEN ELSE SKIP FAIL BREAK
%token WHERE EDGETEST   
%token INDEG OUTDEG _LENGTH					
%token INT CHARACTER STRING ATOM LIST 	                               
%token INTERFACE _EMPTY INJECTIVE 	
%token <mark> MARK ANY_MARK			                        
%token ARROW					                
%token NEQ GTEQ LTEQ			                       
%token <num> NUM 
%token <dnum> DNUM
%token <str> STR 
%token <id> PROCID ID           				
%token ROOT BIDIRECTIONAL	
%token GP_PROGRAM GP_GRAPH GP_RULE					

%left OR 		/* Lowest precedence level */
%left AND
%left NOT
%left '+' '-' 
%left '*' '/'
%left UMINUS		/* UMINUS represents unary '-' */
%left '.'		
%left ':'	        /* Highest precedence level */

%union {  
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
} 

%type <list> Program LocalDecls ComSeq RuleSetCall IDList VarDecls
             VarList Inter NodeIDList NodeList EdgeList List
%type <decl> Declaration
%type <command> MainDecl Command Block SimpleCommand 
%type <proc> ProcDecl
%type <rule> RuleDecl
%type <graph> Graph 
%type <node> Node
%type <edge> Edge
%type <cond_exp> CondDecl Condition 
%type <label> LabelArg Label
%type <atom_exp> AtomExp
%type <list_type> Type  
%type <check_type> Subtype
%type <id> NodeID EdgeID ProcID RuleID Variable
%type <num> HostID

/* This code is called whenever Bison discards a symbol during error recovery.
 * In the case of strings and identifiers, the dynamically allocated semantic
 * value needs to be freed.
 */
%destructor { free($$); } <str> <id>
%destructor { freeAST($$); } <list>
%destructor { freeASTDeclaration($$); } <decl>
%destructor { freeASTCommand($$); } <command>
%destructor { freeASTRule($$); } <rule>
%destructor { freeASTGraph($$); } <graph>
%destructor { freeASTNode($$); } <node>
%destructor { freeASTEdge($$); } <edge>
%destructor { freeASTCondition($$); } <cond_exp>
%destructor { freeASTLabel($$); } <label>
%destructor { freeASTAtom($$); } <atom_exp>

%error-verbose

%start Initialise

%%

 /* The Bison Grammar creates abstract syntax tree (AST) nodes using its 
  * action code after reducing a rule. The action code contains calls to
  * functions defined in ast.c. Each AST node has a node type, for example
  * IF_STATEMENT.
  *
  * Lists in GP2 programs are represented by a linked list of structures in 
  * the AST. AST nodes of type struct List are the spine of these linked lists.
  * They point to both a 'value' (e.g. a global declaration) and to the next
  * struct List node in the list (maybe NULL). These lists are generated in 
  * reverse order as left recursive rules are used to minimise the size of
  * Bison's parser stack. The lists are reversed at a later point in the
  * compilation in order to restore the correct order of items according
  * to the input GP2 program file.
  *
  * The 'add' functions create a new AST node of type struct List. The node 
  * type is provided as the first argument in some cases. The AST node is
  * pointed to the value node and next node provided as the last two arguments
  * to the function. The 'new' functions create all the other AST node structs.
  * They are pointed to their subtrees, if necessary, supplied by the caller. 
  * These functions are defined in the file ast.c.
  *
  * All AST constructor functions have a location argument. This is usually
  * the second argument, but it is the first argument if the AST node type
  * is explicitly passed. The lexer generates the location - a structure of 
  * type YYLTYPE containing line and column numbers - of each token it reads.
  * This information is stored by Bison and used to assign such locations to
  * each AST node. 
  *
  * The rule of thumb used is that AST nodes are assigned the location of the
  * entire syntactic string they represent. 
  * For example, an AST node  representing a variable name will contain the 
  * location of that name in the text file (first character to last character),
  * while an AST node representing a graph will contain the location from the 
  * opening '[' to the closing ']' of the graph in the text file.
  * The main exceptions to this rule are list nodes: they contain the complete
  * location of the value node to which they point. The other exceptions are
  * nodes that act as the root of a named structure (procedure, rule, node, edge).
  * Their locations are the location of the structure's name in the text file.
  *
  * A few locations are assigned explicitly, but most are specified with 
  * Bison's location tokens (@N), which point to the location structure of the
  * Nth RHS symbol of rule. The special token @$ is the location whose 
  * first (last) character is the first (last) character of the first (last)
  * RHS symbol.
  *
  * Identifiers (symbols ProcID, RuleID, NodeID, EdgeID and Variable) and
  * string constants (token STR) are assigned to yylval with strdup. Hence
  * the action code of any rules with these symbols on the RHS must free
  * the appropriate semantic value after the call to the constructor, otherwise
  * the pointer will be lost when yylval is updated.
  */


Initialise: GP_PROGRAM Program		{ gp_program = $2; }
          | GP_GRAPH HostGraph          { }
          | GP_RULE RuleDecl          	  { if ($2) freeASTRule($2); }

 /* Grammar for GP 2 program text. */
Program: Declaration	      		{ $$ = addASTDecl(GLOBAL_DECLARATIONS, 
                                               @1, $1, NULL); }  
       | Program Declaration            { $$ = addASTDecl(GLOBAL_DECLARATIONS, 
                                               @2, $2, $1); }  

Declaration: MainDecl 			{ $$ = newASTMainDecl(@$, $1); }
     	   | ProcDecl			{ $$ = newASTProcedureDecl(@$, $1); }
           | RuleDecl			{ $$ = newASTRuleDecl(@$, $1); }

MainDecl: MAIN '=' ComSeq 		{ $$ = newASTCommandSequence(@1, $3); }

ProcDecl: ProcID '=' ComSeq 		{ $$ = newASTProcedure(@1, $1, NULL, 
                                               newASTCommandSequence(@3 ,$3));
					  if($1) free($1); }

        | ProcID '=' '[' LocalDecls ']' ComSeq 
					{ $$ = newASTProcedure(@1, $1, $4, 
                                               newASTCommandSequence(@6, $6));
				          if($1) free($1); }
        /* Error-catching production */
	| RuleID '=' '[' LocalDecls ']' ComSeq
				        { $$ = newASTProcedure(@1, $1, $4, 
                                               newASTCommandSequence(@6, $6));
                                          report_warning("Procedure names must "
 					   "start with an upper-case letter."); 
					  if($1) free($1); }

LocalDecls: /* empty */			{ $$ = NULL; }
        | LocalDecls RuleDecl           { $$ = addASTDecl(LOCAL_DECLARATIONS, @2, 
                                               newASTRuleDecl(@2, $2), $1); }
	| LocalDecls ProcDecl 		{ $$ = addASTDecl(LOCAL_DECLARATIONS, @2,
                                               newASTProcedureDecl(@2, $2), $1); }

ComSeq: Command 			{ $$ = addASTCommand(@1, $1, NULL); }
      | ComSeq ';' Command  		{ $$ = addASTCommand(@3, $3, $1); }
      /* Error-catching production */
      | ComSeq ',' Command		{ $$ = addASTCommand(@3, $3, $1);
                                          report_warning("Incorrect use of comma "
					    "to separate commands. Perhaps you "
					    "meant to use a semicolon?"); }

Command: Block 				/* default $$ = $1 */ 
       | Block OR Block 	        { $$ = newASTOrStmt(@$, $1, $3); }
       | IF Block THEN Block      	{ $$ = newASTCondBranch(IF_STATEMENT, @$,
                                               $2, $4, newASTSkip(@$)); }
       | IF Block THEN Block ELSE Block { $$ = newASTCondBranch(IF_STATEMENT, @$,
                                               $2, $4, $6); }
       /* Error-catching production */
       | IF Block ELSE Block	   	{ $$ = newASTCondBranch(IF_STATEMENT, @$,
                                               $2, newASTSkip(@$), $4);
                                          report_warning("No 'then' clause in if "
						         "statement."); }
       | TRY Block 			{ $$ = newASTCondBranch(TRY_STATEMENT, @$,
                                               $2, newASTSkip(@$), newASTSkip(@$)); }
       | TRY Block THEN Block		{ $$ = newASTCondBranch(TRY_STATEMENT, @$,
                                               $2, $4, newASTSkip(@$)); }
       | TRY Block ELSE	Block	   	{ $$ = newASTCondBranch(TRY_STATEMENT, @$,
                                               $2, newASTSkip(@$), $4); }
       | TRY Block THEN Block ELSE Block { $$ = newASTCondBranch(TRY_STATEMENT, @$,
                                                $2, $4, $6); }


Block: '(' ComSeq ')' 	                { $$ = newASTCommandSequence(@$, $2); }
     | '(' ComSeq ')' '!' 		{ $$ = newASTAlap(@$, 
                                               newASTCommandSequence(@2, $2)); } 
     /* If an error is found in a code block, continue parsing after the right
      * parenthesis. */
     | error ')'  			{ $$ = NULL; }
     | SimpleCommand 			/* default $$ = $1 */ 
     | SimpleCommand '!'		{ $$ = newASTAlap(@$, $1); }
     | SKIP				{ $$ = newASTSkip(@$); }
     | FAIL				{ $$ = newASTFail(@$); }
     | BREAK				{ $$ = newASTBreak(@$); }

SimpleCommand: RuleSetCall 	        { $$ = newASTRuleSetCall(@$, $1); }
             | RuleID                   { $$ = newASTRuleCall(@$, $1); if($1) free($1); }
	     | ProcID	 		{ $$ = newASTProcCall(@$, $1); if($1) free($1); }

RuleSetCall: '{' IDList '}'		{ $$ = $2; }
           /* If an error is found in an rule set call, continue parsing after
            * the rule set. */
           | error '}' 			{ $$ = NULL; }

IDList: RuleID				{ $$ = addASTRule(@1, $1, NULL);
					  if($1) free($1); }
      | IDList ',' RuleID 		{ $$ = addASTRule(@3, $3, $1); 
					  if($3) free($3);} 
      /* Error-catching productions */
      | ProcID	 			{ $$ = addASTRule(@1, $1, NULL);
                                          report_warning("Procedure name used in "
					   "a rule set. Rule names must start "
					   "with a lower-case letter.");
				          if($1) free($1); }
      | IDList ';' RuleID		{ $$ = addASTRule(@3, $3, $1);
                                          report_warning("Incorrect use of semicolon "
					   "in a rule set. Perhaps you meant to "
					   "use a comma?"); 
					  if($3) free($3); }


 /* Grammar for rules. */
RuleDecl: RuleID '(' VarDecls ')' Graph ARROW Graph Inter CondDecl  
					{ $$ = newASTRule(@1, $1, $3, $5, $7, $8, $9); 
					  if($1) free($1); }
        | RuleID '(' ')' Graph ARROW Graph Inter CondDecl 
      					{ $$ = newASTRule(@1, $1, NULL, $4, $6, $7, $8);
					  if($1) free($1); }
        /* Error-catching productions */
	| ProcID '(' VarDecls ')' Graph ARROW Graph Inter CondDecl
				        { $$ = newASTRule(@1, $1, $3, $5, $7, $8, $9); 
                                          report_warning("Rule names must "
 					   "start with a lower-case letter."
				 	   "letter.");
					  if($1) free($1); }
	/* This production catches a potentially likely syntax error in which
         * the user terminates the variable declaration list with a semicolon.
         */
        | RuleID '(' VarDecls ';' ')' Graph ARROW Graph Inter CondDecl
					{ $$ = newASTRule(@1, $1, $3, $6, $8, $9, $10);  
                                          report_warning("Semicolon at the end "
					    "of a rule's variable list");
					  if($1) free($1); }	


VarDecls: VarList ':' Type		{ $$ = addASTVariableDecl($3, @$, $1, NULL); }  
	/* The location of VarDecls on the LHS is manually set to the location
         * of 'VarList ':' Type' as each Variable Declaration AST node should
         * only represent one list of variables.
 	 */
	| VarDecls ';' VarList ':' Type { @$.first_column = @3.first_column;
				          @$.first_line = @3.first_line;
					  @$.last_column = @5.last_column;
				          @$.last_column = @5.last_column;
					  $$ = addASTVariableDecl($5, @$, $3, $1); }

VarList: Variable 			{ $$ = addASTVariable(@1, $1, NULL); 
					  if($1) free($1); }
       | VarList ',' Variable          	{ $$ = addASTVariable(@3, $3, $1); 
		 	                  if($3) free($3); }

Inter: INTERFACE '=' '{' '}'   		{ $$ = NULL; }
     | INTERFACE '=' '{' NodeIDList '}' { $$ = $4; }
     /* If an error is found in an interface list, continue parsing after the 
      * interface list. */
     | error '}'			{ report_warning("Error in an interface list.");  
                                          $$ = NULL; }

NodeIDList: NodeID			{ $$ = addASTNodeID(@1, $1, NULL); 
					  if($1) free($1); }
          | NodeIDList ',' NodeID 	{ $$ = addASTNodeID(@3, $3, $1);
					  if($3) free($3); }

Type: INT				{ $$ = INT_DECLARATIONS; } 
    | CHARACTER				{ $$ = CHAR_DECLARATIONS; }
    | STRING                            { $$ = STRING_DECLARATIONS; }
    | ATOM 	                        { $$ = ATOM_DECLARATIONS; }
    | LIST				{ $$ = LIST_DECLARATIONS; }


 /* Grammar for rule graphs. */
Graph: '[' '|' ']'			 { $$ = newASTGraph(@$, NULL, NULL); }
     | '[' Position '|' '|' ']'	         { $$ = newASTGraph(@$, NULL, NULL); }
     | '[' NodeList '|' ']'		 { $$ = newASTGraph(@$, $2, NULL); }
     | '[' Position '|' NodeList '|' ']' { $$ = newASTGraph(@$, $4, NULL); }
     | '[' NodeList '|' EdgeList ']' 	 { $$ = newASTGraph(@$, $2, $4); }
     | '[' Position '|' NodeList '|' EdgeList ']'  
     					 { $$ = newASTGraph(@$, $4, $6); }
	
NodeList: Node				{ $$ = addASTNode(@1, $1, NULL); }
        | NodeList Node			{ $$ = addASTNode(@2, $2, $1); }

Node: '(' NodeID RootNode ',' Label ')' { $$ = newASTNode(@2, is_root, $2, $5); 
 					  is_root = false; 	
					  if($2) free($2); } 
    | '(' NodeID RootNode ',' Label Position ')'
    				        { $$ = newASTNode(@2, is_root, $2, $5); 
 					  is_root = false; 	
					  if($2) free($2); } 
EdgeList: Edge				{ $$ = addASTEdge(@1, $1, NULL); }
        | EdgeList Edge			{ $$ = addASTEdge(@2, $2, $1); }

Edge: '(' EdgeID Bidirection ',' NodeID ',' NodeID ',' Label ')'
					{ $$ = newASTEdge(@2, is_bidir, $2, $5, $7, $9);
                                          is_bidir = false; if($2) free($2); 
					  if($5) free($5); if($7) free($7); }

 /* Layout information for the editor. This is ignored by the parser. */
Position: '<' DNUM ',' DNUM '>'         { } 
        | '<' NUM ',' NUM '>'           { } 
        | '<' NUM ',' '-' NUM '>'       { } 
        | '<' '-' NUM ','  NUM '>'      { } 
        | '<' '-' NUM ',' '-' NUM '>'   { } 
        | '<' DNUM ',' NUM '>'          { } 
        | '<' DNUM ',' '-' NUM '>'      { } 
        | '<' NUM ',' DNUM '>'          { }
        | '<' '-' NUM ',' DNUM '>'      { }

RootNode: /* empty */ 
	| ROOT 				{ is_root = true; }

Bidirection: /* empty */ 
	   | BIDIRECTIONAL		{ is_bidir = true; }


 /* Grammar for conditions. */
CondDecl: /* empty */                   { $$ = NULL; }
        | WHERE Condition		{ $$ = $2; }

Condition: Subtype '(' Variable ')' 	{ $$ = newASTSubtypePred($1, @$, $3); 
					  if($3) free($3); }
         | EDGETEST '(' NodeID ',' NodeID  LabelArg ')' 
					{ $$ = newASTEdgePred(@$, $3, $5, $6); 
					  if($3) free($3); if($5) free($5); }
	 | List '=' List 		{ $$ = newASTListComparison(EQUAL, @$, $1, $3); }
	 | List NEQ List 		{ $$ = newASTListComparison(NOT_EQUAL, @$, $1, $3); }
	 | AtomExp '>' AtomExp          { $$ = newASTAtomComparison(GREATER, @$, $1, $3); }    
	 | AtomExp GTEQ AtomExp         { $$ = newASTAtomComparison(GREATER_EQUAL, @$, $1, $3); }    
	 | AtomExp '<' AtomExp          { $$ = newASTAtomComparison(LESS, @$, $1, $3); }    
	 | AtomExp LTEQ AtomExp         { $$ = newASTAtomComparison(LESS_EQUAL, @$, $1, $3); }    
         | NOT Condition	        { $$ = newASTNotExp(@$, $2); }
         | Condition OR Condition  	{ $$ = newASTBinaryExp(BOOL_OR, @$, $1, $3); }
         | Condition AND Condition      { $$ = newASTBinaryExp(BOOL_AND, @$, $1, $3); }
	 | '(' Condition ')' 		{ $$ = $2; }

Subtype: INT				{ $$ = INT_CHECK; } 
       | CHARACTER			{ $$ = CHAR_CHECK; }
       | STRING                         { $$ = STRING_CHECK; }
       | ATOM 	                        { $$ = ATOM_CHECK; }

LabelArg: /* empty */ 			{ $$ = NULL; }
 	| ',' Label 			{ $$ = $2; }

 /* Grammar for labels */
Label: List				{ $$ = newASTLabel(@$, NONE, $1); }
     | List '#' MARK	  		{ $$ = newASTLabel(@$, $3, $1); }
     /* Any has a distinct token since it cannot occur in the host graph. */
     | List '#' ANY_MARK		{ $$ = newASTLabel(@$, $3, $1); }


List: AtomExp				{ $$ = addASTAtom(@1, $1, NULL); } 
    | List ':' AtomExp 			{ $$ = addASTAtom(@3, $3, $1); }
    | _EMPTY				{ $$ = NULL; }
    | List ':' _EMPTY			/* default $$ = $1 */

AtomExp: Variable			{ $$ = newASTVariable(@$, $1); if($1) free($1); }
       | NUM 				{ $$ = newASTNumber(@$, $1); }
       | STR 				{ $$ = newASTString(@$, $1); if($1) free($1); }
       | INDEG '(' NodeID ')' 		{ $$ = newASTDegreeOp(INDEGREE, @$, $3); 
					  if($3) free($3); }
       | OUTDEG '(' NodeID ')' 		{ $$ = newASTDegreeOp(OUTDEGREE, @$, $3); 
				 	  if($3) free($3); }
       | _LENGTH '(' Variable ')' 	{ $$ = newASTLength(@$, $3); if($3) free($3); }
       | '-' AtomExp %prec UMINUS 	{ $$ = newASTNegExp(@$, $2); } 
       | '(' AtomExp ')' 		{ $$ = $2; }
       | AtomExp '+' AtomExp 		{ $$ = newASTBinaryOp(ADD, @$, $1, $3);  }
       | AtomExp '-' AtomExp 		{ $$ = newASTBinaryOp(SUBTRACT, @$, $1, $3); }
       | AtomExp '*' AtomExp 		{ $$ = newASTBinaryOp(MULTIPLY, @$, $1, $3); }
       | AtomExp '/' AtomExp 		{ $$ = newASTBinaryOp(DIVIDE, @$, $1, $3); }
       | AtomExp '.' AtomExp 		{ $$ = newASTConcat(@$, $1, $3); }

 /* GP 2 identifiers */
ProcID: PROCID 				/* default $$ = $1 */ 
RuleID: ID		         	/* default $$ = $1 */ 
NodeID: ID				/* default $$ = $1 */
      | NUM				{ char id[64]; int write;
					  write = snprintf(id, 64, "%d", $1);
				          if(write < 0) {
					    yyerror("Node ID conversion failed.");
					    exit(1);
					  }
					  else $$ = strdup(id);
					}
EdgeID: ID				/* default $$ = $1 */ 
      | NUM				{ char id[64]; int write;
					  write = snprintf(id, 64, "%d", $1);
				          if(write < 0) {
					    yyerror("Edge ID conversion failed.");
					    exit(1);
					  }
					  else $$ = strdup(id);
					}
Variable: ID		  		/* default $$ = $1 */ 


/* Grammar for host graphs. It is used to syntax check the host graph file 
 * and to count the number of nodes and edges in the host graph. */
HostGraph: '[' '|' ']'  		{ }
         | '[' Position '|' '|' ']'  	{ }
         | '[' HostNodeList '|' ']'  	{ }
         | '[' Position '|' HostNodeList '|' ']' 
	 				{ }
         | '[' HostNodeList '|' HostEdgeList ']' 
	 				{ }
         | '[' Position '|' HostNodeList '|' HostEdgeList ']' 
     					{ }

HostNodeList: HostNode			{ host_nodes++; }
            | HostNodeList HostNode	{ host_nodes++; }

HostNode: '(' HostID RootNode ',' HostLabel ')'
    					/* default $$ = $1; */ 
HostNode: '(' HostID RootNode ',' HostLabel Position ')'
    					/* default $$ = $1; */

HostEdgeList: HostEdge			{ host_edges++; }
            | HostEdgeList HostEdge	{ host_edges++; } 

HostEdge: '(' HostID ',' HostID ',' HostID ',' HostLabel ')'
    					/* default $$ = $1; */

HostID:	NUM				/* default $$ = $1 */

HostLabel: HostList			{ }
         | HostList '#' MARK	  	{ }

HostList: HostExp 			{ } 
        | HostList ':' HostExp 		{ }
	| _EMPTY			{ }
        | HostList ':' _EMPTY	        { }

HostExp: NUM 				{ }
       | '-' NUM %prec UMINUS 	        { } 
       | STR 				{ if($1) free($1); }

%%

/* Bison calls yyerror whenever it encounters an error. It prints error
 * messages to stderr and log_file. */
void yyerror(const char *error_message)
{
   fprintf(stderr, "Error at '%c': %s\n\n", yychar, error_message);
   fprintf(log_file, "%d.%d-%d.%d: Error at '%s': %s\n\n", 
           yylloc.first_line, yylloc.first_column, yylloc.last_line, 
           yylloc.last_column, yytext, error_message);
}

/* report_warning is identical to yyerror except that it doesn't refer to yytext.
 * This is called in the action code of error-catching Bison rules in which
 * the value of yytext may be misleading. */
void report_warning(const char *error_message)
{
   fprintf(stderr, "Error: %s\n\n", error_message);
   fprintf(log_file, "%d.%d-%d.%d: Error: %s\n\n", 
           yylloc.first_line, yylloc.first_column, yylloc.last_line, 
           yylloc.last_column, error_message);
   syntax_error = true;
}
        
