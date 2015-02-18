/* ////////////////////////////////////////////////////////////////////////////

  ==========
  GP2 Parser				
  ==========

  The Bison specification for GP2's parser. Defines GP2's abstract syntax
  and calls the appropriate AST constructor for each rule.
  
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
extern GPGraph *ast_host_graph; 
extern int yylineno;
extern string yytext;
extern FILE *yyin;
extern bool syntax_error;
}

/* Code placed in parser.c. */
%{
#include "globals.h"

void yyerror(const char *error_message);
void report_warning(const char *error_message);

/* Flags used in the AST construction. */
bool is_root = false;
bool is_bidir = false;

/* Pointers to data structures constructed by the parser. */
struct List *gp_program = NULL; 
struct GPGraph *ast_host_graph = NULL;

bool syntax_error = false;
%}

%locations /* Generates code to process locations of symbols in the source file. */

%union {  
  int num;   /* value of NUM token. */
  char *str; /* value of STRING and CHAR tokens. */
  char *id;  /* value of PROCID and ID tokens. */
  int mark;  /* enum MarkTypes, value of MARK token. */
}

/* Single character tokens do not need to be explicitly declared. */

%token MAIN IF TRY THEN ELSE SKIP FAIL                          
%token WHERE EDGETEST  		               
%token INDEG OUTDEG LLEN SLEN					
%token INT CHARACTER STRING ATOM LIST 	                               
%token INTERFACE _EMPTY INJECTIVE 	
%token <mark> MARK ANY_MARK			                        
%token ARROW					                
%token NEQ GTEQ LTEQ			                       
%token <num> NUM 
%token <str> STR CHAR
%token <id> PROCID ID           				
%token ROOT BIDIRECTIONAL	
%token GP_PROGRAM GP_GRAPH						

%left AND 		/* Lowest precedence level */
%left NOT
%left OR
%left '+' '-' 
%left '*' '/'
%left UMINUS		/* UMINUS represents unary '-' */
%left '.'		
%left ':'	        /* Highest precedence level */

%union {  
  struct List *list; 
  struct GPDeclaration *decl;
  struct GPStatement *stmt;
  struct GPProcedure *proc;
  struct GPRule *rule;
  struct GPGraph *graph;
  struct GPNode *node;
  struct GPEdge *edge;
  struct GPPos *pos;
  struct GPCondExp *cond_exp;
  struct GPLabel *label;  
  struct GPAtomicExp *atom_exp;

  int list_type; /* enum ListType */
  int check_type; /* enum CondExpType */
} 

%type <list> Program LocalDecls ComSeq RuleSetCall IDList VarDecls
             VarList Inter NodeIDList NodeList HostNodeList EdgeList 
             HostEdgeList List HostList
%type <decl> Declaration
%type <stmt> MainDecl Command Block SimpleCommand 
%type <proc> ProcDecl
%type <rule> RuleDecl
%type <graph> Graph HostGraph
%type <node> Node HostNode
%type <edge> Edge HostEdge
/* %type <pos> Position */
%type <cond_exp> CondDecl Condition 
%type <label> LabelArg Label HostLabel
%type <atom_exp> AtomExp HostExp
%type <list_type> Type  
%type <check_type> Subtype
%type <id> NodeID EdgeID ProcID RuleID Variable

/* This code is called whenever Bison discards a symbol during error recovery.
 * In the case of strings and identifiers, the dynamically allocated semantic
 * value needs to be freed.
 */
%destructor { free($$); } <str> <id>
%destructor { freeAST($$); } <list>
%destructor { freeASTDeclaration($$); } <decl>
%destructor { freeASTStatement($$); } <stmt>
%destructor { freeASTRule($$); } <rule>
%destructor { freeASTGraph($$); } <graph>
%destructor { freeASTNode($$); } <node>
%destructor { freeASTEdge($$); } <edge>
%destructor { freeASTCondition($$); } <cond_exp>
%destructor { freeASTLabel($$); } <label>
%destructor { freeASTAtomicExp($$); } <atom_exp>

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
          | GP_GRAPH HostGraph          { ast_host_graph = $2; }

 /* Grammar for GP2 Program Text. */

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
       | IF Block THEN Block      	{ $$ = newASTCondBranch(IF_STATEMENT, @$,
                                               $2, $4, newASTSkip(@$)); }
       | IF Block THEN Block ELSE Block { $$ = newASTCondBranch(IF_STATEMENT, @$,
                                               $2, $4, $6); }
       /* Error-catching production */
       | IF Block ELSE Block	   	{ $$ = newASTCondBranch(IF_STATEMENT, @$,
                                               $2, NULL, $4);
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


Block: '(' ComSeq ')' 	                { $$ = newASTCommandSequence(@$,$2); }
     | '(' ComSeq ')' '!' 		{ $$ = newASTAlap(@$, 
                                               newASTCommandSequence(@2, $2)); } 
     /* If an error is found in a code block, continue parsing after the right
      * parenthesis. */
     | error ')'  			{ $$ = NULL; }
     | SimpleCommand 			/* default $$ = $1 */ 
     | SimpleCommand '!'		{ $$ = newASTAlap(@$, $1); }
     | Block OR Block 			{ $$ = newASTOrStmt(@$, $1, $3); }
     | SKIP				{ $$ = newASTSkip(@$); }
     | FAIL				{ $$ = newASTFail(@$); }

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


 /* Grammar for GP2 Rule Definitions. */

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


 /* Grammar for GP2 Graph Definitions. */

Graph: '[' '|' ']'			 { $$ = newASTGraph(@$, NULL, NULL); }
     | '[' NodeList '|' ']'		 { $$ = newASTGraph(@$, $2, NULL); }
     | '[' NodeList '|' EdgeList ']' 	 { $$ = newASTGraph(@$, $2, $4); }
	
NodeList: Node				{ $$ = addASTNode(@1, $1, NULL); }
        | NodeList Node			{ $$ = addASTNode(@2, $2, $1); }

Node: '(' NodeID RootNode ',' Label ')' { $$ = newASTNode(@2, is_root, $2, $5); 
 					  is_root = false; 	
					  if($2) free($2); } 

EdgeList: Edge				{ $$ = addASTEdge(@1, $1, NULL); }
        | EdgeList Edge			{ $$ = addASTEdge(@2, $2, $1); }

Edge: '(' EdgeID Bidirection ',' NodeID ',' NodeID ',' Label ')'
					{ $$ = newASTEdge(@2, is_bidir, $2, $5, $7, $9);
                                          is_bidir = false;
					  if($2) free($2); 
					  if($5) free($5); 
					  if($7) free($7); }

RootNode: /* empty */ 
	| ROOT 				{ is_root = true; }

Bidirection: /* empty */ 
	   | BIDIRECTIONAL		{ is_bidir = true; }

/* Position: '(' NUM ',' NUM ')' 	{ $$ = newASTPosition(@$, $2, $4); } */


 /* Grammar for GP2 Conditions. */

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

 /* Grammar for GP2 Labels */

Label: List				{ $$ = newASTLabel(@$, NONE, $1); }
     | List '#' MARK	  		{ $$ = newASTLabel(@$, $3, $1); }
     /* Any has a distinct token since it cannot occur in the host graph. */
     | List '#' ANY_MARK		{ $$ = newASTLabel(@$, $3, $1); }


List: AtomExp				{ $$ = addASTAtom(@1, $1, NULL); } 
    | List ':' AtomExp 			{ $$ = addASTAtom(@3, $3, $1); }
    | _EMPTY				{ $$ = addASTEmptyList(@$); }


AtomExp: Variable			{ $$ = newASTVariable(@$, $1); if($1) free($1); }
       | NUM 				{ $$ = newASTNumber(@$, $1); }
       | CHAR				{ $$ = newASTCharacter(@$, $1); 
   					  if($1) free($1); }
       | STR 				{ $$ = newASTString(@$, $1); 
					  if($1) free($1); }
       | INDEG '(' NodeID ')' 		{ $$ = newASTDegreeOp(INDEGREE, @$, $3); 
					  if($3) free($3); }
       | OUTDEG '(' NodeID ')' 		{ $$ = newASTDegreeOp(OUTDEGREE, @$, $3); 
				 	  if($3) free($3); }
       | LLEN '(' List ')' 		{ $$ = newASTListLength(@$, $3); }
       | SLEN '(' AtomExp ')' 		{ $$ = newASTStringLength(@$, $3); }
       | '-' AtomExp %prec UMINUS 	{ $$ = newASTNegExp(@$, $2); } 
       | '(' AtomExp ')' 		{ $$ = $2; }
       | AtomExp '+' AtomExp 		{ $$ = newASTBinaryOp(ADD, @$, $1, $3);  }
       | AtomExp '-' AtomExp 		{ $$ = newASTBinaryOp(SUBTRACT, @$, $1, $3); }
       | AtomExp '*' AtomExp 		{ $$ = newASTBinaryOp(MULTIPLY, @$, $1, $3); }
       | AtomExp '/' AtomExp 		{ $$ = newASTBinaryOp(DIVIDE, @$, $1, $3); }
       | AtomExp '.' AtomExp 		{ $$ = newASTBinaryOp(CONCAT, @$, $1, $3); }

 /* GP2 Identifiers */

ProcID: PROCID 				/* default $$ = $1 */ 
RuleID: ID		         	/* default $$ = $1 */ 
NodeID: ID				/* default $$ = $1 */ 
EdgeID: ID				/* default $$ = $1 */ 
Variable: ID		  		/* default $$ = $1 */ 

/* Grammar for host graphs. This is identical in structure to the grammar for
 * graphs in the main syntax, but host graphs come with a name and their labels
 * must only contain constant values. The 'C' before various nonterminal 
 * symbols stands for 'Constant'. The Position and RootNode rules from above
 * are used.
 */

HostGraph: '[' '|' ']'  		{ $$ = newASTGraph(@$, NULL, NULL); }
         | '[' HostNodeList '|' ']'  	{ $$ = newASTGraph(@$, $2, NULL); }
         | '[' HostNodeList '|' HostEdgeList ']' 
     					{ $$ = newASTGraph(@$, $2, $4); }

HostNodeList: HostNode			{ $$ = addASTNode(@1, $1, NULL); }
            | HostNodeList HostNode	{ $$ = addASTNode(@2, $2, $1); }

HostNode: '(' NodeID RootNode ',' HostLabel ')'
    					{ $$ = newASTNode(@2, is_root, $2, $5); 
 					  is_root = false; 	
					  if($2) free($2); } 

HostEdgeList: HostEdge			{ $$ = addASTEdge(@1, $1, NULL); }
            | HostEdgeList HostEdge	{ $$ = addASTEdge(@2, $2, $1); } 

HostEdge: '(' EdgeID ',' NodeID ',' NodeID ',' HostLabel ')'
					{ $$ = newASTEdge(@2, false, $2, $4, $6, $8);
					  if($2) free($2); 
					  if($4) free($4); 
                     			  if($6) free($6); }

HostLabel: HostList			{ $$ = newASTLabel(@$, NONE, $1); }
         | HostList '#' MARK	  	{ $$ = newASTLabel(@$, $3, $1); }

HostList: HostExp 			{ $$ = addASTAtom(@1, $1, NULL); } 
        | HostList ':' HostExp 		{ $$ = addASTAtom(@3, $3, $1); }
        | _EMPTY			{ $$ = addASTEmptyList(@$); }


HostExp: NUM 				{ $$ = newASTNumber(@$, $1); }
       | CHAR				{ $$ = newASTCharacter(@$, $1); if($1) free($1); }
       | STR 				{ $$ = newASTString(@$, $1); if($1) free($1); }

%%

/* Bison calls yyerror whenever it encounters an error. It prints error
 * messages to stderr and log_file. */
void yyerror(const char *error_message)
{
   fprintf(stderr, "Error at '%s': %s\n", yytext, error_message);
   fprintf(log_file, "%d.%d-%d.%d: Error at '%s': %s\n\n", 
           yylloc.first_line, yylloc.first_column, yylloc.last_line, 
           yylloc.last_column, yytext, error_message);
}

/* report_warning is identical to yyerror except that it doesn't refer to yytext.
 * This is called in the action code of error-catching Bison rules in which
 * the value of yytext may be misleading. */
void report_warning(const char *error_message)
{
   fprintf(stderr, "Warning: %s\n", error_message);
   fprintf(log_file, "%d.%d-%d.%d: Error: %s\n\n", 
           yylloc.first_line, yylloc.first_column, yylloc.last_line, 
           yylloc.last_column, error_message);
   syntax_error = true;
}
        
