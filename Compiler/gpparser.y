/* ////////////////////////////////////////////////////////////////////////////

  ===================================
  gpparser.y - Chris Bak (28/05/2013) 					
  ===================================

  The Bison specification for GP2's parser. In combination with Flex, it 
  performs syntax checking and generates an Abstract Syntax Tree for GP2
  programs and host graphs

//////////////////////////////////////////////////////////////////////////// */


%{

#include "ast.h" /* MarkType, ListType, cond_exp_t, AST constructors */
#include <stdio.h>  /* printf, fprintf, fopen */
#include <stdlib.h> /* malloc, free */
#include <stdarg.h> /* va_start, va_list, va_end */

int yyerror(const char *error_message);
int report_error(const char *error_message);

/* Flags used in the AST construction. */
  bool is_root = false;
  bool is_injective = false;

extern List *gp_program; /* This will point to the root of the program AST.
			  * Defined in main.c. */
extern GPGraph *host_graph; /* This will point to the root of the host graph AST.
			     * Defined in main.c */
extern bool abort_scan; /* Defined in main.c */


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
%token WHERE EDGETEST TRUE FALSE 		               
%token INDEG OUTDEG LLEN SLEN					
%token INT STRING ATOM LIST 	                               
%token INTERFACE EMPTY INJECTIVE 	
%token <mark> MARK CYAN_MARK			                        
%token ARROW					                
%token NEQ GTEQ LTEQ			                       
%token <num> NUM 
%token <str> STR CHAR
%token <id> PROCID ID           				
%token ROOT	
%token GP_PROGRAM GP_GRAPH						


%left '+' '-' AND	/* Lowest precedence level */
%left '*' '/' OR
%left UMINUS NOT	/* UMINUS represents unary '-' */
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
%type <pos> Position
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
%destructor { freeDeclaration($$); } <decl>
%destructor { freeStatement($$); } <stmt>
%destructor { freeRule($$); } <rule>
%destructor { freeGraph($$); } <graph>
%destructor { freeNode($$); } <node>
%destructor { freeEdge($$); } <edge>
%destructor { freeCondition($$); } <cond_exp>
%destructor { freeLabel($$); } <label>
%destructor { freeAtomicExp($$); } <atom_exp>

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
          | GP_GRAPH HostGraph          { host_graph = $2; }

 /* Grammar for GP2 Program Text. */

Program: Declaration	      		{ $$ = addDecl(GLOBAL_DECLARATIONS, 
                                               @1, $1, NULL); }  
       | Program Declaration            { $$ = addDecl(GLOBAL_DECLARATIONS, 
                                               @2, $2, $1); }  

Declaration: MainDecl 			{ $$ = newMainDecl(@$, $1); }
     	   | ProcDecl			{ $$ = newProcedureDecl(@$, $1); }
           | RuleDecl			{ $$ = newRuleDecl(@$, $1); }

MainDecl: MAIN '=' ComSeq 		{ $$ = newCommandSequence(@1, $3); }

ProcDecl: ProcID '=' ComSeq 		{ $$ = newProcedure(@1, $1, NULL, 
                                               newCommandSequence(@3 ,$3));
					  free($1); }

        | ProcID '=' '[' LocalDecls ']' ComSeq 
					{ $$ = newProcedure(@1, $1, $4, 
                                               newCommandSequence(@6, $6));
				          free($1); }
        /* Error-catching production */
	| RuleID '=' '[' LocalDecls ']' ComSeq
				        { $$ = newProcedure(@1, $1, $4, 
                                               newCommandSequence(@6, $6));
                                          report_error("Procedure names must "
 					   "start with an upper-case letter."); 
					  free($1); }

LocalDecls: /* empty */			{ $$ = NULL; }
        | LocalDecls RuleDecl           { $$ = addDecl(LOCAL_DECLARATIONS, @2, 
                                               newRuleDecl(@2, $2), $1); }
	| LocalDecls ProcDecl 		{ $$ = addDecl(LOCAL_DECLARATIONS, @2,
                                               newProcedureDecl(@2, $2), $1); }

ComSeq: Command 			{ $$ = addCommand(@1, $1, NULL); }
      | ComSeq ';' Command  		{ $$ = addCommand(@3, $3, $1); }
      /* Error-catching production */
      | ComSeq ',' Command		{ $$ = addCommand(@3, $3, $1);
                                          report_error("Incorrect use of comma "
					    "to separate commands. Perhaps you "
					    "meant to use a semicolon?"); }

Command: Block 				/* default $$ = $1 */ 
       | IF Block THEN Block      	{ $$ = newCondBranch(IF_STATEMENT, @$,
                                               $2, $4, newSkip(@$)); }
       | IF Block THEN Block ELSE Block { $$ = newCondBranch(IF_STATEMENT, @$,
                                               $2, $4, $6); }
       /* Error-catching production */
       | IF Block ELSE Block	   	{ $$ = newCondBranch(IF_STATEMENT, @$,
                                               $2, NULL, $4);
                                          report_error("No 'then' clause in if "
						       "statement."); }
       | TRY Block 			{ $$ = newCondBranch(TRY_STATEMENT, @$,
                                               $2, newSkip(@$), newSkip(@$)); }
       | TRY Block THEN Block		{ $$ = newCondBranch(TRY_STATEMENT, @$,
                                               $2, $4, newSkip(@$)); }
       | TRY Block ELSE	Block	   	{ $$ = newCondBranch(TRY_STATEMENT, @$,
                                               $2, newSkip(@$), $4); }
       | TRY Block THEN Block ELSE Block { $$ = newCondBranch(TRY_STATEMENT, @$,
                                                $2, $4, $6); }


Block: '(' ComSeq ')' 	                { $$ = newCommandSequence(@$,$2); }
     | '(' ComSeq ')' '!' 		{ $$ = newAlap(@$, 
                                               newCommandSequence(@2, $2)); } 
     /* If an error is found in a code block, continue parsing after the right
      * parenthesis. */
     | error ')'  			{ $$ = NULL; }
     | SimpleCommand 			/* default $$ = $1 */ 
     | SimpleCommand '!'		{ $$ = newAlap(@$, $1); }
     | Block OR Block 			{ $$ = newOrStmt(@$, $1, $3); }
     | SKIP				{ $$ = newSkip(@$); }
     | FAIL				{ $$ = newFail(@$); }

SimpleCommand: RuleSetCall 	        { $$ = newRuleSetCall(@$, $1); }
             | RuleID                   { $$ = newRuleCall(@$, $1); free($1); }
	     | ProcID	 		{ $$ = newProcCall(@$, $1); free($1); }

RuleSetCall: '{' IDList '}'		{ $$ = $2; }
           /* If an error is found in an rule set call, continue parsing after
            * the rule set. */
           | error '}' 			{ $$ = NULL; }

IDList: RuleID				{ $$ = addRule(@1, $1, NULL);
					  free($1); }
      | IDList ',' RuleID 		{ $$ = addRule(@3, $3, $1); 
					  free($3);} 
      /* Error-catching productions */
      | ProcID	 			{ $$ = addRule(@1, $1, NULL);
                                          report_error("Procedure name used in "
					   "a rule set. Rule names must start "
					   "with a lower-case letter.");
				          free($1); }
      | IDList ';' RuleID		{ $$ = addRule(@3, $3, $1);
                                          report_error("Semicolon used in a "
					   "rule set. Perhaps you meant to "
					   "use a comma?"); 
					  free($1); }


 /* Grammar for GP2 Rule Definitions. */

RuleDecl: RuleID '(' VarDecls ')' Graph ARROW Graph Inter CondDecl INJECTIVE 
          '=' Bool			{ $$ = newRule(@1, is_injective,
 					       $1, $3, $5, $7, $8, $9); 
					  free($1); }
        | RuleID '(' ')' Graph ARROW Graph Inter CondDecl INJECTIVE '=' Bool	 
      					{ $$ = newRule(@1, is_injective,
 					       $1, NULL, $4, $6, $7, $8); 
					  free($1); }
        /* Error-catching productions */
	| ProcID '(' VarDecls ')' Graph ARROW Graph Inter CondDecl INJECTIVE 
          '=' Bool		        { $$ = newRule(@1, is_injective,
 					       $1, $3, $5, $7, $8, $9); 
                                          report_error("Rule names must "
 					   "start with a lower-case letter."
				 	   "letter.");
					  free($1); }
	/* This production catches a potentially likely syntax error in which
         * the user terminates the variable declaration list with a semicolon.
         */
        | RuleID '(' VarDecls ';' ')' Graph ARROW Graph Inter CondDecl INJECTIVE 
          '=' Bool			{ $$ = newRule(@1, is_injective,
 					       $1, $3, $6, $8, $9, $10);  
                                          report_error("Semicolon at the end "
					    "of a rule's variable list");
					  free($1); }	


VarDecls: VarList ':' Type		{ $$ = addVariableDecl($3, @$, $1, NULL); }  
	/* The location of VarDecls on the LHS is manually set to the location
         * of 'VarList ':' Type' as each Variable Declaration AST node should
         * only represent one list of variables.
 	 */
	| VarDecls ';' VarList ':' Type { @$.first_column = @3.first_column;
				          @$.first_line = @3.first_line;
					  @$.last_column = @5.last_column;
				          @$.last_column = @5.last_column;
					  $$ = addVariableDecl($5, @$, $3, $1); }

VarList: Variable 			{ $$ = addVariable(@1, $1, NULL); 
					  free($1); }
       | VarList ',' Variable          	{ $$ = addVariable(@3, $3, $1); 
		 	                  free($3); }

Inter: INTERFACE '=' '{' '}'   		{ $$ = NULL; }
     | INTERFACE '=' '{' NodeIDList '}' { $$ = $4; }
     /* If an error is found in an interface list, continue parsing after the 
      * interface list. */
     | error '}'			{ report_error("Error in an interface "
                                                       " list.");  
                                          $$ = NULL; }

NodeIDList: NodeID			{ $$ = addNodeID(@1, $1, NULL); 
					  free($1); }
          | NodeIDList ',' NodeID 	{ $$ = addNodeID(@3, $3, $1);
					  free($3); }

Bool: TRUE 				{ is_injective = true; }
    | FALSE				{ is_injective = false; }

Type: INT				{ $$ = INT_DECLARATIONS; } 
    | CHAR				{ $$ = CHAR_DECLARATIONS; }
    | STRING                            { $$ = STRING_DECLARATIONS; }
    | ATOM 	                        { $$ = ATOM_DECLARATIONS; }
    | LIST				{ $$ = LIST_DECLARATIONS; }


 /* Grammar for GP2 Graph Definitions. */

Graph: '[' Position '|' '|' ']'		 { $$ = newGraph(@$, $2, NULL, NULL); }
     | '[' Position '|' NodeList '|' ']' { $$ = newGraph(@$, $2, $4, NULL); }
     | '[' Position '|' NodeList '|' EdgeList ']' 
     					{ $$ = newGraph(@$, $2, $4, $6); }

NodeList: Node				{ $$ = addNode(@1, $1, NULL); }
        | NodeList ',' Node		{ $$ = addNode(@3, $3, $1); }

Node: '(' NodeID RootNode ',' Label ',' Position ')'
    					{ $$ = newNode(@2, is_root, $2, $5, $7); 
 					  is_root = false; 	
					  free($2); } 

EdgeList: Edge				{ $$ = addEdge(@1, $1, NULL); }
        | EdgeList ',' Edge		{ $$ = addEdge(@3, $3, $1); }

Edge: '(' EdgeID ',' NodeID ',' NodeID ',' Label ')'
					{ $$ = newEdge(@2, $2, $4, $6, $8);
					  free($2); free($4); free($6); }

RootNode: /* empty */ 
	| ROOT 				{ is_root = true; }

Position: '(' NUM ',' NUM ')' 		{ $$ = newPosition(@$, $2, $4); }


 /* Grammar for GP2 Conditions. */

CondDecl: /* empty */                   { $$ = NULL; }
        | WHERE Condition		{ $$ = $2; }

Condition: Subtype '(' Variable ')' 	{ $$ = newSubtypePred($1, @$, $3); 
					  free($3); }
         | EDGETEST '(' NodeID ',' NodeID  LabelArg ')' 
					{ $$ = newEdgePred(@$, $3, $5, $6); 
					  free($3); free($5); }
	 | List '=' List 		{ $$ = newListComparison(EQUAL, @$, $1, $3); }
	 | List NEQ List 		{ $$ = newListComparison(NOT_EQUAL, @$, $1, $3); }
	 | AtomExp '>' AtomExp          { $$ = newAtomComparison(GREATER, @$, $1, $3); }    
	 | AtomExp GTEQ AtomExp         { $$ = newAtomComparison(GREATER_EQUAL, @$, $1, $3); }    
	 | AtomExp '<' AtomExp          { $$ = newAtomComparison(LESS, @$, $1, $3); }    
	 | AtomExp LTEQ AtomExp         { $$ = newAtomComparison(LESS_EQUAL, @$, $1, $3); }    
         | NOT Condition	        { $$ = newNotExp(@$, $2); }
         | Condition OR Condition  	{ $$ = newBinaryExp(BOOL_OR, @$, $1, $3); }
         | Condition AND Condition      { $$ = newBinaryExp(BOOL_AND, @$, $1, $3); }
	 | '(' Condition ')' 		{ $$ = $2; }

Subtype: INT				{ $$ = INT_CHECK; } 
       | CHAR				{ $$ = CHAR_CHECK; }
       | STRING                         { $$ = STRING_CHECK; }
       | ATOM 	                        { $$ = ATOM_CHECK; }

LabelArg: /* empty */ 			{ $$ = NULL; }
 	| ',' Label 			{ $$ = $2; }

 /* Grammar for GP2 Labels */

Label: List				{ $$ = newLabel(@$, NONE, $1); }
     | List '#' MARK	  		{ $$ = newLabel(@$, $3, $1); }
     /* Cyan has a distinct token since it cannot occur in the host graph. */
     | List '#' CYAN_MARK		{ $$ = newLabel(@$, $3, $1); }


List: AtomExp				{ $$ = addAtom(@1, $1, NULL); } 
    | List ':' AtomExp 			{ $$ = addAtom(@3, $3, $1); }
    | EMPTY				{ $$ = addEmptyList(@$); }


AtomExp: Variable			{ $$ = newVariable(@$, $1); free($1); }
       | NUM 				{ $$ = newNumber(@$, $1); }
       | CHAR				{ $$ = newCharacter(@$, $1); 
   					  if($1) free($1); }
       | STR 				{ $$ = newString(@$, $1); 
					  if($1) free($1); }
       | INDEG '(' NodeID ')' 		{ $$ = newDegreeOp(INDEGREE, @$, $3); 
					  free($3); }
       | OUTDEG '(' NodeID ')' 		{ $$ = newDegreeOp(OUTDEGREE, @$, $3); 
				 	  free($3); }
       | LLEN '(' List ')' 		{ $$ = newListLength(@$, $3); }
       | SLEN '(' AtomExp ')' 		{ $$ = newStringLength(@$, $3); }
       | '-' AtomExp %prec UMINUS 	{ $$ = newNegExp(@$, $2); } 
       | '(' AtomExp ')' 		{ $$ = $2; }
       | AtomExp '+' AtomExp 		{ $$ = newBinaryOp(ADD, @$, $1, $3);  }
       | AtomExp '-' AtomExp 		{ $$ = newBinaryOp(SUBTRACT, @$, $1, $3); }
       | AtomExp '*' AtomExp 		{ $$ = newBinaryOp(MULTIPLY, @$, $1, $3); }
       | AtomExp '/' AtomExp 		{ $$ = newBinaryOp(DIVIDE, @$, $1, $3); }
       | AtomExp '.' AtomExp 		{ $$ = newBinaryOp(CONCAT, @$, $1, $3); }

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

HostGraph: '[' Position '|' '|' ']'  	{ $$ = newGraph(@$, $2, NULL, NULL); }
         | '[' Position '|' HostNodeList '|' ']'  
					{ $$ = newGraph(@$, $2, $4, NULL); }
         | '[' Position '|' HostNodeList '|' HostEdgeList ']' 
     					{ $$ = newGraph(@$, $2, $4, $6); }

HostNodeList: HostNode			{ $$ = addNode(@1, $1, NULL); }
            | HostNodeList ',' HostNode	{ $$ = addNode(@3, $3, $1); }

HostNode: '(' NodeID RootNode ',' HostLabel ',' Position ')'
    					{ $$ = newNode(@2, is_root, $2, $5, $7); 
 					  is_root = false; 	
					  free($2); } 

HostEdgeList: HostEdge			{ $$ = addEdge(@1, $1, NULL); }
            | HostEdgeList ',' HostEdge	{ $$ = addEdge(@3, $3, $1); } 

HostEdge: '(' EdgeID ',' NodeID ',' NodeID ',' HostLabel ')'
					{ $$ = newEdge(@2, $2, $4, $6, $8);
					  free($2); free($4); free($6); }

HostLabel: HostList			{ $$ = newLabel(@$, NONE, $1); }
         | HostList '#' MARK	  	{ $$ = newLabel(@$, $3, $1); }

HostList: HostExp 			{ $$ = addAtom(@1, $1, NULL); } 
        | HostList ':' HostExp 		{ $$ = addAtom(@3, $3, $1); }
        | EMPTY				{ $$ = addEmptyList(@$); }


HostExp: NUM 				{ $$ = newNumber(@$, $1); }
       | CHAR				{ $$ = newCharacter(@$, $1); free($1); }
       | STR 				{ $$ = newString(@$, $1); free($1); }

%%

/* Bison calls yyerror whenever it encounters an error. It prints error
 * messages to stderr and log_file, and sets abort_scan to prevent the semantic
 * analysis functions from being called. 
 */

int yyerror(const char *error_message)
{
   if(yylloc.first_line)
     fprintf(stderr, "Error at '%s': %s\n", yytext, error_message);
     fprintf(log_file, "%d.%d-%d.%d: Error at '%s': %s\n", 
             yylloc.first_line, yylloc.first_column, yylloc.last_line, 
             yylloc.last_column, yytext, error_message);
     abort_scan = true;
   return 0;
}

/* report_error is identical to yyerror except that it doesn't refer to yytext.
 * This is called in the action code of error-catching Bison rules in which
 * the value of yytext may be misleading.
 */

int report_error(const char *error_message)
{
   if(yylloc.first_line)
     fprintf(stderr, "Error: %s\n", error_message);
     fprintf(log_file, "%d.%d-%d.%d: Error: %s\n", 
             yylloc.first_line, yylloc.first_column, yylloc.last_line, 
             yylloc.last_column, error_message);
     abort_scan = true;
   return 0;
}
        
