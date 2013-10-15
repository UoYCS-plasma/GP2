/*/////////////////////////////////////////////////////////////////////////////

                                ast.h                               
 
  This is the header file for the GP2 parser. It contains enumerated
  type definitions, AST node definitions and prototypes for AST constructors.
 

                   Created on 28/5/13 by Chris Bak 

//////////////////////////////////////////////////////////////////////////// */


/* Bison uses a global variable yylloc of type YYLTYPE to keep track of the locations of 
 * tokens and nonterminals. The scanner will set these values upon reading each token. 
 * This is the standard YYLTYPE definition but I define it here so it is seen by every file.
 */

#include <stdbool.h> 

typedef struct YYLTYPE {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;

# define YYLTYPE_IS_DECLARED 1 /* tells the parser that YYLTYPE is defined here */

extern FILE *yyin; /* Created by Bison. */


/* Declarations for functions and variables defined in gplexer.l */
extern int yylineno; 
extern char *yytext; 


/* Declarations for functions and variables defined in gpparser.y */
int yyerror(char *s);
int yyparse(void);
void print_error(YYLTYPE loc, char *errormsg, ...);
extern struct List *gp_program; 
extern char *file_name; 
extern bool is_injective;
extern bool is_root;
extern int yydebug;


/* enum used by the lexer for mark keywords */

typedef enum {RED=0, GREEN, BLUE, GREY, DASHED, NONE} mark_t; 


/* Definition of AST nodes representing lists. */

typedef enum {GLOBAL_DECLARATIONS=0, LOCAL_DECLARATIONS, COMMANDS, RULES, 
              INT_DECLARATIONS, STRING_DECLARATIONS, ATOM_DECLARATIONS, 
              LIST_DECLARATIONS, VARIABLE_LIST, INTERFACE_LIST, NODE_LIST, 
              EDGE_LIST, GP_LIST} list_t;

typedef struct List {
  int node_id;
  list_t list_type;  
  YYLTYPE location;  /* location of symbol in the source file */
  union {
    struct GPDeclaration *decl;
    struct GPStatement *command;
    char *rule_name;
    struct List *vars; /* multiple variables declared with the same type: INT_DECLARATIONS, 
                        * STRING_DECLARATIONS, ATOM_DECLARATIONS */
    char *var; 	  
    struct GPNodePair *node_pair; /* pair of nodes specified in the interface of a rule */	   
    struct GPNode *node;    
    struct GPEdge *edge;    
    struct GPAtomicExp *atom;
  } value;
  struct List *next;
} List;

List *addDecl (list_t list_type, YYLTYPE location, struct GPDeclaration *decl, 
	struct List *next);
List *addCommand (YYLTYPE location, struct GPStatement *command, 
	struct List *next);
List *addRule (YYLTYPE location, char *rule_name, struct List *next);
List *addVariableDecl (list_t list_type, YYLTYPE location, struct List *vars,
	struct List *next);
List *addVariable (YYLTYPE location, char *var, struct List *next);
List *addNodePair (YYLTYPE location, struct GPNodePair *node_pair,
	struct List *next);
List *addNode (YYLTYPE location, struct GPNode *node, struct List *next);
List *addEdge (YYLTYPE location, struct GPEdge *edge, struct List *next);
List *addAtom (YYLTYPE location, struct GPAtomicExp *atom, struct List *next);


/* Definition of AST nodes representing declarations */

typedef enum {MAIN_DECLARATION=0, PROCEDURE_DECLARATION, RULE_DECLARATION} decl_t;

typedef struct GPDeclaration {
  int node_id;
  decl_t decl_type;
  YYLTYPE location;
  union {
    struct GPStatement *main_program;
    struct GPProcedure *proc;
    struct GPRule *rule;
  } value;
} GPDeclaration;

GPDeclaration *newMainDecl (YYLTYPE location, struct GPStatement *main_program);
GPDeclaration *newProcedureDecl (YYLTYPE location, struct GPProcedure *proc);
GPDeclaration *newRuleDecl (YYLTYPE location, struct GPRule *rule);


/* Definition of AST nodes representing GP program statements. */

typedef enum {COMMAND_SEQUENCE=0, RULE_CALL, RULE_SET_CALL, PROCEDURE_CALL, 
              IF_STATEMENT, TRY_STATEMENT, ALAP_STATEMENT, PROGRAM_OR, 
              SKIP_STATEMENT, FAIL_STATEMENT} stmt_t;

typedef struct GPStatement {
  int node_id;
  stmt_t statement_type;
  YYLTYPE location;
  union {    
    struct List *cmd_seq;
    char *rule_name;
    struct List *rule_set;
    char *proc_name;
    struct { struct GPStatement *condition; struct GPStatement *then_stmt; 
	     struct GPStatement *else_stmt; } cond_branch;
             /* IF_STATEMENT, TRY_STATEMENT */
    struct GPStatement *loop_stmt;
    struct { struct GPStatement *left_stmt; struct GPStatement *right_stmt; } or_stmt;
    /* skip and fail are predefined GP rules represented by a struct GPStatement
     * containing only a statement_type and location */
  } value;
} GPStatement;

GPStatement *newCommandSequence(YYLTYPE location, struct List *cmd_seq);
GPStatement *newRuleCall(YYLTYPE location, char *rule_name);
GPStatement *newRuleSetCall(YYLTYPE location, struct List *rule_set);
GPStatement *newProcCall(YYLTYPE location, char *proc_name);
GPStatement *newCondBranch(stmt_t statement_type, YYLTYPE location, 
	      struct GPStatement *condition, struct GPStatement *then_stmt, 
	      struct GPStatement *else_stmt);
GPStatement *newAlap(YYLTYPE location, struct GPStatement *loop_stmt);
GPStatement *newOrStmt(YYLTYPE location, struct GPStatement *left_stmt, 
	      struct GPStatement *right_stmt);
GPStatement *newSkip(YYLTYPE location);
GPStatement *newFail(YYLTYPE location);


/* Definition of AST nodes representing conditional expressions.*/

typedef enum {INT_CHECK=0, STRING_CHECK, ATOM_CHECK, EDGE_PRED, REL_EXP, 
              BOOL_NOT, BOOL_OR, BOOL_AND, EQUAL, NOT_EQUAL, GREATER, 
	      GREATER_EQUAL, LESS, LESS_EQUAL} condexp_t;

typedef struct GPCondExp {
  int node_id;
  condexp_t exp_type;
  YYLTYPE location;
  union {
    char *var; /* type checking predicates: INT_CHECK, STRING_CHECK, ATOM_CHECK */
    struct { char *source; char *target; struct GPLabel *label; } edge_pred;
    struct List *rel_exp; 
    struct { struct List *left_list; struct List *right_list; } list_cmp; /* EQUAL, NOT_EQUAL */
    struct { struct GPAtomicExp *left_exp; struct GPAtomicExp *right_exp; } atom_cmp;
            /* GREATER, GREATER_EQUAL, LESS, LESS_EQUAL */
    struct GPCondExp *not_exp;
    struct { struct GPCondExp *left_exp; struct GPCondExp *right_exp; } bin_exp; /* OR, AND */
  } value;
} GPCondExp;

GPCondExp *newSubtypePred (condexp_t exp_type, YYLTYPE location, char *var);
GPCondExp *newEdgePred (YYLTYPE location, char *source, char *target,
	    struct GPLabel *label);
GPCondExp *newListComparison (condexp_t exp_type, YYLTYPE location, 
	    struct List *left_list, struct List *right_list);
GPCondExp *newAtomComparison (condexp_t exp_type, YYLTYPE location,
	    struct GPAtomicExp *left_exp, struct GPAtomicExp *right_exp);
GPCondExp *newRelationalExp (YYLTYPE location, struct List *rel_exp);
GPCondExp *newNotExp (YYLTYPE location, struct GPCondExp *not_exp);
GPCondExp *newBinaryExp (condexp_t exp_type, YYLTYPE location, 
	    struct GPCondExp *left_exp, struct GPCondExp *right_exp);


/* Definition of AST nodes representing integer or string expressions. */

typedef enum {EMPTY_LIST=0, VARIABLE, INT_CONSTANT, STRING_CONSTANT, INDEGREE, 
              OUTDEGREE, LIST_LENGTH, STRING_LENGTH, NEG, ADD, SUBTRACT, 
              MULTIPLY, DIVIDE, CONCAT} atomexp_t;

typedef struct GPAtomicExp {
  int node_id;
  atomexp_t exp_type;
  YYLTYPE location;
  union {
    char *name;
    int num;
    char *str;
    char *node_id; /* The indegree and outdegree operators take a node as their argument. */
    struct List *list_arg; /* Query for the length of a list. */
    struct GPAtomicExp *str_arg; /* Query for the length of a string. */
    struct GPAtomicExp *exp; /* Negated expression  */
    struct { struct GPAtomicExp *left_exp; struct GPAtomicExp *right_exp; } bin_op; 
            /* ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT */
  } value;
} GPAtomicExp;

GPAtomicExp *newEmpty (YYLTYPE location);
GPAtomicExp *newVariable (YYLTYPE location, char *name);
GPAtomicExp *newNumber (YYLTYPE location, int num);
GPAtomicExp *newString (YYLTYPE location, char *str);
GPAtomicExp *newDegreeOp (atomexp_t exp_type, YYLTYPE location, char *node_id);
GPAtomicExp *newListLength (YYLTYPE location, struct List *list_arg);
GPAtomicExp *newStringLength (YYLTYPE location, struct GPAtomicExp *str_arg);
GPAtomicExp *newNegExp (YYLTYPE location, struct GPAtomicExp *exp);
GPAtomicExp *newBinaryOp (atomexp_t exp_type, YYLTYPE location, 
	      struct GPAtomicExp *left_exp, struct GPAtomicExp *right_exp);


/* Definition of the remaining AST node types. */

typedef enum {PROCEDURE=0, RULE, NODE_PAIR, GRAPH, NODE, EDGE, POSITION, LABEL} ast_node_t;

/* Root node for a procedure definition. */

typedef struct GPProcedure {
  int node_id;
  ast_node_t node_type;
  YYLTYPE location;
  char *name; 
  struct List *local_decls; 
  struct GPStatement *cmd_seq; 
} GPProcedure;

GPProcedure *newProcedure(YYLTYPE location, char *name, struct List *local_decls, 
              struct GPStatement *cmd_seq);


/* Root node for a rule definition. */

typedef struct GPRule {
  int node_id;
  ast_node_t node_type;
  YYLTYPE location;
  bool injective; /* Integer flag to mark whether the rule should be matched injectively or not. */
  char *name; 
  struct List *variables;
  struct GPGraph *lhs;
  struct GPGraph *rhs;
  struct List *interface;
  struct GPCondExp *condition;  
} GPRule;

GPRule *newRule(YYLTYPE location, bool injective, char *name, 
	 struct List *variables, struct GPGraph *lhs, struct GPGraph *rhs, 
	 struct List *interface, struct GPCondExp *condition);


typedef struct GPNodePair {
  int node_id;
  ast_node_t node_type; 
  YYLTYPE location;
  char *left_node;
  char *right_node;
} GPNodePair;

GPNodePair *newNodePair (YYLTYPE location, char *left_node, char *right_node);


typedef struct GPGraph {
  int node_id;
  ast_node_t node_type;	
  YYLTYPE location;
  struct GPPos *position;
  struct List *nodes;
  struct List *edges;
} GPGraph;

GPGraph *newGraph (YYLTYPE location, struct GPPos *position, 
          struct List *nodes, struct List *edges);


typedef struct GPNode {
  int node_id;
  ast_node_t node_type;	
  YYLTYPE location; 
  bool root; /* Integer flag to mark whether the node is a root node or not. */
  char *name; 
  struct GPLabel *label; 
  struct GPPos *position; 
} GPNode;

GPNode *newNode (YYLTYPE location, bool root, char *name, struct GPLabel *label,
	 struct GPPos *position);


typedef struct GPEdge {
  int node_id;
  ast_node_t node_type;	
  YYLTYPE location; 
  char *name; 
  char *source; 
  char *target; 
  struct GPLabel *label; 
} GPEdge;

GPEdge *newEdge (YYLTYPE location, char *name, char *source, char *target,
	 struct GPLabel *label);


/* AST node for specifying locations in the graphical editor. */

typedef struct GPPos {
  int node_id;
  ast_node_t node_type; 
  YYLTYPE location; 
  int x;
  int y;
} GPPos;

GPPos *newPosition (YYLTYPE location, int x, int y);


typedef struct GPLabel {
  int node_id;
  ast_node_t node_type; 
  YYLTYPE location; 
  mark_t mark;
  struct List *gp_list;
} GPLabel;

GPLabel *newLabel (YYLTYPE location, mark_t mark, struct List *gp_list);



