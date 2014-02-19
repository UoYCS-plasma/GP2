/*/////////////////////////////////////////////////////////////////////////////

                                ast.h                               
 
  This is the header file for the GP2 parser. It contains enumerated
  type definitions, AST node definitions and prototypes for AST constructors.
 

                   Created on 28/5/13 by Chris Bak 

//////////////////////////////////////////////////////////////////////////// */


/* Bison uses a global variable yylloc of type YYLTYPE to keep track of the 
 * locations of tokens and nonterminals. The scanner will set these values upon
 * reading each token. This is the standard YYLTYPE definition but I define it
 * here so it is seen by every file.
 */

#ifndef INC_AST_H
#define INC_AST_H 

#include <stdio.h> /* FILE type */
#include <stdbool.h>

typedef struct YYLTYPE {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;

# define YYLTYPE_IS_DECLARED 1 /* tells the parser that YYLTYPE is defined here */

extern FILE *yyin; /* Created by Bison. */
extern FILE *log_file; /* Created in main.c */


/* Declarations for functions and variables defined in gplexer.l */
extern int yylineno; 
extern char *yytext; 


/* Declarations for functions and variables defined in gpparser.y */
int yyparse(void);
extern struct List *gp_program; 
extern int yydebug;

/* enum used by the parser for mark keywords */

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
    struct GPDeclaration *declaration; /* GLOBAL_DECLARATIONS, 
					* LOCAL_DECLARATIONS */
    struct GPStatement *command;       /* COMMANDS */
    char *rule_name; 		       /* RULES */
    struct List *variables; 
     				       /* INT_DECLARATIONS, STRING_DECLARATIONS,
					* ATOM_DECLARATIONS */ 
    char *variable_name; 	       /* VARIABLE_LIST */	  
    char *node_id; 		       /* INTERFACE_LIST */	   
    struct GPNode *node; 	       /* NODE_LIST */   
    struct GPEdge *edge; 	       /* EDGE_LIST */   
    struct GPAtomicExp *atom;          /* GP_LIST */
  } value;
  struct List *next;
} List;

List *addDecl (list_t list_type, YYLTYPE location, 
	struct GPDeclaration *declaration, struct List *next);
List *addCommand (YYLTYPE location, struct GPStatement *command, 
	struct List *next);
List *addRule (YYLTYPE location, char *rule_name, struct List *next);
List *addVariableDecl (list_t list_type, YYLTYPE location, 
	struct List *variables, struct List *next);
List *addVariable (YYLTYPE location, char *variable_name, struct List *next);
List *addNodeID (YYLTYPE location, char *node_id, struct List *next);
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
    struct GPStatement *main_program; 	/* MAIN_DECLARATION */
    struct GPProcedure *procedure; 	/* PROCEDURE_DECLARATION */
    struct GPRule *rule; 		/* RULE_DECLARATION */
  } value;
} GPDeclaration;

GPDeclaration *newMainDecl (YYLTYPE location, struct GPStatement *main_program);
GPDeclaration *newProcedureDecl (YYLTYPE location, struct GPProcedure *procedure);
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
    struct List *cmd_seq; 		/* COMMAND_SEQUENCE */
    char *rule_name; 			/* RULE_CALL */
    struct List *rule_set; 		/* RULE_SET_CALL */
    char *proc_name;			/* PROCEDURE_CALL */

    struct {  
      struct GPStatement *condition;
      struct GPStatement *then_stmt; 
      struct GPStatement *else_stmt; 
    } cond_branch; 			/* IF_STATEMENT, TRY_STATEMENT */

    struct GPStatement *loop_stmt; 	/* ALAP_STATEMENT */

    struct { 
      struct GPStatement *left_stmt; 
      struct GPStatement *right_stmt; 
    } or_stmt;			        /* PROGRAM_OR */
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

typedef enum {INT_CHECK=0, CHAR_CHECK, STRING_CHECK, ATOM_CHECK, EDGE_PRED,
              EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, 
	      BOOL_NOT, BOOL_OR, BOOL_AND } condexp_t;

typedef struct GPCondExp {
  int node_id;
  condexp_t exp_type;
  YYLTYPE location;
  union {
    char *var; 			/* INT_CHECK, CHAR_CHECK, STRING_CHECK, 
				 * ATOM_CHECK */
    struct {
      char *source; 
      char *target; 
      struct GPLabel *label;
    } edge_pred; 		/* EDGE_PRED */

    struct { 
      struct List *left_list;
      struct List *right_list; 
    } list_cmp; 		/* EQUAL, NOT_EQUAL */

    struct { 
      struct GPAtomicExp *left_exp; 
      struct GPAtomicExp *right_exp; 
    } atom_cmp; 		/* GREATER, GREATER_EQUAL, LESS, LESS_EQUAL */

    struct GPCondExp *not_exp;  /* BOOL_NOT */

    struct { 
      struct GPCondExp *left_exp; 
      struct GPCondExp *right_exp; 
    } bin_exp; 			/* BOOL_OR, BOOL_AND */
  } value;
} GPCondExp;

GPCondExp *newSubtypePred (condexp_t exp_type, YYLTYPE location, char *var);
GPCondExp *newEdgePred (YYLTYPE location, char *source, char *target,
	    struct GPLabel *label);
GPCondExp *newListComparison (condexp_t exp_type, YYLTYPE location, 
	    struct List *left_list, struct List *right_list);
GPCondExp *newAtomComparison (condexp_t exp_type, YYLTYPE location,
	    struct GPAtomicExp *left_exp, struct GPAtomicExp *right_exp);
GPCondExp *newNotExp (YYLTYPE location, struct GPCondExp *not_exp);
GPCondExp *newBinaryExp (condexp_t exp_type, YYLTYPE location, 
	    struct GPCondExp *left_exp, struct GPCondExp *right_exp);


/* Definition of AST nodes representing integer or string expressions. */

typedef enum {EMPTY_LIST=0, VARIABLE, INT_CONSTANT, CHARACTER_CONSTANT,
              STRING_CONSTANT, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH,
              NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} atomexp_t;

typedef struct GPAtomicExp {
  int node_id;
  atomexp_t exp_type;
  YYLTYPE location;
  union {
    char *name;			  /* VARIABLE */
    int number; 	 	  /* INT_CONSTANT */
    char *string;		  /* CHARACTER_CONSTANT, STRING_CONSTANT */
    char *node_id; 		  /* INDEGREE, OUTDEGREE */
    struct List *list_arg; 	  /* LIST_LENGTH */
    struct GPAtomicExp *str_arg;  /* STRING_LENGTH */
    struct GPAtomicExp *exp; 	  /* NEG */
    struct { 
      struct GPAtomicExp *left_exp;
      struct GPAtomicExp *right_exp;
    } bin_op; 		   	 /* ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT */
  } value;
} GPAtomicExp;

GPAtomicExp *newEmpty (YYLTYPE location);
GPAtomicExp *newVariable (YYLTYPE location, char *name);
GPAtomicExp *newNumber (YYLTYPE location, int number);
GPAtomicExp *newCharacter (YYLTYPE location, char *character);
GPAtomicExp *newString (YYLTYPE location, char *string);
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

/* Prototypes for deallocation functions */

void free_ast(List *ast);
void free_declaration(GPDeclaration *decl);
void free_statement(GPStatement *stmt);
void free_condition(GPCondExp *cond);
void free_atomic_exp(GPAtomicExp *atom);
void free_procedure(GPProcedure *proc);
void free_rule(GPRule *rule);
void free_graph(GPGraph *graph);
void free_node(GPNode *node);
void free_edge(GPEdge *edge);
void free_label(GPLabel *label);

#endif /* INC_AST_H */

