/* ///////////////////////////////////////////////////////////////////////////

  ==============================
  ast.h - Chris Bak (28/05/2013)
  ==============================
                             
  Module for creating and processing GP2's abstract syntax tree.  

  Contains enum type definitions, AST node definitions, prototypes for AST 
  constructors and prototypes for AST freeing functions.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_AST_H
#define INC_AST_H 

/* Wrappers for frequently occurring calls to fprintf. */

#define print_to_log(error_message, ...)                    \
  do { fprintf(log_file, error_message, ##__VA_ARGS__); }   \
  while(0)

#define print_to_console(error_message, ...)                \
  do { fprintf(stderr, error_message, ##__VA_ARGS__); }     \
  while(0) 

#include <stdio.h> /* FILE type */
#include <stdbool.h>
#include <stdlib.h> /* malloc */
#include <string.h> /* strdup */

typedef char* string;

/* Bison uses a global variable yylloc of type YYLTYPE to keep track of the 
 * locations of tokens and nonterminals. The scanner will set these values upon
 * reading each token. This is the standard YYLTYPE definition but I define it
 * here so it is seen by every file.
 */
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
extern string yytext; 


/* Declarations for functions and variables defined in gpparser.y */
int yyparse(void);
extern struct List *gp_program; 
extern int yydebug;

/* enum used by the parser for mark keywords */

typedef enum {RED=0, GREEN, BLUE, GREY, DASHED, CYAN, NONE} MarkType; 

/* The functions after each struct definition are AST node constructors. The
 * constructors are called from the Bison parser (gpparser.y) which provides 
 * the appropriate arguments from the semantic values and locations of symbols
 * in the rules it reduces. The functions assign the passed pointers to the
 * corresponding structure fields. 
 *
 * Strings, such as rule names and variable names, are dynamically allocated 
 * with strdup. This is because the pointer passed to the function is freed in 
 * gpparser.y immediately after the constructor call. Therefore a new allocation
 * is required to prevent a double free.
 */ 


/* Definition of AST nodes representing lists. */

typedef enum {GLOBAL_DECLARATIONS=0, LOCAL_DECLARATIONS, COMMANDS, 
              RULES, INT_DECLARATIONS, CHAR_DECLARATIONS, 
              STRING_DECLARATIONS, ATOM_DECLARATIONS,  
              LIST_DECLARATIONS, VARIABLE_LIST, INTERFACE_LIST, 
              NODE_LIST, EDGE_LIST, GP_LIST, EMPTY_LIST} ListType;

typedef struct List {
  int node_id;
  ListType list_type;  
  YYLTYPE location;  /* location of symbol in the source file */
  union {
    struct GPDeclaration *declaration; /* GLOBAL_DECLARATIONS, 
					* LOCAL_DECLARATIONS */
    struct GPStatement *command;       /* COMMANDS */
    string rule_name; 		       /* RULES */
    struct List *variables; 
     				       /* INT_DECLARATIONS, CHAR_DECLARATIONS,
                                        * STRING_DECLARATIONS, ATOM_DECLARATIONS */ 
    string variable_name; 	       /* VARIABLE_LIST */	  
    string node_id; 		       /* INTERFACE_LIST */	   
    struct GPNode *node; 	       /* NODE_LIST */   
    struct GPEdge *edge; 	       /* EDGE_LIST */   
    struct GPAtomicExp *atom;          /* GP_LIST */
  } value;
  struct List *next;
} List;

/* Constructors for struct List. */

List *addDecl (ListType list_type, YYLTYPE location, 
	struct GPDeclaration *declaration, struct List *next);
List *addCommand (YYLTYPE location, struct GPStatement *command, 
	struct List *next);
List *addRule (YYLTYPE location, string rule_name, struct List *next);
List *addVariableDecl (ListType list_type, YYLTYPE location, 
	struct List *variables, struct List *next);
List *addVariable (YYLTYPE location, string variable_name, struct List *next);
List *addNodeID (YYLTYPE location, string node_id, struct List *next);
List *addNode (YYLTYPE location, struct GPNode *node, struct List *next);
List *addEdge (YYLTYPE location, struct GPEdge *edge, struct List *next);
List *addAtom (YYLTYPE location, struct GPAtomicExp *atom, struct List *next);
List *addEmptyList (YYLTYPE location);


/* Definition of AST nodes representing declarations. */

typedef enum {MAIN_DECLARATION=0, PROCEDURE_DECLARATION, RULE_DECLARATION} DeclType;

typedef struct GPDeclaration {
  int node_id;
  DeclType decl_type;
  YYLTYPE location;
  union {
    struct GPStatement *main_program; 	/* MAIN_DECLARATION */
    struct GPProcedure *procedure; 	/* PROCEDURE_DECLARATION */
    struct GPRule *rule; 		/* RULE_DECLARATION */
  } value;
} GPDeclaration;

/* Constructors for struct GPDeclaration. */

GPDeclaration *newMainDecl (YYLTYPE location, struct GPStatement *main_program);
GPDeclaration *newProcedureDecl (YYLTYPE location, struct GPProcedure *procedure);
GPDeclaration *newRuleDecl (YYLTYPE location, struct GPRule *rule);


/* Definition of AST nodes representing GP program statements. */

typedef enum {COMMAND_SEQUENCE=0, RULE_CALL, RULE_SET_CALL, PROCEDURE_CALL, 
              IF_STATEMENT, TRY_STATEMENT, ALAP_STATEMENT, PROGRAM_OR, 
              SKIP_STATEMENT, FAIL_STATEMENT} StatementType;

typedef struct GPStatement {
  int node_id;
  StatementType statement_type;
  YYLTYPE location;
  union {    
    struct List *cmd_seq; 		/* COMMAND_SEQUENCE */
    string rule_name; 			/* RULE_CALL */
    struct List *rule_set; 		/* RULE_SET_CALL */
    string proc_name;			/* PROCEDURE_CALL */

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

/* Constructors for struct GPStatement. */

GPStatement *newCommandSequence(YYLTYPE location, struct List *cmd_seq);
GPStatement *newRuleCall(YYLTYPE location, string rule_name);
GPStatement *newRuleSetCall(YYLTYPE location, struct List *rule_set);
GPStatement *newProcCall(YYLTYPE location, string proc_name);
GPStatement *newCondBranch(StatementType statement_type, YYLTYPE location, 
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
	      BOOL_NOT, BOOL_OR, BOOL_AND } CondExpType;

typedef struct GPCondExp {
  int node_id;
  CondExpType exp_type;
  YYLTYPE location;
  union {
    string var; 		/* INT_CHECK, CHAR_CHECK, STRING_CHECK, 
				 * ATOM_CHECK */
    struct {
      string source; 
      string target; 
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

/* Constructors for struct GPCondExp. */

GPCondExp *newSubtypePred (CondExpType exp_type, YYLTYPE location, string var);
GPCondExp *newEdgePred (YYLTYPE location, string source, string target,
	    struct GPLabel *label);
GPCondExp *newListComparison (CondExpType exp_type, YYLTYPE location, 
	    struct List *left_list, struct List *right_list);
GPCondExp *newAtomComparison (CondExpType exp_type, YYLTYPE location,
	    struct GPAtomicExp *left_exp, struct GPAtomicExp *right_exp);
GPCondExp *newNotExp (YYLTYPE location, struct GPCondExp *not_exp);
GPCondExp *newBinaryExp (CondExpType exp_type, YYLTYPE location, 
	    struct GPCondExp *left_exp, struct GPCondExp *right_exp);


/* Definition of AST nodes representing integer or string expressions. */

typedef enum {VARIABLE=0, INT_CONSTANT, CHARACTER_CONSTANT,
              STRING_CONSTANT, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH,
              NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomExpType;

typedef struct GPAtomicExp {
  int node_id;
  AtomExpType exp_type;
  YYLTYPE location;
  union {
    string name;		  /* VARIABLE */
    int number; 	 	  /* INT_CONSTANT */
    string string;		  /* CHARACTER_CONSTANT, STRING_CONSTANT */
    string node_id; 		  /* INDEGREE, OUTDEGREE */
    struct List *list_arg; 	  /* LIST_LENGTH */
    struct GPAtomicExp *str_arg;  /* STRING_LENGTH */
    struct GPAtomicExp *exp; 	  /* NEG */
    struct { 
      struct GPAtomicExp *left_exp;
      struct GPAtomicExp *right_exp;
    } bin_op; 		   	 /* ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT */
  } value;
} GPAtomicExp;

/* Constructors for struct GPAtomicExp. */

GPAtomicExp *newVariable (YYLTYPE location, string name);
GPAtomicExp *newNumber (YYLTYPE location, int number);
GPAtomicExp *newCharacter (YYLTYPE location, string character);
GPAtomicExp *newString (YYLTYPE location, string string);
GPAtomicExp *newDegreeOp (AtomExpType exp_type, YYLTYPE location, string node_id);
GPAtomicExp *newListLength (YYLTYPE location, struct List *list_arg);
GPAtomicExp *newStringLength (YYLTYPE location, struct GPAtomicExp *str_arg);
GPAtomicExp *newNegExp (YYLTYPE location, struct GPAtomicExp *exp);
GPAtomicExp *newBinaryOp (AtomExpType exp_type, YYLTYPE location, 
	      struct GPAtomicExp *left_exp, struct GPAtomicExp *right_exp);


/* Definition of the remaining AST node types. */

typedef enum {PROCEDURE=0, RULE, NODE_PAIR, GRAPH, NODE, EDGE, POSITION, LABEL} ASTNodeType;

/* Root node for a procedure definition. */

typedef struct GPProcedure {
  int node_id;
  ASTNodeType node_type;
  YYLTYPE location;
  string name; 
  struct List *local_decls; 
  struct GPStatement *cmd_seq; 
} GPProcedure;

/* Constructs a struct GPProcedure. */

GPProcedure *newProcedure(YYLTYPE location, string name, struct List *local_decls, 
              struct GPStatement *cmd_seq);


/* Root node for a rule definition. */

typedef struct GPRule {
  int node_id;
  ASTNodeType node_type;
  YYLTYPE location;
  bool injective; /* Integer flag to mark whether the rule should be matched injectively or not. */
  string name; 
  struct List *variables;
  struct GPGraph *lhs;
  struct GPGraph *rhs;
  struct List *interface;
  struct GPCondExp *condition;  
} GPRule;

/* Constructs a struct GPRule. */

GPRule *newRule(YYLTYPE location, bool injective, string name, 
	 struct List *variables, struct GPGraph *lhs, struct GPGraph *rhs, 
	 struct List *interface, struct GPCondExp *condition);

/* Root node for a graph definition. This data structure is used for
 * graphs in rules and for the host graph. */

typedef struct GPGraph {
  int node_id;
  ASTNodeType node_type;	
  YYLTYPE location;
  struct GPPos *position;
  struct List *nodes;
  struct List *edges;
} GPGraph;

/* Constructs a struct GPGraph. */

GPGraph *newGraph (YYLTYPE location, struct GPPos *position, 
          struct List *nodes, struct List *edges);


typedef struct GPNode {
  int node_id;
  ASTNodeType node_type;	
  YYLTYPE location; 
  bool root;
  string name; 
  struct GPLabel *label; 
  struct GPPos *position; 
} GPNode;

/* Constructs a struct GPNode. */

GPNode *newNode (YYLTYPE location, bool root, string name, 
                 struct GPLabel *label, struct GPPos *position);


typedef struct GPEdge {
  int node_id;
  ASTNodeType node_type;	
  YYLTYPE location; 
  bool bidirectional; 
  string name; 
  string source; 
  string target; 
  struct GPLabel *label; 
} GPEdge;

/* Constructs a struct GPEdge. */

GPEdge *newEdge (YYLTYPE location, bool bidirectional, string name, 
                 string source, string target, struct GPLabel *label);


/* AST node for specifying locations in the graphical editor. */

typedef struct GPPos {
  int node_id;
  ASTNodeType node_type; 
  YYLTYPE location; 
  int x;
  int y;
} GPPos;

/* Constructs a struct GPPos. */

GPPos *newPosition (YYLTYPE location, int x, int y);


typedef struct GPLabel {
  int node_id;
  ASTNodeType node_type; 
  YYLTYPE location; 
  MarkType mark;
  struct List *gp_list;
} GPLabel;

/* Constructs a struct GPLabel */

GPLabel *newLabel (YYLTYPE location, MarkType mark, struct List *gp_list);

/* freeAST takes a pointer to the root of an AST and walks through the AST,
 * calling the other freeing functions depending on the subtrees it
 * encounters. */

void freeAST(List *ast);
void freeDeclaration(GPDeclaration *decl);
void freeStatement(GPStatement *stmt);
void freeCondition(GPCondExp *cond);
void freeAtomicExp(GPAtomicExp *atom);
void freeProcedure(GPProcedure *proc);
void freeRule(GPRule *rule);
void freeGraph(GPGraph *graph);
void freeNode(GPNode *node);
void freeEdge(GPEdge *edge);
void freeLabel(GPLabel *label);

#endif /* INC_AST_H */

