/* /////////////////////////////////////////////////////////////////////////////////////// */

/*                                     gpparser.h                               
 * 
 * This is a header file for the GP2 parser. It contains an interface to the lexer, enumerated
 * type definitions, AST node definitions and prototypes for AST constructors.
 * 
 *
 * Created on 28/5/13 by Chris Bak */


/* /////////////////////////////////////////////////////////////////////////////////////// */

/* interface to the lexer. From gplexer.lex */
extern int yylineno;	
extern FILE* yyin;	
extern char *curfilename;	

/* enums used by the lexer for mark keywords */

typedef enum {RED, GREEN, BLUE, GREY, DASHED, NONE} mark_t; 

/* yay AST */

/* node types of independent AST nodes */

typedef enum {PROCEDURE_DECL RULE, NODE_PAIR, GRAPH, NODE, EDGE, POSITION} ast_node_t;

/* node types for AST nodes that form the backbone of a list */

typedef enum {GLOBAL_DECL, LOCAL_DECLS, COMMAND_SEQUENCE, RULES, LABEL, GP_LIST, INT_DECL, STRING_DECL, ATOM_DECL, LIST_DECL, VARIABLE, INTERFACE, NODES, EDGES} list_t;

typedef struct List {
  list_t listtype; 
  YYLTYPE location;  /* location of symbol in the source file */
  union {
    GPStatement *global_decl;
    GPStatement *local_decl; 
    GPStatement *command;
    symbol *rulename;
    struct List *vars; /* list of variables declared with a specific type in a rule */
    symbol *var; 	  
    GPNodePair *nodepair; /* pair of nodes specified in the interface of a rule */	   
    GPNode *node;    
    GPEdge *edge;
    mark_t mark; /* start of a GP2 list in the AST, points to first atom via *next and has mark as its attribute */
    GPAtomicExp *atom;
  } value;
  struct List *next;
} List;

List *addGlobalDecl (list_t decl_type; YYLTYPE location, GPStatement *decl, List *next);
List *addLocalDecl (YYLTYPE location, GPStatement *decl, List *next);
List *addCommand (YYLTYPE location, GPStatement *command, List *next);
List *addRule (YYLTYPE location, symbol *name, List *next);
List *addVariableDecl (list_t nodetype, YYLTYPE location, List *vars, List *next);
List *addVariable (YYLTYPE location, symbol *var, List *next);
List *addNodePair (YYLTYPE location, GPNodePair *nodepair, List *next);
List *newLabel (YYLTYPE location, mark_t mark, List *next);
List *addAtom (YYLTYPE location, GPAtomicExp *atom, List *next);
List *addNode (YYLTYPE location, GPNode *node, List *next);
List *addEdge (YYLTYPE location, GPEdge *edge, List *next);

typedef enum {MAIN_DECL, PROCEDURE_DECL, RULE_DECL} decl_t;

typedef struct GPGlobalDeclaration {
  decl_t decl_type;
  YYLTYPE location;
  union {
    List *main;
    struct { symbol *name; List *local_decls; List *com_seq; } proc;
    GPRule *rule;
  } value;
} GPGlobalDeclaration;

GPProcedure *newMain (YYLTYPE location, List *com_seq);
GPProcedure *newProcedure (YYLTYPE location, symbol *name, List *local_decls, List *com_seq);


typedef enum {RULE_CALL, RULE_SET_CALL, PROCEDURE_CALL, IF, TRY, ALAP, OR, SKIP, FAIL} stmt_t;

typedef struct GPStatement {
  stmt_t statementtype;
  YYLTYPE location;
  union {
    List *main;
    GPGlobalDeclaration *proc;
    GPGlobalDeclaration *rule;
    symbol *rulename;
    List *ruleset;
    symbol *procname;
    struct { struct GPStatement *cond; struct GPStatement *then_stmt; struct GPStatement *else_stmt; } condbranch;
    GPStatement *loop_stmt;
    struct { struct GPStatement *left_stmt; struct GPStatement *right_stmt; } or_stmt;
  } value;
} GPStatement;

GPStatement *newRuleCall(YYLTYPE location, symbol *rulename);
GPStatement *newRuleSetCall(YYLTYPE location, List *ruleset);
GPStatement *newProcCall(YYLTYPE location, symbol *procname);
GPStatement *newCondBranch(stmt_t stmt_type; YYLTYPE location, GPStatement *cond, GPStatement *then_stmt, GPStatement *else_stmt);
GPStatement *newAlap(YYLTYPE location, GPStatement *stmt);
GPStatement *newOrStmt(YYLTYPE location, GPStatement *left_stmt, GPStatement *right_stmt);
GPStatement *newSkip(YYLTYPE location);
GPStatement *newFail(YYLTYPE location);


typedef struct GPNodePair {
  ast_node_t nodetype; /* NODE_PAIR */
  YYLTYPE location;
  symbol *leftnode;
  symbol *rightnode;
} GPInterface;

GPNodePair *newNodePair (YYLTYPE location, symbol *leftnode, symbol *rightnode);

typedef struct GPNode {
  ast_node_t nodetype;	
  YYLTYPE location; 
  int root; /* 1 if node is a root, 0 otherwise */
  symbol *name; 
  List *label; 
  GPPos *position; 
} GPNode;

GPNode *newNode (YYLTYPE location, int root, symbol *name, List *label, GPPos *position);

typedef struct GPEdge {
  ast_node_t nodetype;	
  YYLTYPE location; 
  symbol *name; 
  symbol *source; 
  symbol *target; 
  List *label; 
} GPEdge;

GPEdge *newEdge (YYLTYPE location, symbol *name, symbol *source, symbol *target, List *label);

typedef struct GPGraph {
  ast_node_t nodetype;	
  YYLTYPE location;
  GPPos *gpposition;
  List *nodes;
  List *edges;
} GPGraph;

GPGraph *newGraph (YYLTYPE location, GPPos *position, List *nodes, List *edges);



/* AST node for atomic expressions in GP, corresponding to AtomExp production */

typedef enum {VARIABLE, INT_CONSTANT, STRING_CONSTANT, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH, NEGATIVE, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} atomexp_t;

typedef struct GPAtomicExp {
  atomexp_t nodetype;
  YYLTYPE location;
  union {
    symbol *var;
    int num;
    char *str;
    struct { symbol *node_id; } degree;
    struct { AST *list; } llength;
    struct GPAtomicExp *slength; 
    struct { struct GPAtomicExp *left; struct GPAtomicExp *right; } binOp;
  } value;
} GPAtomicExp;

GPAtomicExp *newVariable (YYLTYPE location, symbol *name);
GPAtomicExp *newNumber (YYLTYPE location, int num);
GPAtomicExp *newString (YYLTYPE location, char *str);
GPAtomicExp *newDegreeOp (atomexp_t nodetype, YYLTYPE location, symbol *node_id);
GPAtomicExp *newListLength (YYLTYPE location, AST *list);
GPAtomicExp *newStringLength (YYLTYPE location, GPAtomicExp *slength);
GPAtomicExp *newBinaryOp (atomexp_t nodetype, YYLTYPE location, GPAtomicExp *leftexp, GPAtomicExp *rightexp);


/* AST node for conditional expressions in GP, corresponding to Condition production */

typedef enum {INT_CHECK, STRING_CHECK, ATOM_CHECK, EDGE_PRED, NOT, OR, AND, EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL} condexp_t;

typedef struct GPCondExp {
  condexp_t nodetype;
  YYLTYPE location;
  union {
    symbol *var;
    struct { symbol *source; symbol *target; List *label; } edgePred;
    struct GPCondExp *notExp;
    struct { struct GPCondExp *left; struct GPCondExp *right; } binExp;
    struct { List *left; List *right; } relExp;  
  } value;
} GPCondExp;

GPCondExp *newSubtypePred (condexp_t nodetype, YYLTYPE location, symbol *var);
GPCondExp *newEdgePred (YYLTYPE location, symbol *source, symbol *target, List *label);
GPCondExp *newNotExp (YYLTYPE location, GPCondExp *exp);
GPCondExp *newBinaryExp (condexp_t nodetype, YYLTYPE location, GPCondExp *leftexp, GPCondExp *rightexp);
GPCondExp *newRelationalExp (condexp_t nodetype, YYLTYPE location, List *leftexp List *rightexp);

typedef struct GPPos {
  ast_node_t nodetype; 
  YYLTYPE location; 
  int x;
  int y;
} GPPos;

GPPos *newPosition (YYLTYPE location, int x, int y);

typedef struct GPRule {
  ast_node_t nodetype;
  YYLTYPE location;
  int injective; /* 1 for injective matching, 0 otherwise */
  symbol *name; 
  List *parameters;
  GPGraph *lhs;
  GPGraph *rhs;
  List *interface;
  GPCondExp *condition;  
} GPRule;

GPRule newRule(YYLTYPE location, int injective, symbol *name, List *parameters, GPGraph *lhs, GPGraph *rhs, List *interface, GPCondExp *condition);



/* ------------------------- */


typedef struct GPCondStmt {
  ast_node_t nodetype; /* IF_STMT, TRY_STMT */
  YYLTYPE location; 
  struct AST *condition;
  struct AST *then_branch;
  struct AST *else_Branch;
} GPCondStmt;

typedef struct GPMacroDecl {
  ast_node_t nodetype; /* MACRO_DECL */
  YYLTYPE location;
  symbol *name 
  struct AST *localmacro
  struct AST *localrule
  struct AST *comseq
} GPMacroDecl;

typedef struct GPAlap {
  ast_node_t nodetype; /* ALAP */
  YYLTYPE location;  
  struct AST *comseq;
} GPAlap;

typedef struct GPChoice {
  ast_node_t nodetype; /* RULE_CHOICE */
  YYLTYPE location;  
  struct AST *first_comseq;
  struct AST *second_comseq;
} GPChoice;






typedef struct GPMark {
  ast_node_t nodetype; /* MARK_CONST */
  YYLTYPE location; 
  mark_t val;
} GPMark;

/*
typedef struct GPEdgePred {
  ast_node_t nodetype; /* EDGEPRED 
  YYLTYPE location; 
  struct AST *source;
  struct AST *target;
  struct AST *label;
} GPEdgePred;


typedef struct GPDegree {
  ast_node_t nodetype; /* INDEGREE, OUTDEGREE 
  YYLTYPE location; 
  symbol *node;
} GPDegree; 

typedef struct GPLength {
  ast_node_t nodetype; /* LIST_LENGTH, STRING_LENGTH 
  YYLTYPE location;
  struct AST *arg;
} 

typedef struct GPTypeCheck {
  ast_node_t nodetype; /* INT_CHECK, STRING_CHECK, ATOM_CHECK 
  YYLTYPE location; 
  symbol *var;
} GPTypeCheck;

typedef struct GPNumber {
  ast_node_t nodetype; /* INT_CONST 
  YYLTYPE location;  
  int val;
} GPNumber;

typedef struct GPString 
  ast_node_t nodetype; /* STRING_CONST 
  YYLTYPE location; 
  char *val;
} GPString; */

/* constructors */


AST *newCond (ast_node_t nodetype, YYLTYPE location, AST *condition, AST *then_branch, AST *else_branch);
AST *newMacroDecl (ast_node_t nodetype, YYLTYPE location, symbol *name, AST *localmacro, AST *localrule, AST* comseq);
AST *newAlap (ast_node_t nodetype, YYLTYPE location, AST *comseq);
AST *newChoice (ast_node_t nodetype, YYLTYPE location, AST *first_comseq, AST* second_comseq);
AST *newRuleDecl (ast_node_t nodetype, YYLTYPE location, symbol *name, int injective, AST *vars, AST *graphs, AST *interface, AST *condition);
AST *newVar (ast_node_t nodetype, YYLTYPE location, symbol *name);
AST *newGraph (ast_node_t nodetype, YYLTYPE location, AST *gpposition, AST *nodes, AST *edges);
AST *newNode (ast_node_t nodetype, YYLTYPE location, symbol *name, int root, AST *label, AST *gpposition);
AST *newEdge (ast_node_t nodetype, YYLTYPE location, symbol *name, AST *source, AST *target, AST *label);
AST *newEdgePred (ast_node_t nodetype, YYLTYPE location, AST *source, AST *target, AST *label);
AST *newDegree (ast_node_t nodetype, YYLTYPE location, symbol *name);
AST *newLength (ast_node_t nodetype, YYLTYPE location, AST *arg);
AST *newTypeCheck (ast_node_t nodetype, YYLTYPE location, symbol *var);
AST *newNumber (ast_node_t nodetype, YYLTYPE location, int val);
AST *newString (ast_node_t nodetype, YYLTYPE location, char *val);
AST *newMark (ast_node_t nodetype, YYLTYPE location, mark_t val);


  








