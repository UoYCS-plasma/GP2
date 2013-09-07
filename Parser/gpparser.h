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

typedef enum {RED, GREEN, BLUE, GREY, DASHED, NONE} mark_t; /* node/edge marks */

typedef enum {GP_INT, GP_STRING, GP_ATOM, GP_LIST} type_t; 

typedef enum {EQ, NEQ, GT, GTEQ, LT, LTEQ} rel_t; /* relational operators */

/* struct AST node types are as follows:
 * Program - The root of the AST. List of declarations. 
 * Main - Points to the command sequence following 'main ='. Only one such node allowed.
 * Sequence - The sequential composition operator ';'. The command sequence of the left
 *            branch precedes that of the right branch.
 * LocalMacro - Occurs within a macro declaration AST. Points to a MacroDecl node.
 * LocalRule - Occurs within a macro declaration AST. Points to a rule declaration AST
 *             and possibly to another LocalRule node.
 * RuleList - List of rules in a { } construct.
 * VarList - Points to symbol table entries of variables in the parameter declaration section of a rule.
 *           There may be AtomList, IntList etc. but this is unclear at the moment.            
 * Graphs - Points to the LHS graph and RHS graph ASTs
 * InterfaceList - List of pairs of nodes
 * NodePair - Points to symbol table of two nodes, one in the LHS and one in the RHS.
 * NodeList - List of nodes in a graph.
 * EdgeList - List of edges in a graph.
 * Boolean Operators - AND, OR, NOT
 * Relational Operators - =, NE, >, GE, <, LE
 * Label - Points to a mark and to a list of AtomExps
 * Cons - The list operator ':'
 * Concat - The string operator '.'
 * Arithmetic Operators - +, -, *, /  
 */

/* general structure for lists. Maybe rename to ListExp or something */

typedef enum {PROGRAM, MAIN, SEQUENCE, LOCAL_MACRO, LOCAL_RULE, RULE_LIST, VAR_LIST, RULE_GRAPHS, INTERFACE_LIST, NODE_PAIR, NODE_LIST, EDGE_LIST, IF_STMT, TRY_STMT, MACRO_DECL, ALAP, RULE_CHOICE, RULE_DECL, VARIABLE, GRAPH, POSITION, NODE, EDGE, GREATER, GREATER_EQUAL, EQUAL, NOT_EQUAL, LESS, LESS_EQUAL} astnode_t; /* AST node types */

typedef enum {LABEL, LIST} listnode_t;

typedef struct ListNode {
  listnode_t nodetype; 
  YYLTYPE position;  /* location of symbol in the source file */
  union {
    mark_t mark; /* root of a GP2 list in the AST, points to first atom via *next and has mark as its attribute */
    GPAtomicExp *atom;
    
    

  } value;
  struct ListNode *next;
} ListNode;

ListNode *newLabelHead (YYLTYPE position, mark_t mark, ListNode *next);
ListNode *newAtom (YYLTYPE position, GPAtomicExp *atom, ListNode *next);


/* AST node for atomic expressions in GP, corresponding to AtomExp production */

typedef enum {VARIABLE, INT_CONSTANT, STRING_CONSTANT, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH, NEGATIVE, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} atomexp_t;

typedef struct GPAtomicExp {
  atomexp_t nodetype;
  YYLTYPE position;
  union {
    symbol *var;
    int num;
    char *str;
    struct { symbol *node_id; } indeg;
    struct { symbol *node_id; } outdeg;
    struct { AST *list; } llength;
    struct GPAtomicExp *slength; 
    struct { struct GPAtomicExp *left; struct GPAtomicExp *right; } binOp;
  } value;
} GPAtomicExp;

GPAtomicExp *newVariable (YYLTYPE position, symbol *name);
GPAtomicExp *newNumber (YYLTYPE position, int num);
GPAtomicExp *newString (YYLTYPE position, char *str);
GPAtomicExp *newDegreeOp (atomexp_t nodetype, YYLTYPE position, symbol *node_id);
GPAtomicExp *newListLength (YYLTYPE position, AST *list);
GPAtomicExp *newStringLength (YYLTYPE position, GPAtomicExp *slength);
GPAtomicExp *newBinaryOp (atomexp_t nodetype; YYLTYPE position, GPAtomicExp *left, GPAtomicExp *right);


/* AST node for conditional expressions in GP, corresponding to Condition production */

typedef enum {SUBTYPE, EDGE_PRED, NOT, OR, AND, EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL} condexp_t;

typedef struct GPCondExp {
  condexp_t nodetype;
  YYLTYPE position;
  union {
    symbol *var;
    struct { symbol *source; symbol *target; AST *label; } edgePred;
    struct GPCondExp *notExp;
    struct { struct GPCondExp *left; struct GPCondExp *right; } binExp;
    struct { AST *left; AST *right; } relExp;  
  } value;
} GPCondExp;

GPCondExp *newSubtypePred (YYLTYPE position, symbol *var);
GPCondExp *newEdgePred (YYLTYPE position, symbol *source, symbol *target, AST *label);
GPCondExp *newNotExp (YYLTYPE position, GPCondExp *notExp);
GPCondExp *newBinaryExp (condexp_t nodetype; YYLTYPE position, GPCondExp *left, GPCondExp *right);
GPCondExp *newRelationalOp (condexp_t nodetype; YYLTYPE position, AST *left, AST *right);


typedef struct GPCondStmt {
  ast_node_t nodetype; /* IF_STMT, TRY_STMT */
  YYLTYPE position; 
  struct AST *condition;
  struct AST *then_branch;
  struct AST *else_Branch;
} GPCondStmt;

typedef struct GPMacroDecl {
  ast_node_t nodetype; /* MACRO_DECL */
  YYLTYPE position;
  symbol *name 
  struct AST *localmacro
  struct AST *localrule
  struct AST *comseq
} GPMacroDecl;

typedef struct GPAlap {
  ast_node_t nodetype; /* ALAP */
  YYLTYPE position;  
  struct AST *comseq;
} GPAlap;

typedef struct GPChoice {
  ast_node_t nodetype; /* RULE_CHOICE */
  YYLTYPE position;  
  struct AST *first_comseq;
  struct AST *second_comseq;
} GPChoice;

typedef struct GPRuleDecl {
  ast_node_t nodetype; /* RULE_DECL */
  YYLTYPE position;
  symbol *name 
  int injective; /* 0 for non-injective matching, 1 otherwise */
  struct AST *vars
  struct AST *graphs
  struct AST *interface
  struct AST *condition  
} GPRuleDecl;

typedef struct GPVar {
  ast_node_t nodetype; /* VARIABLE */
  YYLTYPE position;
  symbol *name;
}

typedef struct GPGraph {
  ast_node_t nodetype; /* GRAPH */
  YYLTYPE position
  struct AST *gpposition;
  struct AST *nodes;
  struct AST *edges;
} GPGraph;

typedef struct GPPos {
  ast_node_t nodetype; /* POSITION */
  YYLTYPE position; 
  int x;
  int y;
} GPPos;

typedef struct GPNode {
  ast_node_t nodetype; /* NODE */
  YYLTYPE position; 
  symbol *name; 
  int root; /* 0 if node is not a root, 1 otherwise */
  struct AST *label; 
  struct AST *gpposition; 
} GPNode;

typedef struct GPEdge {
  ast_node_t nodetype; /* EDGE */
  YYLTYPE position;
  symbol *name; 
  struct AST *source; 
  struct AST *target; 
  struct AST *label; 
} GPEdge;

typedef struct GPMark {
  ast_node_t nodetype; /* MARK_CONST */
  YYLTYPE position; 
  mark_t val;
} GPMark;

/*
typedef struct GPEdgePred {
  ast_node_t nodetype; /* EDGEPRED 
  YYLTYPE position; 
  struct AST *source;
  struct AST *target;
  struct AST *label;
} GPEdgePred;


typedef struct GPDegree {
  ast_node_t nodetype; /* INDEGREE, OUTDEGREE 
  YYLTYPE position; 
  symbol *node;
} GPDegree; 

typedef struct GPLength {
  ast_node_t nodetype; /* LIST_LENGTH, STRING_LENGTH 
  YYLTYPE position;
  struct AST *arg;
} 

typedef struct GPTypeCheck {
  ast_node_t nodetype; /* INT_CHECK, STRING_CHECK, ATOM_CHECK 
  YYLTYPE position; 
  symbol *var;
} GPTypeCheck;

typedef struct GPNumber {
  ast_node_t nodetype; /* INT_CONST 
  YYLTYPE position;  
  int val;
} GPNumber;

typedef struct GPString 
  ast_node_t nodetype; /* STRING_CONST 
  YYLTYPE position; 
  char *val;
} GPString; */

/* constructors */


AST *newCond (ast_node_t nodetype, YYLTYPE position, AST *condition, AST *then_branch, AST *else_branch);
AST *newMacroDecl (ast_node_t nodetype, YYLTYPE position, symbol *name, AST *localmacro, AST *localrule, AST* comseq);
AST *newAlap (ast_node_t nodetype, YYLTYPE position, AST *comseq);
AST *newChoice (ast_node_t nodetype, YYLTYPE position, AST *first_comseq, AST* second_comseq);
AST *newRuleDecl (ast_node_t nodetype, YYLTYPE position, symbol *name, int injective, AST *vars, AST *graphs, AST *interface, AST *condition);
AST *newVar (ast_node_t nodetype, YYLTYPE position, symbol *name);
AST *newGraph (ast_node_t nodetype, YYLTYPE position, AST *gpposition, AST *nodes, AST *edges);
AST *newPos (ast_node_t nodetype, YYLTYPE position, int x, int y);
AST *newNode (ast_node_t nodetype, YYLTYPE position, symbol *name, int root, AST *label, AST *gpposition);
AST *newEdge (ast_node_t nodetype, YYLTYPE position, symbol *name, AST *source, AST *target, AST *label);
AST *newEdgePred (ast_node_t nodetype, YYLTYPE position, AST *source, AST *target, AST *label);
AST *newDegree (ast_node_t nodetype, YYLTYPE position, symbol *name);
AST *newLength (ast_node_t nodetype, YYLTYPE position, AST *arg);
AST *newTypeCheck (ast_node_t nodetype, YYLTYPE position, symbol *var);
AST *newNumber (ast_node_t nodetype, YYLTYPE position, int val);
AST *newString (ast_node_t nodetype, YYLTYPE position, char *val);
AST *newMark (ast_node_t nodetype, YYLTYPE position, mark_t val);


  








