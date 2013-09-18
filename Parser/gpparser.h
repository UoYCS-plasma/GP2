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

typedef enum {RED, GREEN, BLUE, GREY, DASHED} mark_t; /* node/edge marks */

typedef enum {GP_INT, GP_STRING, GP_ATOM, GP_LIST} type_t; 

typedef enum {EQ, NEQ, GT, GTE, LT, LTE} rel_t; /* relational operators */

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

typedef enum {PROGRAM, MAIN, SEQUENCE, LOCAL_MACRO, LOCAL_RULE, RULE_LIST, VAR_LIST, RULE_GRAPHS, INTERFACE_LIST, NODE_PAIR, NODE_LIST, EDGE_LIST, BOOL_OP, REL_OP, LABEL, CONS, CONCAT, ARITH_OP,
IF_STMT, TRY_STMT, MACRO_DECL, ALAP, RULE_CHOICE, RULE_DECL, VARIABLE, GRAPH, POSITION, NODE, EDGE, EDGE_PRED, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH, INT_CHECK, STR_CHECK, ATOM_CHECK, INT_CONST, STRING_CONST, ATOM_CONST} ast_node_t; /* AST node types */

typedef struct AST {
  ast_node_t nodetype; 
  YYLTYPE position;  /* location of symbol in the source file */
  struct AST *left; 
  struct AST *right;
} AST;

typedef struct GPCond {
  ast_node_t nodetype; /* IF_STMT, TRY_STMT */
  YYLTYPE position; 
  struct AST *condition;
  struct AST *then_branch;
  struct AST *else_Branch;
} GPCond;

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

typedef struct GPEdgePred {
  ast_node_t nodetype; /* EDGEPRED */
  YYLTYPE position; 
  struct AST *source;
  struct AST *target;
  struct AST *label;
} GPEdgePred;

typedef struct GPDegree {
  ast_node_t nodetype; /* INDEGREE, OUTDEGREE */
  YYLTYPE position; 
  symbol *node;
} GPDegree;

typedef struct GPLength {
  ast_node_t nodetype; /* LIST_LENGTH, STRING_LENGTH */
  YYLTYPE position;
  struct AST *arg;
}

typedef struct GPTypeCheck {
  ast_node_t nodetype; /* INT_CHECK, STRING_CHECK, ATOM_CHECK */
  YYLTYPE position; 
  symbol *var;
} GPTypeCheck;

typedef struct GPNumber {
  ast_node_t nodetype; /* INT_CONST */
  YYLTYPE position;  
  int val;
} GPNumber;

typedef struct GPString {
  ast_node_t nodetype; /* STRING_CONST */
  YYLTYPE position; 
  char *val;
} GPString;

typedef struct GPMark {
  ast_node_t nodetype; /* MARK_CONST */
  YYLTYPE position; 
  mark_t val;
} GPMark;

/* constructors */

AST *newAST (ast_node_t nodetype, YYLTYPE position, AST *left, AST* right);
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


  








