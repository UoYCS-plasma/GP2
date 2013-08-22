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

typedef enum {EQ, NEQ, GT, GTE, LT, LTE} rel_t; /* relational operators *?

/* AST node types are as follows:
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
 * Arithmetic Operatprs - +, -, *, /  
 */

typedef struct AST {
  int nodetype; 
  YYLTYPE position;  /* Location of symbol in the source file */
  struct AST *left; 
  struct AST *right;
} AST;

typedef struct GPCond {
  int nodetype; /* IfStmt, TryStmt */
  YYLTYPE position; 
  struct AST *condition;
  struct AST *then_branch;
  struct AST *else_Branch;
} GPCond;

typedef struct GPMacroDecl {
  int nodetype; /* MacroDecl */
  YYLTYPE position;
  symbol *name /* name of macro */
  struct AST *localmacro
  struct AST *localrule
  struct AST *comseq
} GPMacroDecl;

typedef struct GPAlap {
  int nodetype; /* '!' */
  YYLTYPE position;  
  struct AST *comseq;
} GPAlap;

typedef struct GPChoice {
  int nodetype; /* RuleChoice */
  YYLTYPE position;  
  struct AST *first_comseq;
  struct AST *second_comseq;
} GPChoice;

typedef struct GPRuleDecl {
  int nodetype; /* RuleDecl */
  YYLTYPE position;
  symbol *name /* name of rule */
  int injective; /* 0 or 1 */
  struct AST *vars
  struct AST *graphs
  struct AST *interface
  struct AST *condition  
} GPRuleDecl;

typedef struct GPGraph {
  int nodetype; /* Graph */
  YYLTYPE position
  struct AST *gpposition;
  struct AST *nodes;
  struct AST *edges;
} GPGraph;

typedef struct GPPos {
  int nodetype; /* Position */
  YYLTYPE position; 
  int x;
  int y;
} GPPos;

typedef struct GPNode {
  int nodetype; /* Node */
  YYLTYPE position; 
  symbol *name; /* pointer to identifier entry in symbol table */
  int root; /* 0 if node is not a root, 1 otherwise */
  struct AST *label; /* probably belongs in the symbol table */
  struct AST *gpposition; 
} GPNode;

typedef struct GPEdge {
  int nodetype; /* Edge */
  YYLTYPE position;
  symbol *name; /* pointer to edge identifier in symbol table */
  /* below three fields should probably be in the symbol table entry */
  struct AST *source; /* pointer to source node in symbol table */
  struct AST *target; 
  struct AST *label; 
} GPEdge;

typedef struct GPEdgePred {
  int nodetype; /* EdgePred */
  YYLTYPE position; 
  struct AST *source;
  struct AST *target;
  struct AST *label;
} GPEdgePred;

typedef struct GPDegree {
  int nodetype; /* Indegree or Outdegree */
  YYLTYPE position; 
  symbol *node;
} GPDegree;

typedef struct GPLength {
  int nodetype; /* List length or String length */
  YYLTYPE position;
  struct AST *arg;
}

typedef struct GPTypeCheck {
  int nodetype; /* Int, String or Atom */
  YYLTYPE position; 
  symbol *var;
} GPTypeCheck;

/* an idea is to merge GP's constants into one node type with
a union value. */

typedef struct GPNumber {
  int nodetype; /* Integer constant */
  YYLTYPE position;  
  int val;
} GPNumber;

typedef struct GPString {
  int nodetype; /* String constant */
  YYLTYPE position; 
  char *val;
} GPString;

typedef struct GPMark {
  int nodetype; /* Mark */
  YYLTYPE position; 
  mark_t val; /* enum type of all the possible marks in GP */
} GPMark;

/* constructors */

AST *newAST (int nodetype, YYLTYPE pos, AST *left, AST* right);
AST *newCond (int nodetype, YYLTYPE pos, AST *condition, AST *then_branch, AST *else_branch);
AST *newMacroDecl (int nodetype, YYLTYPE pos, symbol *name, AST *localmacro, AST *localrule, AST* comseq);
AST *newAlap (int nodetype, YYLTYPE pos, AST *comseq);
AST *newChoice (int nodetype, YYLTYPE pos, AST *first_comseq, AST* second_comseq);
AST *newRuleDecl (int nodetype, YYLTYPE pos, symbol *name, int injective, AST *vars, AST *graphs, AST *interface, AST *condition);
AST *newGraph (int nodetype, YYLTYPE pos, AST *gpposition, AST *nodes, AST *edges);
AST *newPos (int nodetype, YYLTYPE pos, int x, int y);
AST *newNode (int nodetype, YYLTYPE pos, symbol *name, AST *label, AST *gpposition);
AST *newEdge (int nodetype, YYLTYPE pos, symbol *name, AST *source, AST *target, AST *label);
AST *newEdgePred (int nodetype, YYLTYPE pos, AST *source, AST *target, AST *label);
AST *newTypeCheck (int nodetype, YYLTYPE pos, symbol *var);
AST *newNumber (int nodetype, YYLTYPE pos, int val);
AST *newString (int nodetype, YYLTYPE pos, char *val);
AST *newMark (int nodetype, YYLTYPE pos, mark_t val);


  








