/* /////////////////////////////////////////////////////////////////////////////////////// */

/*                                     gpparser.h
 *                                     Version 1.0
 * 
 * This is a header file for the GP2 parser. It contains an interface to the lexer and
 * some other stuff which I will identify later.
 *
 * Created on 28/5/13 by Chris Bak */


/* /////////////////////////////////////////////////////////////////////////////////////// */

/* interface to the lexer */
extern int yylineno;	
extern FILE* yyin;	
extern char *curfilename;	/* all from gplexer.lex */

/* AST interface. Currently incorrect. Requires a re-think. You must design your node types
 * based on how many nodes they point to and the types of those nodes. Make it as general
 * as possible. */

struct AST {
  int nodetype; /* ComSeq, RuleSetCall, ParamList, NodeList, EdgeList, LabelList
		   AtomExp, AND, OR, RelOps, ArithOps */
  YYLTYPE position;  /* Location of symbol in the source file */
  struct AST *left; 
  struct AST *right; /* points to the next construct in the list or sequence */
}

struct GPCondBranch {
  int nodetype; /* IfStmt, TryStmt */
  YYLTYPE position; 
  struct AST *condition;
  struct AST *then_branch;
  struct AST *else_Branch;
}

struct GPNumber {
  int nodetype; /* Integer constant */
  YYLTYPE position;  
  int value;
}

struct GPString {
  int nodetype; /* String constant */
  YYLTYPE position; 
  char *value;
}

struct GPMark {
  int nodetype; /* Mark */
  YYLTYPE position; 
  gp_mark value; /* enum type of all the possible marks in GP */
}

struct GPDegree {
  int nodetype; /* Indegree or Outdegree */
  YYLTYPE position; 
  struct GPNode *argument;
}

struct GPEdgePred {
  int nodetype; /* EdgePred */
  YYLTYPE position; 
  struct GPNode *source;
  struct GPNode *target;
  struct AST* label;
}

struct GPVarExp {
  int nodetype; /* Subtype Predicate or Parameter declaration */
  YYLTYPE position;
  char *name; /* pointer to variable identifier in symbol table */ 
  gp_var value; /* enum type of GP types. In symbol table. */
}

struct GPPos {
  int nodetype; /* Position */
  YYLTYPE position; 
  int XPos;
  int YPos;
}

struct GPNode {
  int nodetype; /* Node */
  YYLTYPE position; 
  symbol *name; /* pointer to identifier entry in symbol table */
  int root; /* 0 if node is not a root, 1 otherwise */
  struct AST *label; /* probably belongs in the symbol table */
  struct GPPos *position; /* see above */
}

struct GPEdge {
  int nodetype; /* Edge */
  YYLTYPE position;
  symbol *name; /* pointer to edge identifier in symbol table */
  /* below three fields should probably be in the symbol table entry */
  struct GPNode *source; /* pointer to source node in symbol table */
  struct GPNode *target; 
  struct AST* label; 
}

struct GPGraph {
  int nodetype; /* Graph */
  YYLTYPE position;
  
  








