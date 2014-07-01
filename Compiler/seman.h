/* ////////////////////////////////////////////////////////////////////////////

  ================================
  seman.h - Chris Bak (24/10/2013)      
  ================================                        
 
  Contains the interface for the symbol table and the declarations of
  the semantic analysis functions.

//////////////////////////////////////////////////////////////////////////// */

#ifndef INC_SEMAN_H
#define INC_SEMAN_H

#include "ast.h" /* struct List */
#include <glib.h>
#include <stdbool.h>
#include <stdlib.h> /* malloc, free */
#include <string.h> /* strdup, strcmp */
#include <stdio.h> /* fprintf */


/* GP2 symbols are stored in struct Symbol. These are values of the
 * symbol table. The symbol's identifier, namely the identifier in GP2,
 * is the symbol's hash key.
 *
 * GP2's symbols are as follows:
 * - Procedures
 * - Rules
 * - Variables
 * - Nodes
 * - Edges
 *
 * The Symbol structure contains the following:
 *
 * Type: A variable symbol's type is the variable's GP2 type according to its 
 *       declaration in a rule. The type of a node or edge is determined by 
 *       which side of the rule it occurs in. This is required for semantic
 *       analysis.
 *
 * Scope: The procedure in which the symbol is visible. Either "Global" or the
 *        name of a procedure in the program. All symbols have a scope.
 *
 * Containing Rule: The rule in which the symbol is visible. NULL for symbols
 *                  for symbols of type 'rule' and 'procedure'. This field is
 *                  used to uniquely identify variables, nodes and edges as 
 *                  they can have the same name in different rules.
 * 
 * Flags: is_var is set to true if the symbol represents a GP variable. This
 *        is for more concise code: better to compare to a single bool than to
 *        compare with each of the individual variable types.
 *
 *        in_lhs is set to true if the symbol has type variable and occurs in
 *        the LHS of a rule. Set to false in all other cases. 
 * 
 *        wildcard is set to true if the symbol is a node or edge with the cyan
 *        mark. Set to false in all other cases.
 *
 *        bidirectional is set to true if the symbol is a bidirectional edge.
 *        Set to false in all other cases.
 */
 
typedef enum {PROCEDURE_S=0, RULE_S, INT_S, CHAR_S, STRING_S, ATOM_S,
               LIST_S, LEFT_NODE_S, RIGHT_NODE_S, LEFT_EDGE_S, RIGHT_EDGE_S} 
               SymbolType;

typedef struct Symbol {
  SymbolType type;
  string scope; 
  string containing_rule; 
  bool is_var;
  bool in_lhs; 
  bool wildcard; 
  bool bidirectional;
} Symbol;


/* BiEdge is used to store the necessary information for semantic checking of
 * bidirectional edges. We need to check for parallel bidirectional edges,
 * which requires the scope, the type of the graph (LHS or RHS) and the
 * source and target IDs of the edge's incident nodes. All non-parallel
 * bidirectional edges in a rule are stored in a linked list of BiEdges.
 */

typedef struct BiEdge {
  string scope;
  string containing_rule;
  char graph;
  string source;
  string target;
} BiEdge;

typedef struct BiEdgeList {
  struct BiEdge value;
  struct BiEdgeList *next;
} BiEdgeList;

/* Always points to the start of BiEdgeList. Defined in seman.c */
extern BiEdgeList *bidirectional_edges;

extern bool abort_compilation; /* Defined in main.c */

/* GLib's hashtable data structure is used to implement GP2's symbol table.
 * GP2 identifiers are the keys. The values are lists of struct Symbols,
 * defined below. I further take advantage of GLib by using GLib's 
 * singly-linked lists (GSList).
 *
 * The following glib function calls are used extensively in the function
 * definitions.
 *
 * ===============================================
 * GSList *list = g_hash_table_lookup(table, key);
 * ===============================================
 *
 * Creates a pointer to a GSList by calling g_hash_table_lookup. This function 
 * looks up a name in the symbol table and returns a pointer to the identifier
 * list if the name is already present, otherwise it returns NULL. 
 * Note that g_hash_table_lookup returns a void pointer.
 *
 * =====================================
 * list = g_slist_prepend(list, symbol);     
 * =====================================    
 *
 * Adds the symbol given by the second argument to the start of the GSList
 * list. If list is NULL then a new list is created with the single element
 * symbol.
 *
 *
 * ======================================
 * g_hash_table_insert(table, key, list); 
 * ======================================
 *
 * Inserts the GSList list to the symbol table. If the key already exists 
 * then the old value for that key is freed with <value_destroy_func> and 
 * replaced with the new value. This means that only one symbol list will 
 * exist for a particular key.
 *
 *
 * These three function calls are made in succession to ensure that the
 * symbol list for a particular identifier name always contains all the 
 * objects with that name.
 */

extern GHashTable *gp_symbol_table; /* Defined in main.c */



/* Function to reverse a sequence of List nodes in the AST. Given a
 * pointer to a List node representing the head of an AST list, it
 * reverses the list and returns a pointer to the new head. 
 *
 * This function is required as Bison generates lists in reverse order
 * due to left-recursive grammar rules. 
 */

struct List *reverse (struct List * listHead);



/* The host graph AST contains lists that need to be reversed.
 * First the node list and the edge list are reversed, then the function
 * iteratively steps through each node/edge list, reversing all the lists in 
 * the node and edge labels. 
 */

void reverseGraphAST (GPGraph *graph); 



/* glib requires the user to provide functions to free hash table values.
 * freeSymbolList frees a list of struct Symbols. This function is passed
 * to the glib through g_hash_table_new_full, called in main.c. 
 * freeSymbolList is called by glib's hashtable functions under the hood.
 */

void freeSymbolList(gpointer key, gpointer value, gpointer data);



/* declarationScan traverses the global declaration list and any local 
 * declaration lists. It adds all procedure declarations and rule declarations
 * to the symbol table. 
 *
 * Argument 1: The root of the AST which is the head of the global declaration
 *             list.
 * Argument 2: The global symbol table.
 * Argument 3: The scope of the declaration list the function is traversing.
 *             This is either "Main" (initial value) or a procedure name.
 *
 * declarationScan performs semantic checking. It searches for name clashes:
 * two procedures declared with the same name, or two rules declared with the
 * same name in the same scope. If a name clash is found, the function reports
 * error and returns true. This informs main.c to terminate semantic checking. 
 * It also checks that there is exactly one Main declaration.
 */

bool declarationScan(List * ast, GHashTable *table, string const scope);



/* semanticCheck performs semantic analysis on a GP program after parsing. 
 * It uses the function reverse to reverse lists in the AST.
 *
 * Argument 1: A pointer to the abstract syntax tree of the input 
 *             program.
 * Argument 2: The symbol table. When called for the first time, it contains
 *             only rule and procedure identifiers added by declarationScan.
 *             semanticCheck will added other symbols to the symbol table and 
 *             use them for semantic analysis. This argument is passed to all
 *             subfunctions.
 * Argument 3: The current scope. semanticCheck is initially called with 
 *             scope "Global". 
 *
 */

bool semanticCheck(List * declarations, GHashTable *table, string const scope);

/* Called by semanticCheck, to free the list of struct BiEdges. */

void freeBiEdgeList(BiEdgeList *edge_list); 



/* statementScan is called whenever a GPStatement node is reached in the AST.
 * Called only by semanticCheck and itself. It searches for rule and 
 * procedure calls, and checks them for semantic correctness
 * with the auxiliary function validateCall.
 *
 * Argument 1: A pointer to the GPStatement node.
 * Argument 2: The symbol table.
 * Argument 3: The current scope, passed from declarationScan.
 *
 */


void statementScan(GPStatement * const statement, GHashTable *table, 
                   string const scope);



/* validateCall searches the symbol list with key <name> for a symbol with
 * the same type whose scope is either Global or <scope>. Called by 
 * statementScan when a RULE_CALL or PROCEDURE_CALL AST node is reached.
 *
 * Argument 1: The name of the rule or procedure in question. Used to hash
 *             into the symbol table.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The type of call, either RULE_S or PROCEDURE_S. This argument
 *             is passed by statementScan.
 */

void validateCall(string const name, GHashTable *table, string const scope, 
                  SymbolType const type);



/* ruleScan processes a struct GPRule. First it reverses the rule's parameter
 * list and interface list. Then it iterates down the variable list and enters
 * each variable into the symbol table with the auxiliary function
 * enterVariables. Finally it processes the rest of the rule using some
 * subfunctions. 
 *
 * Argument 1: A pointer to the GPRule node.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 */

void ruleScan(GPRule * const rule, GHashTable *table, string const scope);



/* enterVariables adds variable declarations from a rule's parameter list
 * into the symbol table. It also checks that each variable name in the
 * parameter list is unique. Variable names are not added to the symbol
 * table if a clash is found. This function is called only by ruleScan.
 *
 * Argument 1: Variable type, passed from ruleScan. It is one of INT_S,
 *             CHAR_S, STRING_S, ATOM_S, LIST_S.
 * Argument 2: Pointer to the list of variables declared with a specific type
 *             in the AST.
 * Argument 3: The symbol table.
 * Argument 4: The current scope.
 * Argument 5: The current rule being processed. This extra information
 *             is required for variable symbols.
 */

void enterVariables(SymbolType const type, List * variables, GHashTable *table, 
		     string const scope, string const rule_name);



/* graphScan is responsible for adding nodes and edges to the symbol table.
 * It also performs some semantic analysis: source and target nodes of edges
 * must exist and the union of node IDs and edge IDs in the graph must not 
 * contain duplicates. This function is called only by ruleScan.
 *
 * Argument 1: A pointer to a struct GPGraph.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 * Argument 5: Either 'l' for the LHS graph or 'r' for the RHS graph.
 *
 */

void graphScan(GPGraph *graph, GHashTable *table, string const scope, 
                string const rule_name, char const side);



/* interfaceScan performs semantic checking on the interface list of a rule.
 * All nodes in the list are checked to see if they appear in both graphs of
 * the rule. This function is called only by ruleScan.
 *
 * Argument 1: Pointer to the head of an AST interface list.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 */

void interfaceScan(List * interface, GHashTable *table, string const scope,
                    string const rule_name);



/* conditionScan navigates a subtree of the AST with a GPCondExp node
 * as its root. It performs semantic checking on all possible types
 * of GP2 conditions, usually calling auxiliary functions. This function
 * is called only by ruleScan.
 *
 * Argument 1: Pointer to a struct GPCondExp.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 */   


void conditionScan(GPCondExp * const condition, GHashTable *table, string const scope,
                    string const rule_name);



/* gpListScan takes as input the head of a GP2 list expression in the AST.
 * It first reverses the list to place it in the correct order according
 * to the input file. Then the function traverses the list, calling 
 * atomicExpScan for each item in the list.
 *
 * The function removes unnecessary EMPTY_LIST nodes. EMPTY_LIST is
 * one of the types of struct GPAtomicExp. It is used only as a marker to
 * signify an empty list: a list with no proper GPAtomicExp nodes.
 * Explicitly, EMPTY_LIST should only appear as follows, where List is
 * the head of the list. 
 *			 List --[next]-->NULL
 *  		  	  |
 *  			[value]-->EMPTY_LIST
 * In other cases, an EMPTY_LIST node has no meaning, due to the following
 * GP2 law: empty:a = a = a:empty, where a is any atomic expression.
 *
 * Hence, if an EMPTY_LIST node occurs in the list but not in the above
 * scenario, they are removed by first redirecting pointers and then
 * freeing the memory.
 *
 * gpListScan uses atomicExpScan to check for semantic errors. In 
 * particular, expressions occurring in a LHS label must be simple.
 * An expression e is simple if:
 * (1) e contains no arithmetic operators.
 * (2) e contains at most one occurrence of a list variable.
 * (3) any string expression in e must contain at most one string variable.
 *
 * Three file-scope variables are used to record the number of list variables,
 * the number of string variables, and whether an arithmetic expression occurs
 * in any label in the LHS of a rule in order to control error reporting. These
 * variables are set by atomicExpScan; gpListScan reads the variables and
 * reports an error if necessary.
 *
 * Argument 1: Pointer to the head of a list in the AST, passed by reference.
 *             If called from graphScan or case EDGE_PRED in conditionScan,
 *             this argument is the address of the gp_list pointer in a
 *             struct GPLabel.
 *             If called from cases EQUAL or NOT_EQUAL in conditionScan,
 *             this argument is the address of a pointer in the struct list_cmp
 *             of a struct GPConditionExp.
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 * Argument 5: The location of the list in the program.
 *             Either [l]eft-hand graph, [r]ight-hand graph or [c]ondition.
 *             If called from graphScan, this is the 'side' parameter
 *             passed to the graphScan call. If called from conditionScan,
 *             this is 'c'.
 */

void gpListScan(List **gp_list, GHashTable *table, string const scope,
                  string const rule_name, char const location);



/* atomicExpScan checks variables and nodes in expressions to see if they 
 * have been declared in the rule. If the function is called with location
 * 'l', it checks for expressions that violate the simple list condition.
 * The function also performs type checking with the use of two flags to
 * designate when an expression should expect integer/string variables.
 * Specifically, the cases for arithmetic operators recursively call
 * atomicExpScan with int_exp (argument 6) set to true, while the cases for
 * SLENGTH and CONCAT recursively call atomicExpScan with string_exp
 * (argument 7) set to true. This function should never be called with both
 * of these arguments set to true.
 *
 * Argument 1: Pointer to a struct GPAtomicExp
 * Argument 2: The symbol table.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 * Argument 5: The location of the atomic expression in the program.
 *             Either [l]eft-hand graph, [r]ight-hand graph or [c]ondition.
 *             Passed from the caller.
 * Argument 6: If true, then the expression pointed to by atom_exp is an
 *             integer expression. Erros are reported if string expressions
 *             are encountered.
 * Argument 7: If true, then the expression pointed to by atom_exp is a
 *             string expression. Errors are reported if integer expressions 
 *             are encountered.
 */

void atomicExpScan(GPAtomicExp * const atom_exp, GHashTable *table, string const scope,
                    string const rule_name, char const location, bool const int_exp,
		    bool const string_exp);

#endif /* INC_SEMAN_H */
