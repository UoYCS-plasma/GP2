/* ////////////////////////////////////////////////////////////////////////////

  Copyright 2015-2016 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>.

  ========================
  Semantic Analysis Module  
  ========================                        
 
  Contains the definitions and descriptions of the semantic analysis functions.

//////////////////////////////////////////////////////////////////////////// */

#ifndef INC_SEMAN_H
#define INC_SEMAN_H

#include "ast.h"
#include "common.h"
#include "pretty.h"
#include "symbol.h"

#include <glib.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

/* GLib's hashtable data structure is used to implement GP2's symbol table.
 * GP2 identifiers are the keys. The values are lists of struct Symbols,
 * defined in the symbol module. 
 *
 * The following glib function calls are used extensively in the source file. 
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
 * =======================================
 * g_hash_table_replace(table, key, list); 
 * =======================================
 *
 * Inserts the GSList list to the symbol table. If the key already exists 
 * then the old value for that key is freed with <value_destroy_func> and 
 * replaced with the new value. This means that only one symbol list will 
 * exist for a particular key.
 *
 * These three function calls are made in succession to ensure that the
 * symbol list for a particular identifier name always contains all the 
 * objects with that name.
 */
extern GHashTable *symbol_table; 

/* Global pointer to the head of the bidirectional edge list. */
extern BiEdgeList *bidirectional_edges;

/* The top level semantic analysis function. Creates the symbol table and
 * calls declarationScan and semanticCheck with their initial arguments
 * (gp_program and "Main"). If debug is set to true, the symbol table is
 * printed to <program_name_1.dot> before function exit. */
bool analyseProgram(List *gp_program, bool debug, string program_name);

/* declarationScan traverses the global declaration list and any local 
 * declaration lists. It adds all rule declarations to the symbol table.
 * It reverses the command sequence lists of procedure declarations (including
 * Main).
 * Returns true if a name conflict is found, such as two procedures with the
 * same name, or if there is not exactly one Main declaration. 
 *
 * Argument 1: The root of the AST which is the head of the global declaration
 *             list.
 * Argument 2: The scope of the declaration list the function is traversing.
 *             This is either "Main" (initial value) or a procedure name. */
bool declarationScan(List *ast, string scope);

/* Returns the string <scope>_<rule_name>. These strings are used to uniquely
 * identify each rule since rules with the same name may occur in different
 * scopes. */
string makeRuleIdentifier(string rule_name, string scope);

/* semanticCheck performs semantic analysis on a GP program after parsing. 
 * Called after declarationScan because rule and procedure symbols in the
 * symbol table are used to validate rule and procedure calls in the program
 * text.
 *
 * Argument 1: A pointer to the abstract syntax tree of the input program.
 * Argument 2: The current scope. semanticCheck is initially called with "Main". */
bool semanticCheck(List *declarations, string scope);

/* commandScan is called whenever a GPCommand node is reached in the AST.
 * Called only by semanticCheck and itself. It searches for rule and procedure
 * calls and checks them for semantic correctness by searching for their 
 * declarations in the appropriate scopes. It also checks that each break
 * statement occurs in a loop body. 
 *
 * Argument 1: A pointer to the GPCommand node.
 * Argument 2: The current scope, passed from declarationScan. 
 * Argument 3: The main declaration list. Passed to findRuleDeclaration
 *             and findProcedureDeclaration when a rule call or procedure
 *             call is encountered.
 * Argument 4: Flag set to true if scanning is taking place in a loop body. */
void commandScan(GPCommand *command, string scope, List *declarations, bool in_loop);

/* findRuleDeclaration searches for a GPRule AST node corresponding to the
 * passed name. It starts the search in the local declaration list of a
 * procedure passed by the caller. If the procedure is NULL, or if an
 * appropriate node does not exist in that procedure's declaration list,
 * search resumes in the global declaration list. 
 *
 * Argument 1: The global declaration list.
 * Argument 2: The name of the rule to find.
 * Argument 3: The procedure in which to start search, or NULL if search
 *             should only be conducted in the global declaration list. */
GPRule *findRuleDeclaration(List *declarations, string name, 
                            GPProcedure *procedure);

/* findRuleDeclaration searches for a GPProcedure AST node corresponding to the
 * passed name. It starts search from the global declaration list and 
 * recursively searches declaration lists of procedures. If a procedure node
 * equalling the third (optional) argument is found, it is ignored and search
 * resumed. This feature is used when checking for occurrences of more than
 * one procedure declaration with the same name.
 *
 * Argument 1: The global declaration list.
 * Argument 2: The name of the procedure to find.
 * Argument 3: A procedure to ignore if it is found in the search. */
GPProcedure *findProcedureDeclaration(List *declarations, string name,
                                      GPProcedure *excluded_procedure);

/* ruleScan processes a struct GPRule. First it reverses the rule's parameter
 * list and interface list. Then it iterates over the variable list and enters
 * each variable into the symbol table with the auxiliary functions 
 * checkDeclarations and enterVariables. Finally, it processes the rest of the
 * rule using various subfunctions. 
 *
 * Argument 1: A pointer to the GPRule node.
 * Argument 2: The current scope. */
void ruleScan(GPRule *rule, string scope);
void checkDeclaration(GPRule *rule, List *variables, string scope,
                      SymbolType type, int count, string type_name);

/* enterVariables adds variable declarations from a rule's parameter list
 * into the symbol table. It also checks that each variable name in the
 * parameter list is unique. Variable names are not added to the symbol
 * table if a clash is found. This function is called only by ruleScan.
 * Returns the number of variables processed by the function. The return
 * value is used to update the rule's variable count.
 *
 * Argument 1: Variable type, passed from ruleScan. It is one of INT_S,
 *             CHAR_S, STRING_S, ATOM_S, LIST_S.
 * Argument 2: Pointer to the list of variables declared with a specific type
 *             in the AST.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed. This extra information
 *             is required for variable symbols. */
int enterVariables(SymbolType type, List *variables, string scope,
                   string rule_name);

/* graphScan is responsible for adding nodes and edges to the symbol table.
 * It performs some semantic analysis: source and target nodes of edges
 * must exist and the union of node IDs and edge IDs in the graph must not 
 * contain duplicates. It also updates the rule's left nodes and left edges
 * count. This function is called only by ruleScan.
 *
 * Argument 1: A pointer to a struct GPRule.
 * Argument 2: The rule's interface, passed to gpListScan.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 * Argument 5: Either 'l' for the LHS graph or 'r' for the RHS graph. */
void graphScan(GPRule *rule, List *interface, string scope, string rule_name,
               char side);

/* interfaceScan performs semantic checking on the interface list of a rule.
 * All nodes in the list are checked to see if they appear in both graphs of
 * the rule. The function also reports a warning if any node appears more than
 * once in the list. Repeated nodes are not removed from the AST interface
 * list. This function is called only by ruleScan.
 *
 * Argument 1: Pointer to the head of an AST interface list.
 * Argument 2: The current scope.
 * Argument 3: The current rule being processed. */
void interfaceScan(List *interface, string scope, string rule_name);


/* conditionScan navigates a subtree of the AST with a GPCondition node
 * as its root. It performs semantic checking on all possible types
 * of GP2 conditions, sometimes calling auxiliary functions. This function
 * is called only by ruleScan. It returns the number of predicates in
 * the condition.
 *
 * Argument 1: Pointer to a struct GPCondition.
 * Argument 2: The rule's interface, used to check node IDs in the edge
 *             predicate.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed. */   
int conditionScan(GPCondition *condition, List *interface, string scope, 
                  string rule_name);


/* gpListScan takes as input the head of a GP2 list expression in the AST.
 * It reverses the list and calls atomicExpScan on each element to check 
 * for semantic errors. In particular, expressions occurring in a LHS label 
 * must be simple. An expression e is simple if:
 * (1) e contains no arithmetic operators.
 * (2) e contains at most one occurrence of a list variable.
 * (3) any string expression in e must contain at most one string variable.
 *
 * Three file-scope variables are used to record the number of list variables,
 * the number of string variables, and whether an arithmetic expression occurs
 * in any label in the LHS of a rule in order to control error reporting. These
 * variables are set by the calls to atomicExpScan. gpListScan reads their
 * values and reports an error if necessary.
 *
 * Argument 1: Pointer to the head of a list in the AST, passed by reference.
 *             If called from graphScan or case EDGE_PRED in conditionScan,
 *             this argument is the address of the gp_list pointer in a
 *             struct GPLabel.
 *             If called from cases EQUAL or NOT_EQUAL in conditionScan,
 *             this argument is the address of a pointer in the struct list_cmp
 *             of a struct GPConditionExp.
 * Argument 2: The rule's interface, passed to atomScan.
 * Argument 3: The current scope.
 * Argument 4: The current rule being processed.
 * Argument 5: The location of the list in the program.
 *             Either [l]eft-hand graph, [r]ight-hand graph or [c]ondition.
 *             If called from graphScan, this is the 'side' parameter
 *             passed to the graphScan call. If called from conditionScan,
 *             this is 'c'. */
void gpListScan(List **gp_list, List *interface, string scope, string rule_name, 
                char location);

/* atomScan checks variables and nodes in expressions to see if they have
 * been declared in the rule. If the function is called with location 'l',
 * it checks for expressions that violate the simple list condition.
 * The function also performs type checking with the use of two flags to
 * designate when an expression should expect integer/string variables.
 *
 * Argument 1: Pointer to a struct GPAtom.
 * Argument 2: The rule's interface, used to check semantic correctness of 
 *             degree operators.
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
 *             are encountered. */
void atomScan(GPAtom *atom, List *interface, string scope, string rule_name,
              char location, bool int_exp, bool string_exp);

/* Called by atomScan when it encounters a variable or a length operator. 
 * variableScan updates the AST node of variables with their type, obtained
 * from the symbol table, and performs semantic checking on the variables. 
 * The arguments are the same as those passed to atomScan. */
void variableScan(GPAtom *atom, string scope, string rule_name, 
                  char location, bool int_exp, bool string_exp, bool length);

#endif /* INC_SEMAN_H */
