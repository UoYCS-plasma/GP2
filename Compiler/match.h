/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.h - Chris Bak (14/08/2014)
  ================================
                             
  Header file for the rule matching module. Defines the data structures for
  variables, conditions, rules and graph morphisms.
  
/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_MATCH_H
#define INC_MATCH_H

#include "globals.h"
#include "graph.h"
#include "rule.h"

/* Association list for mappings between rule nodes/edges and host nodes/edges. 
 * The integers refer to the item's indices in the pointer arrays of their 
 * respective graphs.
 
 * The boolean flag controls backtracking.
 * For a node mapping, the flag is true if we need to search for another match for
 * the rule node when backtracking. This is not necessary for nodes matched as
 * the source or target of an edge because there is only one candidate node in
 * the host graph.
 * For an edge, the flag is true if the edge was matched from its source node.
 * The flag is needed to determine which list of host graph edges to
 * interrogate for a match. 
 */
typedef struct GraphMapping {
   int rule_item;
   int host_item;
   bool flag;
   struct GraphMapping *next;
} GraphMapping;

/* Create a new mapping specified by the latter three arguments and prepend it
 * to the mapping given by the first argument. Returns a pointer to the start of
 * the new mapping.
 */
 
GraphMapping *addMapping(GraphMapping *mapping, int rule_item, int host_item, 
                         bool flag); 
                         
/* Deletes and frees the first element of the mapping. */        
                
GraphMapping *removeMapping(GraphMapping *mapping);

/* Standard association list lookup. These functions return -1 if the item
 * does not exist in the given mapping.
 */

int lookupFromRule(GraphMapping *mapping, int rule_item);
int lookupFromHost(GraphMapping *mapping, int host_item);

void freeMapping(GraphMapping *mapping);


/* Association list to represent variable-value mappings. */

typedef struct Assignment {
  string variable;
  GList *value;
  struct Assignment *next;
} Assignment;


/* Create a new assignment specified by the last two arguments and prepends it
 * to the assignment given by the first argument. Returns a pointer to the 
 * start of the new assignment.
 */
Assignment *addAssignment(Assignment *assignment, string name, GList *value);

/* Deletes and frees the first element of the assignment. */        
                
Assignment *removeAssignment(Assignment *assignment);

/* Given an assignment and the name of a variable, lookupValue returns the value
 * of the variable if it exists in the assignment. Otherwise it returns NULL.
 */
GList *lookupValue(Assignment *assignment, string name);

void freeAssignment(Assignment *assignment);


/* A graph morphism is a set of node-to-node mappings, a set of edge-to-edge
 * mappings and a variable-value assignment. */

typedef struct Morphism {
   GraphMapping *node_matches;
   GraphMapping *edge_matches;
   Assignment *assignment;
} Morphism;

void printMorphism(Morphism *morphism);
void freeMorphism(Morphism *morphism);
 
/* applyRule will call findMatch */
void applyRule (Rule *rule, Morphism *match, Graph *host);


/* Creates a heap copy of the passed ListElement. Used to pass fresh copies of
 * ListElements to the verifyVariable functions so that the ListElements in the
 * host graph labels are not accidentally freed.
 *
 * Currently only copies simple expressions i.e. no degree, length or 
 * arithmetic operators. */

ListElement *copyListElement(ListElement *atom);

/* labelMatch checks if two labels match. The rule label may contain variables,
 * so it must also perform variable-value assignments. It first compares the 
 * marks directly and then steps through the lists of both labels, comparing
 * corresponding elements with the compareAtoms function. 
 *
 * labelMatch returns false if the labels cannot be matched:
 * (1) The marks are not equal, except if the rule label's mark is cyan.
 * (2) Two corresponding elements of the lists do not match (compareAtoms)
 * (3) A list variable is assigned a value which clashes with a value already
 *     assigned to the same variable from a previous label match.
 * (4) The lists do not have an equal number of atomic values to pair up.
 *     This is somewhat complicated to test given the presence of list
 *     variables.
 *
 * If labelMatch fails it restores the assignment to the state it was in before
 * the function was called. This is monitored by a count of the number of
 * assignments made in this function's scope which is updated using the return
 * values of compareAtoms and verifyAtomVariable. The exception to this is
 * the function verifyStringVariable which undoes its own local assignment
 * changes if it fails. 
 *
 * Argument 1: The label of the item in the rule graph.
 * Argument 2: The (constant) label of the item in the host graph.
 * Argument 3: The list of variables from the rule.
 * Argument 4: The current assignment, passed by reference, as it may be 
 *             updated during label matching.  
 */

/* Global assignment, used by all the functions below this point and all functions
 * in the staticsearch module. */

extern Assignment *assignment;
 
bool labelMatch (Label rule_label, Label host_label, VariableList *variables);
			              
/* compareAtoms checks if two atomic expressions match. The expression from the
 * rule may contain variables so it must also perform variable-value assignments. 
 *
 * compareAtoms returns -1 if the expressions do not match:
 * (1) Two constant expressions are not equal.
 * (2) A variable is assigned a value which clashes with a value already
 *     assigned to the same variable from a previous label match.
 *
 * Otherwise, it returns the number of assignments made while evaluating the
 * atoms. This is 0 in the case that two constants were compared. A single
 * call to compareAtoms may add more than one variable assignment in the 
 * case of a concatenated string with multiple character variables.
 *
 * Argument 1: An atomic expression from a rule graph label.
 * Argument 2: An atomic expression from a host graph label.
 * Argument 3: The list of variables from the rule.
 */			              
			              
int compareAtoms(ListElement *rule_atom, ListElement *host_atom,
                VariableList *variables);

/* concatExpToList returns a list of struct StringExps. */

typedef enum {CONSTANT_S, VARIABLE_S} stringType;

typedef struct StringExp {
   stringType type;
   string value;
} StringExp;

void freeStringExp(gpointer data);

/* To be executed at the AST transformation stage, but I put it here for now. 
 * Expects a ListElement of type CONCAT. */

GList *concatExpToList(ListElement *string_exp);


/* Tests a string expression against a host character or string. The string
 * expression can contain multiple character variables but at most one
 * string variable. The algorithm is analogous to labelMatch in the way
 * it traverses lists, compares elements and handles variabes. 
 *
 * Since multiple variables can be assigned values in this function, it
 * internally keeps a count of how many assignments are made. If the 
 * expressions do not match, it removes any local assignment additions
 * and returns -1. Otherwise, it returns the number of assignments
 * made (possibly none).
 *
 * Argument 1: The string expression, taken from a label of the rule graph.
 * Argument 2: The constant string, taken from a label of the host graph.
 * Argument 3: The list of variables from the rule.
 */

int verifyStringExp(GList *string_exp, ListElement *host_atom,
                    VariableList *variables);


/* Auxiliary functions for compareAtoms in the case of a concatenated string
 * expression.
 * isPrefix checks if test is a prefix of str. If so, it returns str with the
 * prefix removed. Otherwise, it returns NULL.
 * isSuffix checks if test is a suffix of str. If so, it returns str with the
 * suffix removed. Otherwise, it returns NULL. 
 * Both functions return a string allocated in the heap. It is the 
 * responsibility of the caller to free the output.
 */	
 
int isPrefix(const string test, const string str);
int isSuffix(const string test, const string str);


/* The two verify variable functions are called during label matching. In the
 * case that a variable-value mapping is required to match two labels, these
 * functions check if the variable in question has a different value assigned 
 * to it in the existing assignment. 
 *
 * verifyAtomVariable is called by compareAtoms. It returns an integer as
 * compareAtoms needs to know if an assignment was made. If the passed value 
 * differs from the value assigned to the variable in the current assignment, 
 * it returns -1. If the values are equal, the function returns 0. If the
 * variable does not exist in the assignment, a copy of list/value is 
 * dynamically allocated and added to the assignment. In this case the function
 * returns 1 because an assignment was made.
 *
 * Since the assignment to the list variable is always the last variable  
 * assignment made in labelMatch, verifyListVariable does not need to report
 * whether an assignment was made or not. Hence it returns false if the values
 * differ, and true otherwise, whether an assignment was added or not.
 *
 * Argument 1: The name of the variable to be verified.
 * Argument 2: The value required for a label match. If the value is to be
 *             added to the assignment, a heap copy is created inside the
 *             function. verifyListVariable is passed a shallow copy of a
 *             GList, which is freed with g_list_free inside the function.
 *             verifyAtomVariable is passed the address of a local ListElement
 *             and does not need to free anything.
 */                    

bool verifyListVariable(string name, GList *list);
int verifyAtomVariable(string name, ListElement *value);

/* compareConstants takes two ListElements representing a constant value and 
 * checks if the values they represent are equal. Used as an auxiliary function
 * to the verify variable functions. To be consistent with the return value
 * of the verify functions, it returns -1 on failure and 0 on success. */
 
int compareConstants(ListElement *atom, ListElement *test_atom);

#endif /* INC_MATCH_H */
