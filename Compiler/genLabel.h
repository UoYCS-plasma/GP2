/* ///////////////////////////////////////////////////////////////////////////

  =====================
  Label Matching Module
  =====================

  Defines the functions for label matching code generation.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_LABEL_H
#define INC_GEN_LABEL_H

#include "error.h"
#include "globals.h"
#include "label.h"

/* Concatenated string expressions are flattened into a doubly-linked list
 * structure. This is because it is difficult to match a tree structure
 * of string expressions, which couldcharacter variables and string variables,
 * to a single C string. */
typedef struct StringList {
   int type; /* (1) string constant, (2) char variable, (3) string variable. */
   string value; /* The value of a string constant or a variable name. */
   struct StringList *next;
   struct StringList *prev;
} StringList;

/* Uses appendStringExp to grow the passed StringList according to the value
 * of the passed Atom. */
StringList *stringExpToList(StringList *list, Atom *string_exp);
StringList *appendStringExp(StringList *list, int type, string value);
void freeStringList(StringList *list);

/* Two functions to generate code matching a particular rule list to a list in
 * the host graph. Fixed-length rule lists (lists not containing a list variable)
 * and variable-length rule lists are handled by two distinct functions. 
 * The former admits simple code generation as each rule atom maps directly
 * to the host atom in the same position. The latter is more involved
 * as it establishes the length of the list variable, generated code to
 * match the atoms either side of the list variable, and matches the list
 * variable at the very end. */
void generateFixedListMatchingCode(Label label, int indent, FILE *file);
void generateVariableListMatchingCode(Label label, int indent, FILE *file);

/* The following functions generate code to match a LHS rule atom with a 
 * corresponding host atom. generateAtomMatchingCode directly handles constants. 
 * It passes variables and concatenated expressions to their respective functions. */
void generateAtomMatchingCode(Atom atom, int indent, FILE *file);
void generateVariableMatchingCode(Atom atom, int indent, FILE *file);

/* generateConcatMatchingCode builds a StringList from the AST representation
 * of a concatenated string. It then iterates the resulting StringList, calling
 * generateStringMatchingCode on each element, similar to how list matching
 * code is generated. The string variable, if it exists, is the last item
 * to be matched. */
void generateConcatMatchingCode(Atom atom, int indent, FILE *file);
void generateStringMatchingCode(StringList *string_exp, bool prefix, int indent, FILE *file);

/* Called before generateRHSLabelCode. For each variable in the rule, code is
 * generated to assign that variable's value (from the morphism) to a local
 * variable for use by the rule application code. 
 * 
 * For example, the assignment (i -> 1, word -> "razzmatazz") will result in
 * the following:
 * int i_var = 1;
 * string word_var = "razzmatazz";
 * 
 * Uniqueness of variable names in a rule ensures uniqueness of the variable
 * name generated. */
void generateVariableCode(string name, GPType type, FILE *file);

/* Takes a RHS label and generates code to build the corresponding host label,
 * including code to evaluate expressions and substitute variables for values
 * according to the assignment in the morphism. */
void generateRHSLabelCode(Label label, int count, int indent, FILE *file);

/* Called by generateRHSLabelCode if it encounters an arithmetic operator or 
 * the length operator. generateIntEvalCode is responsible for navigating the
 * AST subtree of the expression and generating fresh integer variables 
 * to store the components of the expression. Subsequently, generateIntExpression
 * performs a second traversal of the subtree to write the expression.
 *
 * For example, given the RHS label (i + 1) * 2, generateIntEvalCode would
 * be responsible for printing something like:
 * int i0 = getIntegerValue("i", morphism);
 * int i1 = 1;
 * int i2 = 2;
 *
 * generateIntExpression would then print the expression:
 * <host_list_element> = (i0 + i1) * i2;
 *
 * Correct code generation comes from the fact that both functions traverse
 * the same AST subtree in the same order. */
void generateIntEvalCode(Atom atom, int count, int indent, FILE *file);
void generateIntExpression(Atom atom, int count, bool nested, FILE *file);

/* Similar to the above. As an example, the code generated for the RHS label
 * "a".s.c (s string variable, c character variable) is as follows.
 *
 * generateStringEvalCode prints:
 * length = 0;
 * string s0 = "a"; length += strlen(s0);
 * string s1 = getStringValue("s", morphism); length += strlen(s1);
 * string s2 = getStringValue("c", morphism); length += strlen(s2);
 *
 * The character array host_string of size <length> is created by the caller
 * before calling generateStringExpression. This is because it is a recursive 
 * function; it was simplier to print that code externally.
 *
 * generateStringExpression prints:
 * host_string = strcpy(host_string, s0); 
 * host_string = strcat(host_string, s1); 
 * host_string = strcat(host_string, s2);
 *
 * The 'bool first' argument of generateStringExpression is used to control
 * which of 'strcpy' and 'strcat' is printed. C doesn't like it when you call
 * strcpy on a newly-created character array. */
void generateStringEvalCode(Atom atom, int count, int indent, FILE *file);
void generateStringExpression(Atom atom, int count, bool first, int indent, FILE *file);

#endif /* INC_GEN_LABEL_H */
