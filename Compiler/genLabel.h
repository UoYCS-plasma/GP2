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

/* When matching an isolated node or edge, this function is called. It uses
 * the passed label to generate the appropriate for loops to iterate over the 
 * host graph's label class array. */
void generateIteratorCode(Label label, int indent, FILE *file);

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
void generateRHSLabelCode(Label label, bool node, int count, int indent, FILE *file);

/* Called by generateRHSLabelCode if it encounters an arithmetic operator or 
 * the length operator. generateIntExpression is responsible for navigating the
 * AST subtree of the expression and writing the arithmetic expression it 
 * represents.
 *
 * For example, given the RHS label (i + 1) * length(s), where i is an integer
 * variable and s is a string variable, generateIntExpression would print:
 * (i_var + 1) * (int)strlen(s_var); */
void generateIntExpression(Atom atom, bool nested, FILE *file);

/* The string case is analogous to the above but complicated by the fact that
 * the runtime code builds a string whose length needs to be known in advance.
 * Therefore two functions are used to generate the string expression.
 * The first obtains the total length of the evaluated RHS string and assigns
 * it to a runtime variable. The second generates the string.
 *
 * For example, "a".s.c (s string variable, c character variable) is as follows.
 * generateStringLengthCode prints:
 * length = 0; 
 * length += strlen("a"); 
 * length += strlen(s_var);
 * length += strlen(c_var);
 *
 * The character array host_string of size <length> is created by the caller
 * before calling generateStringExpression. 
 *
 * generateStringExpression prints:
 * host_string = strcpy(host_string, "a"); 
 * host_string = strcat(host_string, s_var); 
 * host_string = strcat(host_string, c_var);
 *
 * The 'bool first' argument of generateStringExpression is used to control
 * which of 'strcpy' and 'strcat' is printed. C doesn't like it when you call
 * strcpy on a newly-created character array. */
void generateStringLengthCode(Atom atom, int indent, FILE *file);
void generateStringExpression(Atom atom, bool first, int indent, FILE *file);

#endif /* INC_GEN_LABEL_H */
