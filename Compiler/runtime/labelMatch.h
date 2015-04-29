/* ///////////////////////////////////////////////////////////////////////////

  =====================
  Label Matching Module
  =====================

  Defines the functions and auxiliary structures for label matching.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_LABEL_MATCH_H
#define INC_LABEL_MATCH_H

#include "../error.h"
#include "../globals.h"
#include "../label.h"
#include "morphism.h"

/* The matching functions in this module return the number of variable-value
 * assignments made during that function's execution in case of a successful
 * match. This could be 0. -1 is returned if no match was possible. */
 
/* labelMatch returns -1 if the labels cannot be matched. Specifically:
 * (1) The marks are not equal and the rule label's mark is not a wildcard mark.
 * (2) Two corresponding elements of the lists do not match (compareAtoms)
 * (3) A variable is assigned a value which clashes with a value already
 *     assigned to the same variable.
 * (4) The lists do not have an equal number of atomic values to pair up. */
int labelMatch (Label rule_label, Label host_label, Morphism *morphism);
int compareAtoms(Atom rule_atom, Atom host_atom, Morphism *morphism);

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

/* Tries to match a string list, generated from a concatenated string in the
 * rule, to a string constant in a host label. */
int verifyStringExp(StringList *list, string host_string, Morphism *morphism);

/* Used to test string constants in the rule against a host string. If 
 * rule_string is a prefix of the host_string, then the index of the host 
 * character directly after this prefix is returned, so that the caller knows
 * where in the host string to resume matching. 
 * For example, isPrefix("ab", "abcd") returns 2, the index of the first 
 * character ('c') after the matched substring ("ab").
 * Returns -1 if it the rule string is not a prefix of the host string. */
int isPrefix(const string rule_string, const string host_string);

/* Analogous to isPrefix. Example: isSuffix("cd", "abcd") returns 1, the index
 * of the character ('b') directly preceding the matched suffix ("cd"). 
 * The exception is if rule_string equals host_string, in which case 0 is
 * returned. */
int isSuffix(const string rule_string, const string host_string);

/* Tests a potential variable-value assignment against the existing assignment. 
 * If the variable is not in the assignment, its name and value is added.
 *
 * Returns -1 if the variable has already been assigned to a different value
 * in the assignment.
 * Returns 0 if the variable has a value in the assignment that is equal to
 * the passed value.
 * Returns 1 if the variable did not previously exist in the assignment. */
int verifyListVariable(string name, Atom *list, int length, Morphism *morphism);
int verifyAtomVariable(string name, Atom atom, Morphism *morphism);

/* Tests equality of two atoms of type INTEGER_CONSTANT or STRING_CONSTANT. */
bool compareConstants(Atom test_atom, Atom atom);

#endif /* INC_LABEL_MATCH_H */
