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

void generateFixedListMatchingCode(Label label, int indent, FILE *file);
void generateVariableListMatchingCode(Label label, int indent, FILE *file);
void generateAtomMatchingCode(Atom atom, int indent, FILE *file);
void generateVariableMatchingCode(Atom atom, int indent, FILE *file);
void generateConcatMatchingCode(Atom atom, int indent, FILE *file);
void generateStringMatchingCode(StringList *string_exp, bool prefix, int indent, FILE *file);
void generateRHSLabelCode(Label label, int count, int indent, FILE *file);
void generateIntEvalCode(Atom atom, int count, int indent, FILE *file);
void generateIntExpression(Atom atom, int count, bool nested, FILE *file);
void generateStringEvalCode(Atom atom, int count, int indent, FILE *file);
void generateStringExpression(Atom atom, int count, bool first, int indent, FILE *file);

#endif /* INC_GEN_LABEL_H */
