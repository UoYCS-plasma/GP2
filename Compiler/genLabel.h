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
#include "rule.h"

/* Used by genLabel, genRule and genCondition. Defined in genRule. */
extern FILE *file;

/* Generates the appropriate for loops that iterate over the host graph's label class 
 * array to get candidate matches for rule nodes and rule edges. Returns the number
 * of loops generated. */
int generateIteratorCode(Label label, bool node);

/* Generates code to match a rule list not containing a list variable to a host graph list. */
void generateFixedListMatchingCode(Rule *rule, Label label, int indent);

/* Generates code to match a rule list containing a list variable to a host graph list. */
void generateVariableListMatchingCode(Rule *rule, Label label, int indent);

/* Generates code to evaluate predicates containing the given variable. */
void generateVariableResultCode(Rule *rule, string name, bool list_variable, int indent);

/* Generate runtime C variables to store the values of the GP 2 values used
 * in rule application. */
void generateVariableCode(string name, GPType type);

/* Takes a RHS label and generates code to build the corresponding host label.
 * This includes code to evaluate arithmetic expressions, code to create C strings
 * from concatenated expressions and code to substitute variables for values
 * according to the assignment in the morphism. */
void generateLabelEvaluationCode(Label label, bool node, int count, int predicate, int indent);

/* Emits C code for the integer expression represented by the passed atom. */
void generateIntExpression(Atom atom, int context, bool nested);

#endif /* INC_GEN_LABEL_H */
