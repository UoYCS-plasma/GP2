/* ///////////////////////////////////////////////////////////////////////////

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

  =====================
  Label Matching Module
  =====================

  Defines the functions for label matching code generation.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_LABEL_H
#define INC_GEN_LABEL_H

#include <inc/common.h>
#include <inc/gp2enums.h>
#include "rule.h"

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

/* Used by genLabel, genRule and genCondition. Defined in genRule. */
extern FILE *file;

/* Generates code to match a rule list not containing a list variable to a host graph list. */
void generateFixedListMatchingCode(Rule *rule, RuleLabel label, int indent);

/* Generates code to match a rule list containing a list variable to a host graph list. */
void generateVariableListMatchingCode(Rule *rule, RuleLabel label, int indent);

/* Generates code to evaluate predicates containing the given variable. */
void generateVariableResultCode(Rule *rule, int id, bool list_variable, int indent);

/* Generate runtime C variables to store the values of the GP 2 values used
 * in rule application. */
void generateVariableCode(int id, GPType type);

/* Takes a RHS label and generates code to build the corresponding host label.
 * This includes code to evaluate arithmetic expressions, code to create C strings
 * from concatenated expressions and code to substitute variables for values
 * according to the assignment in the morphism. */
void generateLabelEvaluationCode(RuleLabel label, bool node, int count, int predicate, int indent);

/* Emits C code for the integer expression represented by the passed atom. */
void generateIntExpression(RuleAtom *atom, int context, bool nested);

#endif /* INC_GEN_LABEL_H */
