/* ///////////////////////////////////////////////////////////////////////////

  Copyright 2015-2017 Christopher Bak

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

  =========================
  Generate Condition Module
  =========================

  Generates code to carry out the evaluation of GP 2 rule conditions.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_CONDITION_H
#define INC_GEN_CONDITION_H

#include "common.h"
#include "genLabel.h"
#include "rule.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 

/* GP 2's condition code generation is demonstrated by example. 
 * Consider the condition
 *
 * (indegree(0) > 1 or length(l) = 2) and not atom(l).
 *
 * generateConditionVariables prints the following global variables
 * to the source file of the rule matcher.
 *
 * bool b0 = true; 
 * bool b1 = true;
 * bool b2 = false;
 *
 * b0 corresponds to the predicate indegree(0) > 1.
 * b1 corresponds to the predicate length(1) = 2.
 * b2 corresponds to the predicate atom(1).
 *
 * generateConditionEvaluator prints the following function:
 *
 * static bool evaluateCondition(void)
 * {
 *    return ((b0 || b1) and !b2);
 * }
 *
 * The initial values of the variables are assigned such that evaluateCondition
 * evaluates to true under these variables. This is because the condition is
 * evaluated during rule matching, in which case In particular, b3 is assigned false
 * because its predicate is negated in the condition. 
 *
 * generatePredicateEvaluators writes functions to evaluate the predicates and
 * set its associated boolean variable to the result. For example, indegree(0) > 1
 * translates to:
 *
 * static bool evaluatePredicate1(void)
 * {
 *    int n0 = lookupNode(morphism, 0);
 *    if(n0 == -1) return false;
 *    
 *    if(getIndegree(host, 0) > 1) b0 = true;
 *    return true;
 * }
 *
 * The function returns false if the values requires for the condition (node degrees
 * and variable values) have not yet been instantiated by rule matching. */

void generateConditionVariables(Condition *condition);
void generateConditionEvaluator(Condition *condition, bool nested);
void generatePredicateEvaluators(Rule *rule, Condition *condition);

#endif /* INC_GEN_CONDITION_H */
