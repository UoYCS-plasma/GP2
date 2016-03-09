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

  ====================
  Generate Rule Module
  ====================

  Module for generating code to apply a rule. The functions in this module
  take a rule and produce a C module with two principal functions: one to match
  the rule (creating a morphism) and one that takes a morphism and applies the
  rule. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_RULE_H
#define INC_GEN_RULE_H

#include "ast.h"
#include "genCondition.h"
#include "genLabel.h"
#include "globals.h"
#include "rule.h"
#include "searchplan.h"
#include "transform.h"

/* The functions in this module use a static searchplan of the LHS of a rule
 * to generate matching code. Each searchplan operation has a function that
 * emits a C function to match its rule item to a compatible host graph item
 * at runtime. The structure of the complete C source file is described below.
 *
 * For a searchplan op_1,...,op_n and corresponding generated C functions
 * f_1,...,f_n, the main matching function match_R calls f_1. If f_i
 * finds a match, it updates the appropriate data structures and calls f_i+1.
 * If f_i doesn't find a match, it returns false and the previous matcher handles
 * that result by either testing another host graph item or by backtracking, namely
 * undoing data structure updates from a previous match. If the first matching
 * function f_1 returns false, then match_R returns false, signalling that the 
 * rule matching failed. If the last matching function f_n finds a match, then
 * it returns true. This propagates back through all the matching functions to 
 * match_R, which returns true, signalling that the rule match is a success. */
 
/* Takes the root of the AST of a GP 2 program and generates C modules for
 * each rule in the program. */
void generateRules(List *declarations, string output_dir);

/* Create a C module to match and apply the rule. The generated files are
 * called <rule_name>.h and <rule_name>.c.*/
void generateRuleCode(Rule *rule, bool predicate, string output_dir);

/* The three functions below write the function apply_<rule_name> that makes the 
 * necessary changes to the host graph according to the rule and morphism. 
 *
 * generateRemoveLHSCode is called when the RHS is the empty graph. The generated
 * function uses the morphism to remove the image of the LHS from the host graph.
 *
 * generateAddRHSCode is called when the LHS is the empty graph. The generated
 * function does not take the morphism as an argument, as that is not needed to
 * add the RHS to the host graph.
 *
 * generateApplicationCode is called when both the LHS and the RHS are not the 
 * empty graph. It generates full rule application code, including the evaluation
 * of RHS labels, supported by the RHS label generation functions in the genLabel
 * module. */ 
void generateRemoveLHSCode(string rule_name);
void generateAddRHSCode(Rule *rule);
void generateApplicationCode(Rule *rule);

#endif /* INC_GEN_RULE_H */
