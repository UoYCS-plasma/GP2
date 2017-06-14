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

  =========================
  AST Transformation Module
  =========================
                             
  Module for transforming the AST into intermediate data structures.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_TRANSFORM_H
#define INC_TRANSFORM_H 

#include "ast.h"
#include "common.h"
#include "rule.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

Rule *transformRule(GPRule *ast_rule);

/* Populates the rule's LHS graph from its AST equivalent, including the
 * transformation of AST-labels to the data structure defined in label.h. */
void scanLHS(Rule *rule, GPGraph *ast_lhs);

/* Populates the rule's RHS graph from its AST equivalent, including the
 * transformation of AST-labels to the data structure defined in label.h.
 * Also responsible for pointing rule graph nodes and edges to their 
 * corresponding interface items. Finally, it searches for variables and 
 * degree operators in RHS-labels in order to annotate variables and nodes 
 * with information about their usage by the rule. These annotations are 
 * used to support code generation. */
void scanRHS(Rule *rule, GPGraph *ast_rhs, List *interface);

#endif /* INC_TRANSFORM_H */
