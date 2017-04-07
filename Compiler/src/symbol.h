/* ////////////////////////////////////////////////////////////////////////////

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

  =============
  Symbol Module  
  =============                  
 
  Contains the data structure for GP 2 symbols and symbol processing functions.

//////////////////////////////////////////////////////////////////////////// */

#ifndef INC_SYMBOL_TABLE_H
#define INC_SYMBOL_TABLE_H

#include "common.h"

#include <glib.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

/* GP2 symbols are stored in struct Symbol. These are values of the symbol
 * table. The hash key of each symbol is its name in the GP2 program text.
 *
 * GP2's symbols are:
 * - Procedures
 * - Rules
 * - Variables
 * - Nodes
 * - Edges
 *
 * The Symbol structure contains the following:
 *
 * Type: A variable symbol's type is the variable's GP2 type according to its 
 *       declaration in a rule. The type of a node or edge is determined by 
 *       which side of the rule it occurs in. This is required for semantic
 *       analysis.
 *
 * Scope: The procedure in which the symbol is visible. Either "Main" or the
 *        name of a procedure in the program. All symbols have a scope.
 *
 * Rule Name: The rule in which the symbol is visible. NULL for symbols of type
 *            'rule' and 'procedure'. This field is used to uniquely identify
 *            variables, nodes and edges as they can have the same name in
 *            different rules.
 *       
 * Flags: is_var is set to true if the symbol represents a GP variable. This
 *        is for more concise code: better to compare to a single bool than to
 *        compare with each of the individual variable types.
 *
 *        in_lhs is set to true if the symbol has type variable and occurs in
 *        the LHS of a rule. Set to false in all other cases. 
 * 
 *        wildcard is set to true if the symbol is a node or edge with the cyan
 *        mark. Set to false in all other cases.
 *
 *        bidirectional is set to true if the symbol is a bidirectional edge.
 *        Set to false in all other cases.
 */
typedef enum {PROCEDURE_S=0, RULE_S, INT_S, CHAR_S, STRING_S, ATOM_S,
              LIST_S, LEFT_NODE_S, RIGHT_NODE_S, LEFT_EDGE_S, RIGHT_EDGE_S} 
              SymbolType;

typedef struct SymbolList {
  SymbolType type;
  string scope;
  string rule_name;
  bool is_var;
  bool in_lhs; 
  bool wildcard; 
  bool bidirectional;
  struct SymbolList *next;
} SymbolList;

SymbolList *addSymbol(SymbolList *list, SymbolType type, string scope, string rule,
                      bool is_var, bool in_lhs, bool wildcard, bool bidirectional);
bool symbolInScope(SymbolList *symbol, string scope, string rule_name);
void freeSymbolList(gpointer key, gpointer value, gpointer user_data); 

/* BiEdge is used to store the necessary information for semantic checking of
 * bidirectional edges. We need to check for parallel bidirectional edges,
 * which requires the scope, the type of the graph (LHS or RHS) and the
 * source and target IDs of the edge's incident nodes. All non-parallel
 * bidirectional edges in a rule are stored in a linked list of BiEdges.
 */
typedef struct BiEdge {
  string scope;
  string rule_name;
  char graph;
  string source;
  string target;
} BiEdge;

typedef struct BiEdgeList {
  struct BiEdge value;
  struct BiEdgeList *next;
} BiEdgeList;

BiEdgeList *addBiEdge(BiEdgeList *list, string scope, string rule_name, 
                      char graph, string source, string target);
void freeBiEdgeList(BiEdgeList *list); 

#endif /* INC_SYMBOL_TABLE_H */
