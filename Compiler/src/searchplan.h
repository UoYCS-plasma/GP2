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

  =================
  Searchplan Module
  =================  

  Defines a data structure for searchplans and functions operating on this
  data structure. Also defines a function to construct a static searchplan
  from a graph.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_SEARCHPLAN_H
#define INC_SEARCHPLAN_H

#include "rule.h"
#include <inc/common.h>

#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 

/* Search operations are categorised by a character as follows:
 * 'n': Non-root node.
 * 'r': Root node.
 * 'i': Node matched from its incoming edge.
 * 'o': Node matched from its outgoing edge.
 * 'b': Node matched from an incident bidirectional edge.
 * 'e': Edge.
 * 's': Edge matched from its source.
 * 't': Edge matched from its target.
 * 'l': Looping edge matched from its incident node.
 *
 * The index of a search operation refers to the index of the node or edge
 * in the LHS graph's corresponding pointer array. */
typedef struct SearchOp {
   bool is_node;
   char type;
   int index;
   struct SearchOp *next;
} SearchOp;

/* Operations are appended to the searchplan, so a pointer to the last
 * searchplan operation is maintained for efficiency. */
typedef struct Searchplan {
   SearchOp *first;
   SearchOp *last;
} Searchplan;

/* generateSearchplan traverses a graph in order to create a searchplan
 * using the following algorithm:
 * (1) Walk the graph in a depth-first manner starting at the root nodes. 
 *     Tag each item, including the initial root node, when it is encountered.
 *     If the next item in the traversal is untagged, tag it and append the
 *     appropriate operation to the searchplan. For instance, if we examine an
 *     outgoing edge of a root node, add the 's' operation to the searchplan.
 *     Once this step is complete, all connected components containing root
 *     nodes have been examined.
 * (2) Scan the node list of the graph, performing step 2 on any untagged nodes.
 *     Unnecessary if the input graph is root-connected.
 *
 * The depth-first search is performed by recursive calls to traverseNode and
 * traverseEdge. These two functions are responsible for checking if items
 * are tagged, tagging items, and adding new operations to the searchplan. */ 

Searchplan *generateSearchplan(RuleGraph *lhs);

void printSearchplan(Searchplan *searchplan);
void freeSearchplan(Searchplan *searchplan);
#endif /* INC_SEARCHPLAN_H */
