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

  ===================
  Graph Stacks Module
  ===================

  Data structures and functions for graph backtracking. There are two types
  of graph backtracking: 
  (1) A complete memory copy of the working graph.
  (2) A stack of graph changes maintained so that the graph can be rolled back
      if necessary.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GRAPH_STACKS_H
#define INC_GRAPH_STACKS_H

#define GRAPH_STACK_SIZE 4

#include "common.h"
#include "graph.h"
#include "label.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

/* A GraphChange stores the data sufficient to undo a particular graph modification.
 * Specifically, an undo operation must restore the graph to its exact state,
 * which includes the indices of nodes and edges in their arrays and the graph's
 * holes arrays. */
typedef enum {
   ADDED_NODE = 0,
   ADDED_EDGE,
   REMOVED_NODE,
   REMOVED_EDGE,
	 RELABELLED_NODE,
   RELABELLED_EDGE,
   REMARKED_NODE,
   REMARKED_EDGE,
   CHANGED_ROOT_NODE
} __attribute__ ((__packed__)) GraphChangeType;

typedef struct GraphChange
{
   union
   {
      Node *added_node;
      Edge *added_edge;
      Node *removed_node;
      Edge *removed_edge;
      /* Records the relabelled item and the item's previous label. */
      struct {
         Node *node;
         HostLabel old_label;
      } relabelled_node;
      struct {
         Edge *edge;
         HostLabel old_label;
      } relabelled_edge;
      /* Records the remarked item and the item's previous mark. */
      struct {
         Node *node;
         MarkType old_mark;
      } remarked_node;
      struct {
         Edge *edge;
         MarkType old_mark;
      } remarked_edge;
      /* Records the node whose root status was changed. */
      Node *changed_root;
   };
   GraphChangeType type;
   bool first_occurrence; // true if node/edge first appears here in stack
} GraphChange;

void setStackGraph(Graph *graph);

int topOfGraphChangeStack(void);
void pushAddedNode(Node *node);
void pushAddedEdge(Edge *edge);
void pushRemovedNode(Node *node);
void pushRemovedEdge(Edge *edge);
void pushRelabelledNode(Node *node, HostLabel old_label);
void pushRelabelledEdge(Edge *edge, HostLabel old_label);
void pushRemarkedNode(Node *node, MarkType old_mark);
void pushRemarkedEdge(Edge *edge, MarkType old_mark);
void pushChangedRootNode(Node *node);
void undoChanges(int restore_point);
// Need to pass graph here in case node/edges need to be collected
void discardChanges(int restore_point);
#ifndef MINIMAL_GC
void freeGraphChangeStack(void);
#endif

#endif /* INC_GRAPH_STACKS_H */
