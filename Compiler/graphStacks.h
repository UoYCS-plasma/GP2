/* ///////////////////////////////////////////////////////////////////////////

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

#include "error.h"
#include "globals.h"
#include "graph.h"
#include "stack.h"

extern Stack *graph_stack;
/* Creates a memory copy of the passed graph and pushes it to the graph stack. 
 * If replace is true, then the graph stack is popped before pushing the graph
 * copy. */
void copyGraph(Graph *graph);
/* restoreGraph frees the passed graph and returns the graph <depth> items
 * down the stack. All intermediate graphs are freed. */
Graph *restoreGraph(Graph *graph, int depth);
void freeGraphStack(Stack *graph_stack);

extern Stack *graph_change_stack;

/* A GraphChange stores the data sufficient to perform the inverse operation
 * of a previously-made change to the graph. For instance, if a node is removed
 * from the graph, the data needed for the inverse operation (addNode) are the
 * node's root flag and the node's label. */
typedef enum { ADDED_NODE = 0, ADDED_EDGE, REMOVED_NODE, REMOVED_EDGE, 
	       RELABELLED_NODE, RELABELLED_EDGE } GraphChangeType; 
typedef struct GraphChange 
{
   GraphChangeType type;
   union
   {
      int added_node_index; 
      int added_edge_index;

      struct {
         bool root;
         Label *label;
      } removed_node;

      struct {
         bool bidirectional;
         Label *label;
         int source;
         int target;
      } removed_edge;

      struct {
         int index;
         bool change_flag;
         Label *old_label;
      } relabelled_node, relabelled_edge;   
   } data;
} GraphChange; 

GraphChange *newGraphChange(void);
void pushAddedNode(int index);
void pushAddedEdge(int index);
void pushRemovedNode(bool root, Label *label);
void pushRemovedEdge(bool bidirectional, Label *label, int source, int target);
void pushRelabelledNode(int index, bool change_flag, Label *old_label);
void pushRelabelledEdge(int index, bool change_flag, Label *old_label);
void rollBackGraph(Graph *graph, int restore_point);
void discardChanges(int restore_point);
void freeGraphChange(GraphChange *change); 
void freeGraphChangeStack(Stack *graph_change_stack);

#endif /* INC_GRAPH_STACKS_H */
