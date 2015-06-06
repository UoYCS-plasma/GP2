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

#define GRAPH_STACK_SIZE 4
#define GRAPH_CHANGE_STACK_SIZE 32

#include "error.h"
#include "globals.h"
#include "graph.h"

extern Graph **graph_stack;
extern int graph_stack_index;
/* Creates a memory copy of the passed graph and pushes it to the graph stack. 
 * If replace is true, then the graph stack is popped before pushing the graph
 * copy. */
void copyGraph(Graph *graph);
/* popGraphs pops and frees the graph change stack until the restore point.
 * is reached. It returns the graph at that stack entry. */
Graph *popGraphs(Graph *current_graph, int restore_point);
void discardGraphs(int depth);
void freeGraphStack(void);

/* A GraphChange stores the data sufficient to perform the inverse operation
 * of a previously-made change to the graph. For instance, if a node is removed
 * from the graph, the data needed for the inverse operation (addNode) are the
 * node's root flag and the node's label. */
typedef enum { ADDED_NODE = 0, ADDED_EDGE, REMOVED_NODE, REMOVED_EDGE, 
	       RELABELLED_NODE, RELABELLED_EDGE, CHANGED_ROOT_NODE} GraphChangeType; 
typedef struct GraphChange 
{
   GraphChangeType type;
   union
   {
      int added_node_index; 
      int added_edge_index;

      struct {
         bool root;
         Label label;
      } removed_node;

      struct {
         Label label;
         int source;
         int target;
      } removed_edge;

      struct {
         int index;
         Label old_label;
      } relabelled_node, relabelled_edge;   

      int changed_root_node_index;
   };
} GraphChange; 

extern GraphChange *graph_change_stack;
extern int graph_change_index;

void pushAddedNode(int index);
void pushAddedEdge(int index);
void pushRemovedNode(bool root, Label label);
void pushRemovedEdge(Label label, int source, int target);
void pushRelabelledNode(int index, Label old_label);
void pushRelabelledEdge(int index, Label old_label);
void pushChangedRootNode(int index);
void undoChanges(Graph *graph, int restore_point);
void discardChanges(int restore_point);
void freeGraphChangeStack(void);

#endif /* INC_GRAPH_STACKS_H */
