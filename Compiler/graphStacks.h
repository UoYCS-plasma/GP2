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

#include "error.h"
#include "globals.h"
#include "graph.h"
#include "label.h"

/* A GraphChange stores the data sufficient to undo a particular graph modification.
 * Specifically, an undo operation must restore the graph to its exact state,
 * which includes the indices of nodes and edges in their arrays and the graph's
 * holes arrays. */
typedef enum { ADDED_NODE = 0, ADDED_EDGE, REMOVED_NODE, REMOVED_EDGE, 
	       RELABELLED_NODE, RELABELLED_EDGE, CHANGED_ROOT_NODE} GraphChangeType; 
typedef struct GraphChange 
{
   GraphChangeType type;
   union
   {
      /* Records the array index to which this item was added and a flag to signal
       * whether the item was added using an index from the holes array or not. */
      struct {
         int index; 
         bool hole_filled;  
      } added_node, added_edge;
      /* Records the root status and label of the removed node, along with its 
       * index in the node array and a flag set to true if the removal of this
       * node created a hole in the node array. */
      struct {
         bool root;
         HostLabel label;
         int index;
         bool hole_created;
      } removed_node;
      /* Records the label, source and target of the removed edge, along with its 
       * index in the edge array and a flag set to true if the removal of this
       * edge created a hole in the edge array. */
      struct {
         HostLabel label;
         int source;
         int target;
         int index;
         bool hole_created;
      } removed_edge;
      /* Records the index of the relabelled item and the item's previous label. */
      struct {
         int index;
         HostLabel old_label;
      } relabelled_node, relabelled_edge;   
      /* Records the index of the node whose root status was changed. */
      int changed_root_index;
   };
} GraphChange; 

struct GraphChangeStack;
extern struct GraphChangeStack *graph_change_stack;
extern int graph_change_count;

int topOfGraphChangeStack(void);
void pushAddedNode(int index, bool hole_filled);
void pushAddedEdge(int index, bool hole_filled);
void pushRemovedNode(bool root, HostLabel label, int index, bool hole_created);
void pushRemovedEdge(HostLabel label, int source, int target, int index, bool hole_created);
void pushRelabelledNode(int index, HostLabel old_label);
void pushRelabelledEdge(int index, HostLabel old_label);
void pushChangedRootNode(int index);
void undoChanges(Graph *graph, int restore_point);
void discardChanges(int restore_point);
void freeGraphChangeStack(void);


extern Graph **graph_stack;
extern int graph_stack_index;
extern int graph_copy_count;

/* Creates a memory copy of the passed graph and pushes it to the graph stack. */
void copyGraph(Graph *graph);

/* popGraphs pops and frees the graph change stack until the restore point
 * is reached. It returns the graph at that stack entry. */
Graph *popGraphs(Graph *current_graph, int restore_point);
void discardGraphs(int depth);
void freeGraphStack(void);


#endif /* INC_GRAPH_STACKS_H */
