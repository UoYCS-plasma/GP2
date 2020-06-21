/* Copyright 2015-2017 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>. */

#include "graphStacks.h"

typedef struct GraphChangeStack {
   int size;
   int capacity;
   GraphChange *stack;
   Graph *graph;
} GraphChangeStack;

static GraphChangeStack *graph_change_stack = NULL;

static void makeGraphChangeStack(int initial_capacity)
{
   GraphChangeStack *stack = mallocSafe(sizeof(GraphChangeStack), "makeGraphChangeStack");
   stack->size = 0;
   stack->capacity = initial_capacity;
   stack->stack = mallocSafe(initial_capacity * sizeof(GraphChange), "makeGraphChangeStack"); 
   stack->graph = NULL;
   graph_change_stack = stack;
}

static void growGraphChangeStack(void)
{
   graph_change_stack->capacity *= 2;
   graph_change_stack->stack = reallocSafe(
     graph_change_stack->stack,
     graph_change_stack->capacity * sizeof(GraphChange),
     "growGraphChangeStack"
   );
}

static void pushGraphChange(GraphChange change)
{
   if(graph_change_stack == NULL) makeGraphChangeStack(128);
   else if(graph_change_stack->size >= graph_change_stack->capacity) growGraphChangeStack();
   graph_change_stack->stack[graph_change_stack->size++] = change;
}

static GraphChange pullGraphChange(void)
{
   assert(graph_change_stack != NULL);
   assert(graph_change_stack->size > 0);
   return graph_change_stack->stack[--graph_change_stack->size];
}

void setStackGraph(Graph *graph)
{
   if(graph_change_stack == NULL) makeGraphChangeStack(128);
   graph_change_stack->graph = graph;
}

int topOfGraphChangeStack(void)
{
   return graph_change_stack == NULL ? 0 : graph_change_stack->size;
}

void pushAddedNode(Node *node)
{
   GraphChange change;
   change.type = ADDED_NODE;
   change.added_node = node;
   change.first_occurrence = !nodeInStack(node);
   setNodeInStack(node);
   pushGraphChange(change);
}

void pushAddedEdge(Edge *edge)
{
   GraphChange change;
   change.type = ADDED_EDGE;
   change.added_edge = edge;
   change.first_occurrence = !edgeInStack(edge);
   setEdgeInStack(edge);
   pushGraphChange(change);
}

void pushRemovedNode(Node *node)
{
   GraphChange change;
   change.type = REMOVED_NODE;
   change.removed_node = node;
   change.first_occurrence = !nodeInStack(node);
   setNodeInStack(node);
   pushGraphChange(change);
}

void pushRemovedEdge(Edge *edge)
{
   GraphChange change;
   change.type = REMOVED_EDGE;
   change.removed_edge = edge;
   change.first_occurrence = !edgeInStack(edge);
   setEdgeInStack(edge);
   pushGraphChange(change);
}

void pushRelabelledNode(Node *node, HostLabel old_label)
{
   GraphChange change;
   change.type = RELABELLED_NODE;
   change.relabelled_node.node = node;
   change.first_occurrence = !nodeInStack(node);
   setNodeInStack(node);
   change.relabelled_node.old_label = old_label;
   /* Keep a record of the list as the relabelling of the node could free this
    * list or remove its bucket from the hash table. */
   #ifndef MINIMAL_GC
   addHostList(old_label.list);
   #endif
   pushGraphChange(change);
}

void pushRelabelledEdge(Edge *edge, HostLabel old_label)
{
   GraphChange change;
   change.type = RELABELLED_EDGE;
   change.relabelled_edge.edge = edge;
   change.first_occurrence = !edgeInStack(edge);
   setEdgeInStack(edge);
   change.relabelled_edge.old_label = old_label;
   /* Keep a record of the list as the relabelling of the edge could free this
    * list or remove its bucket from the hash table. */
   #ifndef MINIMAL_GC
   addHostList(old_label.list);
   #endif
   pushGraphChange(change);
}

void pushRemarkedNode(Node *node, MarkType old_mark)
{
   GraphChange change;
   change.type = REMARKED_NODE;
   change.remarked_node.node = node;
   change.first_occurrence = !nodeInStack(node);
   setNodeInStack(node);
   change.remarked_node.old_mark = old_mark;
   pushGraphChange(change);
}

void pushRemarkedEdge(Edge *edge, MarkType old_mark)
{
   GraphChange change;
   change.type = REMARKED_EDGE;
   change.remarked_edge.edge = edge;
   change.first_occurrence = !edgeInStack(edge);
   setEdgeInStack(edge);
   change.remarked_edge.old_mark = old_mark;
   pushGraphChange(change);
}

void pushChangedRootNode(Node *node)
{
   GraphChange change;
   change.type = CHANGED_ROOT_NODE;
   change.changed_root = node;
   change.first_occurrence = !nodeInStack(node);
   setNodeInStack(node);
   pushGraphChange(change);
}

void undoChanges(int restore_point)
{
   if(graph_change_stack == NULL) return;
   assert(restore_point >= 0);
   Graph *graph = graph_change_stack->graph;
   while(graph_change_stack->size > restore_point)
   {
      GraphChange change = pullGraphChange();
      switch(change.type)
      {
         case ADDED_NODE:
              if(change.first_occurrence)
                clearNodeInStack(change.added_node);
              removeNode(graph, change.added_node);
              break;

         case ADDED_EDGE:
              if(change.first_occurrence)
                clearEdgeInStack(change.added_edge);
              removeEdge(graph, change.added_edge);
              break;

         case REMOVED_NODE:
              if(change.first_occurrence)
                clearNodeInStack(change.removed_node);
              clearNodeDeleted(change.removed_node);
              if(!nodeInGraph(change.removed_node))
                recoverNode(graph, change.removed_node);
              break;

         case REMOVED_EDGE:
              if(change.first_occurrence)
                clearEdgeInStack(change.removed_edge);
              clearEdgeDeleted(change.removed_edge);
              break;

         case RELABELLED_NODE:
              if(change.first_occurrence)
                clearNodeInStack(change.relabelled_node.node);
              #ifndef MINIMAL_GC
              removeHostList((change.relabelled_node.node)->label.list);
              #endif
              relabelNode(change.relabelled_node.node, change.relabelled_node.old_label);
              break;

         case RELABELLED_EDGE:
              if(change.first_occurrence)
                clearEdgeInStack(change.relabelled_edge.edge);
              #ifndef MINIMAL_GC
              removeHostList((change.relabelled_edge.edge)->label.list);
              #endif
              relabelEdge(change.relabelled_edge.edge, change.relabelled_edge.old_label);
              break;

         case REMARKED_NODE:
              if(change.first_occurrence)
                clearNodeInStack(change.remarked_node.node);
              changeNodeMark(change.remarked_node.node, change.remarked_node.old_mark);
              break;

         case REMARKED_EDGE:
              if(change.first_occurrence)
                clearEdgeInStack(change.remarked_edge.edge);
              changeEdgeMark(change.remarked_edge.edge, change.remarked_edge.old_mark);
              break;

         case CHANGED_ROOT_NODE:
              if(change.first_occurrence)
                clearNodeInStack(change.changed_root);
              changeRoot(graph, change.changed_root);
              break;

         default:
              print_to_log("Error (restoreGraph): Unexepected change type %d.\n",
                           change.type);
              break;
      }
   }
}

static void freeGraphChange(GraphChange change)
{
   #ifndef MINIMAL_GC
   Graph *graph = graph_change_stack->graph;
   #endif
   switch(change.type)
   {
      case ADDED_NODE:
           if(change.first_occurrence)
             clearNodeInStack(change.added_node);
           break;

      case ADDED_EDGE:
           if(change.first_occurrence)
             clearEdgeInStack(change.added_edge);
           #ifndef MINIMAL_GC
           if(edgeFree(change.added_edge))
           {
             removeHostList(change.added_edge->label.list);
             removeFromBigArray(&(graph->_edgearray), change.added_edge->index);
           }
           #endif
           break;

      case REMOVED_NODE:
           if(change.first_occurrence)
             clearNodeInStack(change.removed_node);
           break;

      case REMOVED_EDGE:
           if(change.first_occurrence)
             clearEdgeInStack(change.removed_edge);
           break;

      case RELABELLED_NODE:
           if(change.first_occurrence)
             clearNodeInStack(change.relabelled_node.node);
           break;

      case RELABELLED_EDGE:
           if(change.first_occurrence)
             clearEdgeInStack(change.relabelled_edge.edge);
           break;

      case REMARKED_NODE:
           if(change.first_occurrence)
             clearNodeInStack(change.remarked_node.node);
           break;

      case REMARKED_EDGE:
           if(change.first_occurrence)
             clearEdgeInStack(change.remarked_edge.edge);
           break;

      case CHANGED_ROOT_NODE:
           if(change.first_occurrence)
             clearNodeInStack(change.changed_root);
           break;

      default:
           print_to_log("Error (freeGraphChange): Unexepected graph change "
                        "type %d.\n",change.type);
           break;
   }
}

void discardChanges(int restore_point)
{
   if(graph_change_stack == NULL) return;
   while(graph_change_stack->size > restore_point)
   {
      GraphChange change = pullGraphChange();
      freeGraphChange(change);
   }
} 

#ifndef MINIMAL_GC
void freeGraphChangeStack(void)
{
   if(graph_change_stack == NULL) return;
   discardChanges(0);
   free(graph_change_stack->stack);
   free(graph_change_stack);
}
#endif
