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
} GraphChangeStack;

GraphChangeStack *graph_change_stack = NULL;
int graph_change_count = 0;

static void makeGraphChangeStack(int initial_capacity)
{
   GraphChangeStack *stack = malloc(sizeof(GraphChangeStack));
   if(stack == NULL)
   {
      print_to_log("Error (makeGraphChangeStack): malloc failure.\n");
      exit(1);
   }
   stack->size = 0;
   stack->capacity = initial_capacity;
   stack->stack = calloc(initial_capacity, sizeof(GraphChange)); 
   if(stack->stack == NULL)
   {
      print_to_log("Error (makeGraphChangeStack): malloc failure.\n");
      exit(1);
   }
   graph_change_stack = stack;
}

static void growGraphChangeStack(void)
{
   graph_change_stack->capacity *= 2;
   graph_change_stack->stack = realloc(graph_change_stack->stack,
                                       graph_change_stack->capacity * sizeof(GraphChange)); 
   if(graph_change_stack->stack == NULL)
   {
      print_to_log("Error (growGraphChangeStack): malloc failure.\n");
      exit(1);
   }
}

static void pushGraphChange(GraphChange change)
{
   if(graph_change_stack == NULL) makeGraphChangeStack(128);
   else if(graph_change_stack->size >= graph_change_stack->capacity) growGraphChangeStack();
   graph_change_stack->stack[graph_change_stack->size++] = change;
   graph_change_count++;
}

static GraphChange pullGraphChange(void)
{
   assert(graph_change_stack != NULL);
   assert(graph_change_stack->size > 0);
   return graph_change_stack->stack[--graph_change_stack->size];
}

int topOfGraphChangeStack(void)
{
   return graph_change_stack->size;
}

void pushAddedNode(Node *node)
{
   GraphChange change;
   change.type = ADDED_NODE;
   change.added_node = node;
   node->in_stack++;
   pushGraphChange(change);
}

void pushAddedEdge(Edge *edge)
{
   GraphChange change;
   change.type = ADDED_EDGE;
   change.added_edge = edge;
   edge->in_stack++;
   pushGraphChange(change);
}

void pushRemovedNode(Node *node)
{
   GraphChange change;
   change.type = REMOVED_NODE;
   change.removed_node = node;
   node->in_stack++;
   pushGraphChange(change);
}

void pushRemovedEdge(Edge *edge)
{
   GraphChange change;
   change.type = REMOVED_EDGE;
   change.removed_edge = edge;
   edge->in_stack++;
   pushGraphChange(change);
}

void pushRelabelledNode(Node *node, HostLabel old_label)
{
   GraphChange change;
   change.type = RELABELLED_NODE;
   change.relabelled_node.node = node;
   node->in_stack++;
   change.relabelled_node.old_label = old_label;
   /* Keep a record of the list as the relabelling of the node could free this
    * list or remove its bucket from the hash table. */
   #ifdef LIST_HASHING
      addHostList(old_label.list);
   #else
      change.relabelled_node.old_label.list = copyHostList(old_label.list);
   #endif
   pushGraphChange(change);
}

void pushRelabelledEdge(Edge *edge, HostLabel old_label)
{
   GraphChange change;
   change.type = RELABELLED_EDGE;
   change.relabelled_edge.edge = edge;
   edge->in_stack++;
   change.relabelled_edge.old_label = old_label;
   /* Keep a record of the list as the relabelling of the edge could free this
    * list or remove its bucket from the hash table. */
   #ifdef LIST_HASHING
      addHostList(old_label.list);
   #else
      change.relabelled_edge.old_label.list = copyHostList(old_label.list);
   #endif
   pushGraphChange(change);
}

void pushRemarkedNode(Node *node, MarkType old_mark)
{
   GraphChange change;
   change.type = REMARKED_NODE;
   change.remarked_node.node = node;
   node->in_stack++;
   change.remarked_node.old_mark = old_mark;
   pushGraphChange(change);
}

void pushRemarkedEdge(Edge *edge, MarkType old_mark)
{
   GraphChange change;
   change.type = REMARKED_EDGE;
   change.remarked_edge.edge = edge;
   edge->in_stack++;
   change.remarked_edge.old_mark = old_mark;
   pushGraphChange(change);
}

void pushChangedRootNode(Node *node)
{
   GraphChange change;
   change.type = CHANGED_ROOT_NODE;
   change.changed_root = node;
   change.changed_root->in_stack++;
   pushGraphChange(change);
}

void undoChanges(Graph *graph, int restore_point)
{
   if(graph_change_stack == NULL) return;
   assert(restore_point >= 0);
   while(graph_change_stack->size > restore_point)
   {
      GraphChange change = pullGraphChange();
      switch(change.type)
      {
         case ADDED_NODE:
              change.added_node->in_stack--;
              removeNode(graph, change.added_node);
              break;

         case ADDED_EDGE:
              change.added_edge->in_stack--;
              removeEdge(graph, change.added_edge);
              break;

         case REMOVED_NODE:
              change.removed_node->in_stack--;
              change.removed_node->deleted = false;
              if(!change.removed_node->in_graph)
                recoverNode(graph, change.removed_node);
              break;

         case REMOVED_EDGE:
              change.removed_edge->in_stack--;
              change.removed_edge->deleted = false;
              if(!change.removed_edge->in_graph)
                recoverEdge(graph, change.removed_edge);
              break;

         case RELABELLED_NODE:
              change.relabelled_node.node->in_stack--;
              relabelNode(change.relabelled_node.node, change.relabelled_node.old_label);
              break;

         case RELABELLED_EDGE:
              change.relabelled_edge.edge->in_stack--;
              relabelEdge(change.relabelled_edge.edge, change.relabelled_edge.old_label);
              break;

         case REMARKED_NODE:
              change.remarked_node.node->in_stack--;
              changeNodeMark(change.remarked_node.node, change.remarked_node.old_mark);
              break;

         case REMARKED_EDGE:
              change.remarked_edge.edge->in_stack--;
              changeEdgeMark(change.remarked_edge.edge, change.remarked_edge.old_mark);
              break;

         case CHANGED_ROOT_NODE:
              change.changed_root->in_stack--;
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
   switch(change.type)
   {
      case ADDED_NODE:
           change.added_node->in_stack--;
           tryGarbageCollectNode(change.added_node);
           break;

      case ADDED_EDGE:
           change.added_edge->in_stack--;
           tryGarbageCollectEdge(change.added_edge);
           break;

      case CHANGED_ROOT_NODE:
           break;

      case REMOVED_NODE:
           change.removed_node->in_stack--;
           break;

      case REMOVED_EDGE:
           change.removed_edge->in_stack--;
           break;

      case RELABELLED_NODE:
           change.relabelled_node.node->in_stack--;
           removeHostList(change.relabelled_node.old_label.list);
           break;

      case RELABELLED_EDGE:
           change.relabelled_edge.edge->in_stack--;
           removeHostList(change.relabelled_edge.old_label.list);
           break;

      case REMARKED_NODE:
           change.remarked_node.node->in_stack--;
           break;

      case REMARKED_EDGE:
           change.remarked_edge.edge->in_stack--;
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

void freeGraphChangeStack(void)
{
   if(graph_change_stack == NULL) return;
   #ifndef LIST_HASHING
      discardChanges(0);
   #endif
   free(graph_change_stack->stack);
   free(graph_change_stack);
}
