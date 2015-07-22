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

void pushAddedNode(int index, bool hole_filled)
{
   GraphChange change;
   change.type = ADDED_NODE;
   change.added_node.index = index;
   change.added_node.hole_filled = hole_filled;
   pushGraphChange(change);
}
   
void pushAddedEdge(int index, bool hole_filled)
{
   GraphChange change;
   change.type = ADDED_EDGE;
   change.added_edge.index = index;
   change.added_edge.hole_filled = hole_filled;
   pushGraphChange(change);
}

void pushRemovedNode(bool root, HostLabel label, int index, bool hole_created)
{
   GraphChange change;
   change.type = REMOVED_NODE;
   change.removed_node.root = root;
   change.removed_node.label = label;
   /* Keep a record of the list as the removal of the node could free this list
    * or remove its bucket from the hash table. */
   #ifdef LIST_HASHING
      addHostList(label.list);
   #else
      change.removed_node.label.list = copyHostList(label.list);
   #endif
   change.removed_node.index = index;
   change.removed_node.hole_created = hole_created;
   pushGraphChange(change);
}

void pushRemovedEdge(HostLabel label, int source, int target, int index, bool hole_created)
{
   GraphChange change;
   change.type = REMOVED_EDGE;
   change.removed_edge.label = label;
   /* Keep a record of the list as the removal of the node could free this list
    * or remove its bucket from the hash table. */
   #ifdef LIST_HASHING
      addHostList(label.list);
   #else
      change.removed_edge.label.list = copyHostList(label.list);
   #endif
   change.removed_edge.source = source;
   change.removed_edge.target = target;
   change.removed_edge.index = index;
   change.removed_edge.hole_created = hole_created;
   pushGraphChange(change);
}

void pushRelabelledNode(int index, HostLabel old_label)
{
   GraphChange change;
   change.type = RELABELLED_NODE;
   change.relabelled_node.index = index;
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

void pushRelabelledEdge(int index, HostLabel old_label)
{
   GraphChange change;
   change.type = RELABELLED_EDGE;
   change.relabelled_edge.index = index;
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

void pushRemarkedNode(int index, MarkType old_mark)
{
   GraphChange change;
   change.type = REMARKED_NODE;
   change.remarked_node.index = index;
   change.remarked_node.old_mark = old_mark;
   pushGraphChange(change);
}

void pushRemarkedEdge(int index, MarkType old_mark)
{
   GraphChange change;
   change.type = REMARKED_EDGE;
   change.remarked_edge.index = index;
   change.remarked_edge.old_mark = old_mark;
   pushGraphChange(change);
}

void pushChangedRootNode(int index)
{
   GraphChange change;
   change.type = CHANGED_ROOT_NODE; 
   change.changed_root_index = index;
   pushGraphChange(change);
}
  
/* The reversal of addition and removal of graph items is done manually as opposed
 * to calling the appropriate graph modification functions. This is because, due to
 * the management of holes in the graph's arrays, calling the normal functions is
 * not guaranteed to preserve the exact graph data structure as it was before the
 * modifications were made. */
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
         {
              int index = change.added_node.index;
              Node *node = getNode(graph, index);  

              if(node->out_edges.items != NULL) free(node->out_edges.items);
              if(node->in_edges.items != NULL) free(node->in_edges.items); 
              if(node->root) removeRootNode(graph, index);
              removeHostList(node->label.list);

              if(change.added_node.hole_filled) 
                 graph->nodes.holes.items[graph->nodes.holes.size++] = index;
              else graph->nodes.size--;

              graph->nodes.items[index] = dummy_node;
              graph->number_of_nodes--;
              break;
         }

         case ADDED_EDGE:
         {
              int index = change.added_edge.index;
              Edge *edge = getEdge(graph, index);

              Node *source = getNode(graph, edge->source);
              if(source->first_out_edge == index) source->first_out_edge = -1;
              else if(source->second_out_edge == index) source->second_out_edge = -1;
              else removeFromIntArray(&(source->out_edges), index);
              source->outdegree--;

              Node *target = getNode(graph, edge->target);
              if(target->first_in_edge == index) target->first_in_edge = -1;
              else if(target->second_in_edge == index) target->second_in_edge = -1;
              else removeFromIntArray(&(target->in_edges), index);
              target->indegree--;
              removeHostList(edge->label.list);

              if(change.added_edge.hole_filled)
                 graph->edges.holes.items[graph->edges.holes.size++] = index;
              else graph->edges.size--;

              graph->edges.items[index] = dummy_edge;
              graph->number_of_edges--;
              break;
         }
         case REMOVED_NODE:
         {
              Node node;
              node.index = change.removed_node.index;
              node.root = change.removed_node.root;
              node.label = change.removed_node.label;
              node.first_out_edge = -1;
              node.second_out_edge = -1;
              node.first_in_edge = -1;
              node.second_in_edge = -1;
              node.out_edges = makeIntArray(0);
              node.in_edges = makeIntArray(0);
              node.outdegree = 0;
              node.indegree = 0;

              graph->nodes.items[change.removed_node.index] = node;
              /* If the removal of the node created a hole, manually remove it from
               * the holes array. */
              if(change.removed_node.hole_created)
              {
                 graph->nodes.holes.size--;
                 graph->nodes.holes.items[graph->nodes.holes.size] = -1;
              }
              else graph->nodes.size++;
              if(node.root) addRootNode(graph, change.removed_node.index);
              graph->number_of_nodes++;
              break;
         }
         case REMOVED_EDGE:
         {
              Edge edge;
              edge.index = change.removed_edge.index;
              edge.label = change.removed_edge.label;
              edge.source = change.removed_edge.source;
              edge.target = change.removed_edge.target;
 
              int index = change.removed_edge.index;
              graph->edges.items[index] = edge;

              Node *source = getNode(graph, change.removed_edge.source);
              assert(source != NULL);
              if(source->first_out_edge == -1) source->first_out_edge = index;
              else if(source->second_out_edge == -1) source->second_out_edge = index;
              else addToIntArray(&(source->out_edges), index);
              source->outdegree++;

              Node *target = getNode(graph, change.removed_edge.target);
              assert(target != NULL);
              if(target->first_in_edge == -1) target->first_in_edge = index;
              else if(target->second_in_edge == -1) target->second_in_edge = index;
              else addToIntArray(&(target->in_edges), index);
              target->indegree++;
              /* If the removal of the edge created a hole, manually remove it from
               * the holes array. */
              if(change.removed_edge.hole_created)
              {
                 graph->edges.holes.size--;
                 graph->edges.holes.items[graph->edges.holes.size] = -1;
              }
              else graph->edges.size++;
              graph->number_of_edges++;
              break;
         }
         case RELABELLED_NODE:
              relabelNode(graph, change.relabelled_node.index, change.relabelled_node.old_label);
              break;

         case RELABELLED_EDGE:
              relabelEdge(graph, change.relabelled_edge.index, change.relabelled_edge.old_label);
              break;

         case REMARKED_NODE:
              changeNodeMark(graph, change.remarked_node.index, change.remarked_node.old_mark);
              break;

         case REMARKED_EDGE:
              changeEdgeMark(graph, change.remarked_edge.index, change.remarked_edge.old_mark);
              break;

         case CHANGED_ROOT_NODE:
              changeRoot(graph, change.changed_root_index);
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
      case ADDED_EDGE:
      case CHANGED_ROOT_NODE:
           break;

      case REMOVED_NODE:
           removeHostList(change.removed_node.label.list);
           break;

      case REMOVED_EDGE:
           removeHostList(change.removed_edge.label.list);
           break;

      case RELABELLED_NODE: 
           removeHostList(change.relabelled_node.old_label.list);
           break;

      case RELABELLED_EDGE: 
           removeHostList(change.relabelled_edge.old_label.list);
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


Graph **graph_stack = NULL;
int graph_stack_index = 0;
int graph_copy_count = 0;

void copyGraph(Graph *graph)
{ 
   printf("Copying host graph.\n");
   if(graph_stack_index == GRAPH_STACK_SIZE)
   {
      print_to_log("Error: copyGraph called with a full graph stack.\n");
      return;
   }
   if(graph_stack == NULL) graph_stack = calloc(GRAPH_STACK_SIZE, sizeof(Graph*));
   if(graph_stack == NULL)
   {
      print_to_log("Error (copyGraph): malloc failure.\n");
      exit(1);
   }
   Graph *graph_copy = newGraph(graph->nodes.capacity, graph->edges.capacity); 

   graph_copy->nodes.size = graph->nodes.size;
   graph_copy->nodes.capacity = graph->nodes.capacity;
   memcpy(graph_copy->nodes.items, graph->nodes.items, graph->nodes.capacity * sizeof(Node));

   graph_copy->edges.size = graph->edges.size;
   graph_copy->edges.capacity = graph->edges.capacity;
   memcpy(graph_copy->edges.items, graph->edges.items, graph->edges.capacity * sizeof(Edge));

   /* newGraph allocates an initial holes array of size 16. This may be smaller
    * then the holes array in the original graph. */
   if(graph_copy->nodes.holes.capacity < graph->nodes.holes.capacity)
   {
      free(graph_copy->nodes.holes.items);
      graph_copy->nodes.holes.items = calloc(graph->nodes.holes.capacity, sizeof(int));
      if(graph_copy->nodes.holes.items == NULL)
      {
         print_to_log("Error (copyGraph): malloc failure.\n");
         exit(1);
      }
   }
   graph_copy->nodes.holes.size = graph->nodes.holes.size;
   graph_copy->nodes.holes.capacity = graph->nodes.holes.capacity;
   memcpy(graph_copy->nodes.holes.items, graph->nodes.holes.items,
          graph->nodes.holes.capacity * sizeof(int));

   if(graph_copy->edges.holes.capacity < graph->edges.holes.capacity)
   {
      free(graph_copy->edges.holes.items);
      graph_copy->edges.holes.items = calloc(graph->edges.holes.capacity, sizeof(int));
      if(graph_copy->edges.holes.items == NULL)
      {
         print_to_log("Error (copyGraph): malloc failure.\n");
         exit(1);
      }
   }
   graph_copy->edges.holes.size = graph->edges.holes.size;
   graph_copy->edges.holes.capacity = graph->edges.holes.capacity;
   memcpy(graph_copy->edges.holes.items, graph->edges.holes.items,
          graph->edges.holes.capacity * sizeof(int));
   
   graph_copy->number_of_nodes = graph->number_of_nodes;
   graph_copy->number_of_edges = graph->number_of_edges;
   graph_copy->root_nodes = NULL;
 
   int index;
   for(index = 0; index < graph_copy->nodes.size; index++)
   {
      Node *node_copy = getNode(graph_copy, index);
      /* The entry in the node array may be a dummy node, in which case nothing
       * needs to be done. This is tested by checking the node's index. */
      if(node_copy->index >= 0)
      {
         Node *node = getNode(graph, index);
         /* If necessary, copy the edges arrays of the original node. */
         if(node->out_edges.items != NULL)
         {
            node_copy->out_edges.items = calloc(node_copy->out_edges.size, sizeof(int));
            if(node_copy->out_edges.items == NULL)
            {
               print_to_log("Error: (copyGraph): malloc failure.\n");
               exit(1);
            }
            memcpy(node_copy->out_edges.items, node->out_edges.items,
                   node_copy->out_edges.size * sizeof(int));
         }
         if(node->in_edges.items != NULL)
         {
            node_copy->in_edges.items = calloc(node_copy->in_edges.size, sizeof(int));
            if(node_copy->in_edges.items == NULL)
            {
               print_to_log("Error: (copyGraph): malloc failure.\n");
               exit(1);
            }
            memcpy(node_copy->in_edges.items, node->in_edges.items,
                   node_copy->in_edges.size * sizeof(int));
         }
         /* Populate the root nodes list. */
         if(node_copy->root) addRootNode(graph_copy, node_copy->index);
         #ifdef LIST_HASHING
            addHostList(node->label.list);
         #else
            node_copy->label.list = copyHostList(node->label.list);
         #endif
      }
   }
   for(index = 0; index < graph_copy->edges.size; index++)
   {
      Edge *edge_copy = getEdge(graph_copy, index);
      if(edge_copy->index >= 0)
      {
         HostLabel label = getEdgeLabel(graph, index);
         #ifdef LIST_HASHING
            addHostList(label.list);
         #else
            edge_copy->label.list = copyHostList(label.list);
         #endif
      }
   }
   graph_stack[graph_stack_index++] = graph_copy;
   graph_copy_count++;
}

Graph *revertGraph(Graph *current_graph, int restore_point)
{
   printf("Popping copy of host graph.\n");
   if(graph_stack == NULL) return NULL;
   assert(graph_stack_index >= restore_point);
   if(graph_stack_index == restore_point) return current_graph;
   else freeGraph(current_graph);

   Graph *graph = NULL;
   while(graph_stack_index > restore_point)
   { 
      graph = graph_stack[--graph_stack_index];
      /* Free graphs between the passed restore point and the top stack entry. */
      if(graph_stack_index > restore_point) freeGraph(graph);
   }
   return graph;
}

void discardGraphs(int depth)
{
   if(graph_stack == NULL) return;
   while(graph_stack_index > depth) freeGraph(graph_stack[--graph_stack_index]);
}

void freeGraphStack(void)
{
   if(graph_stack == NULL) return;
   discardGraphs(0);
   free(graph_stack);
}

