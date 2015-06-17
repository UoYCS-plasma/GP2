#include "graphStacks.h"

Graph **graph_stack = NULL;
int graph_stack_index = 0;

void copyGraph(Graph *graph)
{ 
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
      Node *node = getNode(graph_copy, index);
      /* The entry in the node array may be a dummy node, in which case nothing
       * needs to be done. This is tested by checking the node's index. */
      if(node->index >= 0)
      {
         Node *original_node = getNode(graph, index);
         copyLabel(&(original_node->label), &(node->label));
 
         /* If necessary, copy the edges arrays of the original node. */
         if(original_node->out_edges.items != NULL)
         {
            node->out_edges.items = calloc(node->out_edges.size, sizeof(int));
            if(node->out_edges.items == NULL)
            {
               print_to_log("Error: (copyGraph): malloc failure.\n");
               exit(1);
            }
            memcpy(node->out_edges.items, original_node->out_edges.items,
                   node->out_edges.size * sizeof(int));
         }
         if(original_node->in_edges.items != NULL)
         {
            node->in_edges.items = calloc(node->in_edges.size, sizeof(int));
            if(node->in_edges.items == NULL)
            {
               print_to_log("Error: (copyGraph): malloc failure.\n");
               exit(1);
            }
            memcpy(node->in_edges.items, original_node->in_edges.items,
                   node->in_edges.size * sizeof(int));
         }
         /* Populate the root nodes list. */
         if(node->root) addRootNode(graph_copy, node->index);
      }
   }

   /* Update the labels of each edge by copying the label from the 
    * corresponding edge in the original graph. */
   for(index = 0; index < graph_copy->edges.size; index++)
   {
      Edge *edge = getEdge(graph_copy, index);
      /* The entry in the edge array may be a edge node, in which case nothing
       * needs to be done. This is tested by checking the edge's index. */
      if(edge->index >= 0)
      {
         Edge *original_edge = getEdge(graph, index);
         copyLabel(&(original_edge->label), &(edge->label));
      }
   }  
   graph_stack[graph_stack_index++] = graph_copy;
}

Graph *popGraphs(Graph *current_graph, int restore_point)
{
   if(graph_stack == NULL) return NULL;
   assert(graph_stack_index >= restore_point);

   if(graph_stack_index == restore_point) return current_graph;

   freeGraph(current_graph);
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
   if(graph_change_stack == NULL) makeGraphChangeStack(16);
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

void pushAddedNode(int index)
{
   GraphChange change;
   change.type = ADDED_NODE;
   change.added_node_index = index;
   pushGraphChange(change);
}
   
void pushAddedEdge(int index)
{
   GraphChange change;
   change.type = ADDED_EDGE;
   change.added_edge_index = index;
   pushGraphChange(change);
}

void pushRemovedNode(bool root, Label label)
{
   GraphChange change;
   change.type = REMOVED_NODE;
   change.removed_node.root = root;
   change.removed_node.label = label;
   pushGraphChange(change);
}

void pushRemovedEdge(Label label, int source, int target)
{
   GraphChange change;
   change.type = REMOVED_EDGE;
   change.removed_edge.label = label;
   change.removed_edge.source = source;
   change.removed_edge.target = target;
   pushGraphChange(change);
}

void pushRelabelledNode(int index, Label old_label)
{
   GraphChange change;
   change.type = RELABELLED_NODE;
   change.relabelled_node.index = index;
   change.relabelled_node.old_label = old_label;
   pushGraphChange(change);
}

void pushRelabelledEdge(int index, Label old_label)
{
   GraphChange change;
   change.type = RELABELLED_EDGE;
   change.relabelled_edge.index = index;
   change.relabelled_edge.old_label = old_label;
   pushGraphChange(change);
}

void pushChangedRootNode(int index)
{
   GraphChange change;
   change.type = CHANGED_ROOT_NODE; 
   change.changed_root_node_index = index;
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
              removeNode(graph, change.added_node_index, true);
              break;

         case ADDED_EDGE:
              removeEdge(graph, change.added_edge_index, true);
              break;

         case REMOVED_NODE:
              addNode(graph, change.removed_node.root, change.removed_node.label);
              break;

         case REMOVED_EDGE:
              addEdge(graph, change.removed_edge.label, change.removed_edge.source, 
                      change.removed_edge.target);
              break; 

         case RELABELLED_NODE:
              relabelNode(graph, change.relabelled_node.index, 
                          change.relabelled_node.old_label, true);
              break;

         case RELABELLED_EDGE:
              relabelEdge(graph, change.relabelled_edge.index,
                          change.relabelled_edge.old_label, true);
              break;

         case CHANGED_ROOT_NODE:
              changeRoot(graph, change.changed_root_node_index);
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
           freeLabel(change.removed_node.label);
           break;

      case REMOVED_EDGE:
           freeLabel(change.removed_edge.label);
           break;

      case RELABELLED_NODE: 
           freeLabel(change.relabelled_node.old_label);
           break;

      case RELABELLED_EDGE: 
           freeLabel(change.relabelled_edge.old_label);
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
   discardChanges(0);
   free(graph_change_stack->stack);
   free(graph_change_stack);
}

