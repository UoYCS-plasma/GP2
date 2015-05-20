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
   Graph *graph_copy = newGraph(graph->node_pool_size, graph->edge_pool_size); 

   /* The node array, edge array, free node slots array, free edge slots array,
    * and the label class lists are copied from the original graph. The original
    * graph may also point to heap memory through the items pointers of its
    * label class lists. These are inspected in the for loop below the copying
    * of nodes_by_label and edges_by_label. */
   memcpy(graph_copy->nodes, graph->nodes, graph->node_pool_size * sizeof(Node));
   memcpy(graph_copy->edges, graph->edges, graph->edge_pool_size * sizeof(Edge));

   memcpy(graph_copy->free_node_slots, graph->free_node_slots,
          graph->node_pool_size * sizeof(int));
   memcpy(graph_copy->free_edge_slots, graph->free_edge_slots, 
          graph->edge_pool_size * sizeof(int));
   
   graph_copy->free_node_index = graph->free_node_index;
   graph_copy->free_edge_index = graph->free_edge_index;

   graph_copy->node_index = graph->node_index;
   graph_copy->edge_index = graph->edge_index;

   graph_copy->number_of_nodes = graph->number_of_nodes;
   graph_copy->number_of_edges = graph->number_of_edges;

   graph_copy->node_classes = copyLabelClassTable(graph->node_classes); 
   graph_copy->edge_classes = copyLabelClassTable(graph->edge_classes); 
   graph_copy->root_nodes = NULL;
 
   int index;
   /* For each node, make a copy of its label and, if necessary, make a copy
    * of its extra edges arrays. */
   for(index = 0; index < graph_copy->node_index; index++)
   {
      Node *node = getNode(graph_copy, index);
      /* The entry in the node array may be a dummy node, in which case nothing
       * needs to be done. This is tested by checking the node's index. */
      if(node->index >= 0)
      {
         Node *original_node = getNode(graph, index);
         copyLabel(&(original_node->label), &(node->label));
 
         /* If necessary, copy the edges arrays of the original node. */
         if(original_node->out_edges != NULL)
         {
            node->out_edges = calloc(node->out_pool_size, sizeof(int));
            if(node->out_edges == NULL)
            {
               print_to_log("Error: (copyGraph): malloc failure.\n");
               exit(1);
            }
            memcpy(node->out_edges, original_node->out_edges,
                   node->out_pool_size * sizeof(int));
         }
         if(original_node->in_edges != NULL)
         {
            node->in_edges = calloc(node->in_pool_size, sizeof(int));
            if(node->in_edges == NULL)
            {
               print_to_log("Error: (copyGraph): malloc failure.\n");
               exit(1);
            }
            memcpy(node->in_edges, original_node->in_edges,
                   node->in_pool_size * sizeof(int));
         }
         /* Populate the root nodes list. */
         if(node->root) addRootNode(graph_copy, node->index);
      }
   }

   /* Update the labels of each edge by copying the label from the 
    * corresponding edge in the original graph. */
   for(index = 0; index < graph_copy->edge_index; index++)
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
   if(graph_stack_index < restore_point)
   {
      print_to_log("popGraphs called with restore point greater than the top "
                   "of the graph stack.\n");
      return current_graph;
   }
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


GraphChange *graph_change_stack = NULL;
int graph_change_index = 0;

bool validGraphChangeStack(void)
{
   if(graph_change_stack == NULL)
   {
      graph_change_stack = calloc(GRAPH_CHANGE_STACK_SIZE, sizeof(GraphChange));
      return true;
   }
   if(graph_change_index == GRAPH_CHANGE_STACK_SIZE)
   {
      print_to_log("Error: Push to graph change stack attempted with a full "
                   "graph change stack.\n");
      return false;
   }
   return true;
}
 
void pushAddedNode(int index)
{
   if(!validGraphChangeStack()) return;
   int gci = graph_change_index++; 
   graph_change_stack[gci].type = ADDED_NODE;
   graph_change_stack[gci].data.added_node_index = index;
}
   
void pushAddedEdge(int index)
{
   if(!validGraphChangeStack()) return;
   int gci = graph_change_index++; 
   graph_change_stack[gci].type = ADDED_EDGE;
   graph_change_stack[gci].data.added_edge_index = index;
}

void pushRemovedNode(bool root, Label label)
{
   if(!validGraphChangeStack()) return;
   int gci = graph_change_index++; 
   graph_change_stack[gci].type = REMOVED_NODE;
   graph_change_stack[gci].data.removed_node.root = root;
   graph_change_stack[gci].data.removed_node.label = label;
}

void pushRemovedEdge(bool bidirectional, Label label, int source, int target)
{
   if(!validGraphChangeStack()) return;
   int gci = graph_change_index++; 
   graph_change_stack[gci].type = REMOVED_EDGE;
   graph_change_stack[gci].data.removed_edge.bidirectional = bidirectional;
   graph_change_stack[gci].data.removed_edge.label = label;
   graph_change_stack[gci].data.removed_edge.source = source;
   graph_change_stack[gci].data.removed_edge.target = target;
}

void pushRelabelledNode(int index, bool change_flag, Label old_label)
{
   if(!validGraphChangeStack()) return;
   int gci = graph_change_index++; 
   graph_change_stack[gci].type = RELABELLED_NODE;
   graph_change_stack[gci].data.relabelled_node.index = index;
   graph_change_stack[gci].data.relabelled_node.change_flag = change_flag;
   graph_change_stack[gci].data.relabelled_node.old_label = old_label;
}

void pushRelabelledEdge(int index, Label old_label)
{
   if(!validGraphChangeStack()) return;
   int gci = graph_change_index++; 
   graph_change_stack[gci].type = RELABELLED_NODE;
   graph_change_stack[gci].data.relabelled_edge.index = index;
   graph_change_stack[gci].data.relabelled_edge.change_flag = false;
   graph_change_stack[gci].data.relabelled_edge.old_label = old_label;
}
   
void undoChanges(Graph *graph, int restore_point)
{ 
   if(graph_change_stack == NULL) return; 
   while(graph_change_index > restore_point)
   { 
      GraphChange change = graph_change_stack[--graph_change_index];
      switch(change.type)
      {
         case ADDED_NODE:
              removeNode(graph, change.data.added_node_index, true);
              break;

         case ADDED_EDGE:
              removeEdge(graph, change.data.added_edge_index, true);
              break;

         case REMOVED_NODE:
              addNode(graph, change.data.removed_node.root,
                      change.data.removed_node.label);
              break;

         case REMOVED_EDGE:
              addEdge(graph, change.data.removed_edge.bidirectional,
                      change.data.removed_edge.label, 
                      change.data.removed_edge.source, 
                      change.data.removed_edge.target);
              break; 

         case RELABELLED_NODE:
         {
              int index = change.data.relabelled_node.index;
              relabelNode(graph, index, change.data.relabelled_node.old_label, true);
              if(change.data.relabelled_node.change_flag) changeRoot(graph, index);           
              break;
         }
         case RELABELLED_EDGE:
         {
              int index = change.data.relabelled_edge.index;
              relabelEdge(graph, index, change.data.relabelled_edge.old_label, true);
              if(change.data.relabelled_edge.change_flag) 
                 changeBidirectional(graph, index);
              break;
         }
         default: 
              print_to_log("Error (restoreGraph): Unexepected change type %d.\n",
                           change.type); 
              break;
      }
   } 
} 

void discardChanges(int restore_point)
{
   if(graph_change_stack == NULL) return;
   while(graph_change_index > restore_point)
      freeGraphChange(graph_change_stack[--graph_change_index]);
} 

void freeGraphChange(GraphChange change)
{
   switch(change.type)
   {
      case ADDED_NODE:
      case ADDED_EDGE:
           break;

      case REMOVED_NODE: freeLabel(change.data.removed_node.label);
           break;

      case REMOVED_EDGE: freeLabel(change.data.removed_edge.label);
           break;

      case RELABELLED_NODE:
      case RELABELLED_EDGE: freeLabel(change.data.relabelled_edge.old_label);
           break;

      default: 
           print_to_log("Error (freeGraphChange): Unexepected graph change "
                        "type %d.\n",change.type); 
           break;      
   }  
} 

void freeGraphChangeStack(void)
{
   if(graph_change_stack == NULL) return;
   discardChanges(0);
   free(graph_change_stack);
}

