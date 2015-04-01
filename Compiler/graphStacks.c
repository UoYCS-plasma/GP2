#include "graphStacks.h"

Stack *graph_stack = NULL;

void copyGraph(Graph *graph, int depth)
{
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

   memcpy(graph_copy->nodes_by_label, graph->nodes_by_label, sizeof(LabelClassTable));
   memcpy(graph_copy->edges_by_label, graph->edges_by_label, sizeof(LabelClassTable));

   int index;
   for(index = 0; index < LABEL_CLASSES; index++)
   {
      /* If the pool size is greater than 0, allocate memory to the items 
       * array of the copied LabelClassTable and copy the corresponding array
       * from the original graph. */
      LabelClassTable *list = &graph_copy->nodes_by_label[index];
      if(list->pool_size > 0)
      {
         list->items = calloc(list->pool_size, sizeof(int));
         if(list->items == NULL)
         {
            print_to_log("Error: Memory exhausted during graph copying.\n");
            exit(1);
         }
         memcpy(list->items, graph->nodes_by_label[index].items, 
                list->pool_size * sizeof(int));
      }
      list = &graph_copy->edges_by_label[index];
      if(list->pool_size > 0)
      {
         list->items = calloc(list->pool_size, sizeof(int));
         if(list->items == NULL)
         {
            print_to_log("Error: Memory exhausted during graph copying.\n");
            exit(1);
         }
         memcpy(list->items, graph->edges_by_label[index].items, 
                list->pool_size * sizeof(int));
      }
   }
   graph_copy->root_nodes = NULL;

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
         node->label = copyLabel(original_node->label);
 
         /* If necessary, copy the extra edges arrays of the original node. */
         if(original_node->extra_out_edges != NULL)
         {
            node->extra_out_edges = calloc(node->out_pool_size, sizeof(int));
            if(node->extra_out_edges == NULL)
            {
               print_to_log("Error: Memory exhausted during graph copying.\n");
               exit(1);
            }
            memcpy(node->extra_out_edges, original_node->extra_out_edges,
                   node->out_pool_size * sizeof(int));
         }
         if(original_node->extra_in_edges != NULL)
         {
            node->extra_in_edges = calloc(node->in_pool_size, sizeof(int));
            if(node->extra_in_edges == NULL)
            {
               print_to_log("Error: Memory exhausted during graph copying.\n");
               exit(1);
            }
            memcpy(node->extra_in_edges, original_node->extra_in_edges,
                   (node->in_pool_size - MAX_INCIDENT_EDGES) * sizeof(int));
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
         edge->label = copyLabel(original_edge->label);
      }
   }  
 
   if(graph_stack == NULL) graph_stack = newStack(GRAPH_STACK_SIZE);
   StackData data;
   data.graph = graph_copy;
   /* If there is a graph at the passed depth, pop it and replace it with the
    * copied graph. */
   if(depth == graph_stack->top - 1)
   {
      StackData old_data = pop(graph_stack);
      freeGraph(old_data.graph);
   }
   push(graph_stack, data);
}

Graph *restoreGraph(Graph *graph, int restore_point)
{
   if(graph_stack->top == restore_point) return graph;
   freeGraph(graph);
   while(graph_stack->top > restore_point)
   { 
      /* Pop decrements graph_stack->top. */
      StackData data = pop(graph_stack);
      if(graph_stack->top == restore_point) return data.graph;
      else freeGraph(data.graph);
   }
   printf("Error! Restore Point %d, Stack Top %d.\n", restore_point, 
          graph_stack->top);
   exit(1);
}

void freeGraphStack(Stack *graph_stack)
{
   int count;
   for(count = 0; count < graph_stack->top; count++)
      if(graph_stack->data[count].graph) 
         freeGraph(graph_stack->data[count].graph);
   freeStack(graph_stack);
}

Stack *graph_change_stack = NULL;

GraphChange *newGraphChange(void)
{
   GraphChange *change = malloc(sizeof(GraphChange));
   if(change == NULL)
   {
      print_to_log("Error (newGraphChange): memory allocation failed.\n");
      exit(1);
   }
   return change;
}

void pushAddedNode(int index)
{
   GraphChange *change = newGraphChange();
   change->type = ADDED_NODE;
   change->data.added_node_index = index;
   if(graph_change_stack == NULL) 
      graph_change_stack = newStack(GRAPH_CHANGE_STACK_SIZE);
   StackData data;
   data.graph_change = change;
   push(graph_change_stack, data);
}
   
void pushAddedEdge(int index)
{
   GraphChange *change = newGraphChange();
   change->type = ADDED_EDGE;
   change->data.added_edge_index = index;
   if(graph_change_stack == NULL) 
      graph_change_stack = newStack(GRAPH_CHANGE_STACK_SIZE);
   StackData data;
   data.graph_change = change;
   push(graph_change_stack, data);
}

void pushRemovedNode(bool root, Label *label)
{
   GraphChange *change = newGraphChange();
   change->type = REMOVED_NODE;
   change->data.removed_node.root = root;
   change->data.removed_node.label = label;
   if(graph_change_stack == NULL) 
      graph_change_stack = newStack(GRAPH_CHANGE_STACK_SIZE);
   StackData data;
   data.graph_change = change;
   push(graph_change_stack, data);
}

void pushRemovedEdge(bool bidirectional, Label *label, int source, int target)
{
   GraphChange *change = newGraphChange();
   change->type = REMOVED_EDGE;
   change->data.removed_edge.bidirectional = bidirectional;
   change->data.removed_edge.label = label;
   change->data.removed_edge.source = source;
   change->data.removed_edge.target = target;
   if(graph_change_stack == NULL) 
      graph_change_stack = newStack(GRAPH_CHANGE_STACK_SIZE);
   StackData data;
   data.graph_change = change;
   push(graph_change_stack, data);
}

void pushRelabelledNode(int index, bool change_flag, Label *old_label)
{
   GraphChange *change = newGraphChange();
   change->type = RELABELLED_NODE;
   change->data.relabelled_node.index = index;
   change->data.relabelled_node.change_flag = change_flag;
   change->data.relabelled_node.old_label = old_label;
   if(graph_change_stack == NULL) 
      graph_change_stack = newStack(GRAPH_CHANGE_STACK_SIZE);
   StackData data;
   data.graph_change = change;
   push(graph_change_stack, data);
}

void pushRelabelledEdge(int index, bool change_flag, Label *old_label)
{
   GraphChange *change = newGraphChange();
   change->type = RELABELLED_NODE;
   change->data.relabelled_edge.index = index;
   change->data.relabelled_edge.change_flag = change_flag;
   change->data.relabelled_edge.old_label = old_label;
   if(graph_change_stack == NULL) 
      graph_change_stack = newStack(GRAPH_CHANGE_STACK_SIZE);
   StackData data;
   data.graph_change = change;
   push(graph_change_stack, data);
}
 
void rollBackGraph(Graph *graph)
{
   while(graph_change_stack->top > 0)
   {
      StackData data = pop(graph_change_stack);
      GraphChange *change = data.graph_change;
      switch(change->type)
      {
         case ADDED_NODE:
              removeNode(graph, change->data.added_node_index);
              break;

         case ADDED_EDGE:
              removeEdge(graph, change->data.added_edge_index);
              break;

         case REMOVED_NODE:
              addNode(graph, change->data.removed_node.root,
                      change->data.removed_node.label);
              break;

         case REMOVED_EDGE:
              addEdge(graph, change->data.removed_edge.bidirectional,
                      change->data.removed_edge.label, 
                      change->data.removed_edge.source, 
                      change->data.removed_edge.target);
              break; 

         case RELABELLED_NODE:
              relabelNode(graph, change->data.relabelled_node.index,
                          change->data.relabelled_node.old_label,
                          change->data.relabelled_node.change_flag);           
              break;

         case RELABELLED_EDGE:
              relabelEdge(graph, change->data.relabelled_edge.index,
                          change->data.relabelled_edge.old_label,
                          change->data.relabelled_edge.change_flag);           
              break;
              
         default: 
              print_to_log("Error (restoreGraph): Unexepected change type %d.\n",
                           change->type); 
              break;
      }
      free(change);
   } 
} 

void freeGraphChange(GraphChange *change)
{
   switch(change->type)
   {
      case ADDED_NODE:
      case ADDED_EDGE:
           break;

      case REMOVED_NODE:
           if(!isConstantLabel(change->data.removed_node.label))
             freeLabel(change->data.removed_node.label);
           break;

      case REMOVED_EDGE:
           if(!isConstantLabel(change->data.removed_edge.label))
             freeLabel(change->data.removed_edge.label);
           break;

      case RELABELLED_NODE:
      case RELABELLED_EDGE:
           if(!isConstantLabel(change->data.relabelled_edge.old_label)) 
              freeLabel(change->data.relabelled_edge.old_label);
           if(change) free(change);           
           break;

      default: 
           print_to_log("Error (freeGraphChange): Unexepected graph change "
                        "type %d.\n",change->type); 
           break;      
   }  
} 

void freeGraphChangeStack(Stack *graph_change_stack)
{
   int count;
   for(count = 0; count < graph_stack->top; count++)
      if(graph_stack->data[count].graph) 
         freeGraphChange(graph_stack->data[count].graph_change);
   freeStack(graph_change_stack);
}

