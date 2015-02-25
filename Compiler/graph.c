#include "graph.h"

Node dummy_node = {-1, false, EMPTY_L, NULL, {-1}, {-1}, NULL, NULL, 0, 0, 0, 0, 0, 0, 0};
Edge dummy_edge = {-1, false, EMPTY_L, NULL, -1, -1};

Graph *newGraph(int nodes, int edges) 
{
    int index;

    Graph *graph = malloc(sizeof(Graph));
    if(graph == NULL) 
    {
      printf("Memory exhausted during graph construction.\n");
      exit(1);
    }
    graph->nodes = calloc(nodes, sizeof(Node));
    if(graph->nodes == NULL)
    {
      print_to_log("Memory exhausted during graph construction.\n");
      exit(1);
    }
    graph->free_node_slots = calloc(nodes, sizeof(int));
    if(graph->free_node_slots == NULL)
    {
      print_to_log("Memory exhausted during graph construction.\n");
      exit(1);
    }
    for(index = 0; index < nodes; index ++)
    {
       graph->nodes[index] = dummy_node;
       graph->free_node_slots[index] = -1;
    }
 
    graph->edges = calloc(edges, sizeof(Edge));
    if(graph->edges == NULL)
    {
      print_to_log("Memory exhausted during graph construction.\n");
      exit(1);
    }
    graph->free_edge_slots = calloc(edges, sizeof(int));
    if(graph->free_edge_slots == NULL)
    {
      print_to_log("Memory exhausted during graph construction.\n");
      exit(1);
    }
    for(index = 0; index < edges; index ++)
    {
       graph->edges[index] = dummy_edge;
       graph->free_edge_slots[index] = -1;
    }

    graph->node_pool_size = nodes;
    graph->edge_pool_size = edges;
    graph->free_node_index = 0;
    graph->free_edge_index = 0;
    graph->node_index = 0;
    graph->edge_index = 0;
    graph->number_of_nodes = 0;
    graph->number_of_edges = 0;

    for(index = 0; index < LABEL_CLASSES; index++)
    {
       graph->nodes_by_label[index].pool_size = 0;  
       graph->nodes_by_label[index].index = 0;  
       graph->nodes_by_label[index].items = NULL;  

       graph->edges_by_label[index].pool_size = 0;  
       graph->edges_by_label[index].index = 0;  
       graph->edges_by_label[index].items = NULL;
    }
    graph->root_nodes = NULL;
    return graph;
}

int addNode(Graph *graph, bool root, Label *label) 
{
   /* Get the index of the new node by examining the free node slots array.
    * A non-negative array element is the index of a hole in the graph's node
    * array. If there are no free slots, use the graph's node index. */
   int index = graph->free_node_slots[graph->free_node_index];
   if(index == -1) 
   {
      index = graph->node_index++;
      if(index >= graph->node_pool_size) 
      {
         graph->node_pool_size *= 2;
         graph->nodes = realloc(graph->nodes, graph->node_pool_size * sizeof(Node));
         if(graph->nodes == NULL)
         {
            print_to_log("Memory exhausted during node array allocation.\n");
            exit(1);
         }
         graph->free_node_slots = realloc(graph->free_node_slots,
                                          graph->node_pool_size * sizeof(Node));
         if(graph->free_node_slots == NULL)
         {
            print_to_log("Memory exhausted during node array allocation.\n");
            exit(1);
         }
      }
   }
   else 
   {
      graph->free_node_slots[graph->free_node_index] = -1;
      if(graph->free_node_index > 0) graph->free_node_index--;
   }

   graph->nodes[index].index = index;
   graph->nodes[index].root = root;
   graph->nodes[index].out_index = 0;
   graph->nodes[index].in_index = 0;
   graph->nodes[index].extra_out_edges = NULL;
   graph->nodes[index].extra_in_edges = NULL;
   graph->nodes[index].out_pool_size = 0;
   graph->nodes[index].in_pool_size = 0;
   graph->nodes[index].outdegree = 0;
   graph->nodes[index].indegree= 0;
   graph->nodes[index].bidegree= 0;
   
   int count;
   for(count = 0; count < MAX_INCIDENT_EDGES; count++)
   {
      graph->nodes[index].out_edges[count] = -1;
      graph->nodes[index].in_edges[count] = -1;
   }

   LabelClass label_class;
   if(label == NULL)
   {
      label_class = EMPTY_L;
      graph->nodes[index].label = &blank_label;
   }
   else
   { 
      label_class = getLabelClass(label);
      graph->nodes[index].label = label;
   }
   graph->nodes[index].label_class = label_class;
   addLabelClassIndex(&graph->nodes_by_label[label_class], index,
                      graph->node_pool_size);
    
   if(root) addRootNode(graph, index);
   graph->number_of_nodes++;
   return index; 
}

void addRootNode(Graph *graph, int index)
{
   RootNodes *root_node = malloc(sizeof(RootNodes));
   if(root_node == NULL)
   {
      print_to_log("Error: Memory exhausted during root node allocation.\n");
      exit(1);
   }
   root_node->index = index;
   root_node->next = graph->root_nodes;
   graph->root_nodes = root_node;
}

int addEdge(Graph *graph, bool bidirectional, Label *label, int source_index,
            int target_index) 
{
   /* Get the index of the new edge by examining the free edge slots array.
    * A non-negative array element is the index of a hole in the graph's edge
    * array. If there are no free slots, use the graph's edge index. */
   int index = graph->free_edge_slots[graph->free_edge_index];
   if(index == -1) 
   {
      index = graph->edge_index++;
      if(index >= graph->edge_pool_size) 
      {
         graph->edge_pool_size *= 2;
         graph->edges = realloc(graph->edges, 
                                graph->edge_pool_size * sizeof(Edge));
         if(graph->edges == NULL)
         {
            print_to_log("Memory exhausted during extra edge allocation.\n");
            exit(1);
         }
         graph->free_edge_slots = realloc(graph->free_edge_slots,
                                          graph->edge_pool_size * sizeof(Edge));
         if(graph->free_edge_slots == NULL)
         {
            print_to_log("Memory exhausted during extra edge allocation.\n");
            exit(1);
         }
      }
   }
   else
   {
      graph->free_edge_slots[graph->free_edge_index] = -1;
      if(graph->free_edge_index > 0) graph->free_edge_index--;
   }

   graph->edges[index].index = index;
   graph->edges[index].bidirectional = bidirectional;
   graph->edges[index].source = source_index;
   graph->edges[index].target = target_index;

   LabelClass label_class;
   if(label == NULL)
   {
      label_class = EMPTY_L;
      graph->edges[index].label = &blank_label;
   }
   else
   { 
      label_class = getLabelClass(label);
      graph->edges[index].label = label;
   }
   graph->edges[index].label_class = label_class;
   addLabelClassIndex(&graph->edges_by_label[label_class], index, 
                      graph->edge_pool_size);

   /* For the source's outedge store and the target's inedge store, do the
    * following:
    * (1) If the current index exceeds the bounds of the edge pointer array,
    *     grow the edge pointer store. 
    * (2) If the edge store has entered the extra_edges array, put
    *     the new edge there. Otherwise, put the new edge in the edges array. 
    * (3) Increment the source's out index and outdegree, or increment the
    *     target's out index and indegree. */
   Node *source = getNode(graph, source_index);
   if(source->out_index >= source->out_pool_size + MAX_INCIDENT_EDGES)
   {
      if(source->extra_out_edges == NULL) 
           source->out_pool_size = MAX_INCIDENT_EDGES;
      else source->out_pool_size *= 2;

      source->extra_out_edges = realloc(source->extra_out_edges,
                                        source->out_pool_size * sizeof(int));
      if(source->extra_out_edges == NULL)
      {
         print_to_log("Memory exhausted during extra incident outedge "
                      "allocation.\n");
         exit(1);
      }
   }
   if(source->extra_out_edges != NULL)
   {
      /* source->out_index includes the indices of the edges array.
       * Subtract the size of that array to get the correct index into
       * the extra edges array. */
      int out_index = source->out_index - MAX_INCIDENT_EDGES;
      source->extra_out_edges[out_index] = index;
   }
   else source->out_edges[source->out_index] = index;
   source->out_index++;
   if(bidirectional) source->bidegree++; else source->outdegree++;

   Node *target = getNode(graph, target_index);
   if(target->in_index >= target->in_pool_size + MAX_INCIDENT_EDGES)
   {
      if(target->extra_in_edges == NULL) 
           target->in_pool_size = MAX_INCIDENT_EDGES;
      else target->in_pool_size *=2;

      target->extra_in_edges = realloc(target->extra_in_edges,
                                       target->in_pool_size * sizeof(int));
      if(target->extra_in_edges == NULL)
      {
         print_to_log("Memory exhausted during extra incident inedge "
                      "allocation.\n");
         exit(1);
      }
   }
   if(target->extra_in_edges != NULL)
   {
      /* target->in_index includes the indices of the edges array.
       * Subtract the size of that array to get the correct index into
       * the extra edges array. */
      int in_index = target->in_index - MAX_INCIDENT_EDGES;
      target->extra_in_edges[in_index] = index;
   }
   else target->in_edges[target->in_index] = index;
   target->in_index++;
   if(bidirectional) target->bidegree++; else target->indegree++;

   graph->number_of_edges++;
   return index;
}

void addLabelClassIndex(LabelClassTable *table, int index, int initial_size)
{
   if(table->pool_size == 0)
   {
      table->pool_size = (initial_size <= 4) ? initial_size : initial_size / 4;
      table->items = calloc(table->pool_size, sizeof(int));
      if(table->items == NULL)
      {
         print_to_log("Error: Memory exhausted during label class indexing.\n");
         exit(1);
      }
   }
   else
   {
      if(table->index >= table->pool_size)
      {
         table->pool_size *= 2;
         table->items = realloc(table->items, table->pool_size * sizeof(int));
         if(table->items == NULL)
         {
            print_to_log("Error: Memory exhausted during label class indexing.\n");
            exit(1);
         }
      }
   }
   table->items[table->index++] = index;
}

void removeNode(Graph *graph, int index)
{   
    Node *node = getNode(graph, index);  
 
    if(node->indegree > 0 || node->outdegree > 0)
    {
       print_to_console("Error (removeNode): Cannot remove node (%d) with "
                        "incident edges.\n", node->index);
       print_to_log("Error (removeNode): Cannot remove node (%d) with "
                    "incident edges.\n", node->index);
       return;
    }
    /* Deallocate memory in the node structure. */
    if(node->label && node->label != &blank_label) freeLabel(node->label);
    if(node->extra_out_edges) free(node->extra_out_edges);
    if(node->extra_in_edges) free(node->extra_in_edges); 

    removeLabelClassIndex(&graph->nodes_by_label[node->label_class], index);
    if(node->root) removeRootNode(graph, index);
    
    graph->nodes[index] = dummy_node;
    graph->number_of_nodes--;

    /* If the node's index is the last index in the array, it is not 
     * necessary to create a free slot: only decrement node_index. */
    if(index == graph->node_index - 1) graph->node_index--;
    else
    {
       /* Only increment the free node index if the array entry at that index
        * is not -1. This is not the case when this array contains no free
        * slots. */
       if(graph->free_node_slots[graph->free_node_index] >= 0)
          graph->free_node_index++;
       graph->free_node_slots[graph->free_node_index] = index;
    }
}

void removeRootNode(Graph *graph, int index)
{
   RootNodes *current = graph->root_nodes, *previous = NULL;
   while(current != NULL)
   {
      if(current->index == index) 
      {
         if(previous == NULL) graph->root_nodes = current->next;
         else previous->next = current->next;
         free(current);
         break;
      }
      previous = current;
      current = current->next;
   }
}

void removeEdge(Graph *graph, int index) 
{
   Edge *edge = getEdge(graph, index);
   removeLabelClassIndex(&graph->edges_by_label[edge->label_class], index);
   if(edge->label && edge->label != &blank_label) freeLabel(edge->label);

   Node *source = getNode(graph, edge->source);
   Node *target = getNode(graph, edge->target);
   int counter;
   bool source_found = false, target_found = false;

   /* Search for the edge pointer in the out-edge array of the source node
    * and in the in-edge array of the target node. The bools source_found
    * and target_found control the for loop to minimise array checks. */
   for(counter = 0; counter < MAX_INCIDENT_EDGES; counter++)
   {
      if(!source_found)
      {
         if(source->out_edges[counter] == index) 
         {
            source->out_edges[counter] = -1;
            source_found = true;
            /* If the index of the removed edge directly precedes the source's
             * out-index, decrement the out-index until it refers to an array
             * element one place beyond the right-most -1 element. */
            if(counter == source->out_index - 1) 
            {
               source->out_index--;
               while(source->out_index > 0)
               {
                  if(source->out_edges[source->out_index - 1] == -1)
                     source->out_index--;
                  else break;
               }
            }
            if(target_found) break;
         }
      }
      if(!target_found)
      {
         if(target->in_edges[counter] == index) 
         {
            target->in_edges[counter] = -1;
            target_found = true;
            /* If the index of the removed edge directly precedes the target's
             * in-index, decrement the in-index until it refers to an array
             * element one place beyond the right-most -1 element. */
            if(counter == target->in_index - 1) 
            {
               target->in_index--;
               while(target->in_index > 0)
               {
                  if(target->in_edges[target->in_index - 1] == -1) 
                     target->in_index--;
                  else break;
               }
            }
            if(source_found) break;
         }
      }
   }
   /* If the source was not found in the normal store, search the extra out
    * edges array. */
   if(!source_found)
   {
      if(source->extra_out_edges == NULL)
      {
         print_to_log("Error (removeEdge): Outedge pointer not found.\n");
         return;
      }
      for(counter = 0; counter < source->out_index - MAX_INCIDENT_EDGES; counter++)
      if(source->extra_out_edges[counter] == index) 
      {
         source->extra_out_edges[counter] = -1;
         /* If the index of the removed edge directly precedes the source's
          * out-index, decrement the out-index until it refers to an array
          * element one place beyond the right-most -1 element. */
         if(counter == source->out_index - 1) 
         {
            source->out_index--;
            while(source->out_index > 0)
            {
               if(source->extra_out_edges[source->out_index - 1] == -1) 
                  source->out_index--;
               else break;
            }
         }
         break;
      }
   }
   /* ...and again for the target. */
   if(!target_found)
   {
      if(target->extra_in_edges == NULL)
      {
         print_to_log("Error (removeEdgee): Inedge pointer not found.\n");
         return;
      }
      for(counter = 0; counter < target->in_index - MAX_INCIDENT_EDGES; counter++)
      if(target->extra_in_edges[counter] == index) 
      {
         target->extra_in_edges[counter] = -1;
         /* If the index of the removed edge directly precedes the target's
          * in-index, decrement the in-index until it refers to an array
          * element one place beyond the right-most -1 element. */
         if(counter == target->in_index - 1) 
         {
            target->in_index--;
            while(target->in_index > 0)
            {
               if(target->extra_in_edges[target->in_index - 1] == -1) 
                  target->in_index--;
               else break;
               }
         }
         break;
      }
   }
   /* Rule graphs are not modified after their creation. Specifically,
    * removeEdge is never called on a rule graph, so there is no need to
    * check whether to decrement the bidegrees. */
   source->outdegree--;
   target->indegree--;

   graph->edges[index] = dummy_edge;
   graph->number_of_edges--;

   /* If the edge's index is the last index in the array, it is not 
    * necessary to create a free slot: only decrement edge_index. */
   if(index == graph->edge_index - 1) graph->edge_index--;
   else
   {
      /* Only increment the free edge index if the array entry at that index
       * is not -1. This is not the case when this array contains no free
       * slots. */
      if(graph->free_edge_slots[graph->free_edge_index] >= 0)
         graph->free_edge_index++;
      graph->free_edge_slots[graph->free_edge_index] = index;
   }
}

void removeLabelClassIndex(LabelClassTable *table, int index)
{
   int count;
   for(count = 0; count < table->index; count++)
   {
      if(table->items[count] == index)
      {
         table->items[count] = -1;
         /* If the index of the removed item directly precedes the last index,
          * decrement the index until it refers to an array element one place
          * beyond the right-most -1 element. */
         if(count == index - 1)
         {
            index--;
            while(index > 0)
            {
               if(table->items[index] == -1) index--;
               else break;
            }
         }
      }
   }
}

void relabelNode(Graph *graph, Node *node, Label *new_label, bool change_label,
                 bool change_root) 
{
   if(change_root)
   {
      if(node->root) removeRootNode(graph, node->index);
      else addRootNode(graph, node->index);
   }
   if(change_label == false) return;
   else
   {  
      if(node->label != &blank_label) freeLabel(node->label); 

      LabelClass new_label_class;
      if(new_label == NULL)
      {          
         node->label = &blank_label; 
         new_label_class = EMPTY_L;
      }
      else
      {
         node->label = new_label;
         new_label_class = getLabelClass(new_label);
      }
      /* If the label classes differ, update the graph's LabelClassTables. */
      if(node->label_class != new_label_class) 
      {
         addLabelClassIndex(&graph->nodes_by_label[new_label_class], 
                            node->index, graph->node_pool_size);
         removeLabelClassIndex(&graph->nodes_by_label[node->label_class], 
                               node->index);
      }   
   }
}

void relabelEdge(Graph *graph, Edge *edge, Label *new_label, 
                 bool change_label, bool change_bidirectional)
{		
   if(change_bidirectional) edge->bidirectional = !edge->bidirectional;
   if(change_label == false) return;
   else
   {
      if(edge->label != &blank_label) freeLabel(edge->label); 

      LabelClass new_label_class;
      if(new_label == NULL)
      {          
         edge->label = &blank_label; 
         new_label_class = EMPTY_L;
      }
      else
      {
         edge->label = new_label;
         new_label_class = getLabelClass(new_label);
      }
      /* If the label classes differ, update the graph's LabelClassTables. */
      if(edge->label_class != new_label_class) 
      {
         addLabelClassIndex(&graph->edges_by_label[new_label_class], 
                            edge->index, graph->edge_pool_size);
         removeLabelClassIndex(&graph->edges_by_label[edge->label_class], 
                               edge->index);
      }
   }
}

Stack *graph_stack = NULL;

void copyGraph(Graph *graph)
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
 
   if(graph_stack == NULL) graph_stack = newStack(STACK_SIZE);
   StackData data;
   data.graph = graph_copy;
   push(graph_stack, data);
}

Graph *restoreGraph(Graph *graph, int depth)
{ 
   freeGraph(graph);
   int count;
   for(count = 0; count < depth; count++)
   {
      StackData data = pop(graph_stack);
      if(count != depth - 1) freeGraph(data.graph);
      else return data.graph;
   }
}

void freeGraphStack(Stack *graph_stack)
{
   int count;
   for(count = 0; count < graph_stack->top; count++)
      if(graph_stack->data[count].graph) 
         freeGraph(graph_stack->data[count].graph);
   freeStack(graph_stack);
}

/* ================== *
 * Querying functions *
 * ================== */

Node *getNode(Graph *graph, int index)
{
   if(index > graph->node_index) 
   {
      print_to_log("Error (getNode): Passed index exceeds node size "
                   "of the graph.\n");
      return NULL;
   }
   if(index == -1) return NULL;
   else return &(graph->nodes[index]);
}

Edge *getEdge(Graph *graph, int index)
{
   if(index > graph->edge_index) 
   {
      print_to_log("Error (getEdgePointer): Passed index exceeds node size "
                   "of the graph.\n");
      return NULL;
   }
   if(index == -1) return NULL;
   else return &(graph->edges[index]);
}

RootNodes *getRootNodeList(Graph *graph)
{
   return graph->root_nodes;
}


LabelClassTable getNodesByLabel(Graph *graph, LabelClass label_class) 
{
   return graph->nodes_by_label[label_class];
}
   

LabelClassTable getEdgesByLabel(Graph *graph, LabelClass label_class) 
{
   return graph->edges_by_label[label_class];
}


int getOutEdge(Node *node, int index)
{
   if(index > node->out_index) 
   {
      print_to_log("Error (getOutEdge): Passed index exceeds size of the "
                   "node's out_edges array.\n");
      exit(1);
   }
   if(index >= MAX_INCIDENT_EDGES) 
      return node->extra_out_edges[index - MAX_INCIDENT_EDGES];
   else return node->out_edges[index];
}


int getInEdge(Node *node, int index)
{
   if(index > node->in_index) 
   {
      print_to_log("Error (getInEdge): Passed index exceeds size of the "
                   "node's in_edges array.\n");
      exit(1);
   }
   if(index >= MAX_INCIDENT_EDGES) 
      return node->extra_in_edges[index - MAX_INCIDENT_EDGES];
   else return node->in_edges[index];
}


int getSource(Edge *edge) 
{
   return edge->source;
}

int getTarget(Edge *edge) 
{
   return edge->target;
}


Label *getNodeLabel(Node *node) 
{
   return node->label;
}

Label *getEdgeLabel(Edge *edge) 
{
   return edge->label;
}

int getIndegree(Node *node) 
{
   return node->indegree;
}

int getOutdegree(Node *node) 
{
   return node->outdegree;
}


void printGraph(Graph *graph) 
{
   int index, node_count = 0, edge_count = 0;

   if(graph == NULL || graph->number_of_nodes == 0) 
   {
      printf("[ | ]\n");
      return;
   }

   printf("[ ");
   for(index = 0; index < graph->node_index; index++)
   {
      Node *node = getNode(graph, index);
      if(node->index >= 0) 
      {
         /* Five nodes per line */
         if(node_count != 0 && node_count % 5 == 0) printf("\n  ");
         node_count++;

         if(node->root) printf("(n%d(R), ", index);
         else printf("(n%d, ", index);

         if(node->label->list) printGP2List(node->label->list);
         else printf("empty");

         printMark(node->label->mark, false);
         printf(") ");
      }
   }
 
   if(graph->number_of_edges == 0)
   {
      printf("| ]\n\n");
      return;
   }

   printf("|\n  ");
   for(index = 0; index < graph->edge_index; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge->index >= 0) 
      {
         /* Three edges per line */
         if(edge_count != 0 && edge_count % 3 == 0) printf("\n  ");
         edge_count++;

         if(edge->bidirectional) printf("(e%d(B), ", index);
         else printf("(e%d, ", index);

         printf("n%d, n%d, ", edge->source, edge->target);

         if(edge->label->list) printGP2List(edge->label->list);
         else printf("empty");

         printMark(edge->label->mark, false);
         printf(") ");
      }
   }
   printf("]\n\n");
}

void freeGraph(Graph *graph) 
{
   if(graph == NULL) return;

   int index;
   for(index = 0; index < graph->node_index; index++)
   {
      Node *node = getNode(graph, index);
      if(node->index >= 0)
      {
         if(node->label && node->label != &blank_label) freeLabel(node->label);
         if(node->extra_out_edges) free(node->extra_out_edges);
         if(node->extra_in_edges) free(node->extra_in_edges);
      }  
   }
   if(graph->nodes) free(graph->nodes);

   for(index = 0; index < graph->edge_index; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge->index >= 0)
      {
         if(edge->label && edge->label != &blank_label) freeLabel(edge->label);
      }  
   }
   if(graph->edges) free(graph->edges);

   if(graph->free_node_slots) free(graph->free_node_slots);
   if(graph->free_edge_slots) free(graph->free_edge_slots);

   for(index = 0; index < LABEL_CLASSES; index++)
   {
      if(graph->nodes_by_label[index].items != NULL)
         free(graph->nodes_by_label[index].items); 
      if(graph->edges_by_label[index].items != NULL)
         free(graph->edges_by_label[index].items); 
   }

   if(graph->root_nodes != NULL) 
   {
      RootNodes *iterator = graph->root_nodes;
      while(iterator != NULL)
      {
         RootNodes *temp = iterator;
         iterator = iterator->next;
         free(temp);
      }
   }
   /*if(graph->graph_change_stack)
   {
      while(graph->graph_change_stack->top != NULL)
      {
         GraphChange *change = (GraphChange*)pop(graph->graph_change_stack);
         freeGraphChange(change);
      }
      free(graph->graph_change_stack);
   }*/
   free(graph);
}

/* void freeGraphChange(GraphChange *change)
{
   switch(change->type)
   {
      case ADD_NODE:

           if(change->data.added_node) free(change->data.added_node);
           if(change) free(change);

           break;


      case ADD_EDGE:

           if(change->data.added_edge) free(change->data.added_edge);
           if(change) free(change);

           break;


      case REMOVE_NODE:

           if(change->data.removed_node.name) 
              free(change->data.removed_node.name);
           if(change->data.removed_node.label)
             freeLabel(change->data.removed_node.label);
           if(change) free(change);

           break;


      case REMOVE_EDGE:

           if(change->data.removed_edge.name) 
              free(change->data.removed_edge.name);
           if(change->data.removed_edge.label)
             freeLabel(change->data.removed_edge.label);
           if(change->data.removed_edge.source_name) 
              free(change->data.removed_edge.source_name);
           if(change->data.removed_edge.target_name) 
              free(change->data.removed_edge.target_name);
           if(change) free(change);

           break;


      case RELABEL_NODE:

           if(change->data.relabelled_node.name) 
              free(change->data.relabelled_node.name);
           if(change->data.relabelled_node.old_label) 
              free(change->data.relabelled_node.old_label);
           if(change) free(change);           

           break;


      case RELABEL_EDGE:

           if(change->data.relabelled_edge.name) 
              free(change->data.relabelled_edge.name);
           if(change->data.relabelled_edge.old_label) 
              free(change->data.relabelled_edge.old_label);
           if(change) free(change);           

           break;


      default: 
           print_to_log("Error (freeGraphChange): Unexepected change type "
                        "%d.\n",change->type); 
           break;      
   }  
} 

void restoreGraph (Graph *graph)
{
   while(graph->graph_change_stack->top != 0)
   {
      GraphChange *change = (GraphChange*) pop(graph->graph_change_stack);

      switch(change->type)
      {
         case ADD_NODE:
         {
              string name = change->data.added_node;
              Node *node = g_hash_table_lookup(graph->nodes, name);

              removeNode(graph, node, false);

              if(name) free(name);
              if(change) free(change);

              break;
         }

         case ADD_EDGE:
         {
              string name = change->data.added_edge;
              Edge *edge = g_hash_table_lookup(graph->edges, name);

              removeEdge(graph, edge, false);

              if(name) free(name);
              if(change) free(change);

              break;
         }

         case REMOVE_NODE:
         {
              string name = change->data.removed_node.name;

              Node *node = newNode(change->data.removed_node.name,
                                   change->data.removed_node.root, 
                                   change->data.removed_node.label);
              addNode(graph, node, false);

              if(name) free(name);
              if(change) free(change);

              break;
         }

         case REMOVE_EDGE:
         {
              string name = change->data.removed_edge.name;
              string source_name = change->data.removed_edge.source_name;
              string target_name = change->data.removed_edge.target_name;

              Node *source = g_hash_table_lookup(graph->nodes, source_name);
              Node *target = g_hash_table_lookup(graph->nodes, target_name);
              Edge *edge = newEdge(name, change->data.removed_edge.bidirectional,
                                   change->data.removed_edge.label, source,
                                   target);
              addEdge(graph, edge, false);

              if(name) free(name);
              if(source_name) free(source_name);
              if(target_name) free(target_name);
              if(change) free(change);           

              break; 
         }

         case RELABEL_NODE:
         {
              string name = change->data.relabelled_node.name;

              Node *node = g_hash_table_lookup(graph->nodes, name);
              relabelNode(graph, node, change->data.relabelled_node.old_label, 
                          change->data.relabelled_node.change_root, false);
              
              if(name) free(name);
              if(change) free(change);           

              break;
         }

         case RELABEL_EDGE:
         {
              string name = change->data.relabelled_edge.name;

              Edge *edge = g_hash_table_lookup(graph->edges, name);
              relabelEdge(graph, edge, change->data.relabelled_edge.old_label,
                          change->data.relabelled_edge.change_bidirectional, 
                          false);

              if(name) free(name);
              if(change) free(change);    

              break;
         }
         default: 
              print_to_log("Error (restoreGraph): Unexepected change type %d.\n",
                           change->type); 
              break;
      }
   } 
} */

