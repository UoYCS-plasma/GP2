#include "graph.h"

Node null_node = {-1, false, EMPTY_L, NULL, NULL, NULL, 0, 0, 0, 0};
Edge null_edge = {-1, false, EMPTY_L, NULL, NULL, NULL};

Graph *newGraph(void) 
{
    Graph *graph = malloc(sizeof(Graph));

    if(graph == NULL) 
    {
      printf("Memory exhausted during graph construction.\n");
      exit(1);
    }
     
    int index;

    graph->nodes = calloc(MAX_NODES, sizeof(Node));
    if(graph->nodes == NULL)
    {
      printf("Memory exhausted during graph construction.\n");
      exit(1);
    }
    for(index = 0; index < MAX_NODES; index ++)
       graph->nodes[index] = null_node;
 
    graph->edges = calloc(MAX_EDGES, sizeof(Edge));
    if(graph->edges == NULL)
    {
      printf("Memory exhausted during graph construction.\n");
      exit(1);
    }
    for(index = 0; index < MAX_EDGES; index ++)
       graph->edges[index] = null_edge;

    graph->free_node_slots = newStack(); 
    graph->free_edge_slots = newStack();
  
    graph->next_node_index = 0;
    graph->next_edge_index = 0;

    graph->number_of_nodes = 0;
    graph->number_of_edges = 0;

    /* Hash tables indexed by label class. g_direct_hash generates hash keys
     * from void pointers. Label classes are converted to pointers with the
     * G_INT_TO_POINTER macro so that they can be used as hash keys. It is
     * done this way because the integer hashing functions did not function
     * properly. */
    graph->nodes_by_label = g_hash_table_new(g_direct_hash, g_direct_equal);    
    graph->edges_by_label = g_hash_table_new(g_direct_hash, g_direct_equal);    

    graph->root_nodes = NULL;
 
    return graph;
}

int addNode(Graph *graph, bool root, Label *label) 
{
   int index;

   /* Get the index of the added node by examining the free node slots stack. */
   StackData *data = pop(graph->free_node_slots);
   if(data == NULL) 
   {
      index = graph->next_node_index;
      graph->next_node_index++;
   }
   else 
   {
      index = data->free_slot;
      free(data);
   }

   /* Initialise the node's index, root status, next edge array indices and 
    * degrees. The label class, label and edge arrays are initialised after. */
   graph->nodes[index].index = index;
   graph->nodes[index].root = root;
   graph->nodes[index].next_out_edge_index = 0;
   graph->nodes[index].next_in_edge_index = 0;
   graph->nodes[index].outdegree = 0;
   graph->nodes[index].indegree= 0;
   
   if(label == NULL)
   {
      graph->nodes[index].label_class = EMPTY_L;
      graph->nodes[index].label = &blank_label;
   }
   else
   { 
      graph->nodes[index].label_class = getLabelClass(label);
      graph->nodes[index].label = label;
   }

   graph->nodes[index].out_edges = calloc(MAX_INCIDENT_EDGES, sizeof(Edge *));
   if(graph->nodes[index].out_edges == NULL)
   {
      printf("Memory exhausted during node construction.\n");
      exit(1);
   }

   graph->nodes[index].in_edges = calloc(MAX_INCIDENT_EDGES, sizeof(Edge *));
   if(graph->nodes[index].in_edges == NULL)
   {
      printf("Memory exhausted during node construction.\n");
      exit(1);
   }

   /* Update graph->nodes_by_label by adding the new node to the appropriate
    * node list and inserting the updated list into the hash table. */
   void *label_key = GINT_TO_POINTER(graph->nodes[index].label_class);
   GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, label_key);
   node_list = g_slist_prepend(node_list, &(graph->nodes[index]));
   g_hash_table_insert(graph->nodes_by_label, label_key, node_list);
    
   /* Update graph->root_nodes. */
   if(root) graph->root_nodes = g_slist_prepend(graph->root_nodes, 
                                                &(graph->nodes[index]));
   graph->number_of_nodes++;

   return index; 
}

int addEdge(Graph *graph, bool bidirectional, Label *label, Node *source,
            Node *target) 
{
   int index;

   /* Get the index of the added node by examining the free node slots stack. */
   StackData *data = pop(graph->free_edge_slots);
   if(data == NULL) 
   {
      index = graph->next_edge_index;
      graph->next_edge_index++;
   }
   else 
   {
      index = data->free_slot;
      free(data);
   }

   /* Initialise the edge's index, bidirectional status, source and target 
    * The label class and label are initialised after. */
   graph->edges[index].index = index;
   graph->edges[index].bidirectional = bidirectional;
   graph->edges[index].source = source;
   graph->edges[index].target = target;

   if(label == NULL)
   {
      graph->edges[index].label_class = EMPTY_L;
      graph->edges[index].label = &blank_label;
   }
   else
   { 
      graph->edges[index].label_class = getLabelClass(label);
      graph->edges[index].label = label;
   }

   /* Update the source and target nodes with the new edge: add the
    * edge to the out_edges/in_edges array and then update the degree. */ 
   source->out_edges[source->next_out_edge_index++] = &(graph->edges[index]);
   source->outdegree++;

   target->in_edges[target->next_in_edge_index++] = &(graph->edges[index]);
   target->indegree++;

   /* Update graph->nodes_by_label by adding the new edge to the appropriate
    * edge list and inserting the updated list into the hash table. */
   void *label_key = GINT_TO_POINTER(graph->edges[index].label_class);
   GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, label_key);
   edge_list = g_slist_prepend(edge_list, &(graph->edges[index]));
   g_hash_table_insert(graph->edges_by_label, label_key, edge_list);

   graph->number_of_edges++;

   return index;
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

    void *label_key = GINT_TO_POINTER(node->label_class);

    /* Remove the node from the label classes hash table by removing the
     * node from the node list and updating the hash table entry. */
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, label_key);
    node_list = g_slist_remove(node_list, node);

    /* If the node list is empty, remove the key from the hash table. Otherwise
     * overwrite the hash table entry with the updated node list. */
    if(node_list == NULL) g_hash_table_remove(graph->nodes_by_label, label_key);
    else g_hash_table_insert(graph->nodes_by_label, label_key, node_list); 

    if(node->root) graph->root_nodes = g_slist_remove(graph->root_nodes, node); 
 
    if(node->label && node->label != &blank_label) freeLabel(node->label);
    if(node->in_edges) free(node->in_edges);
    if(node->out_edges) free(node->out_edges);    
    graph->nodes[index] = null_node;

    /* If the node's index is the last index in the array, it is not 
     * necessary to create a free slot: only decrement next_node_index. */
    if(index == graph->next_node_index - 1) graph->next_node_index--;
    else
    {
       StackData *data = malloc(sizeof(StackData));
       if(data == NULL)
       {
          print_to_log("Error (removeNode): Memory exhausted during free slot "
                       "construction.\n");
          exit(1);
       }
       data->free_slot = index;
       push(graph->free_node_slots, data);
    }
    graph->number_of_nodes--;
}


void removeEdge(Graph *graph, int index) 
{
    /* Update the source and target nodes: first decrement the appropriate
     * degree and then remove the edge from the out_edges/in_edges pointer 
     * arrays. */ 
    Edge *edge = getEdge(graph, index);
 
    /* Find the edge in the source's out_edges array and set the pointer to 
     * NULL. */
    Node *source = edge->source;
    int counter = 0;
    while(counter < source->next_out_edge_index)
    {
       if(source->out_edges[counter] == edge) 
       {
          source->out_edges[counter] = NULL;
          source->outdegree--;

          /* If the source's index is the last index in the array, decrement
           * next_out_edge_index until it refers to an index exactly one above
           * a non-NULL pointer. */
          if(counter == source->next_out_edge_index - 1) 
          {
             source->next_out_edge_index--;

             while(source->next_out_edge_index > 0)
             {
                if(source->out_edges[source->next_out_edge_index - 1] == NULL)
                   source->next_out_edge_index--;
                else break;
             }
          }
          break;
       }
       else counter++;
    }

    /* Repeat for the target's in_edges array. */
    Node *target = edge->target;
    counter = 0; 
    while(counter < target->next_in_edge_index)
    {
       if(target->in_edges[counter] == edge) 
       {
          target->in_edges[counter] = NULL;
          target->indegree--;

          /* If the target's index is the last index in the array, decrement
           * next_in_edge_index until it refers to an index exactly one above
           * a non-NULL pointer. */
          if(counter == target->next_in_edge_index - 1) 
          {
             target->next_in_edge_index--;

             while(target->next_in_edge_index > 0)
             {
                if(target->in_edges[target->next_in_edge_index - 1] == NULL)
                   target->next_in_edge_index--;
                else break;
             }
          }
          break;
       }
       else counter++;
    }

    void *label_key = GINT_TO_POINTER(edge->label_class);

    /* Remove the edge from the label classes hash table by removing the
     * edge from the edge list and inserting the updated list into
     * the hash table. */
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, label_key);
    edge_list = g_slist_remove(edge_list, edge);

    /* If the edge list is empty, remove the key from the hash table. Otherwise
     * overwrite the hash table entry with the updated edge list. */
    if(edge_list == NULL) g_hash_table_remove(graph->edges_by_label, label_key);
    else g_hash_table_insert(graph->edges_by_label, label_key, edge_list); 

    if(edge->label && edge->label != &blank_label) freeLabel(edge->label);
    graph->edges[index] = null_edge;

    /* If the edge's index is the last index in the array, it is not 
     * necessary to create a free slot: only decrement next_edge_index.
     */
    if(index == graph->next_edge_index - 1) graph->next_edge_index--;
    else
    {
       StackData *data = malloc(sizeof(StackData));
       if(data == NULL)
       {
          print_to_log("Error (removeEdge): Memory exhausted during free slot "
                       "construction.\n");
          exit(1);
       }
       data->free_slot = index;
       push(graph->free_edge_slots, data);
    }
    graph->number_of_edges--;
}


void relabelNode(Graph *graph, Node *node, Label *new_label, bool change_label,
                 bool change_root) 
{
    if(change_root)
    {
       if(node->root)
       {
          node->root = false;
          graph->root_nodes = g_slist_remove(graph->root_nodes, node);
       }
       else
       {
          node->root = true;
          graph->root_nodes = g_slist_prepend(graph->root_nodes, node);
       }
    }

    if(change_label == false) return;
    else
    {  
       /* node.label is freed before being pointed to new_label. */
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
       /* If the label classes differ, the graph's nodes_by_label table needs to 
        * be updated. */
       if(node->label_class != new_label_class) 
       {
          void *hash_key_1 = GINT_TO_POINTER(node->label_class);
          void *hash_key_2 = GINT_TO_POINTER(new_label_class);
          node->label_class = new_label_class;

          /* Remove the node from the list indexed by its old label class. */
          GSList *old_node_list = 
             g_hash_table_lookup(graph->nodes_by_label, hash_key_1);
          old_node_list = g_slist_remove(old_node_list, node); 
          g_hash_table_replace(graph->nodes_by_label, hash_key_1, old_node_list);

          /* Add the node to the list indexed by its new label class. */
          GSList *new_node_list = 
              g_hash_table_lookup(graph->nodes_by_label, hash_key_2);
          new_node_list = g_slist_prepend(new_node_list, node);
          g_hash_table_replace(graph->nodes_by_label, hash_key_2, new_node_list);
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
       /* edge.label is freed before being pointed to new_label. */
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
       /* If the label classes differ, the graph's edges_by_label table needs to
        * be updated. */
       if(edge->label_class != new_label_class) 
       {
          void *hash_key_1 = GINT_TO_POINTER(edge->label_class);
          void *hash_key_2 = GINT_TO_POINTER(new_label_class);
          edge->label_class = new_label_class;

          /* Remove the edge from the list indexed by its old label class. */
          GSList *old_edge_list = 
              g_hash_table_lookup(graph->edges_by_label, hash_key_1);
          old_edge_list = g_slist_remove(old_edge_list, edge); 
          g_hash_table_replace(graph->edges_by_label, hash_key_1, old_edge_list);

          /* Add the edge to the list indexed by its new label class. */
          GSList *new_edge_list = 
              g_hash_table_lookup(graph->edges_by_label, hash_key_2);
          new_edge_list = g_slist_prepend(new_edge_list, edge);
          g_hash_table_replace(graph->edges_by_label, hash_key_2, new_edge_list);
       }   
    }
}


Stack *graph_stack = NULL;

void copyGraph(Graph *graph)
{
   /* TODO: check if arrays in the original graph have increased in size. */
   Graph *graph_copy = newGraph();

   graph_copy->next_node_index = graph->next_node_index;
   graph_copy->next_edge_index = graph->next_edge_index;

   graph_copy->number_of_nodes = graph->number_of_nodes;
   graph_copy->number_of_edges = graph->number_of_edges;

   memcpy(graph_copy->nodes, graph->nodes, MAX_NODES * sizeof(Node));
   memcpy(graph_copy->edges, graph->edges, MAX_EDGES * sizeof(Edge));

   int index;

   /* Copy all nodes and edges with their labels. Note that the index of each
    * node and edge copy is the same as the index of the original. This is
    * important for copying the rest of the graph structure. */
   for(index = 0; index < graph_copy->next_node_index; index++)
   {
      Node *node = getNode(graph_copy, index);
      
      if(node->index == -1)
      {
         StackData *data = malloc(sizeof(StackData));
         if(data == NULL)
         {
            print_to_log("Error (copyGraph): Memory exhausted during free node "
                         "slot construction.\n");
            exit(1);
         }
         data->free_slot = index;
         push(graph_copy->free_node_slots, data);
      }
      else 
      {
         Node *original_node = getNode(graph, index);
         node->label = copyLabel(original_node->label);

         /* Create and populate a new out_edge array since the node copy points
          * to the same array as the original node. */
         int counter;

         node->out_edges = calloc(MAX_INCIDENT_EDGES, sizeof(Edge *));
         if(node->out_edges == NULL)
         {
            print_to_log("Memory exhausted during graph copying.\n");
            exit(1);
         }

         for(counter = 0; counter < original_node->next_out_edge_index; counter++)
         {
            Edge *original_edge = getOutEdge(original_node, counter);
            if(original_edge->index >= 0) 
               node->out_edges[counter] = getEdge(graph_copy, original_edge->index);
         }
         node->next_out_edge_index = counter;

         /* Create and populate a new in_edge array. */
         node->in_edges = calloc(MAX_INCIDENT_EDGES, sizeof(Edge *));
         if(node->in_edges == NULL)
         {
            printf("Memory exhausted during graph copying.\n");
            exit(1);
         }

         for(counter = 0; counter < original_node->next_in_edge_index; counter++)
         {
            Edge *original_edge = getInEdge(original_node, counter);
            if(original_edge->index >= 0) 
               node->in_edges[counter] = getEdge(graph_copy, original_edge->index);
         }
         node->next_in_edge_index = counter;

         /* Populate the node-by-label-class hash table of the graph copy. */
         void *hash_key = GINT_TO_POINTER(node->label_class);

         GSList *node_list = getNodesByLabel(graph_copy, node->label_class);
         node_list = g_slist_prepend(node_list, node);
         g_hash_table_insert(graph_copy->nodes_by_label, hash_key, node_list);     

         /* Populate the root nodes list. */
         if(node->root)
            graph_copy->root_nodes = g_slist_prepend(graph_copy->root_nodes, node);
      }
   }

   /* Iterate through the copied edges to update their source and target
    * pointers. */
   for(index = 0; index < graph_copy->next_edge_index; index++)
   {
      Edge *edge = getEdge(graph_copy, index);

      if(edge->index == -1) 
      {
         StackData *data = malloc(sizeof(StackData));
         if(data == NULL)
         {
            print_to_log("Error (copyGraph): Memory exhausted during free edge "
                         "slot construction.\n");
            exit(1);
         }
         data->free_slot = index;
         push(graph_copy->free_edge_slots, data);
      }

      else
      {
         Edge *original_edge = getEdge(graph, index);
         edge->label = copyLabel(original_edge->label);
         edge->source = getNode(graph_copy, original_edge->source->index);
         edge->target = getNode(graph_copy, original_edge->target->index);

         /* Populate the edge-by-label-class hash table of the graph copy. */
         void *hash_key = GINT_TO_POINTER(edge->label_class);
         GSList *edge_list = getEdgesByLabel(graph_copy, edge->label_class);
         edge_list = g_slist_prepend(edge_list, edge);
         g_hash_table_insert(graph_copy->edges_by_label, hash_key, edge_list);     
      }
   }  
 
   if(graph_stack == NULL) graph_stack = newStack();
   
   StackData *data = malloc(sizeof(StackData));
   if(data == NULL)
   {
      print_to_log("Error (copyGraph): Memory exhausted during graph "
                   "construction.\n");
      exit(1);
   }
   data->graph = graph_copy;
   push(graph_stack, data);
}

Graph *restoreGraph(Graph *graph)
{ 
   freeGraph(graph);
   StackData *data = pop(graph_stack);
   Graph *output = data->graph;
   free(data);
   return output;
}

void freeGraphStack(Stack *graph_stack)
{
   /* Before calling freeStack, free all the pointers to graphs. */
   StackNode *iterator = graph_stack->top;

   while(iterator != NULL)
   {
      if(iterator->data->graph) freeGraph(iterator->data->graph);
      iterator = iterator->next;
   }
   freeStack(graph_stack);
}

/* Querying functions */

Node *getNode(Graph *graph, int index)
{
   if(index > graph->next_node_index) 
   {
      print_to_log("Error (getNode): Passed index exceeds node size "
                   "of the graph.\n");
      return NULL;
   }
   else return &(graph->nodes[index]);
}

Edge *getEdge(Graph *graph, int index)
{
   if(index > graph->next_edge_index) 
   {
      print_to_log("Error (getEdgePointer): Passed index exceeds node size "
                   "of the graph.\n");
      return NULL;
   }
   else return &(graph->edges[index]);
}

GSList *getRootNodes(Graph *graph)
{
   return graph->root_nodes;
}


GSList *getNodesByLabel(Graph *graph, LabelClass label_class) 
{
   void *hash_key = GINT_TO_POINTER(label_class);
   return g_hash_table_lookup(graph->nodes_by_label, hash_key);
}
   

GSList *getEdgesByLabel(Graph *graph, LabelClass label_class) 
{
   void *hash_key = GINT_TO_POINTER(label_class);
   return g_hash_table_lookup(graph->edges_by_label, hash_key);
}


Edge *getOutEdge(Node *node, int index)
{
   if(index > node->next_out_edge_index) 
   {
      print_to_log("Error (getOutEdge): Passed index exceeds size of the "
                   "node's out_edges array.\n");
      return NULL;
   }
   else return node->out_edges[index];
}


Edge *getInEdge(Node *node, int index)
{
   if(index > node->next_in_edge_index) 
   {
      print_to_log("Error (getInEdge): Passed index exceeds size of the "
                   "node's in_edges array.\n");
      return NULL;
   }
   else return node->in_edges[index];
}


Node *getSource(Edge *edge) 
{
   return edge->source;
}

Node *getTarget(Edge *edge) 
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
   for(index = 0; index < graph->next_node_index; index++)
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
   for(index = 0; index < graph->next_edge_index; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge->index >= 0) 
      {
         /* Three edges per line */
         if(edge_count != 0 && edge_count % 3 == 0) printf("\n  ");
         edge_count++;

         if(edge->bidirectional) printf("(e%d(B), ", index);
         else printf("(e%d, ", index);

         printf("n%d, n%d, ", edge->source->index, edge->target->index);

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

   for(index = 0; index < graph->next_node_index; index++)
   {
      Node *node = getNode(graph, index);
      if(node->index >= 0)
      {
         if(node->label && node->label != &blank_label) freeLabel(node->label);
         free(node->in_edges);
         free(node->out_edges);
      }  
   }
   free(graph->nodes);

   for(index = 0; index < graph->next_edge_index; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge->index >= 0)
      {
         if(edge->label && edge->label != &blank_label) freeLabel(edge->label);
      }  
   }
   free(graph->edges);

   if(graph->free_node_slots) freeStack(graph->free_node_slots);
   if(graph->free_edge_slots) freeStack(graph->free_edge_slots);

   if(graph->nodes_by_label) 
   {
      g_hash_table_foreach(graph->nodes_by_label, freeGSList, NULL);
      g_hash_table_destroy(graph->nodes_by_label); 
   }

   if(graph->edges_by_label) 
   {
       g_hash_table_foreach(graph->edges_by_label, freeGSList, NULL);
       g_hash_table_destroy(graph->edges_by_label);  
   }

   if(graph->root_nodes) g_slist_free(graph->root_nodes);

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

void freeGSList(gpointer key, gpointer value, gpointer data) 
{
    g_slist_free(value);
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

