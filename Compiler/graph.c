/* ///////////////////////////////////////////////////////////////////////////

  ================================
  graph.c - Chris Bak (14/07/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "graph.h"

Graph *newGraph(void) 
{
    Graph *new_graph = malloc(sizeof(Graph));

    if(new_graph == NULL) 
    {
      printf("Memory exhausted during graph construction.\n");
      exit(1);
    }
    
    new_graph->next_node_index = 0;
    new_graph->next_edge_index = 0;

    /* The pointer arrays in the graph structhre handle the freeing of Node and 
     * Edge structs. They are passed the freeing functions for nodes and edges.
     */
    new_graph->nodes = g_ptr_array_new_with_free_func(freeNode);
    new_graph->edges = g_ptr_array_new_with_free_func(freeEdge);

    /* Explain this */
    new_graph->nodes_by_label = g_hash_table_new(g_direct_hash, g_direct_equal);    
    new_graph->edges_by_label = g_hash_table_new(g_direct_hash, g_direct_equal);    

    new_graph->root_nodes = NULL;
 
    new_graph->graph_change_stack = newStack(100);

    return new_graph;
}

/* Invariants on graphs:
 * (1) For a graph with N nodes, next_node_index is N.
 * (2) For a graph with E edges, next_edge_index is E.
 * (3) Every edge struct E stores its own index E_i. For each reference to an
 *     edge E outside the graph structure (namely a node's inedge/outedge
 *     lists), the pointer in index E_i of the graph's edge array points to E. 
 * (4) Every node struct N stores its own index N_i. For each reference to a
 *     node N outside the graph structure (namely an edge's source and target 
 *     pointers), the pointer in index N_i of the graph's edge array points to N.
 * (5) For all edges E, if S is E's source and T is E's target, then E is in
 *     S's outedge list and in T's inedge list.
 * (6) A node with label class L is in the nodes_by_label table entry indexed
 *     by the hash key L.
 * (7) An edge with label class L is in the edges_by_label table entry indexed
 *     by the hash key L.
 */

bool validGraph(Graph *graph)
{
   bool return_value = true;
   int counter1;
   guint counter2;
   int node_index = graph->next_node_index;
   int edge_index = graph->next_edge_index;

   /* Invariant (1) */
   if(graph->nodes->len != (guint)node_index) 
   { 
      print_to_console("Invariant (1) of validGraph failed.\n");
      return_value = false;
   }
 
   /* Invariant (2) */
   if(graph->edges->len != (guint)edge_index)
   { 
      print_to_console("Invariant (2) of validGraph failed.\n");
      return_value = false;
   }

   /* Invariants (3) + (6) */
   for(counter1 = 0; counter1 < graph->next_node_index; counter1++)
   {
      Node *node = getNode(graph, counter1);

      for(counter2 = 0; counter2 < node->in_edges->len; counter2++)
      {
         Edge *node_edge = g_ptr_array_index(node->in_edges, counter2);
         Edge *graph_edge = getEdge(graph, node_edge->index); 
         if(node_edge != graph_edge) 
         {
            print_to_console("Invariant (3) of validGraph failed. Node %s's "
                             "inedge %s (index %d) is inconsistent with the "
                             "graph's pointer array.\n", 
                             node->name, node_edge->name, node_edge->index);   
            return_value = false;
         }
      }
      
      for(counter2 = 0; counter2 < node->out_edges->len; counter2++)
      {
         Edge *node_edge = g_ptr_array_index(node->out_edges, counter2);
         Edge *graph_edge = getEdge(graph, node_edge->index); 
         if(node_edge != graph_edge) 
         {
            print_to_console("Invariant (3) of validGraph failed. Node %s's "
                             "outedge %s (index %d) is inconsistent with the "
                             "graph's pointer array.\n", 
                             node->name, node_edge->name, node_edge->index);   
            return_value = false;
         }
      }

      void *hash_key = GINT_TO_POINTER(node->label_class);
      GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, hash_key);

      if(g_slist_find(node_list, node) == NULL)
      {
         print_to_console("Invariant (6) of validGraph failed. Node %s "
                          "does not occcur in the hash list of its label. "
                          "class %d.\n", node->name, node->label_class);   
         return_value = false;
      }   
      
   }
   
   /* Invariants (4) + (5) + (7) */
   for(counter1 = 0; counter1 < graph->next_edge_index; counter1++)
   {
      Edge *edge = getEdge(graph, counter1);

      Node *source = edge->source;
      Node *node = getNode(graph, edge->source->index);
 
      if(source != node) 
      {
         print_to_console("Invariant (4) of validGraph failed. Edge %s's "
                          "source %s (index %d) is inconsistent with the "
                          "graph's pointer array.\n", edge->name, 
                          source->name, edge->source->index);   
         return_value = false;
      }   

      bool edge_found = false;

      /* Search for edge in the out_edges array of its source. */
      for(counter2 = 0; counter2 < source->out_edges->len; counter2++)
      {
         Edge *out_edge = g_ptr_array_index(source->out_edges, counter2);
         if(out_edge == edge)
         {
            edge_found = true;
            break;
         }
      }

      if(!edge_found)
      {
         print_to_console("Invariant (5) of validGraph failed. Edge %s "
                          "does not occcur in node %s's outedge array. "
                          "graph's pointer array.\n", edge->name, 
                          source->name);   
         return_value = false;
      }   


      Node *target = edge->target;
      node = getNode(graph, edge->target->index);
 
      if(target != node) 
      {
         print_to_console("Invariant (4) of validGraph failed. Edge %s's "
                          "target %s (index %d) is inconsistent with the "
                          "graph's pointer array.\n", edge->name,
                          source->name, edge->source->index);   
         return_value = false; 
      }            
 
      edge_found = false;

      /* Search for edge in the in_edges array of its target. */
      for(counter2 = 0; counter2 < source->in_edges->len; counter2++)
      {
         Edge *in_edge = g_ptr_array_index(source->in_edges, counter2);
         if(in_edge == edge)
         {
            edge_found = true;
            break;
         }
      }

      if(!edge_found)
      {
         print_to_console("Invariant (5) of validGraph failed. Edge %s "
                          "does not occcur in node %s's inedge array. "
                          "graph's pointer array.\n", edge->name, 
                          target->name);   
         return_value = false;
      }   
   
      void *hash_key = GINT_TO_POINTER(edge->label_class);
      GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, hash_key);

      if(g_slist_find(edge_list, edge) == NULL)
      {
         print_to_console("Invariant (7) of validGraph failed. Edge %s "
                          "does not occcur in the hash list of its label. "
                          "class %d.\n", edge->name, edge->label_class);   
         return_value = false;
      }   
   }    
   return return_value;
}


LabelClass getLabelClass(Label *label)
{
   int length = label->list_length;

   if(label->has_list_variable) return LISTVAR_L;
   if(length == 0) return EMPTY_L;
   if(length > 1)
   {
      switch(length)
      {
         case 2: return LIST2_L;
         case 3: return LIST3_L;
         case 4: return LIST4_L;
         case 5: return LIST5_L;

         default: print_to_log("Error (getLabelClass): The length of the " 
                               "passed list exceeds the GP 2 maximum.\n");
      }
   }

   /* The list has length 1. */
   ListElement *atom = (ListElement*)label->list->data;
   
   switch(atom->type)
   {
      case VARIABLE:

           return ATOMIC_VAR_L;     

      case INTEGER_CONSTANT:

      case NEG:

           return INT_L;

      case CHARACTER_CONSTANT:

      case STRING_CONSTANT:

           return STRING_L;

      default:
           print_to_log("Error (getLabelClass): First element of passed list "
                        "has unexpected type %d.\n", atom->type);
           break;
   }

   return LISTVAR_L;
}
       
   

Node *newNode(string name, bool root, Label *label)
{
   Node *node = malloc(sizeof(Node));

   if(node == NULL) 
   {
      print_to_log("Memory exhausted during node construction.\n");
      exit(1);
   }

   node->index = 0;
   node->name = strdup(name);
   node->root = root;
   node->label_class = getLabelClass(label);
   node->label = label;
   node->indegree = 0;
   node->outdegree = 0;
   node->in_edges = g_ptr_array_new();
   node->out_edges = g_ptr_array_new();

   return node;
}


Edge *newEdge(string name, bool bidirectional, Label *label, Node *source,
              Node *target)
{
   Edge *edge = malloc(sizeof(Edge));

   if(edge == NULL) {
      print_to_log("Memory exhausted during node construction.\n");
      exit(1);
   }
	
   edge->index = 0;
   edge->name = strdup(name);
   edge->bidirectional = bidirectional;
   edge->label_class = getLabelClass(label);
   edge->label = label;
   edge->source = source;
   edge->target = target;
   
   return edge;
}
	

void addNode(Graph *graph, Node *node, bool record) 
{
    if(record)
    {
       GraphChange *change = NULL;
       change->type = ADD_NODE;
       change->data.added_node = node->name;
       
       push(graph->graph_change_stack, change);        
    }

    node->index = graph->next_node_index;
    void *hash_key = GINT_TO_POINTER(node->label_class);

    /* Add to graph->nodes */
    g_ptr_array_add(graph->nodes, node);

    /* Update graph->nodes_by_label */
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, hash_key);
    node_list = g_slist_prepend(node_list,node);
    g_hash_table_replace(graph->nodes_by_label, hash_key, node_list);
    
    /* Update graph->root_nodes */
    if(node->root) graph->root_nodes = g_slist_prepend(graph->root_nodes,node);

    graph->next_node_index += 1;
}


void addEdge(Graph *graph, Edge *edge, bool record) 
{
    if(record)
    {
       GraphChange *change = NULL;
       change->type = ADD_EDGE;
       change->data.added_edge = edge->name;
       
       push(graph->graph_change_stack, change);        
    }
    edge->index = graph->next_edge_index;
    void *hash_key = GINT_TO_POINTER(edge->label_class);

    /* Update the source and target nodes with the new edge: first increment
     * the appropriate degrees and then add the edge to the out_edges/in_edges
     * list. */ 
    edge->source->outdegree++;
    g_ptr_array_add(edge->source->out_edges, edge);

    edge->target->indegree++;
    g_ptr_array_add(edge->target->in_edges, edge);

    /* Add to graph->edges */
    g_ptr_array_add(graph->edges, edge);

    /* Update graph->edges_by_label */
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, hash_key);
    edge_list = g_slist_prepend(edge_list, edge);
    g_hash_table_replace(graph->edges_by_label, hash_key, edge_list);

    graph->next_edge_index += 1;
}

/* Removes a node from the graph. The function g_ptr_array_remove_fast is used
 * to remove the node from the pointer array. It also moves the last element
 * of the array in its place. Hence, to keep the indices consistent, I update
 * the index of the last element with the index of the node to be deleted.
 * Assumes no incident edges. */

void removeNode(Graph *graph, Node *node, bool record)
{    
    if(node->indegree > 0 || node->outdegree > 0)
    {
       print_to_log("Error (removeNode): Attempt to remove node %s with "
                    "incident edges.\n", node->name);
       return;
    }

    if(record)
    {
       Label *label_copy = copyLabel(node->label);

       GraphChange *change = NULL;
       change->type = REMOVE_NODE;
       change->data.removed_node.name = node->name;
       change->data.removed_node.root = node->root;
       change->data.removed_node.label = label_copy;
       
       push(graph->graph_change_stack, change);        
    }

    /* Get the last element and set its index to that of the node to be removed. */
    Node *node_to_update = 
       g_ptr_array_index(graph->nodes, graph->next_node_index - 1);
    node_to_update->index = node->index;
    
    void *hash_key = GINT_TO_POINTER(node->label_class);
  
    /* Remove the node from the pointer array. */
    g_ptr_array_remove_index_fast(graph->nodes, node->index);
    graph->next_node_index--;

    /* Remove the node from the node array. The pointer is freed by the call to
     * g_ptr_array_remove_index_fast. */
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, hash_key);
    node_list = g_slist_remove(node_list, node);
    g_hash_table_insert(graph->nodes_by_label, hash_key, node_list);

    /* Remove the node from the root node list if necessary. */
    if(node->root) graph->root_nodes = g_slist_remove(graph->root_nodes, node);
}

/* Removes an edge from the graph. The function g_ptr_array_remove_fast is used
 * to remove the edge from the pointer array. It also moves the last element
 * of the array in its place. Hence, to keep the indices consistent, I update
 * the index of the last item to the index of the deleted node. */

void removeEdge(Graph *graph, Edge *edge, bool record) 
{
    if(record)
    {
       Label *label_copy = copyLabel(edge->label);

       GraphChange *change = NULL;
       change->type = REMOVE_EDGE;
       change->data.removed_edge.name = edge->name;
       change->data.removed_edge.bidirectional = edge->bidirectional;
       change->data.removed_edge.label = label_copy;
       change->data.removed_edge.source = edge->source->name;
       change->data.removed_edge.target = edge->target->name;
       
       push(graph->graph_change_stack, change);        
    }

    /* Get the last element and set its index to that of the edge to be 
     * removed. */
    Edge *edge_to_update = 
       g_ptr_array_index(graph->edges, graph->next_edge_index - 1);
    edge_to_update->index = edge->index;
    
    void *hash_key = GINT_TO_POINTER(edge->label_class);
 
    /* Update the source and target nodes: first decrement the appropriate
     * degrees and then remove the edge from the out_edges/in_edges pointer 
     * arrays. */ 
    edge->source->outdegree--;
    edge->target->indegree--;

    /* Removes the first occurrence of the edge pointer from the array and
     * moves the remaining elements down one place to preserve the order.
     * This is important for backtracking in the matching algorithm. */
    g_ptr_array_remove(edge->source->out_edges, edge);
    g_ptr_array_remove(edge->target->in_edges, edge);

    /* Remove the edge from the edge array. The pointer is freed by the call to
     * g_ptr_array_remove_index_fast. */
    g_ptr_array_remove_index_fast(graph->edges, edge->index);
    graph->next_edge_index--;

    /* Remove the edge from the hash table. */
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, hash_key);
    edge_list = g_slist_remove(edge_list,edge);
    g_hash_table_replace(graph->edges_by_label, hash_key, edge_list);
}

void relabelNode(Graph *graph, Node *node, Label *new_label, bool change_root,
                 bool record) 
{
    if(record)
    {
       Label *label_copy = copyLabel(node->label);

       GraphChange *change = NULL;
       change->type = RELABEL_NODE;
       change->data.relabelled_node.name = node->name;
       change->data.relabelled_node.old_label = label_copy;
       change->data.relabelled_node.change_root = change_root;
       
       push(graph->graph_change_stack, change);        
    }

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

    if(new_label == NULL) return;
    else
    {
       node->label->mark = new_label->mark;
       if(node->label->list) g_list_free_full(node->label->list, freeListElement);
       node->label->list = new_label->list;

       /* If the label classes differ, the graph's nodes_by_label table needs to 
        * be updated. */
       LabelClass new_label_class = getLabelClass(new_label);
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

void relabelEdge(Graph *graph, Edge *edge, Label *new_label, bool record) 
{		
    if(record)
    {
       Label *label_copy = copyLabel(edge->label);

       GraphChange *change = NULL;
       change->type = RELABEL_EDGE;
       change->data.relabelled_edge.name = edge->name;
       change->data.relabelled_edge.old_label = label_copy;
       
       push(graph->graph_change_stack, change);        
    }

    edge->label->mark = new_label->mark;
    if(edge->label->list) g_list_free_full(edge->label->list, freeListElement);
    edge->label->list = new_label->list;

    /* If the label classes differ, the graph's edges_by_label table needs to
     * be updated. */
    LabelClass new_label_class = getLabelClass(new_label);
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


/* Querying functions */

Node *getNode(Graph *graph, int index)
{
   return g_ptr_array_index(graph->nodes, index);
}

Node *getNodeById(Graph *graph, string id)
{
   int count;
   
   for(count = 0; count < graph->next_node_index; count++)
   {
      Node *node = getNode(graph, count);
      if(node->name == id) return node;
   }
   return NULL;
}

Edge *getEdge(Graph *graph, int index)
{
   return g_ptr_array_index(graph->edges, index);
}

Edge *getEdgeById(Graph *graph, string id)
{
   int count;
   
   for(count = 0; count < graph->next_edge_index; count++)
   {
      Edge *edge = getEdge(graph, count);
      if(edge->name == id) return edge;
   }
   return NULL;
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


GSList *getInEdge(Node *node, int index)
{
   return g_ptr_array_index(node->in_edges, index);
}


GSList *getOutEdge(Node *node, int index)
{
   return g_ptr_array_index(node->out_edges, index);
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

int getIndegree (Node *node) 
{
   return node->indegree;
}

int getOutdegree (Node *node) 
{
   return node->outdegree;
}


Stack *newStack (int max_size) 
{
   Stack *new_stack = malloc(sizeof(Stack));

   if(new_stack == NULL) 
   {
      print_to_log("Error: Memory exhausted during stack construction.\n");
      exit(1);
   }
 
   void **items = calloc(max_size, sizeof(void*));

   if(items == NULL) 
   {
      print_to_log("Error: Memory exhausted during stack construction.\n");
      exit(1);
   } 

   new_stack->top = 0;
   new_stack->max_size = max_size;
   new_stack->items = items;

   return new_stack;
}

bool push (Stack *stack, void *data) 
{
   if(stack->top == stack->max_size) 
   {
      print_to_log("Error: Trying to push to a full stack.\n");
      return false;
   }
   else 
   {
      stack->items[stack->top] = data;
      stack->top++;
   }
   return true;
}

void *pop (Stack *stack) 
{
   void *popped_item = NULL;

   if(stack->top == 0) 
   {
      print_to_log("Error: Trying to pop from an empty stack.\n");
      return NULL;
   }
   else 
   {
      stack->top--;
      popped_item = stack->items[stack->top];
   }
   
   return popped_item;
}

void freeStack (Stack *stack) 
{
   free(stack->items);
   free(stack);
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
              Node *node = getNodeById(graph, change->data.added_node);
              removeNode(graph, node, false);
              break;
         }

         case ADD_EDGE:
         {
              Edge *edge = getEdgeById(graph, change->data.added_edge);
              removeEdge(graph, edge, false);
              break;
         }

         case REMOVE_NODE:
         {
              Node *node = newNode(change->data.removed_node.name,
                                   change->data.removed_node.root, 
                                   change->data.removed_node.label);
              addNode(graph, node, false);
              break;
         }

         case REMOVE_EDGE:
         {
              Node *source = getNodeById(graph, change->data.removed_edge.source);
              Node *target = getNodeById(graph, change->data.removed_edge.target);
              Edge *edge = newEdge(change->data.removed_edge.name,
                                   change->data.removed_edge.bidirectional,
                                   change->data.removed_edge.label,
                                   source, target);
              addEdge(graph, edge, false);
              break; 
         }

         case RELABEL_NODE:
         {
              Node *node = getNodeById(graph, change->data.relabelled_node.name);
              relabelNode(graph, node, change->data.relabelled_node.old_label,
                          change->data.relabelled_node.change_root, false);
              break;
         }

         case RELABEL_EDGE:
         {
              Edge *edge = getEdgeById(graph, change->data.relabelled_edge.name);
              relabelEdge(graph, edge, change->data.relabelled_edge.old_label,
                          false);
              break;
         }
         default: 
              print_to_log("Error (restoreGraph): Unexepected change type %d.\n",
                           change->type); 
              break;
      }
   }   
}
 

void printGraph (Graph *graph) 
{
    printf("\nGraph Description\n=================\n");
    printf("Next node index: %d\n", graph->next_node_index);
    printf("Next edge index: %d\n\n", graph->next_edge_index);
    printf("Nodes\n=====\n");
    g_ptr_array_foreach(graph->nodes, printNode, NULL);   
    printf("Edges\n=====\n");
    g_ptr_array_foreach(graph->edges, printEdge, NULL);   
    printf("Root Nodes\n==========\n");
    g_slist_foreach(graph->root_nodes, printNode, NULL);   
    printf("\n");
}

void printNode (gpointer data, gpointer user_data) 
{
    Node *node = (Node *)data;

    printf("Name: %s\nIndex: %d\n", node->name, node->index);
    if(node->root) printf("Root\n");
    printf("Label Class: %d\n", node->label_class);
    printf("Mark: %d\n", node->label->mark);
    printf("Label: ");
    if(node->label->list) printList(node->label->list);
    printf("\n");
    printf("Indegree: %d\nOutdegree: %d\n", node->indegree, node->outdegree);
    if(node->in_edges) 
        g_ptr_array_foreach(node->in_edges, printEdgeData, "Incoming");
    if(node->out_edges) 
        g_ptr_array_foreach(node->out_edges, printEdgeData, "Outgoing");
    printf("\n");
}

void printEdge (gpointer data, gpointer user_data) 
{
    Edge *edge = (Edge *)data;

    printf("Name: %s\nIndex: %d\n", edge->name, edge->index);
    if(edge->bidirectional) printf("Bidirectional\n");
    printf("Label Class: %d\n", edge->label_class);
    printf("Mark: %d\n", edge->label->mark);
    printf("Label: ");
    if(edge->label->list) printList(edge->label->list);
    printf("\n");
    printf("Source: %d-%s\n", edge->source->index, edge->source->name);
    printf("Target: %d-%s\n", edge->target->index, edge->target->name);
    printf("\n");
}

void printEdgeData (gpointer data, gpointer user_data) 
{
   Edge *edge = (Edge*)data;
   printf("%s edge: %d %s\n", (string)user_data, edge->index, edge->name);
} 


void printList(GList *list) 
{
    while(list != NULL) 
    {
        printListElement((ListElement*)list->data);
        if(list->next) printf(" : ");
        list = list->next;
    } 
}

void printListElement(ListElement* elem) 
{
       
    switch(elem->type) 
    {
        case EMPTY:
             printf("empty");
             break;

	case VARIABLE: 
	     printf("%s", elem->value.name);
	     break;

	case INTEGER_CONSTANT: 
	     printf("%d", elem->value.number);
	     break;

	case CHARACTER_CONSTANT:
	     printf("\"%s\"", elem->value.string);
	     break;

	case STRING_CONSTANT:
	     printf("\"%s\"", elem->value.string);
	     break;

	case INDEGREE:
	     printf("indeg(%s)", elem->value.node_id);
	     break;
 
	case OUTDEGREE:
	     printf("outdeg(%s)", elem->value.node_id);
	     break;

	case LIST_LENGTH:
	     printf("llength(");
	     printList(elem->value.list_arg);
	     printf(")");
	     break;

	case STRING_LENGTH:
	     printf("slength(");
	     printListElement(elem->value.str_arg);
	     printf(")");
	     break;

	case NEG:
	     printf("- ");
	     printListElement(elem->value.exp);
	     break;

	case ADD:
	     printf("(");
	     printListElement(elem->value.bin_op.left_exp);
	     printf(" + ");
	     printListElement(elem->value.bin_op.right_exp);
	     printf(")");
	     break;

	case SUBTRACT:
	     printf("(");
	     printListElement(elem->value.bin_op.left_exp);
	     printf(" - ");
	     printListElement(elem->value.bin_op.right_exp);
	     printf(")");
	     break;

	case MULTIPLY:
	     printf("(");
	     printListElement(elem->value.bin_op.left_exp);
	     printf(" * ");
	     printListElement(elem->value.bin_op.right_exp);
	     printf(")");
	     break;

	case DIVIDE:
	     printf("(");
	     printListElement(elem->value.bin_op.left_exp);
	     printf(" / ");
	     printListElement(elem->value.bin_op.right_exp);
	     printf(")");
	     break;

	case CONCAT:
	     printf("(");
	     printListElement(elem->value.bin_op.left_exp);
	     printf(" . ");
	     printListElement(elem->value.bin_op.right_exp);
	     printf(")");
	     break;

	default: printf("Unexpected List Element Type: %d\n",
		       (int)elem->type); 
		 break;
    }
}




void freeGraph (Graph *graph) 
{
    /* graph->nodes and graph->edges are initialised with a free function.
     * g_ptr_array_free will not only free the pointer arrays, but each
     * of the nodes and edges to which they point, with the freeNode and
     * freeEdge functions defined below. The second argument tells the
     * function to free the pointer array from heap in addition to the data to
     * which it points. */     
    if(graph->nodes) g_ptr_array_free(graph->nodes, TRUE);
    if(graph->edges) g_ptr_array_free(graph->edges, TRUE);

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
    if(graph) free(graph);
}

void freeNode (void *p) 
{
    Node *node = (Node *)p;
    if(node->name) free(node->name);
    if(node->label) 
    {
       if(node->label->list) 
          g_list_free_full(node->label->list, freeListElement);
       free(node->label);
    }
    if(node->in_edges) g_ptr_array_free(node->in_edges, TRUE);
    if(node->out_edges) g_ptr_array_free(node->out_edges, TRUE);
    if(node) free(node);
}

void freeGSList(gpointer key, gpointer value, gpointer data) 
{
    g_slist_free(value);
}

void freeEdge (void *p) 
{
    Edge *edge = (Edge *)p;
    if(edge->name) free(edge->name);
    /* When freeing a graph, nodes are freed first, so no need to free
     * source and target here. */
    if(edge->label) 
    {
       if(edge->label->list) 
          g_list_free_full(edge->label->list, freeListElement);
       free(edge->label);
    }
    if(edge) free(edge);
}

void freeListElement(void *p)
{
     ListElement* elem = (ListElement*)p;

     switch(elem->type) 
     {
        case EMPTY:

             break;


	case VARIABLE:

	     if(elem->value.name)
               free(elem->value.name);

             break;


	case INTEGER_CONSTANT:

             break;


        case CHARACTER_CONSTANT:
          
	case STRING_CONSTANT:

	     if(elem->value.string)
               free(elem->value.string);

             break;


	case INDEGREE:
 
        case OUTDEGREE:

	     if(elem->value.node_id) 
               free(elem->value.node_id);

             break;


	case LIST_LENGTH:

	     if(elem->value.list_arg)
               g_list_free_full(elem->value.list_arg, freeListElement);
		
             break;


	case STRING_LENGTH:

	     if(elem->value.str_arg)
               freeListElement(elem->value.str_arg);

             break;

	case NEG:

	     if(elem->value.exp) freeListElement(elem->value.exp);

             break;


	case ADD:

	case SUBTRACT:

	case MULTIPLY:

	case DIVIDE:

	case CONCAT:

	     if(elem->value.bin_op.left_exp)
                freeListElement(elem->value.bin_op.left_exp);
	     if(elem->value.bin_op.right_exp)  
                freeListElement(elem->value.bin_op.right_exp);

             break;


	default: printf("Unexpected List Element Type: %d\n",
                       (int)elem->type); 
                 break;

	}

   if(elem) free(elem);
}


Label *copyLabel(Label *label)
{
   Label *label_copy = malloc(sizeof(Label));

   if(label_copy == NULL) {
      print_to_log("Memory exhausted during label copying.\n");
      exit(1);
   }

   GList *list_copy = NULL;

   /* The list is copied in reverse order so that elements are prepended at
    * each step. */
   GList *list_to_copy = g_list_last(label->list);

   while(list_to_copy != NULL)
   {
      ListElement *atom = (ListElement*)list_to_copy->data;
      ListElement *atom_copy = copyListElement(atom);
      list_copy = g_list_prepend(list_copy, atom_copy);
      list_to_copy = list_to_copy->prev;
   }

   label_copy->mark = label->mark;
   label_copy->list = list_copy;
   label_copy->list_length = label->list_length;
   label_copy->has_list_variable = label->has_list_variable;

   return label_copy;
}


ListElement *copyListElement(ListElement *atom)
{ 
   ListElement *atom_copy = malloc(sizeof(ListElement));

   if(atom_copy == NULL)
   {
      print_to_log("Error (copyListElement): Memory exhausted during "
                   "creation of a list element.\n");
      exit(1);
   }

   /* Duplicate any string values and recursively copy any sub-expressions. */
   atom_copy->type = atom->type;

   switch(atom->type)
   {
      case VARIABLE:

           atom_copy->value.name = strdup(atom->value.name);

           break;
      
      case INTEGER_CONSTANT:

           atom_copy->value.number = atom->value.number;

           break;

      case CHARACTER_CONSTANT:

      case STRING_CONSTANT:

           atom_copy->value.string = strdup(atom->value.string);

           break;

      case NEG:
 
           atom_copy->value.exp = copyListElement(atom->value.exp);

           break;

      case CONCAT:

           atom_copy->value.bin_op.left_exp = 
              copyListElement(atom->value.bin_op.left_exp);
           atom_copy->value.bin_op.right_exp = 
              copyListElement(atom->value.bin_op.right_exp);

           break;

      default:
             print_to_log("Error (copyListElement): Atom type %d should not "
                          "occur here.\n", atom->type);
             return NULL;
   }

   return atom_copy;
}



