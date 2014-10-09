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
    
    new_graph->nodes = calloc(MAX_NODES, sizeof(Node *));
    if(new_graph->nodes == NULL)
    {
      printf("Memory exhausted during graph construction.\n");
      exit(1);
    }
 
    new_graph->edges = calloc(MAX_EDGES, sizeof(Edge *));
    if(new_graph->edges == NULL)
    {
      printf("Memory exhausted during graph construction.\n");
      exit(1);
    }
  
    new_graph->next_node_index = 0;
    new_graph->next_edge_index = 0;

    /* Hash tables indexed by label class. g_direct_hash generates hash keys
     * from void pointers. Label classes are converted to pointers with the
     * G_INT_TO_POINTER macro so that they can be used as hash keys. It is
     * done this way because the integer hashing functions did not function
     * properly. */
    new_graph->nodes_by_label = g_hash_table_new(g_direct_hash, g_direct_equal);    
    new_graph->edges_by_label = g_hash_table_new(g_direct_hash, g_direct_equal);    

    new_graph->root_nodes = NULL;
 
    return new_graph;
}

/* Invariants on graphs:
 * (1) The graph's node array has pointers to nodes from index 0 to index 
 *     graph->next_node_index - 1.
 * (2) The graph's edge array has pointers to edges from index 0 to index 
 *     graph->next_edge_index - 1.
 * (3) Each node's out_edge array has pointers to edges from index 0 to index 
 *     node->outdegree - 1.
 * (4) Each node's in_edge array has pointers to edges from index 0 to index 
 *     node->indegree - 1.
 * (5) Edge consistency: For all edges E referred to outside of the graph's
 *     edge array (i.e. in a node's inedge/outedge list), E is the edge pointed
 *     to by the (E.index)th pointer of the edge array.
 * (6) Node consistency: For all nodes N referred to outside of the graph's
 *     node (i.e. the edge's source and target), N is the node pointed to by
 *     the (N.index)th pointer of the node array.
 * (7) For all edges E, if S is E's source and T is E's target, then E is in
 *     S's outedge list and E is in T's inedge list (source/target consistency). 
 * (8) A node with label class L is in the nodes_by_label table entry indexed
 *     by the hash key L.
 * (9) An edge with label class L is in the edges_by_label table entry indexed
 *     by the hash key L.
 */

bool validGraph(Graph *graph)
{
   bool valid_graph = true;
   int graph_index, node_index;
   
   for(graph_index = 0; graph_index < graph->next_node_index; graph_index++)    
   {
      Node *node = getNode(graph, graph_index);
    
      /* Invariant (1) */
      if(node == NULL) 
      {
         print_to_console("The graph does not satisfy the invariants. Pointer "
                          "at node array index %d is NULL.\n", graph_index);
         valid_graph = false;
      }
      
      else
      {         
         for(node_index = 0; node_index < node->outdegree; node_index++)
         {
            Edge *node_edge = getOutEdge(node, node_index);

            /* Invariant (3) */
            if(node == NULL) 
            {
               print_to_console("The graph does not satisfy the invariants. "
                                "Pointer at node %d's outedge array index %d is "
                                "NULL.\n", graph_index, node_index);
               valid_graph = false;
            }   
            else
            {
               Edge *graph_edge = getEdge(graph, node_edge->index); 
               /* Invariant (5) */
               if(node_edge != graph_edge) 
               {
                  print_to_console("Graph does not satisfy the invariants. Node %d's "
                                   "outedge %d is inconsistent with the graph's edge "
                                   "table.\n", graph_index, node_edge->index);   
                  valid_graph = false;
               }
            }
         }
         

         for(node_index = 0; node_index < node->indegree; node_index++)
         {
            Edge *node_edge = getInEdge(node, node_index);

            /* Invariant (4) */
            if(node == NULL) 
            {
               print_to_console("The graph does not satisfy the invariants. "
                                "Pointer at node %d's in edge array index %d is "
                                "NULL.\n", graph_index, node_index);
               valid_graph = false;
            }   
            else
            {
               Edge *graph_edge = getEdge(graph, node_edge->index); 
               /* Invariant (5) */
               if(node_edge != graph_edge) 
               {
                  print_to_console("Graph does not satisfy the invariants. Node %d's "
                                   "outedge %d is inconsistent with the graph's edge "
                                   "table.\n", graph_index, node_edge->index);   
                  valid_graph = false;
               }
            }
         }
      }
      
      /* Invariant (8) */
      GSList *node_list = getNodesByLabel(graph, node->label_class);

      if(g_slist_find(node_list, node) == NULL)
      {
         print_to_console("Graph does not satisfy the invariants. Node %d "
                          "does not occcur in the hash list of its label "
                          "class %d.\n", graph_index, node->label_class);   
         valid_graph = false;
      }   
   }
   
   for(graph_index = 0; graph_index < graph->next_edge_index; graph_index++)    
   {
      Edge *edge = getEdge(graph, graph_index);

      /* Invariant (2) */
      if(edge == NULL) 
      {
         print_to_console("The graph does not satisfy the invariants. Pointer "
                          "at edge array index %d is NULL.\n", graph_index);
         valid_graph = false;
      }

      else
      {
         Node *source = getSource(edge); 
         Node *node = getNode(graph, source->index);
 
         /* Invariant (6) */
         if(source != node) 
         {
            print_to_console("Graph does not satisfy the invariants. Edge %d's "
                             "source %d is inconsistent with the graph's node "
                             "array.\n", graph_index, source->index);   
            valid_graph = false;
         }   
   
         int counter;
         bool edge_found = false;

         /* Search for edge in the out_edges array of its source.  */
         for(counter = 0; counter < source->outdegree; counter++)
         {
            Edge *out_edge = getOutEdge(source, counter);
            if(out_edge == edge)
            {
               edge_found = true;
               break;
            }
         }
 
         /* Invariant (7) */
         if(!edge_found)
         {
            print_to_console("Graph does not satisfy the invariants. Edge %d "
                             "does not occur in node %d's outedge array.\n",
                             graph_index, source->index);   
            valid_graph = false;
         }   


         Node *target = getTarget(edge); 
         node = getNode(graph, target->index);

         /* Invariant (6) */
         if(target != node) 
         {
            print_to_console("Graph does not satisfy the invariants. Edge %d's "
                             "target %d is inconsistent with the graph's node "
                             "array.\n", graph_index, target->index);   
            valid_graph = false;
         }   

         edge_found = false;

         /* Search for edge in the in_edges array of its source.  */
         for(counter = 0; counter < target->indegree; counter++)
         {
            Edge *in_edge = getInEdge(target, counter);
            if(in_edge == edge)
            {
               edge_found = true;
               break;
            }
         }
 
         /* Invariant (7) */
         if(!edge_found)
         {
            print_to_console("Graph does not satisfy the invariants. Edge %d "
                             "does not occur in node %d's inedge array.\n",
                             graph_index, target->index);   
            valid_graph = false;
         }   
      }
   
      /* Invariant (9) */
      GSList *edge_list = getEdgesByLabel(graph, edge->label_class);

      if(g_slist_find(edge_list, edge) == NULL)
      {
         print_to_console("Graph does not satisfy the invariants. Edge %d "
                          "does not occcur in the hash list of its label "
                          "class %d.\n", graph_index, edge->label_class);   
         valid_graph = false;
      }   
   }    
   if(valid_graph) print_to_console ("Graph satisfies all the data invariants!\n");
   return valid_graph;
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

      case CONCAT:

           return STRING_L;

      default:
           print_to_log("Error (getLabelClass): First element of passed list "
                        "has unexpected type %d.\n", atom->type);
           break;
   }

   return LISTVAR_L;
}
       
   

Node *newNode(bool root, Label *label)
{
   Node *node = malloc(sizeof(Node));

   if(node == NULL) 
   {
      print_to_log("Memory exhausted during node construction.\n");
      exit(1);
   }
 
   node->index = 0;
   node->root = root;
   node->label_class = getLabelClass(label);
   node->label = label;
   node->indegree = 0;
   node->outdegree = 0;

   node->out_edges = calloc(MAX_INCIDENT_EDGES, sizeof(Edge *));

   if(node->out_edges == NULL)
   {
      printf("Memory exhausted during node construction.\n");
      exit(1);
   }

   node->in_edges = calloc(MAX_INCIDENT_EDGES, sizeof(Edge *));

   if(node->in_edges == NULL)
   {
      printf("Memory exhausted during node construction.\n");
      exit(1);
   }

   return node;
}


Edge *newEdge(bool bidirectional, Label *label, Node *source,
              Node *target)
{
   Edge *edge = malloc(sizeof(Edge));

   if(edge == NULL) {
      print_to_log("Memory exhausted during node construction.\n");
      exit(1);
   }
	
   edge->index = 0;
   edge->bidirectional = bidirectional;
   edge->label_class = getLabelClass(label);
   edge->label = label;
   edge->source = source;
   edge->target = target;
   
   return edge;
}
	

void addNode(Graph *graph, Node *node) 
{
    /* Add to graph->nodes. */
    graph->nodes[graph->next_node_index] = node;
    node->index = graph->next_node_index++;

    /* Update graph->nodes_by_label by adding the new node to the appropriate
     * node list and inserting the updated list into the hash table. */
    void *label_key = GINT_TO_POINTER(node->label_class);
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, label_key);
    node_list = g_slist_prepend(node_list, node);
    g_hash_table_insert(graph->nodes_by_label, label_key, node_list);
    
    /* Update graph->root_nodes. */
    if(node->root) graph->root_nodes = g_slist_prepend(graph->root_nodes, node);
}


void addEdge(Graph *graph, Edge *edge) 
{
    /* Add to graph->edges. */
    graph->edges[graph->next_edge_index] = edge;
    edge->index = graph->next_edge_index++;

    /* Update the source and target nodes with the new edge: first increment
     * the appropriate degree and then add the edge to the out_edges/in_edges
     * list. */ 
    Node *source = edge->source;
    source->out_edges[source->outdegree] = edge;
    source->outdegree++; 

    Node *target = edge->target;
    target->in_edges[target->indegree] = edge;
    target->indegree++;

    /* Update graph->nodes_by_label by adding the new edge to the appropriate
     * edge list and inserting the updated list into the hash table. */
    void *label_key = GINT_TO_POINTER(edge->label_class);
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, label_key);
    edge_list = g_slist_prepend(edge_list, edge);
    g_hash_table_insert(graph->edges_by_label, label_key, edge_list);
}


void removeNode(Graph *graph, int index)
{   
    Node *node = getNode(graph, index);  
 
    if(node->indegree > 0 || node->outdegree > 0)
    {
       print_to_log("Error (removeNode): Cannot remove node (%d) with "
                    "incident edges.\n", node->index);
       return;
    }

    void *label_key = GINT_TO_POINTER(node->label_class);

    /* Remove the node from the label classes hash table by removing the
     * node from the node list and inserting the updated list into
     * the hash table. */
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, label_key);
    node_list = g_slist_remove(node_list, node);
    g_hash_table_insert(graph->nodes_by_label, label_key, node_list);

    /* Remove the node from the root node list if necessary. */
    if(node->root) graph->root_nodes = g_slist_remove(graph->root_nodes, node); 

    /* Remove the node from the pointer array. */
    freeNode(node);

    /* Shift the elements after the deleted node one place to the left. 
     * This is done by redirecting the (i-1)th pointer to the ith pointer's
     * target for each i from index + 1 to next_node_index. Each node's index
     * is updated appropriately. After the loop, set the last pointer to NULL.
     */ 
    int counter = index + 1;
    while(counter < graph->next_node_index)
    {
       graph->nodes[counter - 1] = graph->nodes[counter];
       graph->nodes[counter - 1]->index = counter - 1;
       counter++;
    }
    graph->nodes[counter - 1] = NULL;
    graph->next_node_index--;
}


void removeEdge(Graph *graph, int index) 
{
    /* Update the source and target nodes: first decrement the appropriate
     * degree and then remove the edge from the out_edges/in_edges pointer 
     * arrays. */ 
    Edge *edge = getEdge(graph, index);

    Node *source = edge->source;
    int counter = 0;
 
    /* Find the edge in the source's out_edges array. */
    while(counter < source->outdegree)
    {
       if(source->out_edges[counter] == edge) break;   
       else counter++;
    }
    
    /* Shift the elements after the deleted edge one place to the left. 
     * This is done by redirecting the (i-1)th pointer to the ith pointer's
     * target for each i from index + 1 to source->outdegree. 
     * After the loop, set the last pointer to NULL.
     */ 
    counter++;
    while(counter < source->outdegree)
    {
       source->out_edges[counter - 1] = source->out_edges[counter];
       counter++;
    }
    source->out_edges[counter - 1] = NULL;
    source->outdegree--;

    Node *target = edge->target;
    counter = 0;
 
    /* Find the edge in the target's in_edges array. */
    while(counter < target->indegree)
    {
       if(target->in_edges[counter] == edge) break;   
       else counter++;
    }

    counter++;
    while(counter < target->indegree)
    {
       target->in_edges[counter - 1] = target->in_edges[counter];
       counter++;
    }
    target->in_edges[counter - 1] = NULL;
    target->indegree--;

    void *label_key = GINT_TO_POINTER(edge->label_class);

    /* Remove the edge from the label classes hash table by removing the
     * edge from the edge list and inserting the updated list into
     * the hash table. */
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, label_key);
    edge_list = g_slist_remove(edge_list,edge);
    g_hash_table_replace(graph->edges_by_label, label_key, edge_list);

    /* Remove the edge from the pointer array. */
    freeEdge(edge);

    /* Shift the elements after the deleted edge one place to the left. 
     * This is done by redirecting the (i-1)th pointer to the ith pointer's
     * target for each i from index + 1 to next_edge_index. Each edge's index
     * is updated appropriately. After the loop, set the last pointer to NULL.
     */ 
    counter = index + 1;
    while(counter < graph->next_edge_index)
    {
       graph->edges[counter - 1] = graph->edges[counter];
       graph->edges[counter - 1]->index = counter - 1;
       counter++;
    }
    graph->edges[counter - 1] = NULL;
    graph->next_edge_index--;
}


void relabelNode(Graph *graph, Node *node, Label *new_label, bool change_root) 
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

    if(new_label == NULL) return;
    else
    {  
       /* node->label is freed before being pointed to new_label. */
       freeLabel(node->label); 
       node->label = new_label; 

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

void relabelEdge(Graph *graph, Edge *edge, Label *new_label, 
                 bool change_bidirectional)
{		
    if(change_bidirectional) edge->bidirectional = !edge->bidirectional;

    /* If record is true, then edge->label is pointed to by the graph
     * change created above. Otherwise, edge->label should be freed
     * before it is pointed to new_label. */

    if(new_label == NULL) return;
    else
    {
       /* edge->label is freed before being pointed to new_label. */
       freeLabel(edge->label); 
       edge->label = new_label; 

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
}


Stack *graph_stack = NULL;

void copyGraph (Graph *graph)
{
   /* Need to check if arrays in the original graph have increased in size. */
   Graph *graph_copy = newGraph();
   graph_copy->next_node_index = graph->next_node_index;
   graph_copy->next_edge_index = graph->next_edge_index;

   int index;

   /* Copy all nodes and edges with their labels. Note that the index of each
    * node and edge copy is the same as the index of the original. This is
    * important for copying the rest of the graph structure. */
   for(index = 0; index < graph->next_edge_index; index++)
   {
      Edge *edge = getEdge(graph, index);
      Edge *edge_copy = malloc(sizeof(Edge));
      if(edge_copy == NULL)
      {
         print_to_log("Memory exhausted during graph copying.\n");
         exit(1);
      }    
      edge_copy = memcpy(edge_copy, edge, sizeof(Edge));
      edge_copy->label = copyLabel(edge->label);
      graph_copy->edges[index] = edge_copy;

      /* Populate the edge-by-label-class hash table of the graph copy. */
      void *hash_key = GINT_TO_POINTER(edge_copy->label_class);

      GSList *edge_list = getEdgesByLabel(graph_copy, edge_copy->label_class);
      edge_list = g_slist_prepend(edge_list, edge_copy);
      g_hash_table_insert(graph_copy->edges_by_label, hash_key, edge_list);     
   }

   for(index = 0; index < graph->next_node_index; index++)
   {
      Node *node = getNode(graph, index);
      Node *node_copy = malloc(sizeof(Node)); 
      if(node_copy == NULL)
      {
         print_to_log("Memory exhausted during graph copying.\n");
         exit(1);
      }   
      node_copy = memcpy(node_copy, node, sizeof(Node));
      node_copy->label = copyLabel(node->label);

      /* Create and populate a new out_edge array since the node copy points
       * to the same array as the original node. */
      int counter;

      node_copy->out_edges = calloc(MAX_INCIDENT_EDGES, sizeof(Edge *));
      if(node->out_edges == NULL)
      {
         printf("Memory exhausted during graph copying.\n");
         exit(1);
      }

      for(counter = 0; counter < node->outdegree; counter++)
      {
         Edge *edge = getOutEdge(node, counter);
         Edge *edge_copy = getEdge(graph_copy, edge->index);
         node_copy->out_edges[counter] = edge_copy;
      }

      /* Create and populate a new in_edge array. */
      node_copy->in_edges = calloc(MAX_INCIDENT_EDGES, sizeof(Edge *));
      if(node->in_edges == NULL)
      {
         printf("Memory exhausted during graph copying.\n");
         exit(1);
      }
      for(counter = 0; counter < node->indegree; counter++)
      {
         Edge *edge = getInEdge(node, counter);
         Edge *edge_copy = getEdge(graph_copy, edge->index);
         node_copy->in_edges[counter] = edge_copy;
      }

      graph_copy->nodes[index] = node_copy;

      /* Populate the node-by-label-class hash table of the graph copy. */
      void *hash_key = GINT_TO_POINTER(node_copy->label_class);

      GSList *node_list = getNodesByLabel(graph_copy, node_copy->label_class);
      node_list = g_slist_prepend(node_list, node_copy);
      g_hash_table_insert(graph_copy->nodes_by_label, hash_key, node_list);     

      /* Populate the root nodes list. */
      if(node_copy->root)
      {
         graph_copy->root_nodes = g_slist_prepend(graph_copy->root_nodes, 
                                                  node_copy);
      }
   }

   /* Iterate through the copied edges to update their source and target
    * pointers. */
   for(index = 0; index < graph_copy->next_edge_index; index++)
   {
      Edge *edge_copy = getEdge(graph_copy, index);

      Node *source = getSource(edge_copy);
      Node *source_copy = getNode(graph_copy, source->index);
      edge_copy->source = source_copy;

      Node *target = getTarget(edge_copy);
      Node *target_copy = getNode(graph_copy, target->index);
      edge_copy->target = target_copy;
   }  
 
   if(graph_stack == NULL) graph_stack = newStack();
   push(graph_stack, graph_copy);
}

Graph *restoreGraph(Graph *graph)
{ 
   freeGraph(graph);
   return (Graph *)pop(graph_stack);
}

void freeGraphStack(Stack *graph_stack)
{ 
   freeStack(graph_stack, freeGraph);
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
      case EMPTY:

           break;
 
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





/* Querying functions */

Node *getNode(Graph *graph, int index)
{
   if(index > graph->next_node_index) 
   {
      print_to_log("Error (getNode): Passed index exceeds node size of the "
                   "graph.\n");
      return NULL;
   }
   else return graph->nodes[index];
}

Edge *getEdge(Graph *graph, int index)
{
   if(index > graph->next_edge_index) 
   {
      print_to_log("Error (getEdge): Passed index exceeds edge size of the "
                   "graph.\n");
      return NULL;
   }
   else return graph->edges[index];
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
   if(index > node->outdegree) 
   {
      print_to_log("Error (getOutEdge): Passed index exceeds size of the "
                   "node's outedge array.\n");
      return NULL;
   }
   else return node->out_edges[index];
}


Edge *getInEdge(Node *node, int index)
{
   if(index > node->indegree) 
   {
      print_to_log("Error (getInEdge): Passed index exceeds size of the "
                   "node's inedge array.\n");
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

int getIndegree (Node *node) 
{
   return node->indegree;
}

int getOutdegree (Node *node) 
{
   return node->outdegree;
}


void printGraph (Graph *graph) 
{
    int index;

    printf("\nGraph Description\n=================\n");

    printf("Nodes\n=====\n");
    for(index = 0; index < graph->next_node_index; index++)
       printNode(graph->nodes[index]);
 
    printf("Edges\n=====\n");
    for(index = 0; index < graph->next_edge_index; index++)
       printEdge(graph->edges[index]);
    printf("\n");

    printf("Root Nodes\n==========\n");
    GSList *iterator = graph->root_nodes;
    while(iterator != NULL)
    {
       Node *node = (Node *)iterator->data;
       printNode(node); 
       iterator = iterator->next;
    }
    printf("\n");
}

void printNode(Node *node)
{
    printf("Index: %d\n", node->index);
    if(node->root) printf("Root\n");
    printf("Label Class: %d\n", node->label_class);
    printf("Mark: %d\n", node->label->mark);
    printf("Label: ");
    if(node->label->list) printList(node->label->list);
    printf("\n");
    printf("Indegree: %d\nOutdegree: %d\n\n", node->indegree, node->outdegree);
}

void printEdge (Edge *edge) 
{
    printf("Index: %d\n", edge->index);
    if(edge->bidirectional) printf("Bidirectional\n");
    printf("Label Class: %d\n", edge->label_class);
    printf("Mark: %d\n", edge->label->mark);
    printf("Label: ");
    if(edge->label->list) printList(edge->label->list);
    printf("\n");
    printf("Source: %d\n", edge->source->index);
    printf("Target: %d\n", edge->target->index);
    printf("\n");
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


void freeGraph(void *graph_pointer) 
{
    Graph *graph = (Graph *)graph_pointer;
    int index;

    for(index = 0; index < graph->next_node_index; index++)
       if(graph->nodes[index]) freeNode(graph->nodes[index]);
    if(graph->nodes) free(graph->nodes);

    for(index = 0; index < graph->next_edge_index; index++)
       if(graph->edges[index]) freeEdge(graph->edges[index]);
    if(graph->edges) free(graph->edges);
 
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
    if(graph) free(graph);
}

void freeNode(Node *node) 
{
    if(node->label) freeLabel(node->label);
    if(node->in_edges) free(node->in_edges);
    if(node->out_edges) free(node->out_edges);
    if(node) free(node);
}

void freeEdge(Edge *edge) 
{
    if(edge->label) freeLabel(edge->label);
    if(edge) free(edge);
}


void freeGSList(gpointer key, gpointer value, gpointer data) 
{
    g_slist_free(value);
}

void freeLabel(Label *label)
{
    if(label->list) 
       g_list_free_full(label->list, freeListElement);
    if(label) free(label);
}

void freeListElement(void *atom)
{
     ListElement* elem = (ListElement*)atom;

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

