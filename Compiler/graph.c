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
    /* The hash tables are passed the freeing functions for nodes, edges,
     * and their keys. 
     */
    new_graph->nodes = g_hash_table_new_full(g_str_hash, g_str_equal, free,
					     freeNode);
    new_graph->edges = g_hash_table_new_full(g_str_hash, g_str_equal, free,
					     freeEdge);

    /* Hash tables indexed by label class. g_direct_hash generates hash keys
     * from void pointers. Label classes are converted to pointers with the
     * G_INT_TO_POINTER macro so that they can be used as hash keys. It is
     * done this way because the integer hashing functions did not function
     * properly. */
    new_graph->nodes_by_label = g_hash_table_new(g_direct_hash, g_direct_equal);    
    new_graph->edges_by_label = g_hash_table_new(g_direct_hash, g_direct_equal);    

    new_graph->root_nodes = NULL;
 
    new_graph->graph_change_stack = newStack();

    return new_graph;
}

/* Invariants on graphs:
 * (1) For each edge pointer E in a node's inedge/outedge list, E is stored
 *     in the edge hashtable with key E->name.
 * (2) A node's indegree (outdegree) is equal to the length of its inedges
 *     (outedges) list.
 * (3) A node with label class L is in the nodes_by_label table entry indexed
 *     by the hash key L.
 * (4) For all edges E, if S is E's source and T is E's target, then E is in
 *     S's outedge list and E is in T's inedge list.
 * (5) For each node pointer N in an edge struct, N is stored in the node
 *     hashtable with key N->name.
 * (6) An edge with label class L is in the edges_by_label table entry indexed
 *     by the hash key L.
 */

bool validGraph(Graph *graph)
{
   bool valid_graph = true;
   unsigned int counter;
   
   GHashTableIter iterator;
   gpointer value;

   g_hash_table_iter_init(&iterator, graph->nodes);

   /* Invariants (1) - (3) */
   while(g_hash_table_iter_next (&iterator, NULL, &value)) 
   {
      Node *node = (Node *)value;

      if(node->indegree != (int)node->in_edges->len)
      {
         print_to_console("Graph does not satisfy the invariants. Node %s's "
                          "indegree (%d) is not equal to the length of its "
                          "inedge list (%d).\n", 
                          node->name, node->indegree, node->in_edges->len);   
         valid_graph = false;
      }

      for(counter = 0; counter < node->in_edges->len; counter++)
      {
         Edge *node_edge = getInEdge(node, counter);
         Edge *graph_edge = getEdge(graph, node_edge->name); 
         if(node_edge != graph_edge) 
         {
            print_to_console("Graph does not satisfy the invariants. Node %s's "
                             "inedge %s is inconsistent with the graph's edge "
                             "table.\n", 
                             node->name, node_edge->name);   
            valid_graph = false;
         }
      }
      
      if(node->outdegree != (int)node->out_edges->len)
      {
         print_to_console("Graph does not satisfy the invariants. Node %s's "
                          "outdegree (%d) is not equal to the length of its "
                          "outedge list (%d).\n", 
                          node->name, node->outdegree, node->out_edges->len);   
         valid_graph = false;
      }

      for(counter = 0; counter < node->out_edges->len; counter++)
      {
         Edge *node_edge = getOutEdge(node, counter);
         Edge *graph_edge = getEdge(graph, node_edge->name); 
         if(node_edge != graph_edge) 
         {
            print_to_console("Graph does not satisfy the invariants. Node %s's "
                             "outedge %s is inconsistent with the graph's edge "
                             "table.\n", 
                             node->name, node_edge->name);   
            valid_graph = false;
         }
      }

      GSList *node_list = getNodesByLabel(graph, node->label_class);

      if(g_slist_find(node_list, node) == NULL)
      {
         print_to_console("Graph does not satisfy the invariants. Node %s "
                          "does not occcur in the hash list of its label "
                          "class %d.\n", node->name, node->label_class);   
         valid_graph = false;
      }   
   }
   
   g_hash_table_iter_init(&iterator, graph->edges);

   /* Invariants (4) - (6) */
   while(g_hash_table_iter_next (&iterator, NULL, &value)) 
   {
      Edge *edge = (Edge *)value;

      Node *source = getSource(edge); 
      Node *node = getNode(graph, source->name);
 
      if(source != node) 
      {
         print_to_console("Graph does not satisfy the invariants. Edge %s's "
                          "source %s is inconsistent with the graph's node "
                          "array.\n", edge->name, source->name);   
         valid_graph = false;
      }   

      bool edge_found = false;

      /* Search for edge in the out_edges array of its source. */
      for(counter = 0; counter < source->out_edges->len; counter++)
      {
         Edge *out_edge = getOutEdge(source, counter);
         if(out_edge == edge)
         {
            edge_found = true;
            break;
         }
      }

      if(!edge_found)
      {
         print_to_console("Graph does not satisfy the invariants. Edge %s "
                          "does not occur in node %s's outedge array.\n ",
                          edge->name, source->name);   
         valid_graph = false;
      }   


      Node *target = getTarget(edge);
      node = getNode(graph, target->name);
 
      if(target != node) 
      {
         print_to_console("Graph does not satisfy the invariants. Edge %s's "
                          "target %s is inconsistent with the graph's node "
                          "array.\n", edge->name, target->name); 
         valid_graph = false; 
      }            
 
      edge_found = false;

      /* Search for edge in the in_edges array of its target.*/
      for(counter = 0; counter < target->in_edges->len; counter++)
      {
         Edge *in_edge = getInEdge(target, counter);
         if(in_edge == edge)
         {
            edge_found = true;
            break;
         }
      }

      if(!edge_found)
      {
         print_to_console("Graph does not satisfy the invariants. Edge %s "
                          "does not occur in node %s's inedge array.\n ",
                          edge->name, target->name);   

         valid_graph = false;
      }   
   
      GSList *edge_list = getEdgesByLabel(graph, edge->label_class);

      if(g_slist_find(edge_list, edge) == NULL)
      {
         print_to_console("Graph does not satisfy the invariants. Edge %s "
                          "does not occcur in the hash list of its label "
                          "class %d.\n", edge->name, edge->label_class);   
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
       
   

Node *newNode(string name, bool root, Label *label)
{
   Node *node = malloc(sizeof(Node));

   if(node == NULL) 
   {
      print_to_log("Memory exhausted during node construction.\n");
      exit(1);
   }

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
       GraphChange *change = malloc(sizeof(GraphChange));
       if(change == NULL)
       {
          print_to_log("Error: Memory exhausted during graph change "
		       "construction.\n");
	  exit(1);
       } 
       change->type = ADD_NODE;
       change->data.added_node = strdup(node->name);
       
       push(graph->graph_change_stack, change);        
    }

    string key = strdup(node->name);
    void *label_key = GINT_TO_POINTER(node->label_class);

    /* Add to graph->nodes. */
    g_hash_table_insert(graph->nodes, key, node);

    /* Update graph->nodes_by_label by adding the new node to the appropriate
     * node list and inserting the updated list into the hash table. */
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, label_key);
    node_list = g_slist_prepend(node_list, node);
    g_hash_table_insert(graph->nodes_by_label, label_key, node_list);
    
    /* Update graph->root_nodes. */
    if(node->root) graph->root_nodes = g_slist_prepend(graph->root_nodes, node);
}


void addEdge(Graph *graph, Edge *edge, bool record) 
{
    if(record)
    {
       GraphChange *change = malloc(sizeof(GraphChange));
       if(change == NULL)
       {
          print_to_log("Error: Memory exhausted during graph change"
		       "construction.\n");
	  exit(1);
       } 
       change->type = ADD_EDGE;
       change->data.added_edge = strdup(edge->name);
       
       push(graph->graph_change_stack, change);        
    }

    string key = strdup(edge->name);
    void *label_key = GINT_TO_POINTER(edge->label_class);

    /* Update the source and target nodes with the new edge: first increment
     * the appropriate degrees and then add the edge to the out_edges/in_edges
     * list. */ 
    edge->source->outdegree++;
    g_ptr_array_add(edge->source->out_edges, edge);

    edge->target->indegree++;
    g_ptr_array_add(edge->target->in_edges, edge);

    /* Add to graph->edges. */
    g_hash_table_insert(graph->edges, key, edge);

    /* Update graph->nodes_by_label by adding the new edge to the appropriate
     * edge list and inserting the updated list into the hash table. */
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, label_key);
    edge_list = g_slist_prepend(edge_list, edge);
    g_hash_table_insert(graph->edges_by_label, label_key, edge_list);
}


void removeNode(Graph *graph, Node *node, bool record)
{    
    if(node->indegree > 0 || node->outdegree > 0)
    {
       print_to_log("Error (removeNode): Cannot remove node (%s) with "
                    "incident edges.\n", node->name);
       return;
    }

    if(record)
    {
       Label *label_copy = copyLabel(node->label);

       GraphChange *change = malloc(sizeof(GraphChange));
       if(change == NULL)
       {
          print_to_log("Error: Memory exhausted during graph change "
		       "construction.\n");
	  exit(1);
       } 
       change->type = REMOVE_NODE;
       change->data.removed_node.name = strdup(node->name);
       change->data.removed_node.root = node->root;
       change->data.removed_node.label = label_copy;
       
       push(graph->graph_change_stack, change);        
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

    /* Remove the node from the main hash table. The node is freed as the
     * hash table was initialised with a node freeing function. */
    g_hash_table_remove(graph->nodes, node->name);
}


void removeEdge(Graph *graph, Edge *edge, bool record) 
{
    if(record)
    {
       Label *label_copy = copyLabel(edge->label);

       GraphChange *change = malloc(sizeof(GraphChange));
       if(change == NULL)
       {
          print_to_log("Error: Memory exhausted during graph change "
		       "construction.\n");
	  exit(1);
       } 
       change->type = REMOVE_EDGE;
       change->data.removed_edge.name = strdup(edge->name);
       change->data.removed_edge.bidirectional = edge->bidirectional;
       change->data.removed_edge.label = label_copy;
       change->data.removed_edge.source_name = strdup(edge->source->name);
       change->data.removed_edge.target_name = strdup(edge->target->name);
       
       push(graph->graph_change_stack, change);        
    }

    /* Update the source and target nodes: first decrement the appropriate
     * degrees and then remove the edge from the out_edges/in_edges pointer 
     * arrays. */ 
    edge->source->outdegree--;
    edge->target->indegree--;

    /* Remove the first occurrence of the edge pointer from the array and
     * move the remaining elements down one place to preserve the order.
     * This is important for backtracking in the matching algorithm. */
    g_ptr_array_remove(edge->source->out_edges, edge);
    g_ptr_array_remove(edge->target->in_edges, edge);

    void *label_key = GINT_TO_POINTER(edge->label_class);

    /* Remove the edge from the label classes hash table by removing the
     * edge from the edge list and inserting the updated list into
     * the hash table. */
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, label_key);
    edge_list = g_slist_remove(edge_list,edge);
    g_hash_table_replace(graph->edges_by_label, label_key, edge_list);

    /* Remove the edge from the edge array. The edge is freed as the
     * hash table was initialised with an edge freeing function. */
    g_hash_table_remove(graph->edges, edge->name);
}


void relabelNode(Graph *graph, Node *node, Label *new_label, bool change_root,
                 bool record) 
{
    if(record)
    {
       GraphChange *change = malloc(sizeof(GraphChange));
       if(change == NULL)
       {
          print_to_log("Error: Memory exhausted during graph change "
		       "construction.\n");
	  exit(1);
       } 
       change->type = RELABEL_NODE;
       change->data.relabelled_node.name = strdup(node->name);
          
       if(new_label == NULL) 
            change->data.relabelled_node.old_label = NULL;
       else change->data.relabelled_node.old_label = node->label;
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
       /* If record is true, then node->label is pointed to by the graph
        * change created above. Otherwise, node->label should be freed
        * before it is pointed to new_label. */
       if(record) node->label = new_label;
       else
       {
          freeLabel(node->label); 
          node->label = new_label; 
       }

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
       GraphChange *change = malloc(sizeof(GraphChange));
       if(change == NULL)
       {
          print_to_log("Error: Memory exhausted during graph change "
		       "construction.\n");
	  exit(1);
       } 
       change->type = RELABEL_EDGE;
       change->data.relabelled_edge.name = strdup(edge->name);
       change->data.relabelled_edge.old_label = edge->label;
       
       push(graph->graph_change_stack, change);        
    }

    /* If record is true, then edge->label is pointed to by the graph
     * change created above. Otherwise, edge->label should be freed
     * before it is pointed to new_label. */
    if(record) edge->label = new_label;
    else
    {
       freeLabel(edge->label); 
       edge->label = new_label;   
    }

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
}


/* Querying functions */

Node *getNode(Graph *graph, string id)
{
   Node *node = g_hash_table_lookup(graph->nodes, id);
   if(node == NULL) 
   {
      print_to_log("Error (getNode): Node %s not found in the graph.\n", id);
      return NULL;
   }
   else return node;
}

Edge *getEdge(Graph *graph, string id)
{
   Edge *edge = g_hash_table_lookup(graph->edges, id);
   if(edge == NULL) 
   {
      print_to_log("Error (getEdge): Edge %s not found in the graph.\n", id);
      return NULL;
   }
   else return edge;
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


Edge *getInEdge(Node *node, unsigned int index)
{
   if(index > node->in_edges->len)
   {
      print_to_log("Error (getInEdge): Passed index exceeds the size of the "
                   "inedges array.\n");
      return NULL;
   }
   else return g_ptr_array_index(node->in_edges, index);
}


Edge *getOutEdge(Node *node, unsigned int index)
{
   if(index > node->out_edges->len)
   {
      print_to_log("Error (getOutEdge): Passed index exceeds the size of the "
                   "outedges array.\n");
      return NULL;
   }
   else return g_ptr_array_index(node->out_edges, index);
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
    printf("\nGraph Description\n=================\n");
    printf("Nodes\n=====\n");
    g_hash_table_foreach(graph->nodes, printNodeFromHash, NULL);   
    printf("Edges\n=====\n");
    g_hash_table_foreach(graph->edges, printEdge, NULL);   
    printf("Root Nodes\n==========\n");
    g_slist_foreach(graph->root_nodes, printNodeFromList, NULL);   
    printf("\n");
}

void printNodeFromHash (gpointer key, gpointer value, gpointer user_data) 
{
    Node *node = (Node *)value;

    printf("Key: %s\nName: %s\n", (string) key, node->name);
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

void printNodeFromList (gpointer data, gpointer user_data) 
{
    Node *node = (Node *)data;

    printf("Name: %s\n", node->name);
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

void printEdge (gpointer key, gpointer value, gpointer user_data) 
{
    Edge *edge = (Edge *)value;

    printf("Key: %s\nName: %s\n", (string) key, edge->name);
    if(edge->bidirectional) printf("Bidirectional\n");
    printf("Label Class: %d\n", edge->label_class);
    printf("Mark: %d\n", edge->label->mark);
    printf("Label: ");
    if(edge->label->list) printList(edge->label->list);
    printf("\n");
    printf("Source: %s\n", edge->source->name);
    printf("Target: %s\n", edge->target->name);
    printf("\n");
}

void printEdgeData (gpointer data, gpointer user_data) 
{
   Edge *edge = (Edge*)data;
   printf("%s edge: %s\n", (string)user_data, edge->name);
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
     * g_hash_table_destroy will not only free the hash tables, but each
     * of the nodes and edges to which they point, with the freeNode and
     * freeEdge functions defined below. */     
    if(graph->nodes) g_hash_table_destroy(graph->nodes); 
    if(graph->edges) g_hash_table_destroy(graph->edges); 

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
    if(graph->graph_change_stack)
    {
       while(graph->graph_change_stack->top != NULL)
       {
          GraphChange *change = (GraphChange*)pop(graph->graph_change_stack);
          freeGraphChange(change);
       }
       free(graph->graph_change_stack);
    }
    if(graph) free(graph);
}

void freeNode (void *p) 
{
    Node *node = (Node *)p;
    if(node->name) free(node->name);
    if(node->label) freeLabel(node->label);
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
    if(edge->label) freeLabel(edge->label);
    if(edge) free(edge);
}

void freeLabel(Label *label)
{
    if(label->list) 
       g_list_free_full(label->list, freeListElement);
    if(label) free(label);
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

void freeGraphChange(GraphChange *change)
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


