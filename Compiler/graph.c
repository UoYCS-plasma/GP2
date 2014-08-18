/* ///////////////////////////////////////////////////////////////////////////

  ================================
  graph.c - Chris Bak (14/07/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "graph.h"

Graph *newGraph(void) {

    Graph *new_graph = malloc(sizeof(Graph));

    if(new_graph == NULL) {
      printf("Memory exhausted during graph construction.\n");
      exit(0);
    }
    
    new_graph->next_node_index = 0;
    new_graph->next_edge_index = 0;

    /* The pointer arrays handle all the freeing of Node and Edge structs.
       Hence they are passed the freeing functions for nodes and edges. */
    new_graph->nodes = g_ptr_array_new_with_free_func(freeNode);
    new_graph->edges = g_ptr_array_new_with_free_func(freeEdge);

    new_graph->nodes_by_label = g_hash_table_new(g_direct_hash,g_direct_equal);    
    new_graph->edges_by_label = g_hash_table_new(g_direct_hash,g_direct_equal);    

    new_graph->root_nodes = NULL;

    return new_graph;
}

/* Intended use: the node structure is created by the caller with index 0. 
 * The actual index of the node is assigned in this function by the third
 * argument. The third argument is always graph->next_node_index which keeps
 * track of the next index in the node array.
 */
void addNode(Graph *graph, Node *node, int index) {

    node->index = index;
    void *hash_key = GINT_TO_POINTER(node->label_class);

    /* Add to graph->nodes */
    g_ptr_array_add(graph->nodes, node);

    /* Update graph->nodes_by_label */
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, hash_key);
    node_list = g_slist_prepend(node_list,node);
    g_hash_table_replace(graph->nodes_by_label, hash_key, node_list);
    
    /* Update graph->root_nodes */
    if(node->root) graph->root_nodes = g_slist_prepend(graph->root_nodes,node);

    graph->next_node_index = index + 1;
}


/* Intended use: the edge structure is created by the caller with index 0. 
 * The actual index of the edge is assigned in this function by the third
 * argument. The third argument is graph->next_edge_index which keeps
 * track of the next index in the edge array.
 */
void addEdge(Graph *graph, Edge *edge, int index) {

    edge->index = index;
    void *hash_key = GINT_TO_POINTER(edge->label_class);

    /* Update the source and target nodes with the new edge: first increment
     * the appropriate degrees and then add the edge to the out_edges/in_edges
     * tables. */ 
    Node *source = edge->source;
    Node *target = edge->target;

    source->outdegree++;
    target->indegree++;

    GSList *out_edges = 
        g_hash_table_lookup(source->out_edges_by_label, hash_key);
    out_edges = g_slist_prepend(out_edges, edge);
    g_hash_table_replace(source->out_edges_by_label, hash_key, out_edges);

    GSList *in_edges = 
        g_hash_table_lookup(target->in_edges_by_label, hash_key);
    in_edges = g_slist_prepend(in_edges, edge);
    g_hash_table_replace(target->in_edges_by_label, hash_key, in_edges);


    /* Add to graph->edges */
    g_ptr_array_add(graph->edges, edge);

    /* Update graph->edges_by_label */
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, hash_key);
    edge_list = g_slist_prepend(edge_list, edge);
    g_hash_table_replace(graph->edges_by_label, hash_key, edge_list);

    graph->next_edge_index = index + 1;
}

/* Removes a node from the graph. The function g_ptr_array_remove_fast is used
 * to remove the node from the pointer array. It also moves the last element
 * of the array in its place. Hence, to keep the indices consistent, I update
 * the index of the last item to the index of the deleted node.
 * Assumes at least one node in the graph and no incident edges. */

void removeNode(Graph *graph, int index) {
    /* Get the last element and set its index to that of the node to be removed. */
    Node *node_to_update = g_ptr_array_index(graph->nodes,graph->next_node_index-1);
    node_to_update->index = index;
    
    Node *node_to_delete = g_ptr_array_index(graph->nodes,index);
    void *hash_key = GINT_TO_POINTER(node_to_delete->label_class);
    bool is_root = node_to_delete->root;
  
    /* Remove the node from the pointer array. */
    g_ptr_array_remove_index_fast(graph->nodes, index);
    graph->next_node_index--;

    /* Remove the node from the hash table. */
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, hash_key);
    node_list = g_slist_remove(node_list, node_to_delete);
    g_hash_table_insert(graph->nodes_by_label, hash_key, node_list);

    /* Remove the node from the root node list if necessary. */
    if(is_root) graph->root_nodes = g_slist_remove(graph->root_nodes, node_to_delete);
}

/* Removes an edge from the graph. The function g_ptr_array_remove_fast is used
 * to remove the edge from the pointer array. It also moves the last element
 * of the array in its place. Hence, to keep the indices consistent, I update
 * the index of the last item to the index of the deleted node.
 * Assumes at least one node in the graph and no incident edges. */

void removeEdge(Graph *graph, int index) {
    /* Get the last element and set its index to that of the edge to be 
     * removed. */
    Edge *edge_to_update = 
       g_ptr_array_index(graph->edges,graph->next_edge_index-1);
    edge_to_update->index = index;
    
    Edge *edge_to_delete = g_ptr_array_index(graph->edges,index);
    void *hash_key = GINT_TO_POINTER(edge_to_delete->label_class);
 
    /* Update the source and target nodes with the deleted edge: first 
     * decrement the appropriate degrees and then add the edge to the 
     * out_edges/in_edges tables. */ 
    Node *source = edge_to_delete->source;
    Node *target = edge_to_delete->target;

    source->outdegree--;
    target->indegree--;

    GSList *out_edges = 
        g_hash_table_lookup(source->out_edges_by_label, hash_key);
    out_edges = g_slist_remove(out_edges, edge_to_delete);
    g_hash_table_replace(source->out_edges_by_label, hash_key, out_edges);

    GSList *in_edges = 
        g_hash_table_lookup(target->in_edges_by_label, hash_key);
    in_edges = g_slist_remove(in_edges, edge_to_delete);
    g_hash_table_replace(target->in_edges_by_label, hash_key, in_edges);

    /* Remove the edge from the edge array. The pointer is freed by the call to
     * g_ptr_array_remove_fast. */
    g_ptr_array_remove_index_fast(graph->edges, index);
    graph->next_edge_index--;

    /* Remove the edge from the hash table. */
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, hash_key);
    edge_list = g_slist_remove(edge_list,edge_to_delete);
    g_hash_table_replace(graph->edges_by_label, hash_key, edge_list);

}

void relabelNode(Graph *graph, int index, GList *new_list, 
                 LabelClass new_label_class) {

    Node *node = g_ptr_array_index(graph->nodes, index);
    if(node->list) g_list_free_full(node->list, freeListElement);
    node->list = new_list;

    /* If the label classes differ, the graph's nodes_by_label table needs to 
     * be updated. */
    if(node->label_class != new_label_class) {

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

void relabelEdge(Graph *graph, int index, GList *new_list, 
                 LabelClass new_label_class) {

    Edge *edge = g_ptr_array_index(graph->edges, index);
    if(edge->list) g_list_free_full(edge->list, freeListElement);
    edge->list = new_list;

    /* If the label classes differ, the graph's edges_by_label table needs to
     * be updated. */
    if(edge->label_class != new_label_class) {

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
 
       /* The hash tables of the source and target node must also be updated. 
        */
       Node *source = edge->source;
       Node *target = edge->target;

       GSList *old_src_edge_list = 
           g_hash_table_lookup(source->out_edges_by_label, hash_key_1);
       old_src_edge_list = g_slist_remove(old_src_edge_list, edge); 
       g_hash_table_replace(source->out_edges_by_label, hash_key_1, 
                            old_src_edge_list);

       GSList *new_src_edge_list =
           g_hash_table_lookup(source->out_edges_by_label, hash_key_2);
       new_src_edge_list = g_slist_prepend(new_src_edge_list, edge);
       g_hash_table_replace(source->out_edges_by_label, hash_key_2, 
                            new_src_edge_list);

       GSList *old_tgt_edge_list = 
	  g_hash_table_lookup(target->in_edges_by_label, hash_key_1);
       old_tgt_edge_list = g_slist_remove(old_tgt_edge_list, edge);
       g_hash_table_replace(target->in_edges_by_label, hash_key_1, 
                            old_tgt_edge_list);

       GSList *new_tgt_edge_list = 
	  g_hash_table_lookup(target->in_edges_by_label, hash_key_2);
       new_tgt_edge_list = g_slist_prepend(new_tgt_edge_list, edge);
       g_hash_table_replace(target->in_edges_by_label, hash_key_2, 
                            new_tgt_edge_list);
    }   
}


/* Querying functions */

GSList *getNodes(Graph *graph, LabelClass label_class) {
   GSList *node_list = g_hash_table_lookup(graph->nodes_by_label, &label_class);
   return node_list;
}
   

GSList *getEdges(Graph *graph, LabelClass label_class) {
   GSList *node_list = g_hash_table_lookup(graph->edges_by_label, &label_class);
   return node_list;
}

GSList *getInEdges(Node *node, LabelClass label_class) {
   GSList *edge_list = g_hash_table_lookup(node->in_edges_by_label, &label_class);
   return edge_list;
}


GSList *getOutEdges(Node *node, LabelClass label_class)  {
   GSList *edge_list = g_hash_table_lookup(node->out_edges_by_label, &label_class);
   return edge_list;
}


Node *getSource(Edge *edge) {
   return edge->source;
}

Node *getTarget(Edge *edge) {
   return edge->target;
}


GList *getNodeLabel(Node *node) {
   return node->list;
}

GList *getEdgeLabel(Edge *edge) {
   return edge->list;
}

int getIndegree (Node *node) {
   return node->indegree;
}

int getOutdegree (Node *node) {
   return node->outdegree;
}

void printGraph (Graph *graph) {
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

void printNode (gpointer data, gpointer user_data) {

    Node *node = (Node *)data;

    printf("Name: %s\nIndex: %d\n", node->name, node->index);
    if(node->root) printf("Root\n");
    printf("Label Class: %d\n", node->label_class);
    printf("Mark: %d\n", node->mark);
    printf("Label: ");
    if(node->list) printList(node->list);
    else printf("empty");
    printf("\n");
    printf("Indegree: %d\nOutdegree: %d\n", node->indegree, node->outdegree);
    if(node->in_edges_by_label) 
        g_hash_table_foreach(node->in_edges_by_label, printEdgeData, "Incoming");
    if(node->out_edges_by_label) 
        g_hash_table_foreach(node->out_edges_by_label, printEdgeData, "Outgoing");
    printf("\n");
}

void printEdge (gpointer data, gpointer user_data) {

    Edge *edge = (Edge *)data;

    printf("Name: %s\nIndex: %d\n", edge->name, edge->index);
    if(edge->bidirectional) printf("Bidirectional\n");
    printf("Label Class: %d\n", edge->label_class);
    printf("Mark: %d\n", edge->mark);
    printf("Label: ");
    if(edge->list) printList(edge->list);
    else printf("empty");
    printf("\n");
    printf("Source: %d-%s\n", edge->source->index, edge->source->name);
    printf("Target: %d-%s\n", edge->target->index, edge->target->name);
    printf("\n");
}

void printEdgeData (gpointer key, gpointer value, gpointer user_data) {

    GSList *current_edge = NULL;
    /* Do not print anything if the edge list is empty. */
    if(value == NULL) return;
    else {
       printf("%s edges: ", (string)user_data);
       for(current_edge = value; current_edge; current_edge = current_edge->next)
       {
	   Edge* edge = (Edge *)(current_edge->data);
	   printf("%d-%s-%d ", edge->index, edge->name, edge->label_class);
       }
       printf("\n");
    }
} 


void printList(GList *list) {

    while(list) {
        printListElement((ListElement*)list->data);
        if(list->next) printf(" : ");
        list = list->next;
    } 
}

void printListElement(ListElement* elem) {
       
    switch(elem->type) {

	case VARIABLE: 
	     printf("%s", elem->value.name);
	     break;

	case INT_CONSTANT: 
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


void freeGraph (Graph *graph) {
    if(graph->nodes) g_ptr_array_free(graph->nodes,TRUE);
    if(graph->edges) g_ptr_array_free(graph->edges,TRUE);
    if(graph->nodes_by_label) {
       g_hash_table_foreach(graph->nodes_by_label, freeGSList, NULL);
       g_hash_table_destroy(graph->nodes_by_label); 
    }
    if(graph->edges_by_label) {
        g_hash_table_foreach(graph->edges_by_label, freeGSList, NULL);
        g_hash_table_destroy(graph->edges_by_label);  
    }
    if(graph->root_nodes) g_slist_free(graph->root_nodes);
    if(graph) free(graph);
}

void freeNode (void *p) {
    Node *node = (Node *)p;
    if(node->name) free(node->name);
    if(node->list) g_list_free_full(node->list, freeListElement);
    if(node->in_edges_by_label) {
        g_hash_table_foreach(node->in_edges_by_label, freeGSList, NULL);
        g_hash_table_destroy(node->in_edges_by_label); 
    }
    if(node->out_edges_by_label) {
        g_hash_table_foreach(node->out_edges_by_label, freeGSList, NULL);
        g_hash_table_destroy(node->out_edges_by_label); 
    }
    if(node) free(node);
}

void freeGSList(gpointer key, gpointer value, gpointer data) {
    g_slist_free(value);
}

void freeEdge (void *p) {
    Edge *edge = (Edge *)p;
    if(edge->name) free(edge->name);
    /* When freeing a graph, nodes are freed first, so no need to free
     * source and target here. */
    if(edge->list) g_list_free_full(edge->list, freeListElement);
    if(edge) free(edge);
}

void freeListElement(void *p)
{
     ListElement* elem = (ListElement*)p;

     switch(elem->type) {

	case VARIABLE:

	     if(elem->value.name)
               free(elem->value.name);

             break;


	case INT_CONSTANT:

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
