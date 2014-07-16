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

    /* freeNode and freeEdge to be defined. Freeing of nodes and edges
     * only takes place in these arrays. */
    new_graph->nodes = g_ptr_array_new_with_free_func(freeNode);
    new_graph->edges = g_ptr_array_new_with_free_func(freeEdge);


    new_graph->nodes_by_label = g_hash_table_new(g_int_hash,g_int_equal);    
    new_graph->edges_by_label = g_hash_table_new(g_int_hash,g_int_equal);    

    new_graph->root_nodes = NULL;

    return new_graph;
}

/* Intended use: node is created by the caller with index 0. The actual index
 * is assigned in this function. 
 * Should always be passed next_node_index which keeps track of the next 
 * index in the array.
 * I am assuming GPtrArrays play nice and add items with incremented
 * index.
 */
void addNode(Graph *graph, Node *node, int index) {

    node->index = index;
    LabelClass node_label = node->label_class;

    /* Add to graph->nodes */
    g_ptr_array_add(graph->nodes, /* gpointer cast? */ node);

    /* Update graph->nodes_by_label */
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label,&node_label);
    /* insert_sorted could be called here, but probably unnecessary. */
    GSList *new_node_list = g_slist_prepend(node_list,node);
    g_hash_table_insert(graph->nodes_by_label,&node_label,new_node_list);

    /* Update graph->root_nodes */
    if(node->root) graph->root_nodes = g_slist_prepend(graph->root_nodes,node);

    graph->next_node_index = index + 1;
}


void addEdge(Graph *graph, Edge *edge, int index) {

    edge->index = index;
    LabelClass edge_label = edge->label_class;

    /* Update the source and target nodes */ 
    Node *source = edge->source;
    Node *target = edge->target;

    source->outdegree++;
    target->indegree++;

    GSList *source_out_edges = 
        g_hash_table_lookup(source->out_edges_by_label, &edge_label);
    GSList *new_out_edges = g_slist_prepend(source_out_edges, edge);
    g_hash_table_insert(source->out_edges_by_label, &edge_label, new_out_edges);

    GSList *target_in_edges = 
        g_hash_table_lookup(target->in_edges_by_label, &edge_label);
    GSList *new_in_edges = g_slist_prepend(target_in_edges, edge);
    g_hash_table_insert(target->in_edges_by_label, &edge_label, new_in_edges);

    /* Add to graph->nodes */
    g_ptr_array_add(graph->edges, /* gpointer cast? */ edge);

    /* Update graph->nodes_by_label */
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label, &edge_label);
    /* insert_sorted could be called here, but probably unnecessary. */
    GSList *new_edge_list = g_slist_prepend(edge_list, edge);
    g_hash_table_insert(graph->edges_by_label, &edge_label, new_edge_list);

    graph->next_edge_index = index + 1;
}

/* g_ptr_array_remove_fast removes an item from the array and moves the last
 * element of the array in its place. Hence, to keep the indices consistent,
 * I update the index of the last item accordingly. 
 */

/* Assumes no incident edges */

void removeNode(Graph *graph, int index) {

    /* Get the last element and set its index to that of the node to be removed. */
    Node *node_to_update = g_ptr_array_index(graph->nodes,graph->next_node_index-1);
    node_to_update->index = index;
    
    Node *node_to_delete = g_ptr_array_index(graph->nodes,index);
    LabelClass label_class = node_to_delete->label_class;
    bool is_root = node_to_delete->root;
  
    /* Remove the node from relevant structures. The pointer is freed
     * by the call to g_ptr_array_remove_fast. */
    g_ptr_array_remove_index_fast(graph->nodes, index);
 
    GSList *node_list = g_hash_table_lookup(graph->nodes_by_label,&label_class);
    GSList *new_node_list = g_slist_remove(node_list,node_to_delete);
    g_hash_table_insert(graph->nodes_by_label,&label_class,new_node_list);

    if(is_root) graph->root_nodes = g_slist_remove(graph->root_nodes,node_to_delete);
}

void removeEdge(Graph *graph, int index) {

    /* Get the last element and set its index to that of the edge to be removed. */
    Node *edge_to_update = g_ptr_array_index(graph->edges,graph->next_edge_index-1);
    edge_to_update->index = index;
    
    Node *edge_to_delete = g_ptr_array_index(graph->edges,index);
    LabelClass label_class = edge_to_delete->label_class;
  
    /* Remove the edge from relevant structures. The pointer is freed
     * by the call to g_ptr_array_remove_fast. */
    g_ptr_array_remove_index_fast(graph->edges, index);
 
    GSList *edge_list = g_hash_table_lookup(graph->edges_by_label,&label_class);
    GSList *new_edge_list = g_slist_remove(edge_list,edge_to_delete);
    g_hash_table_insert(graph->edges_by_label,&label_class,new_edge_list);

}

void relabelNode(Graph *graph, int index, GList *new_list) {

    Node *node = g_ptr_array_index(graph->nodes,index);
    node->list = new_list;
    
}

void relabelEdge(Graph *graph, int index, GList *new_list) {

    Edge *edge = g_ptr_array_index(graph->edges,index);
    edge->list = new_list;

}

void printGraph (Graph *graph) {
    printf("\nNext node index: %d\n", graph->next_node_index);
    printf("Next edge index: %d\n\n", graph->next_edge_index);
    g_ptr_array_foreach(graph->nodes, printNode, NULL);   
    g_ptr_array_foreach(graph->edges, printEdge, NULL);   
    g_slist_foreach(graph->root_nodes, printNode, NULL);   
    printf("\n");
}

void printNode (gpointer data, gpointer user_data) {

    Node *node = (Node *)data;

    printf("Index: %d\nName: %s\n", node->index, node->name);
    if(node->root) printf("Root\n");
    printf("Label Class: %d\n", node->label_class);
    printf("Mark: %d\n", node->mark);
    g_list_foreach(node->list, printListElement, NULL);
    printf("Indegree: %d\nOutdegree: %d\n", node->indegree, node->outdegree);
    g_hash_table_foreach(node->in_edges_by_label, printEdgeData, "Incoming");
    g_hash_table_foreach(node->out_edges_by_label, printEdgeData, "Outgoing");
    printf("\n");
}

void printEdge (gpointer data, gpointer user_data) {

    Edge *edge = (Edge *)data;

    printf("Index: %d\nName: %s\n", edge->index, edge->name);
    if(edge->bidirectional) printf("Bidirectional\n");
    printf("Label Class: %d\n", edge->label_class);
    printf("Mark: %d\n", edge->mark);
    g_list_foreach(edge->list, printListElement, NULL);
    printf("Source: %d-%s\n", edge->source->index, edge->source->name);
    printf("Target: %d-%s\n", edge->target->index, edge->target->name);
    printf("\n");
}

void printEdgeData (gpointer key, gpointer value, gpointer user_data) {

    GSList *current_edge = NULL;

    for(current_edge = value; current_edge; current_edge = current_edge->next)
    {
        Edge* edge = (Edge *)(current_edge->data);
        printf("%s Edges: %d-%s\nLabel Class: %d\n", (char *)user_data, 
               edge->index, edge->name, *(int *)key);
    }
}


void printListElement(gpointer data, gpointer user_data) {
    ListElement *elem = (ListElement *)data;
    
    switch(elem->type) {

	case VARIABLE: 
             printf("Variable %s", elem->value.name);
             break;

	case INT_CONSTANT: 
             printf("Integer %d", elem->value.number);
             break;

        case CHARACTER_CONSTANT:
             printf("Character %s", elem->value.string);
             break;

	case STRING_CONSTANT:
             printf("String %s", elem->value.string);
             break;

	case INDEGREE:
	     printf("Indegree %s", elem->value.node_id);
	     break;
 
        case OUTDEGREE:
	     printf("Outdegree %s", elem->value.node_id);
	     break;

	case LIST_LENGTH:
             printf("Llength ");
             g_list_foreach(elem->value.list_arg, printListElement, NULL);
             break;

	case STRING_LENGTH:
             printf("Slength ");
             printListElement(elem->value.str_arg, NULL);
             break;

	case NEG:
             printf("- ");
             printListElement(elem->value.exp, NULL);
             break;

	case ADD:
             printListElement(elem->value.bin_op.left_exp, NULL);
             printf(" + ");
             printListElement(elem->value.bin_op.right_exp, NULL);
             break;

	case SUBTRACT:
             printListElement(elem->value.bin_op.left_exp, NULL);
             printf(" - ");
             printListElement(elem->value.bin_op.right_exp, NULL);
             break;

	case MULTIPLY:
             printListElement(elem->value.bin_op.left_exp, NULL);
             printf(" * ");
             printListElement(elem->value.bin_op.right_exp, NULL);
             break;

	case DIVIDE:
             printListElement(elem->value.bin_op.left_exp, NULL);
             printf(" / ");
             printListElement(elem->value.bin_op.right_exp, NULL);
             break;

	case CONCAT:
             printListElement(elem->value.bin_op.left_exp, NULL);
             printf(" . ");
             printListElement(elem->value.bin_op.right_exp, NULL);
             break;

	default: printf("Unexpected List Element Type: %d\n",
                       (int)elem->type); 
                 break;
    }

    printf(" : ");
}

void freeGraph (Graph *graph) {
    if(graph->nodes) g_ptr_array_free(graph->nodes,TRUE);
    if(graph->edges) g_ptr_array_free(graph->edges,TRUE);
    if(graph) free(graph);
}

void freeNode (void *p) {
    Node *node = (Node *)p;
    if(node->name) free(node->name);
    if(node->list) g_list_free_full(node->list,freeListElement);
    if(node->in_edges_by_label) g_hash_table_destroy(node->in_edges_by_label); 
    if(node->out_edges_by_label) g_hash_table_destroy(node->out_edges_by_label); 
    if(node) free(node);
}

void freeEdge (void *p) {
    Edge *edge = (Edge *)p;
    if(edge->name) free(edge->name);
    /* When freeing a graph, nodes are freed first, so no need to free
     * source and target here. */
    if(edge->list) g_list_free_full(edge->list,freeListElement);
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
