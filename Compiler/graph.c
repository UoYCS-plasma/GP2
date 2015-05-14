#include "graph.h"

Node dummy_node = {-1, false, {NONE, 0, NULL}, NULL, 0, NULL, 0, 0, 0, 0, 0, 0, -1};
Edge dummy_edge = {-1, false, {NONE, 0, NULL}, -1, -1, -1};

/* ===============
 * Graph Functions
 * =============== */
Graph *newGraph(int nodes, int edges) 
{
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
   int index;
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
   graph->node_classes = NULL;
   graph->edge_classes = NULL;
   graph->root_nodes = NULL;
   return graph;
}

int addNode(Graph *graph, bool root, Label label) 
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
   graph->nodes[index].label.mark = label.mark;
   graph->nodes[index].label.length = label.length;
   graph->nodes[index].label.list = label.list;
   addLabelClassIndex(graph, true, label, index);
    
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

int addEdge(Graph *graph, bool bidirectional, Label label, int source_index,
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

   graph->edges[index].label.mark = label.mark;
   graph->edges[index].label.length = label.length;
   graph->edges[index].label.list = label.list;
   addLabelClassIndex(graph, false, label, index);

   /* For the source's outedge store and the target's inedge store, do the
    * following:
    * (1) If the current index exceeds the bounds of the edge pointer array,
    *     grow the edge pointer store. 
    * (2) If the edge store has entered the extra_edges array, put
    *     the new edge there. Otherwise, put the new edge in the edges array. 
    * (3) Increment the source's out index and outdegree, or increment the
    *     target's out index and indegree. */
   Node *source = getNode(graph, source_index);
   if(source->out_index == source->out_pool_size)
   {
      if(source->out_pool_size == 0) source->out_pool_size = 2;
      else source->out_pool_size *= 2;

      source->out_edges = realloc(source->out_edges, 
                                  source->out_pool_size * sizeof(int));
      if(source->out_edges == NULL)
      {
         print_to_log("Error (addEdge): malloc failure.\n");
         exit(1);
      }
   }
   source->out_edges[source->out_index++] = index;
   if(bidirectional) source->bidegree++; else source->outdegree++;

   Node *target = getNode(graph, target_index);
   if(target->in_index >= target->in_pool_size)
   {
      if(target->in_pool_size == 0) target->in_pool_size = 2;
      else target->in_pool_size *= 2;

      target->in_edges = realloc(target->in_edges,
                                 target->in_pool_size * sizeof(int));
      if(target->in_edges == NULL)
      {
         print_to_log("Error (addEdge): malloc failure.\n");
         exit(1);
      }
   }
   target->in_edges[target->in_index++] = index;
   if(bidirectional) target->bidegree++; else target->indegree++;

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
   removeLabelClassIndex(graph, true, node->label, node->label_table_index);
   /* Deallocate memory in the node structure. */
   freeLabel(node->label);
   if(node->out_edges != NULL) free(node->out_edges);
   if(node->in_edges != NULL) free(node->in_edges); 
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
   removeLabelClassIndex(graph, false, edge->label, edge->label_table_index);
   freeLabel(edge->label);

   Node *source = getNode(graph, edge->source);
   Node *target = getNode(graph, edge->target);
   int counter;
   for(counter = 0; counter < source->out_index; counter++)
   {
      if(source->out_edges[counter] == index) 
      {
         source->out_edges[counter] = -1;
         /* If the index of the removed edge directly precedes the source's
          * out_index, decrement out_index until it refers to an array
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
      }
   }
   for(counter = 0; counter < target->in_index; counter++)
   {
      if(target->in_edges[counter] == index) 
      {
         target->in_edges[counter] = -1;
         /* If the index of the removed edge directly precedes the target's
          * in_index, decrement in_index until it refers to an array
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
      }
   }
   /* removeEdge is never called on a rule graph, so there is no need to
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

void relabelNode(Graph *graph, int index, Label new_label) 
{
   Node *node = getNode(graph, index);
   LabelClass old_label_class = getLabelClass(node->label);
   LabelClass new_label_class = getLabelClass(new_label);
   /* If the label classes or the marks differ, update the label class tables. */
   if(node->label.mark != new_label.mark || old_label_class != new_label_class)
   {
      removeLabelClassIndex(graph, true, node->label, node->label_table_index);
      addLabelClassIndex(graph, true, new_label, index);
   }
   freeLabel(node->label); 
   node->label = new_label;
}

void changeRoot(Graph *graph, int index)
{
   Node *node = getNode(graph, index);
   if(node->root) removeRootNode(graph, node->index);
   else addRootNode(graph, node->index);
   node->root = !node->root;
}

void relabelEdge(Graph *graph, int index, Label new_label)
{	
   Edge *edge = getEdge(graph, index);
   LabelClass old_label_class = getLabelClass(edge->label);
   LabelClass new_label_class = getLabelClass(new_label);
   /* If the label classes or the marks differ, update the label class tables. */
   if(edge->label.mark != new_label.mark || old_label_class != new_label_class)
   {
      removeLabelClassIndex(graph, true, edge->label, edge->label_table_index);
      addLabelClassIndex(graph, true, new_label, index);
   }
   freeLabel(edge->label); 
   edge->label = new_label;
}

void changeBidirectional(Graph *graph, int index)
{
   Edge *edge = getEdge(graph, index);
   edge->bidirectional = !edge->bidirectional;
}

/* ===========================
 * Label Class Table Functions 
 * =========================== */
LabelClassTable **makeLabelClassTable(void)
{
   LabelClassTable **table = malloc(sizeof(LabelClassTable*) * NUMBER_OF_MARKS * 
                                    NUMBER_OF_CLASSES);
   if(table == NULL)
   {
      print_to_log("error (makeLabelClassTable): malloc failure.\n");
      exit(1);
   }
   int index;
   for(index = 0; index < NUMBER_OF_MARKS * NUMBER_OF_CLASSES; index++) table[index] = NULL;
   return table;
}

void addLabelClassIndex(Graph *graph, bool node, Label label, int index)
{
   if(node && graph->node_classes == NULL) return;
   if(!node && graph->edge_classes == NULL) return;

   LabelClassTable *table = NULL;
   LabelClass label_class = getLabelClass(label);
   /* <label.mark> * NUMBER_OF_CLASSES is the index of the first column of row
    * <label.mark>. label_class is added to this to get the correct column. */
   int table_index = label.mark * NUMBER_OF_CLASSES + label_class;
   if(node) table = graph->node_classes[table_index];
   else table = graph->edge_classes[table_index];

   if(table == NULL)
   {
      table = malloc(sizeof(LabelClassTable));
      if(table == NULL)
      {
         print_to_log("error (addNodeLabelClassIndex): malloc failure.\n");
         exit(1);
      }
      table->pool_size = 4;
      table->index = 0;
      table->items = calloc(table->pool_size, sizeof(int));
      if(table->items == NULL)
      {
         print_to_log("Error (addNodeLabelClassIndex): malloc failure.\n");
         exit(1);
      }
      if(node) graph->node_classes[table_index] = table;
      else graph->edge_classes[table_index] = table;
   }
   else
   {
      if(table->index == table->pool_size)
      {
         table->pool_size *= 2;
         table->items = realloc(table->items, table->pool_size * sizeof(int));
         if(table->items == NULL)
         {
            print_to_log("Error (addNodeLabelClassIndex): malloc failure.\n");
            exit(1);
         }
      }
   }
   table->items[table->index++] = index;
   if(node) graph->nodes[index].label_table_index = table->index - 1;
   else graph->edges[index].label_table_index = table->index - 1;
}

void removeLabelClassIndex(Graph *graph, bool node, Label label, int item_index)
{
   if(node && graph->node_classes == NULL) return;
   if(!node && graph->edge_classes == NULL) return;
   LabelClassTable *table = NULL;
   LabelClass label_class = getLabelClass(label);
   /* LABEL_CLASS is the number of columns. <mark> * LABEL_CLASS is the index
    * of the first column of row <mark>. label_class is added to this to get
    * the correct column. */
   int table_index = label.mark * NUMBER_OF_CLASSES + label_class;
   if(node) table = graph->node_classes[table_index];
   else table = graph->edge_classes[table_index];

   table->items[item_index] = -1;
   /* If the index of the removed item directly precedes the last index,
    * decrement the index until it refers to an array element one place
    * beyond the right-most -1 element. */
   if(item_index == table->index - 1)
   {
      table->index--;
      while(table->index > 0)
      {
         if(table->items[table->index] == -1) table->index--;
         else break;
      }
   }
}

/* ========================
 * Graph Querying Functions 
 * ======================== */
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

int getOutEdge(Node *node, int index)
{
   if(index > node->out_index) 
   {
      print_to_log("Error (getOutEdge): Passed index exceeds size of the "
                   "node's out_edges array.\n");
      exit(1);
   }
   return node->out_edges[index];
}

int getInEdge(Node *node, int index)
{
   if(index > node->in_index) 
   {
      print_to_log("Error (getInEdge): Passed index exceeds size of the "
                   "node's in_edges array.\n");
      exit(1);
   }
   return node->in_edges[index];
}

int getSource(Edge *edge) 
{
   return edge->source;
}

int getTarget(Edge *edge) 
{
   return edge->target;
}

Label getNodeLabel(Graph *graph, int index) 
{
   Node *node = getNode(graph, index);
   return node->label;
}

Label getEdgeLabel(Graph *graph, int index) 
{
   Edge *edge = getEdge(graph, index);
   return edge->label;
}

int getIndegree(Graph *graph, int index) 
{
   Node *node = getNode(graph, index);
   return node->indegree;
}

int getOutdegree(Graph *graph, int index) 
{
   Node *node = getNode(graph, index);
   return node->outdegree;
}

LabelClassTable *getNodesByLabel(Graph *graph, Label label) 
{
   if(graph->node_classes == NULL) return NULL;
   LabelClass label_class = getLabelClass(label);
   return graph->node_classes[label.mark * NUMBER_OF_CLASSES + label_class];
}
   
LabelClassTable *getEdgesByLabel(Graph *graph, Label label) 
{
   if(graph->edge_classes == NULL) return NULL;
   LabelClass label_class = getLabelClass(label);
   return graph->edge_classes[label.mark * NUMBER_OF_CLASSES + label_class];
}


void printGraph(Graph *graph, FILE *file) 
{
   int index, node_count = 0, edge_count = 0;
   if(graph == NULL || graph->number_of_nodes == 0) 
   {
      fprintf(file, "[ | ]\n");
      return;
   }
   fprintf(file, "[ ");
   for(index = 0; index < graph->node_index; index++)
   {
      Node *node = getNode(graph, index);
      if(node->index >= 0) 
      {
         /* Five nodes per line */
         if(node_count != 0 && node_count % 5 == 0) fprintf(file, "\n  ");
         node_count++;
         if(node->root) fprintf(file, "(n%d(R), ", index);
         else fprintf(file, "(n%d, ", index);
         printLabel(node->label, file);
         fprintf(file, ") ");
      }
   }
   if(graph->number_of_edges == 0)
   {
      fprintf(file, "| ]\n\n");
      return;
   }
   fprintf(file, "|\n  ");
   for(index = 0; index < graph->edge_index; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge->index >= 0) 
      {
         /* Three edges per line */
         if(edge_count != 0 && edge_count % 3 == 0) fprintf(file, "\n  ");
         edge_count++;
         if(edge->bidirectional) fprintf(file, "(e%d(B), ", index);
         else fprintf(file, "(e%d, ", index);
         fprintf(file, "n%d, n%d, ", edge->source, edge->target);
         printLabel(edge->label, file);
         fprintf(file, ") ");
      }
   }
   fprintf(file, "]\n\n");
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
         freeLabel(node->label);
         if(node->out_edges != NULL) free(node->out_edges);
         if(node->in_edges != NULL) free(node->in_edges);
      }  
   }
   if(graph->nodes) free(graph->nodes);

   for(index = 0; index < graph->edge_index; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge->index >= 0) freeLabel(edge->label);
   }
   if(graph->edges) free(graph->edges);

   if(graph->free_node_slots) free(graph->free_node_slots);
   if(graph->free_edge_slots) free(graph->free_edge_slots);

   if(graph->node_classes != NULL)
   {
      for(index = 0; index < NUMBER_OF_MARKS * NUMBER_OF_CLASSES; index++)
      {
         if(graph->node_classes[index] != NULL)
         {
            if(graph->node_classes[index]->items != NULL) 
               free(graph->node_classes[index]->items); 
            free(graph->node_classes[index]);
         }
      }
      free(graph->node_classes);
   }
   
   if(graph->edge_classes != NULL)
   {
      for(index = 0; index < NUMBER_OF_MARKS * NUMBER_OF_CLASSES; index++)
      { 
         if(graph->edge_classes[index] != NULL)
         {
            if(graph->edge_classes[index]->items != NULL) 
               free(graph->edge_classes[index]->items); 
            free(graph->edge_classes[index]);
         }
      }
      free(graph->edge_classes);
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
   free(graph);
}

