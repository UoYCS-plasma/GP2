#include "graph.h"

Node dummy_node = {-1, false, {NONE, 0, NULL}, NULL, 0, NULL, 0, 0, 0, 0, 0, -1};
Edge dummy_edge = {-1, {NONE, 0, NULL}, -1, -1, -1};

/* ===============
 * Graph Functions
 * =============== */
Graph *newGraph(int nodes, int edges) 
{
   Graph *graph = malloc(sizeof(Graph));
   if(graph == NULL) 
   {
      print_to_log("Error (newGraph): malloc failure.\n");
      exit(1);
   }
   graph->nodes = calloc(nodes, sizeof(Node));
   if(graph->nodes == NULL)
   {
      print_to_log("Error (newGraph): malloc failure.\n");
      exit(1);
   }
   graph->free_node_slots = calloc(nodes, sizeof(int));
   if(graph->free_node_slots == NULL)
   {
      print_to_log("Error (newGraph): malloc failure.\n");
      exit(1);
   }
   int index;
   for(index = 0; index < nodes; index++)
   {
      graph->nodes[index] = dummy_node;
      graph->free_node_slots[index] = -1;
   }
   graph->edges = calloc(edges, sizeof(Edge));
   if(graph->edges == NULL)
   {
      print_to_log("Error (newGraph): malloc failure.\n");
      exit(1);
   }
   graph->free_edge_slots = calloc(edges, sizeof(int));
   if(graph->free_edge_slots == NULL)
   {
      print_to_log("Error (newGraph): malloc failure.\n");
      exit(1);
   }
   for(index = 0; index < edges; index++)
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
      if(index == graph->node_pool_size) 
      {
         graph->node_pool_size *= 2;
         Node *new_nodes = realloc(graph->nodes, graph->node_pool_size * sizeof(Node));
         if(new_nodes == NULL)
         {
            print_to_log("Error (addNode): malloc failure.\n");
            exit(1);
         }
         graph->nodes = new_nodes;

         int *new_node_slots = realloc(graph->free_node_slots,
                                       graph->node_pool_size * sizeof(int));
         if(new_node_slots == NULL)
         {
            print_to_log("Error (addNode): malloc failure.\n");
            exit(1);
         }
         graph->free_node_slots = new_node_slots;

         /* Initialise the new array elements. */
         int count;
         for(count = index; count < graph->node_pool_size; count++)
         {
            graph->nodes[count] = dummy_node;
            graph->free_node_slots[count] = -1;
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
   if(graph->classes) addLabelClassIndex(graph, true, label, index);
    
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

int addEdge(Graph *graph,Label label, int source_index, int target_index) 
{
   /* Get the index of the new edge by examining the free edge slots array.
    * A non-negative array element is the index of a hole in the graph's edge
    * array. If there are no free slots, use the graph's edge index. */
   int index = graph->free_edge_slots[graph->free_edge_index];
   if(index == -1) 
   {
      index = graph->edge_index++;
      if(index == graph->edge_pool_size) 
      {
         graph->edge_pool_size *= 2;
         Edge *new_edges = realloc(graph->edges, graph->edge_pool_size * sizeof(Edge));
         if(new_edges == NULL)
         {
            print_to_log("Memory exhausted during extra edge allocation.\n");
            exit(1);
         }
         graph->edges = new_edges;

         int *new_edge_slots = realloc(graph->free_edge_slots,
                                       graph->edge_pool_size * sizeof(int));
         if(new_edge_slots == NULL)
         {
            print_to_log("Memory exhausted during extra edge allocation.\n");
            exit(1);
         }
         graph->free_edge_slots = new_edge_slots;

         /* Initialise the new array elements. */
         int count;
         for(count = index; count < graph->edge_pool_size; count++)
         {
            graph->edges[count] = dummy_edge;
            graph->free_edge_slots[count] = -1;
         }
      }
   }
   else
   {
      graph->free_edge_slots[graph->free_edge_index] = -1;
      if(graph->free_edge_index > 0) graph->free_edge_index--;
   }

   graph->edges[index].index = index;
   graph->edges[index].source = source_index;
   graph->edges[index].target = target_index;

   graph->edges[index].label.mark = label.mark;
   graph->edges[index].label.length = label.length;
   graph->edges[index].label.list = label.list;
   if(graph->classes) addLabelClassIndex(graph, false, label, index);

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

      int *new_out_edges = realloc(source->out_edges, source->out_pool_size * sizeof(int));
      if(new_out_edges == NULL)
      {
         print_to_log("Error (addEdge): malloc failure.\n");
         exit(1);
      }
      source->out_edges = new_out_edges;
      /* Initialise the new array elements. */
      int count;
      for(count = source->out_index; count < source->out_pool_size; count++)
         source->out_edges[count] = -1;
   }
   source->out_edges[source->out_index++] = index;
   source->outdegree++;

   Node *target = getNode(graph, target_index);
   if(target->in_index >= target->in_pool_size)
   {
      if(target->in_pool_size == 0) target->in_pool_size = 2;
      else target->in_pool_size *= 2;

      int *new_in_edges = realloc(target->in_edges, target->in_pool_size * sizeof(int));
      if(new_in_edges == NULL)
      {
         print_to_log("Error (addEdge): malloc failure.\n");
         exit(1);
      }
      target->in_edges = new_in_edges;
      /* Initialise the new array elements. */
      int count;
      for(count = target->in_index; count < target->in_pool_size; count++)
         target->in_edges[count] = -1;
   }
   target->in_edges[target->in_index++] = index;
   target->indegree++;

   graph->number_of_edges++;
   return index;
}

void removeNode(Graph *graph, int index, bool free_label)
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
   if(graph->classes) removeLabelClassIndex(graph, true, node->label, node->label_table_index);
   /* Deallocate memory in the node structure. */
   if(free_label) freeLabel(node->label);
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

void removeEdge(Graph *graph, int index, bool free_label) 
{
   Edge *edge = getEdge(graph, index);
   if(graph->classes) removeLabelClassIndex(graph, false, edge->label, edge->label_table_index);
   if(free_label) freeLabel(edge->label);

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

void relabelNode(Graph *graph, int index, Label new_label, bool free_label) 
{
   Node *node = getNode(graph, index);
   if(graph->classes)
   {
      LabelClass old_label_class = getLabelClass(node->label);
      LabelClass new_label_class = getLabelClass(new_label);
      /* If the label classes or the marks differ, update the label class tables. */
      if(node->label.mark != new_label.mark || old_label_class != new_label_class)
      {
         removeLabelClassIndex(graph, true, node->label, node->label_table_index);
         addLabelClassIndex(graph, true, new_label, index);
      }
   }
   if(free_label) freeLabel(node->label); 
   node->label = new_label;
}

void changeRoot(Graph *graph, int index)
{
   Node *node = getNode(graph, index);
   if(node->root) removeRootNode(graph, node->index);
   else addRootNode(graph, node->index);
   node->root = !node->root;
}

void relabelEdge(Graph *graph, int index, Label new_label, bool free_label)
{	
   Edge *edge = getEdge(graph, index);
   if(graph->classes)
   {
      LabelClass old_label_class = getLabelClass(edge->label);
      LabelClass new_label_class = getLabelClass(new_label);
      /* If the label classes or the marks differ, update the label class tables. */
      if(edge->label.mark != new_label.mark || old_label_class != new_label_class)
      {
         removeLabelClassIndex(graph, false, edge->label, edge->label_table_index);
         addLabelClassIndex(graph, false, new_label, index);
      }
   }
   if(free_label) freeLabel(edge->label); 
   edge->label = new_label;
}

/* ===========================
 * Label Class Table Functions 
 * =========================== */
void addLabelClassIndex(Graph *graph, bool node, Label label, int index)
{
   LabelClass label_class = getLabelClass(label);
   LabelClassTable *table = node ? getNodeLabelTable(graph, label.mark, label_class): 
                                   getEdgeLabelTable(graph, label.mark, label_class);
   /* If the table for this label class and mark does not exist, allocate 
    * memory for a new LabelClassTable and an initial items array of four
    * items. */
   if(table == NULL)
   {
      LabelClassTable *new_table = malloc(sizeof(LabelClassTable));
      if(new_table == NULL)
      {
         print_to_log("Error (addLabelClassIndex): malloc failure.\n");
         exit(1);
      }
      new_table->mark = label.mark;
      new_table->label_class = label_class;
      new_table->pool_size = 4;
      new_table->index = 1;
      new_table->items = calloc(new_table->pool_size, sizeof(int));
      if(new_table->items == NULL)
      {
         print_to_log("Error (addLabelClassIndex): malloc failure.\n");
         exit(1);
      }
      new_table->items[0] = index;
      /* Initialise the rest of items. */
      new_table->items[1] = -1;
      new_table->items[2] = -1;
      new_table->items[3] = -1;

      /* Prepend the new table to the appropriate list. */
      if(node) 
      {
         new_table->next = graph->node_classes;
         graph->node_classes = new_table;
         graph->nodes[index].label_table_index = 0;
      }
      else 
      {
         new_table->next = graph->edge_classes;
         graph->edge_classes = new_table;
         graph->edges[index].label_table_index = 0;
      }
   }
   /* The table for this label class and mark exists. Update its array with 
    * the new index. */
   else
   {
      /* If the items array is full, double the allocated memory. */
      if(table->index == table->pool_size)
      {
         table->pool_size *= 2;
         int *new_items = realloc(table->items, table->pool_size * sizeof(int));
         if(new_items == NULL)
         {
            print_to_log("Error (addNodeLabelClassIndex): malloc failure.\n");
            exit(1);
         }
         table->items = new_items;
         int count;
         /* Initialise the newly allocated array elements. */
         for(count = table->index; count < table->pool_size; count++) 
            table->items[count] = -1;
      }
      table->items[table->index] = index;
      if(node) graph->nodes[index].label_table_index = table->index;
      else graph->edges[index].label_table_index = table->index;
      table->index++;
   }
}

void removeLabelClassIndex(Graph *graph, bool node, Label label, int item_index)
{
   LabelClass label_class = getLabelClass(label);
   LabelClassTable *table = node ? getNodeLabelTable(graph, label.mark, label_class): 
                                   getEdgeLabelTable(graph, label.mark, label_class);
   assert(table != NULL);
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

LabelClassTable *copyLabelClassTable(LabelClassTable *table)
{
   if(table == NULL) return NULL;
   LabelClassTable *copy = malloc(sizeof(LabelClassTable));
   if(copy == NULL)
   {
      print_to_log("Error (addLabelClassIndex): malloc failure.\n");
      exit(1);
   }
   copy->mark = table->mark;
   copy->label_class = table->label_class;
   copy->pool_size = table->pool_size;
   copy->index = table->index;
   copy->items = calloc(copy->pool_size, sizeof(int));
   if(copy->items == NULL)
   {
      print_to_log("Error: (copyLabelClassTable): malloc failure.\n");
      exit(1);
   }
   memcpy(copy->items, table->items, copy->pool_size * sizeof(int));      
   copy->next = copyLabelClassTable(table->next);
   return copy;
}

void freeLabelClassTable(LabelClassTable *table)
{
   if(table == NULL) return;
   if(table->items != NULL) free(table->items);
   freeLabelClassTable(table->next);
   free(table);
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

LabelClassTable *getNodeLabelTable(Graph *graph, MarkType mark, LabelClass label_class)
{
   LabelClassTable *table = graph->node_classes;
   while(table != NULL)
   {
      if(table->mark == mark && table->label_class == label_class) return table;
      table = table->next;
   }
   return NULL;
}
   
LabelClassTable *getEdgeLabelTable(Graph *graph, MarkType mark, LabelClass label_class) 
{
   LabelClassTable *table = graph->edge_classes;
   while(table != NULL)
   {
      if(table->mark == mark && table->label_class == label_class) return table;
      table = table->next;
   }
   return NULL;
}


void printGraph(Graph *graph, FILE *file) 
{
   /* The node and edge counts are used in the IDs of the printed graph. The item's 
    * index in the graph is not suitable for this purpose because there may be holes
    * in the graph's node array. The counts are also used to control the number of
    * nodes and edges printed per line. */
   int index, node_count = 0, edge_count = 0;
   if(graph == NULL || graph->number_of_nodes == 0) 
   {
      PTF("[ | ]\n");
      return;
   }
   PTF("[ ");
   /* Maps a node's graph-index to the ID it is printed with (node_count). */
   int output_indices[graph->node_index];
   for(index = 0; index < graph->node_index; index++)
   {
      Node *node = getNode(graph, index);
      if(node->index == -1) 
      {
         output_indices[index] = -1;
         continue; 
      }
      /* Five nodes per line */
      if(node_count != 0 && node_count % 5 == 0) PTF("\n  ");
      output_indices[index] = node_count;
      if(node->root) PTF("(n%d(R), ", node_count++);
      else PTF("(n%d, ", node_count++);
      printLabel(node->label, file);
      PTF(") ");
   }
   if(graph->number_of_edges == 0)
   {
      PTF("| ]\n\n");
      return;
   }
   PTF("|\n  ");
   for(index = 0; index < graph->edge_index; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge->index == -1) continue; 

      /* Three edges per line */
      if(edge_count != 0 && edge_count % 3 == 0) PTF("\n  ");
      PTF("(e%d, ", edge_count++);
      PTF("n%d, n%d, ", output_indices[edge->source], output_indices[edge->target]);
      printLabel(edge->label, file);
      PTF(") ");
   }
   PTF("]\n\n");
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
   if(graph->nodes != NULL) free(graph->nodes);

   for(index = 0; index < graph->edge_index; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge->index >= 0) freeLabel(edge->label);
   }
   if(graph->edges != NULL) free(graph->edges);

   if(graph->free_node_slots != NULL) free(graph->free_node_slots);
   if(graph->free_edge_slots != NULL) free(graph->free_edge_slots);

  freeLabelClassTable(graph->node_classes);
  freeLabelClassTable(graph->edge_classes);

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

