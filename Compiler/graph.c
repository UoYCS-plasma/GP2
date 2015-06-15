#include "graph.h"

Node dummy_node = {-1, false, {NONE, 0, NULL, NULL}, -1, -1, -1, -1, 
                   {0, 0, NULL}, {0, 0, NULL}, 0, 0, -1};
Edge dummy_edge = {-1, {NONE, 0, NULL, NULL}, -1, -1, -1};

static IntArray makeIntArray(int initial_capacity)
{
   IntArray array;
   array.capacity = initial_capacity;
   array.size = 0;
   if(initial_capacity > 0)
   {
      array.items = calloc(initial_capacity, sizeof(int));
      if(array.items == NULL)
      {
         print_to_log("Error (makeIntArray): malloc failure.\n");
         exit(1);
      }
      int i;
      for(i = 0; i < initial_capacity; i++) array.items[i] = -1;
   }
   else array.items = NULL;
   return array;
}

static void growIntArray(IntArray *array)
{
   int old_capacity = array->capacity;
   /* Node's incident edge arrays have initial capacity of 0. On the first
    * allocation, they are allocated space for 4 integers. In all other cases,
    * the old capacity is doubled. */
   array->capacity = old_capacity == 0 ? 4 : 2*old_capacity;
   array->items = realloc(array->items, array->capacity * sizeof(int));
   if(array->items == NULL)
   {
      print_to_log("Error (doubleCapacity): malloc failure.\n");
      exit(1);
   }
   int i;
   for(i = old_capacity; i < array->capacity; i++) array->items[i] = -1;
}

static void addToIntArray(IntArray *array, int item)
{
   if(array->size >= array->capacity) growIntArray(array);
   array->items[array->size++] = item;
}

static void removeFromIntArray(IntArray *array, int index)
{
   int i;
   for(i = 0; i < array->size; i++)
   {
      if(array->items[i] == index) 
      {
         array->items[i] = -1;
         /* If the index of the removed item directly precedes the size of the
          * array, decrement the size until it refers to the array element
          * one place to the right of the right-most -1. */
         if(i == array->size - 1) 
         {
            array->size--;
            while(array->size > 0)
            {
               if(array->items[array->size - 1] == -1) array->size--;
               else break;
            }
         }
         break;
      }
   }
}
   
static NodeArray makeNodeArray(int initial_capacity)
{
   NodeArray array;
   array.capacity = initial_capacity;
   array.size = 0;
   array.items = calloc(initial_capacity, sizeof(Node));
   if(array.items == NULL)
   {
      print_to_log("Error (makeNodeArray): malloc failure.\n");
      exit(1);
   }
   array.holes = makeIntArray(16);
   return array;
}

static void doubleNodeArray(NodeArray *array)
{
   array->capacity *= 2;
   array->items = realloc(array->items, array->capacity * sizeof(Node));
   if(array->items == NULL)
   {
      print_to_log("Error (doubleCapacity): malloc failure.\n");
      exit(1);
   }
}

static int addToNodeArray(NodeArray *array, Node node)
{
   /* If the holes array is empty, the node's index is the current size
    * of the node array. */
   if(array->holes.size == 0)
   {
      node.index = array->size;
      if(array->size >= array->capacity) doubleNodeArray(array);
      array->items[array->size++] = node;
   }
   /* If the holes array is non-empty, the node is placed in the hole marked by 
    * the rightmost element of the holes array. */
   else 
   {
      array->holes.size--;
      assert(array->holes.items[array->holes.size] >= 0);
      node.index = array->holes.items[array->holes.size];
      array->items[node.index] = node;
      array->holes.items[array->holes.size] = -1;
   }
   return node.index;
}

static void removeFromNodeArray(NodeArray *array, int index)
{
   array->items[index] = dummy_node;
   /* If the index is the last index in the array, no hole is created. */
   if(index == array->size - 1) array->size--;
   else addToIntArray(&(array->holes), index);
}
   
static EdgeArray makeEdgeArray(int initial_capacity)
{
   EdgeArray array;
   array.capacity = initial_capacity;
   array.size = 0;
   array.items = calloc(initial_capacity, sizeof(Edge));
   if(array.items == NULL)
   {
      print_to_log("Error (makeEdgeArray): malloc failure.\n");
      exit(1);
   }
   array.holes = makeIntArray(16);
   return array;
}


static void doubleEdgeArray(EdgeArray *array)
{
   array->capacity *= 2;
   array->items = realloc(array->items, array->capacity * sizeof(Edge));
   if(array->items == NULL)
   {
      print_to_log("Error (doubleCapacity): malloc failure.\n");
      exit(1);
   }
}

static int addToEdgeArray(EdgeArray *array, Edge edge)
{
   /* If the holes array is empty, the edge's index is the current size
    * of the edge array. */
   if(array->holes.size == 0)
   {
      /* There are no holes in the node array, so the node's index is the current
       * size of the node array. */
      edge.index  = array->size;
      if(array->size >= array->capacity) doubleEdgeArray(array);
      array->items[array->size++] = edge;
   }
   /* If the holes array is non-empty, the edge is placed in the hole marked by 
    * the rightmost element of the holes array. */
   else 
   {
      array->holes.size--;
      assert(array->holes.items[array->holes.size] >= 0);
      edge.index = array->holes.items[array->holes.size];
      array->items[edge.index] = edge;
      array->holes.items[array->holes.size] = -1;
   }
   return edge.index;
}

static void removeFromEdgeArray(EdgeArray *array, int index)
{
   array->items[index] = dummy_edge;
   /* If the index is the last index in the array, no hole is created. */
   if(index == array->size - 1) array->size--;
   else addToIntArray(&(array->holes), index);
}


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
   graph->nodes = makeNodeArray(nodes);
   graph->edges = makeEdgeArray(edges);

   graph->number_of_nodes = 0;
   graph->number_of_edges = 0;
   #ifdef LABEL_CLASS_INDEXING 
      graph->classes = true;
   #else
      graph->classes = false;
   #endif
   graph->node_classes = NULL;
   graph->edge_classes = NULL;
   graph->root_nodes = NULL;
   return graph;
}

int addNode(Graph *graph, bool root, Label label) 
{
   Node node;
   node.root = root;
   node.label = label;
   node.first_out_edge = -1;
   node.second_out_edge = -1;
   node.first_in_edge = -1;
   node.second_in_edge = -1;
   node.out_edges = makeIntArray(0);
   node.in_edges = makeIntArray(0);
   node.outdegree = 0;
   node.indegree = 0;
   node.label_table_index = -1;

   int index = addToNodeArray(&(graph->nodes), node);
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

int addEdge(Graph *graph, Label label, int source_index, int target_index) 
{
   Edge edge;
   edge.label = label;
   edge.source = source_index;
   edge.target = target_index;
   edge.label_table_index = -1;

   int index = addToEdgeArray(&(graph->edges), edge);
   if(graph->classes) addLabelClassIndex(graph, true, label, index);

   Node *source = getNode(graph, source_index);
   assert(source != NULL);
   if(source->first_out_edge == -1) source->first_out_edge = index;
   else if(source->second_out_edge == -1) source->second_out_edge = index;
   else addToIntArray(&(source->out_edges), index);
   source->outdegree++;

   Node *target = getNode(graph, target_index);
   assert(target != NULL);
   if(target->first_in_edge == -1) target->first_in_edge = index;
   else if(target->second_in_edge == -1) target->second_in_edge = index;
   else addToIntArray(&(target->in_edges), index);
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
   if(node->out_edges.items != NULL) free(node->out_edges.items);
   if(node->in_edges.items != NULL) free(node->in_edges.items); 
   if(node->root) removeRootNode(graph, index);
   
   removeFromNodeArray(&(graph->nodes), index);
   graph->number_of_nodes--;
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
   if(source->first_out_edge == index) source->first_out_edge = -1;
   else if(source->second_out_edge == index) source->second_out_edge = -1;
   else removeFromIntArray(&(source->out_edges), index);
   source->outdegree--;

   Node *target = getNode(graph, edge->target);
   if(target->first_in_edge == index) target->first_in_edge = -1;
   else if(target->second_in_edge == index) target->second_in_edge = -1;
   else removeFromIntArray(&(target->in_edges), index);
   target->indegree--;

   removeFromEdgeArray(&(graph->edges), index);
   graph->number_of_edges--;
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
         graph->nodes.items[index].label_table_index = 0;
      }
      else 
      {
         new_table->next = graph->edge_classes;
         graph->edge_classes = new_table;
         graph->edges.items[index].label_table_index = 0;
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
      if(node) graph->nodes.items[index].label_table_index = table->index;
      else graph->edges.items[index].label_table_index = table->index;
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
   assert(index < graph->nodes.size);
   if(index == -1) return NULL;
   else return &(graph->nodes.items[index]);
}

Edge *getEdge(Graph *graph, int index)
{
   assert(index < graph->edges.size);
   if(index == -1) return NULL;
   else return &(graph->edges.items[index]);
}

RootNodes *getRootNodeList(Graph *graph)
{
   return graph->root_nodes;
}

Edge *getNthOutEdge(Graph *graph, Node *node, int n)
{
   assert(n >= 0);
   if(n == 0) return getEdge(graph, node->first_out_edge);
   else if(n == 1) return getEdge(graph, node->second_out_edge);
   else
   {
      assert(n - 2 < node->out_edges.size);
      return getEdge(graph, node->out_edges.items[n - 2]);
   }
}

Edge *getNthInEdge(Graph *graph, Node *node, int n)
{
   assert(n >= 0);
   if(n == 0) return getEdge(graph, node->first_in_edge);
   else if(n == 1) return getEdge(graph, node->second_in_edge);
   else
   {
      assert(n - 2 < node->in_edges.size);
      return getEdge(graph, node->in_edges.items[n - 2]);
   }
}

Node *getSource(Graph *graph, Edge *edge) 
{
   return getNode(graph, edge->source);
}

Node *getTarget(Graph *graph, Edge *edge) 
{
   return getNode(graph, edge->target);
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
   int output_indices[graph->nodes.size];
   for(index = 0; index < graph->nodes.size; index++)
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
   for(index = 0; index < graph->edges.size; index++)
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
   for(index = 0; index < graph->nodes.size; index++)
   {
      Node *node = getNode(graph, index);
      if(node == NULL) continue;
      freeLabel(node->label);
      if(node->out_edges.items != NULL) free(node->out_edges.items);
      if(node->in_edges.items != NULL) free(node->in_edges.items);
   }
   if(graph->nodes.holes.items) free(graph->nodes.holes.items);
   if(graph->nodes.items) free(graph->nodes.items);

   for(index = 0; index < graph->edges.size; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge == NULL) continue;
      freeLabel(edge->label);
   }
   if(graph->edges.holes.items) free(graph->edges.holes.items);
   if(graph->edges.items) free(graph->edges.items);

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

