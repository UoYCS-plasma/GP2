#include "graph.h"

Node dummy_node = {-1, false, {NONE, 0, NULL}, 0, 0, -1, -1, -1, -1, 
                   {0, 0, NULL}, {0, 0, NULL}, false};
Edge dummy_edge = {-1, {NONE, 0, NULL}, -1, -1, false};

IntArray makeIntArray(int initial_capacity)
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

void addToIntArray(IntArray *array, int item)
{
   if(array->size >= array->capacity) growIntArray(array);
   array->items[array->size++] = item;
}

void removeFromIntArray(IntArray *array, int index)
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
   graph->root_nodes = NULL;
   return graph;
}

int addNode(Graph *graph, bool root, HostLabel label) 
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
   node.matched = false;

   int index = addToNodeArray(&(graph->nodes), node);
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

int addEdge(Graph *graph, HostLabel label, int source_index, int target_index) 
{
   Edge edge;
   edge.label = label;
   edge.source = source_index;
   edge.target = target_index;
   edge.matched = false;

   int index = addToEdgeArray(&(graph->edges), edge);

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
   if(node->out_edges.items != NULL) free(node->out_edges.items);
   if(node->in_edges.items != NULL) free(node->in_edges.items); 

   #ifndef LIST_HASHING
      freeHostList(node->label.list);
   #endif

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

void removeEdge(Graph *graph, int index) 
{
   Edge *edge = getEdge(graph, index);

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

   #ifndef LIST_HASHING
     freeHostList(edge->label.list);
   #endif

   removeFromEdgeArray(&(graph->edges), index);
   graph->number_of_edges--;
}

void relabelNode(Graph *graph, int index, HostLabel new_label) 
{
   Node *node = getNode(graph, index);
   #ifndef LIST_HASHING
     freeHostList(node->label.list);
   #endif
   node->label = new_label;
}

void changeRoot(Graph *graph, int index)
{
   Node *node = getNode(graph, index);
   if(node->root) removeRootNode(graph, node->index);
   else addRootNode(graph, node->index);
   node->root = !node->root;
}

void resetMatchedNodeFlag(Graph *graph, int index)
{
   Node *node = getNode(graph, index);
   assert(node->matched);
   node->matched = false;
}

void relabelEdge(Graph *graph, int index, HostLabel new_label)
{	
   Edge *edge = getEdge(graph, index);
   #ifndef LIST_HASHING
     freeHostList(edge->label.list);
   #endif
   edge->label = new_label;
}

void resetMatchedEdgeFlag(Graph *graph, int index)
{
   Edge *edge = getEdge(graph, index);
   assert(edge->matched);
   edge->matched = false;
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

HostLabel getNodeLabel(Graph *graph, int index) 
{
   Node *node = getNode(graph, index);
   return node->label;
}

HostLabel getEdgeLabel(Graph *graph, int index) 
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
      printHostLabel(node->label, file);
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
      printHostLabel(edge->label, file);
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
      if(node->out_edges.items != NULL) free(node->out_edges.items);
      if(node->in_edges.items != NULL) free(node->in_edges.items);
      #ifndef LIST_HASHING
         freeHostList(node->label.list);
      #endif
   }
   if(graph->nodes.holes.items) free(graph->nodes.holes.items);
   if(graph->nodes.items) free(graph->nodes.items);

   for(index = 0; index < graph->edges.size; index++)
   {
      Edge *edge = getEdge(graph, index);
      if(edge == NULL) continue;
      #ifndef LIST_HASHING
         freeHostList(edge->label.list);
      #endif
   }
   if(graph->edges.holes.items) free(graph->edges.holes.items);
   if(graph->edges.items) free(graph->edges.items);
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

