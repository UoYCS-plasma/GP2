#include "graph.h"
#include "ast.h"

int main() {
   
   Node *n1 = malloc(sizeof(Node));

   if(n1 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   n1->index = 0;
   n1->name = strdup("n1");
   n1->root = true;
   n1->label_class = EMPTY;
   n1->mark = NONE;
   n1->list = NULL;
   n1->indegree = 0;
   n1->outdegree = 0;
   n1->in_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 
   n1->out_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 

   Node *n2 = malloc(sizeof(Node));

   if(n2 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   n2->index = 0;
   n2->name = strdup("n2");
   n2->root = false;
   n2->label_class = EMPTY;
   n2->mark = NONE;
   n2->list = NULL;
   n2->indegree = 0;
   n2->outdegree = 0;
   n2->in_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 
   n2->out_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 

   Node *n3 = malloc(sizeof(Node));

   if(n3 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   n3->index = 0;
   n3->name = strdup("n3");
   n3->root = true;
   n3->label_class = EMPTY;
   n3->mark = NONE;
   n3->list = NULL;
   n3->indegree = 0;
   n3->outdegree = 0;
   n3->in_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 
   n3->out_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 

   Node *n4 = malloc(sizeof(Node));

   if(n4 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   n4->index = 0;
   n4->name = strdup("n4");
   n4->root = true;
   n4->label_class = EMPTY;
   n4->mark = NONE;
   n4->list = NULL;
   n4->indegree = 0;
   n4->outdegree = 0;
   n4->in_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 
   n4->out_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 

   Edge *e1 = malloc(sizeof(Edge));

   if(e1 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   e1->index = 0;
   e1->name = strdup("e1");
   e1->bidirectional = false;
   e1->label_class = EMPTY;
   e1->mark = NONE;
   e1->list = NULL;
   e1->source = n2;
   e1->target = n1;

   Edge *e2 = malloc(sizeof(Edge));

   if(e2 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   e2->index = 0;
   e2->name = strdup("e2");
   e2->bidirectional = false;
   e2->label_class = EMPTY;
   e2->mark = NONE;
   e2->list = NULL;
   e2->source = n3;
   e2->target = n1;
   
   Graph *graph = newGraph();
   addNode(graph, n1, graph->next_node_index);
   addNode(graph, n2, graph->next_node_index);
   addNode(graph, n3, graph->next_node_index);
   addNode(graph, n4, graph->next_node_index);
   addEdge(graph, e1, graph->next_edge_index);
   addEdge(graph, e2, graph->next_edge_index);	
   removeEdge(graph,1);
   removeNode(graph,2);
  
   if(graph) printGraph(graph);
   if(graph) freeGraph(graph);

   return 0;
}

   
