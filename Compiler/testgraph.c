#include "graph.h"
#include "ast.h"

int main() {
  

   ListElement *int1 = malloc(sizeof(ListElement));
   if(int1 == NULL) {
      printf("OUT OF MEMORY.\n"); 
      exit(0);
    }
   int1->type = INT_CONSTANT;
   int1->value.number = 1;
   
   ListElement *int2 = malloc(sizeof(ListElement));
   if(int2 == NULL) {
      printf("OUT OF MEMORY.\n"); 
      exit(0);
    }
   int2->type = INT_CONSTANT;
   int2->value.number = 2;
   
   ListElement *int3 = malloc(sizeof(ListElement));
   if(int3 == NULL) {
      printf("OUT OF MEMORY.\n"); 
      exit(0);
    }
   int3->type = INT_CONSTANT;
   int3->value.number = 3;

   ListElement *int5 = malloc(sizeof(ListElement));
   if(int5 == NULL) {
      printf("OUT OF MEMORY.\n"); 
      exit(0);
    }
   int5->type = INT_CONSTANT;
   int5->value.number = 5;

   ListElement *charc = malloc(sizeof(ListElement));
   if(charc == NULL) {
      printf("OUT OF MEMORY.\n"); 
      exit(0);
    }
   charc->type = CHARACTER_CONSTANT;
   charc->value.string = strdup("c");

   ListElement *mul23 = malloc(sizeof(ListElement));
   if(mul23 == NULL) {
      printf("OUT OF MEMORY.\n"); 
      exit(0);
    }
   mul23->type = MULTIPLY;
   mul23->value.bin_op.left_exp = int2;
   mul23->value.bin_op.right_exp = int3;
   
   GList *label1 = NULL;
   label1 = g_list_prepend(label1, int1);
   label1 = g_list_prepend(label1, charc);
   GList *label2 = NULL;
   label2 = g_list_prepend(label2, int5);
   GList *label3 = NULL;
   label3 = g_list_prepend(label3, mul23);
 
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
   n1->list = label1;
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
   n2->list = label2;
   n2->indegree = 0;
   n2->outdegree = 0;
   n2->in_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 
   n2->out_edges_by_label = g_hash_table_new(g_int_hash,g_int_equal); 

   Edge *e1 = malloc(sizeof(Edge));

   if(e1 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   e1->index = 0;
   e1->name = strdup("e1");
   e1->bidirectional = false;
   e1->label_class = EMPTY;
   e1->mark = RED;
   e1->list = label3;
   e1->source = n2;
   e1->target = n1;

   
   Graph *graph = newGraph();
   addNode(graph, n1, graph->next_node_index);
   addNode(graph, n2, graph->next_node_index);
   addEdge(graph, e1, graph->next_edge_index);
   relabelNode(graph, 0, label3); 
   relabelEdge(graph, 0, label1);
   removeEdge(graph,0);
 
   if(graph) printGraph(graph);
   if(graph) freeGraph(graph);
   return 0;
}

   
