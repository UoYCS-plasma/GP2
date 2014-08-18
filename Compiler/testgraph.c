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

   ListElement *int6 = malloc(sizeof(ListElement));
   if(int6 == NULL) {
      printf("OUT OF MEMORY.\n"); 
      exit(0);
    }
   int6->type = INT_CONSTANT;
   int6->value.number = 6;

   ListElement *charc = malloc(sizeof(ListElement));
   if(charc == NULL) {
      printf("OUT OF MEMORY.\n"); 
      exit(0);
    }
   charc->type = CHARACTER_CONSTANT;
   charc->value.string = strdup("c");

   ListElement *chard = malloc(sizeof(ListElement));
   if(chard == NULL) {
      printf("OUT OF MEMORY.\n"); 
      exit(0);
    }
   chard->type = CHARACTER_CONSTANT;
   chard->value.string = strdup("d");

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
   GList *label4 = NULL;
   label4 = g_list_prepend(label4, int6);
   GList *label5 = NULL;
   label5 = g_list_prepend(label5, chard);

   Node *n1 = malloc(sizeof(Node));

   if(n1 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   n1->index = 0;
   n1->name = strdup("n1");
   n1->root = true;
   n1->label_class = LIST2_L;
   n1->mark = NONE;
   n1->list = label1;
   n1->indegree = 0;
   n1->outdegree = 0;
   n1->in_edges_by_label = g_hash_table_new(g_direct_hash,g_direct_equal); 
   n1->out_edges_by_label = g_hash_table_new(g_direct_hash,g_direct_equal); 

   Node *n2 = malloc(sizeof(Node));

   if(n2 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   n2->index = 0;
   n2->name = strdup("n2");
   n2->root = false;
   n2->label_class = INT_L;
   n2->mark = NONE;
   n2->list = label2;
   n2->indegree = 0;
   n2->outdegree = 0;
   n2->in_edges_by_label = g_hash_table_new(g_direct_hash,g_direct_equal); 
   n2->out_edges_by_label = g_hash_table_new(g_direct_hash,g_direct_equal); 

   Edge *e1 = malloc(sizeof(Edge));

   if(e1 == NULL) {
      printf("OUT OF MEMORY.\n");
      exit(0);
   }

   e1->index = 0;
   e1->name = strdup("e1");
   e1->bidirectional = false;
   e1->label_class = INT_L;
   e1->mark = RED;
   e1->list = label3;
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
   e2->label_class = INT_L;
   e2->mark = NONE;
   e2->list = label4;
   e2->source = n1;
   e2->target = n1;


   Graph *graph = newGraph();

   addNode(graph, n1, graph->next_node_index);
   addNode(graph, n2, graph->next_node_index);
   addEdge(graph, e1, graph->next_edge_index);
   addEdge(graph, e2, graph->next_edge_index);

   if(graph) printGraph(graph);
   printf("====================================\n\n");

   relabelEdge(graph, 1, label5, CHAR_L);
   removeEdge(graph, 0);
 
   if(graph) printGraph(graph);
   if(graph) freeGraph(graph);
   return 0;
}

   
