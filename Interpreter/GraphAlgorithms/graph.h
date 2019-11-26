#ifndef INC_GRAPH_H
#define INC_GRAPH_H

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>

/* Simple adjacency-list graph data structure. Supports undirected and
 * directed graphs, controlled by passing the appropriate value to the
 * GRAPHInsertE function when building graphs. */ 
typedef enum {NONE = 0, RED, GREEN, BLUE, GREY, DASHED, ANY} MarkType; 
typedef char *string;

extern bool directed;

typedef struct Node {
   int id;
   struct Node *next;
} Link;

typedef struct Graph {
  int nodes; /* The number of nodes in the graph. */
  int edges; /* The number of edges in the graph. */
  Link **adj; /* Array of nodes. */
  int *label; /* Node-indexed array of labels. The labels are 0, 1 and 2 for 2-colouring,
               * and arbitrary positive integers for topsort. */
} Graph;

Graph *GRAPHinit(int nodes);
void GRAPHinsertE(Graph *G, int src, int tgt, bool directed);
/* Standard graph printing (GP 2 syntax). */
void GRAPHprint(Graph *G, FILE *file);
/* Graph printing that interprets the labels as colours. */
void GRAPHprint2col(Graph *G, FILE *file);
void GRAPHdestroy(Graph *G);

#endif
