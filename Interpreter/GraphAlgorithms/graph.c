#include "graph.h"

Link *NEW(int id, Link *next)
{
   Link *link = malloc(sizeof(*link));
   if(link == NULL)
   {
      printf("Error: malloc failure.\n");
      exit(1);
   }
   link->id = id;
   link->next = next;
   return link;
}

Graph *GRAPHinit(int nodes)
{
   Graph *graph = malloc(sizeof(*graph));
   if(graph == NULL)
   {
      printf("Error: malloc failure.\n");
      exit(1);
   }
   graph->nodes = nodes;
   graph->edges = 0;
   graph->adj = calloc(nodes, sizeof(Link*));
   if(graph->adj == NULL)
   {
      printf("Error: malloc failure.\n");
      exit(1);
   }
   graph->label = calloc(nodes, sizeof(int));
   if(graph->label == NULL)
   {
      printf("Error: malloc failure.\n");
      exit(1);
   }
   return graph;
}

void GRAPHinsertE(Graph *G, int src, int tgt, bool directed)
{
   G->adj[src] = NEW(tgt, G->adj[src]);
   if(!directed) G->adj[tgt] = NEW(src, G->adj[tgt]);
   G->edges++;
}

void GRAPHprint(Graph *G, FILE *file)
{
   int node_count = 0, edge_count = 0;
   fprintf(file, "[ ");
   int v;
   for(v = 0; v < G->nodes; v++)
   {
      if(node_count != 0 && node_count % 5 == 0) fprintf(file, "\n  ");
      fprintf(file, "(n%d, %d) ", node_count++, G->label[v]);
   }
   Link *link;
   fprintf(file, "|\n  ");
   /* This loop prints two edges between the same node to represent undirected edges. */
   for(v = 0; v < G->nodes; v++)
   {
      for(link = G->adj[v]; link != NULL; link = link->next)
      {
         if(edge_count != 0 && edge_count % 3 == 0) fprintf(file, "\n  ");
         fprintf(file, "(e%d, n%d, n%d, empty) ", edge_count++, v, link->id);
      }
   }
   fprintf(file, "]\n\n");
}

void GRAPHprint2col(Graph *G, FILE *file)
{
   int node_count = 0, edge_count = 0;
   fprintf(file, "[ ");
   int v;
   for(v = 0; v < G->nodes; v++)
   {
      if(node_count != 0 && node_count % 5 == 0) fprintf(file, "\n  ");
      fprintf(file, "(n%d, empty", node_count++);
      if(G->label[v] == 0) fprintf(file, ") ");
      else if(G->label[v] == 1) fprintf(file, " # red) ");
      else if(G->label[v] == 2) fprintf(file, " # blue) ");
   }
   Link *link;
   fprintf(file, "|\n  ");
   /* This loop prints two edges between the same node to represent undirected edges. */
   for(v = 0; v < G->nodes; v++)
   {
      for(link = G->adj[v]; link != NULL; link = link->next)
      {
         if(edge_count != 0 && edge_count % 3 == 0) fprintf(file, "\n  ");
         fprintf(file, "(e%d, n%d, n%d, empty) ", edge_count++, v, link->id);
      }
   }
   fprintf(file, "]\n\n");
}

void LINKdestroy(Link *link)
{
   if(link == NULL) return;
   LINKdestroy(link->next);
   free(link);
}

void GRAPHdestroy(Graph *G)
{
   int v;
   for(v = 0; v < G->nodes; v++) LINKdestroy(G->adj[v]);
   free(G->adj);
   free(G->label);
   free(G);
}

