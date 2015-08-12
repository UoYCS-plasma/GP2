#include "graph.h"
#include "hostParser.h"

Graph *host = NULL;
bool directed = true;

static Graph *buildHostGraph(string file_name)
{
   yyin = fopen(file_name, "r");
   if(yyin == NULL)
   {
      perror(file_name);
      return NULL;
   }
   int result = yyparse();
   fclose(yyin);
   if(result == 0) return host;
   else
   {
      GRAPHdestroy(host);
      return NULL;
   }
}

static int *visited = NULL, postorder = 0;

void dfsSort(int node)
{
   Link *link = NULL;
   visited[node] = 1;
   for(link = host->adj[node]; link != NULL; link = link->next)
   {
      if(visited[link->id] == 0) dfsSort(link->id);
   }
   host->label[node] = host->nodes - postorder;
   postorder++;
}

int main(int argc, char **argv)
{
   if(argc != 2)
   {
      printf("Usage: topsort <host_graph_file>.\n");
      return 0;
   }
   host = buildHostGraph(argv[1]);

   visited = calloc(host->nodes, sizeof(int));
   if(visited == NULL)
   {
      printf("Error: malloc failure.\n");
      exit(1);
   }

   int v;
   for(v = 0; v < host->nodes; v++) if(visited[v] == 0) dfsSort(v);

   FILE *output = fopen("output", "w");
   if(output == NULL)
   {
      perror("output");
      exit(1);
   }
   GRAPHprint(host, output);
   fclose(output);
   GRAPHdestroy(host);
   free(visited);
   return 0;
}
