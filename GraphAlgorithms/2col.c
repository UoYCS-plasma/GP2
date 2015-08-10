#include "graph.h"
#include "hostParser.h"

Graph *host = NULL;
bool directed = false;

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

bool dfsColour(int node, int colour)
{
   Link *link = NULL;
   int new_colour = colour == 1 ? 2 : 1;
   host->label[node] = new_colour;
   for(link = host->adj[node]; link != NULL; link = link->next)
   {
      if(host->label[link->id] == 0)
      {
         if(!dfsColour(link->id, new_colour)) return false;
      }
      else if(host->label[link->id] != colour) return false;
   }
   return true;
}

int main(int argc, char **argv)
{
   if(argc != 2)
   {
      printf("Usage: 2col <host_graph_file>.\n");
      return 0;
   }
   host = buildHostGraph(argv[1]);
   bool colourable = true;
   int v;
   for(v = 0; v < host->nodes; v++)
   {
      if(host->label[v] == 0)
      {
         if(!dfsColour(v, 2)) colourable = false;
      }
   }
   /* If the graph is not 2-colourable, reset the host graph by unmarking
    * all its nodes. */
   if(!colourable)
   {
      for(v = 0; v < host->nodes; v++) host->label[v] = 0;
   }
   
   FILE *output = fopen("output", "w");
   if(output == NULL)
   {
      perror("output");
      exit(1);
   }
   GRAPHprint2col(host, output);
   fclose(output);
   GRAPHdestroy(host);
   return 0;
}
