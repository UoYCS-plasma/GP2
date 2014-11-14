#include "runtime.h"

FILE *log_file;

int main() {
  
   log_file = stdout;

   Label *hostlabel = newBlankLabel();  
   Label *hostlabel2 = newBlankLabel();  
   Label *hostlabel3 = newBlankLabel();  

   Node *hn1 = newNode(false, hostlabel);
   Node *hn2 = newNode(false, hostlabel2);
   Edge *he1 = newEdge(false, hostlabel3, hn1, hn2); 

   Graph *host = newGraph();
   addNode(host, hn1);
   addNode(host, hn2);
   addEdge(host, he1);

   Morphism *morphism = match_r1(host);

   if(morphism)
   {
      printMorphism(morphism);
      freeMorphism(morphism);
   }
   else printf("No morphism found.\n");
 
   if(host) freeGraph(host);
   
   return 0;
}
