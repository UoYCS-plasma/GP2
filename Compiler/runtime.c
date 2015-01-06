#include "runtime.h"

int main() {

   openLogFileR();

   Graph *host = makeHostGraph();
   validGraph(host);
   printGraph(host);

   bool result = matchGlobal_rule1(host);
 
   if(result) 
   {
      validGraph(host);
      printGraph(host);
   }
   else printf("Match failed.\n\n");

   freeGraph(host);
   closeLogFile();
   
   return 0;
}
