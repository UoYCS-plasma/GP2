#include "runtime.h"

FILE *log_file;


int main() {

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
   
   return 0;
}
