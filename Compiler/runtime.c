#include "runtime.h"

Graph *host = makeHostGraph();
int fail_mode;
bool success = true;

int main(void)
{
   openLogFileR();

   fail_mode = 1;
   copyGraph(host);

   if(!matchMain_rule1(host)) FAILURE_ACTION
   host = pop(graph_change_stack);
   fail_mode = 0;
   if(success)
   {
   if(!matchMain_rule2(host)) FAILURE_ACTION
   }
   else
   {
   /* skip */
   }

   success = true;

   printGraph(host);
   freeGraph(host);
   closeLogFile();

   return 0;
}

