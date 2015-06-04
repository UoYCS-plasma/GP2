#include "error.h"

FILE *log_file = NULL;

void openLogFile(string log_file_name)
{
   log_file = fopen(log_file_name, "w");
   if(log_file == NULL)
   { 
      perror("gp2.log");
      exit(1);
   }
}

void closeLogFile(void)
{
   fclose(log_file);
}

FILE *trace_file = NULL;

void openTraceFile(string trace_file_name)
{
   trace_file = fopen(trace_file_name, "w");
   if(trace_file == NULL)
   { 
      perror("gp2.trace");
      exit(1);
   }
}

void closeTraceFile(void)
{
   fclose(trace_file);
}

