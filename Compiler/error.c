#include "error.h"

FILE *log_file = NULL;

void openLogFile(string log_file_name)
{
   log_file = fopen(log_file_name, "a");
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

