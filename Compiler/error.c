#include "error.h"

FILE *log_file = NULL;

void openLogFile()
{
   log_file = fopen("gp2.log", "w");
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

