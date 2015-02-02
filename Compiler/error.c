#include "error.h"

FILE *log_file = NULL;

void openLogFileC(string file_name)
{
   int length = strlen(file_name) + 4; 
   char log_file_name[length];
   strcpy(log_file_name, file_name);
   strncat(log_file_name, ".log", 4);

   log_file = fopen(log_file_name, "w");
   if(log_file == NULL)
   { 
      perror(log_file_name);
      exit(1);
   }
}

void openLogFileR()
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

