#include "error.h"

void openLogFile(string file_name)
{
   int length = strlen(file_name) + 4; 
   char log_file_name[length];
   strcpy(log_file_name, file_name);
   strncat(log_file_name, ".log", 4);

   FILE *log_file = fopen(log_file_name, "w");
   if(log_file == NULL)
   { 
      perror(log_file_name);
      exit(1);
   }
}

void closeLogFile(void)
{
   fclose(log_file);
}

