/* Copyright 2015-2016 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>. */

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

