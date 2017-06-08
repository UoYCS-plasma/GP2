/* ///////////////////////////////////////////////////////////////////////////

  Copyright 2015-2016 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>.

  ============
  Error Module 
  ============

  Module for error reporting macros and functions. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_ERROR_H
#define INC_ERROR_H

#include <inc/common.h>

#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 

/* Wrappers for frequently occurring calls to fprintf. */
#define print_error(error_message, ...)                   \
  do {                                                    \
     fprintf(stderr, error_message, ##__VA_ARGS__);       \
     fprintf(log_file, error_message, ##__VA_ARGS__);     \
  } while(0) 

#define print_to_console(error_message, ...)                \
  do { fprintf(stderr, error_message, ##__VA_ARGS__); }     \
  while(0) 

void openLogFile(string log_file_name);
void closeLogFile(void);

#endif /* INC_ERROR_H */
