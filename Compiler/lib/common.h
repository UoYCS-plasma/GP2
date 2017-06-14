/* ///////////////////////////////////////////////////////////////////////////

  Copyright 2015-2017 Christopher Bak

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

  ==================
  Common Header File
  ==================

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_COMMON_H
#define INC_COMMON_H

/* If defined, lists are stored in a hash table at runtime as a single point of
 * reference. Otherwise, nodes and edges point to their own copies of their list. */
#define LIST_HASHING

#define print_to_log(error_message, ...)                    \
  do { fprintf(log_file, error_message, ##__VA_ARGS__); }   \
  while(0)

/* Convenience macro for printing. */
#define printToFile(code, ...)	               \
  do { fprintf(file, code, ##__VA_ARGS__); }   \
  while(0) 

#define PTF printToFile

#include <stdio.h>

typedef char* string;

extern FILE *log_file;

#endif /* INC_COMMON_H */
