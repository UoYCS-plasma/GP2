/* Copyright 2015-2017 Christopher Bak

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

#include "common.h"

void *callocSafe(size_t ni, size_t sz, char *fn)
{
  void *result = calloc(ni, sz);
  if(result == NULL)
  {
    print_to_log("Error (%s): calloc failure.\n", fn);
    exit(1);
  }
  return result;
}

void *mallocSafe(size_t sz, char *fn)
{
  void *result = malloc(sz);
  if(result == NULL)
  {
    print_to_log("Error (%s): malloc failure.\n", fn);
    exit(1);
  }
  return result;
}

void *reallocSafe(void *ptr, size_t sz, char *fn)
{
  void *result = realloc(ptr, sz);
  if(result == NULL)
  {
    print_to_log("Error (%s): realloc failure.\n", fn);
    exit(1);
  }
  return result;
}
