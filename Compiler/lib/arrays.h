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

  ============
  Arrays Module
  ============
                             
  An API for GP2 graphs. Defines structures for graphs, nodes, edges, label
  class tables and functions that operate on these structures.

/////////////////////////////////////////////////////////////////////////// */

#include <assert.h>
#include <stdlib.h>
#include "common.h"
 
 // 8 bytes
typedef struct BigArrayElem {
  void *items;
} BigArrayElem;

// A hole in a BigArray (below) is filled with the following structure.
// The holes make up an internal linked list of holes, so the next one
// can be retrieved quickly.
// Stores addresses so indices are available at no traversal cost

 // 24 bytes
typedef struct BigArrayHole {
  struct BigArrayHole *prev;
  struct BigArrayHole *next;
  int index; // TODO: UNSIGNED
} BigArrayHole;

// Dynamic data struct of arbitrary size, which never moves elements.
// Hence, pointers to its elements are never invalidated.
// This structure is a linked list of arrays repeatedly doubling in size
// and an IntArray of available holes in said array.
// Useful for minimizing the number of malloc's while keeping pointers valid.

// 24/32 bytes + BIGAR_INIT_SZ
// currently, 216/224 bytes
typedef struct BigArray {
  int capacity; // TODO: UNSIGNED
  int size; // TODO: UNSIGNED
  int elem_sz; // TODO: UNSIGNED
  unsigned short max_array;
  unsigned short num_arrays;
#define BIGAR_INIT_SZ 192
  char firstelems[BIGAR_INIT_SZ]; // use this before malloc'ing space
  BigArrayElem *elems;
  #ifndef MINIMAL_GC
  BigArrayHole *first_hole;
  #endif
} BigArray;

BigArray makeBigArray(size_t elem_sz);
int genFreeBigArrayPos(BigArray *array);
void *getBigArrayValue(BigArray *array, int index);

#ifndef MINIMAL_GC
void removeFromBigArray(BigArray *array, int index);
void emptyBigArray(BigArray *array);
#endif
