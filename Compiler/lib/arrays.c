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

#include <stdint.h>
#include "graph.h"

BigArray makeBigArray(int initial_capacity, size_t elem_sz)
{
  BigArray array;
  array.size = 0;

  if(sizeof(BigArrayHole) > elem_sz)
    array.elem_sz = sizeof(BigArrayHole);
  else
    array.elem_sz = elem_sz;

  if(initial_capacity < BIGAR_INIT_SZ / array.elem_sz)
  {
    array.elems = NULL;
    array.capacity = BIGAR_INIT_SZ / array.elem_sz;
  }
  else
  {
    array.capacity = initial_capacity;
    array.elems = malloc(sizeof(BigArrayElem));
    if(array.elems == NULL)
    {
      print_to_log("Error (makeArray): malloc failure.\n");
      exit(1);
    }
    array.elems->size = initial_capacity - BIGAR_INIT_SZ / array.elem_sz;
    array.elems->items = malloc((initial_capacity * array.elem_sz) - BIGAR_INIT_SZ);
    if(array.elems->items == NULL)
    {
      print_to_log("Error (makeArray): malloc failure.\n");
      exit(1);
    }
    array.elems->next = NULL;
  }

  array.first_hole = NULL;
  return array;
}

void doubleBigArray(BigArray *array)
{
  BigArrayElem *new_elem = malloc(sizeof(BigArrayElem));
  if(new_elem == NULL)
  {
    print_to_log("Error (doubleArray): malloc failure.\n");
    exit(1);
  }
  new_elem->size = array->capacity;
  new_elem->items = malloc(new_elem->size * array->elem_sz);
  if(new_elem->items == NULL)
  {
    print_to_log("Error (doubleArray): malloc failure.\n");
    exit(1);
  }
  new_elem->next = array->elems;
  array->elems = new_elem;
  array->capacity += new_elem->size;
}

int genFreeBigArrayPos(BigArray *array)
{
  if(array->first_hole == NULL)
  {
    if(array->size == array->capacity) doubleBigArray(array);
    return array->size++;
  }
  else
  {
    BigArrayHole *hole = array->first_hole;
    array->first_hole = array->first_hole->next;
    array->first_hole->prev = NULL;
    assert(hole->index >= 0);
    return hole->index;
  }
}

void *getBigArrayValue(BigArray *array, int index)
{
  if(index < BIGAR_INIT_SZ / array->elem_sz)
    return (void *) &(array->firstelems[index * array->elem_sz]);
  else
    index -= BIGAR_INIT_SZ / array->elem_sz;
  BigArrayElem *curr = array->elems;
  int curr_min_index = array->capacity - (BIGAR_INIT_SZ / array->elem_sz) - curr->size;
  for(; curr != NULL && index < curr_min_index; curr = curr->next)
    if(curr->next != NULL) curr_min_index -= curr->next->size;

  assert(curr != NULL);

  index -= curr_min_index;
  return (void *) (((uintptr_t) curr->items) + index * array->elem_sz);
}

void removeFromBigArray(BigArray *array, int index)
{
  if(index == array->size - 1) array->size--;
  else
  {
    BigArrayHole *hole;
    if(index < BIGAR_INIT_SZ / array->elem_sz)
    {
      hole = (BigArrayHole *) &(array->firstelems[index]);
      hole->index = index;
    }
    else
    {
      index -= BIGAR_INIT_SZ / array->elem_sz;
      // First elem in linked list will represent last one in array;
      // must go backwards in indices.
      BigArrayElem *curr = array->elems;
      int curr_min_index = array->capacity - (BIGAR_INIT_SZ / array->elem_sz) - curr->size;
      for(; curr != NULL && index < curr_min_index; curr = curr->next)
        if(curr->next != NULL) curr_min_index -= curr->next->size;

      assert(curr != NULL);

      hole = (BigArrayHole *) (((uintptr_t) curr->items)
                               + (index-curr_min_index) * array->elem_sz);
      hole->index = index - curr_min_index + (BIGAR_INIT_SZ / array->elem_sz);
    }

    hole->next = array->first_hole;
    if (array->first_hole != NULL) {
      array->first_hole->prev = hole;
    }
    array->first_hole = hole;
  }
}

void emptyBigArray(BigArray *array)
{
  if(array->elems != NULL)
  {
    BigArrayElem *curr = array->elems, *prev = array->elems;
    for(; curr != NULL; curr = curr->next)
    {
      if(curr != prev)
      {
        free(prev->items);
        free(prev);
      }
      prev = curr;
    }
  }
}
