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
#include <stddef.h>
#include "graph.h"

// Find leading bit in an int.
#define fls(x) (int) ((sizeof(int)<<3) - __builtin_clz(x) - 1)

BigArray makeBigArray(size_t elem_sz)
{
  BigArray array;
  array.size = 0;

  // THIS IS ONLY SAFE TO NOT EXECUTE BECAUSE sizeof(BigArrayHole) > elem_sz
  // IS ALWAYS FALSE IN OUR USAGE. CALLERS BEWARE!

  // if(sizeof(BigArrayHole) > elem_sz)
  //   array.elem_sz = (int) sizeof(BigArrayHole);
  // else
  //   array.elem_sz = (int) elem_sz;

  array.elem_sz = (int) elem_sz;
  array.num_arrays = 0;
  array.max_array = 0;
  array.elems = NULL;
  array.capacity = BIGAR_INIT_SZ / array.elem_sz;
  array.first_hole = NULL;

  return array;
}

void doubleBigArray(BigArray *array)
{
  if(array->elems == NULL)
  {
    array->elems = mallocSafe(sizeof(BigArrayElem), "doubleBigArray");
    array->num_arrays = 1;
  }
  else if(array->num_arrays == array->max_array + 1)
  {
    array->elems = reallocSafe(array->elems,
        sizeof(BigArrayElem) * (array->max_array++), "doubleBigArray");
    array->num_arrays <<= 1;
  }
  else
    array->max_array++;

  BigArrayElem *new_elem = &(array->elems[array->max_array]);
  new_elem->items = mallocSafe(array->elem_sz * (1 << array->max_array), "doubleBigArray");
  array->capacity += (1 << array->max_array);
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

  index -= BIGAR_INIT_SZ / array->elem_sz;
  BigArrayElem subarray = array->elems[fls(index)];
  ptrdiff_t inarray_index = (ptrdiff_t) index - (1 << fls(index));

  return (void *) subarray.items + inarray_index;
}

void removeFromBigArray(BigArray *array, int index)
{
  if(index == array->size - 1) array->size--;
  else
  {
    BigArrayHole *hole = (BigArrayHole *) getBigArrayValue(array, index);
    hole->index = index;

    hole->next = array->first_hole;
    hole->prev = NULL;
    if (array->first_hole != NULL)
      array->first_hole->prev = hole;
    array->first_hole = hole;
  }
}

void emptyBigArray(BigArray *array)
{
  if(array->elems != NULL)
  {
    for(int i = 0; i < array->max_array; i++)
      free(array->elems[i].items);
    free(array->elems);
  }
}
