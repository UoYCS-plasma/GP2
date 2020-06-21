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
#include "arrays.h"

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
  #ifndef MINIMAL_GC
  array.first_hole = NULL;
  #endif

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
    array->elems = reallocSafe(array->elems, sizeof(BigArrayElem) * (array->num_arrays << 1), "doubleBigArray");
    array->num_arrays <<= 1;
    array->max_array++;
  }
  else array->max_array++;

  long array_size = (long) 1 << (array->max_array+1);
  array->elems[array->max_array].items = mallocSafe((long) array->elem_sz * array_size, "doubleBigArray");
  array->capacity += array_size;
}

int genFreeBigArrayPos(BigArray *array)
{
  assert(array->size <= array->capacity);
  #ifndef MINIMAL_GC
  if(array->first_hole == NULL)
  {
    if(array->size == array->capacity)
      doubleBigArray(array);
    return array->size++;
  }
  else
  {
    BigArrayHole *hole = array->first_hole;
    array->first_hole = array->first_hole->next;
    if(array->first_hole != NULL)
      array->first_hole->prev = NULL;
    assert(hole->index >= 0);
    assert(hole->index < array->size);
    return hole->index;
  }
  #else
  if(array->size == array->capacity)
    doubleBigArray(array);
  return array->size++;
  #endif
}

void *getBigArrayValue(BigArray *array, int index)
{
  assert(index >= 0);
  assert(index < array->size);

  if(index < BIGAR_INIT_SZ / array->elem_sz)
    return (void *) &(array->firstelems[index * array->elem_sz]);

  index -= BIGAR_INIT_SZ / array->elem_sz;
  ptrdiff_t inarray_index = (ptrdiff_t) index - (1 << fls(index+2)) + 2;

  return (void *) array->elems[fls(index+2)-1].items + inarray_index*array->elem_sz;
}

#ifndef MINIMAL_GC
void removeFromBigArray(BigArray *array, int index)
{
  assert(index >= 0);
  assert(index < array->size);

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
    for(int i = 0; i <= array->max_array; i++)
      free(array->elems[i].items);
    free(array->elems);
  }
}
#endif
