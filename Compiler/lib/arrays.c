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

#include "graph.h"

NodeList dummy_nodelist = {0, NULL, NULL, NULL};
EdgeList dummy_edgelist = {0, NULL, NULL, NULL};
Node dummy_node = {0, NULL, false, {NONE, 0, NULL}, 0, 0,
                   NULL, NULL, false, false, 0, 0};
Edge dummy_edge = {0, NULL, {NONE, 0, NULL}, NULL, NULL, false, false, 0, 0};

IntArray makeIntArray(int initial_capacity)
{
   IntArray array;
   array.capacity = initial_capacity;
   array.size = 0;
   if(initial_capacity > 0)
   {
      array.items = calloc(initial_capacity, sizeof(int));
      if(array.items == NULL)
      {
         print_to_log("Error (makeIntArray): malloc failure.\n");
         exit(1);
      }
      int i;
      for(i = 0; i < initial_capacity; i++) array.items[i] = -1;
   }
   else array.items = NULL;
   return array;
}

void growIntArray(IntArray *array)
{
   int old_capacity = array->capacity;
   /* Node's incident edge arrays have initial capacity of 0. On the first
    * allocation, they are allocated space for 4 holes. In all other cases,
    * the old capacity is doubled. */
   array->capacity = old_capacity == 0 ? 4 : 2*old_capacity;
   array->items = realloc(array->items, array->capacity * sizeof(int));
   if(array->items == NULL)
   {
      print_to_log("Error (doubleCapacity): malloc failure.\n");
      exit(1);
   }
   int i;
   for(i = old_capacity; i < array->capacity; i++) array->items[i] = -1;
}

void addToIntArray(IntArray *array, int item)
{
   if(array->size >= array->capacity) growIntArray(array);
   array->items[array->size++] = item;
}

void removeFromIntArray(IntArray *array, int index)
{
   int i;
   for(i = 0; i < array->size; i++)
   {
      if(array->items[i] == index)
      {
         array->items[i] = -1;
         /* If the index of the removed item directly precedes the size of the
          * array, decrement the size until it refers to the array element
          * one place to the right of the right-most -1. */
         if(i == array->size - 1)
         {
            array->size--;
            while(array->size > 0)
            {
               if(array->items[array->size - 1] == -1) array->size--;
               else break;
            }
         }
         break;
      }
   }
}

BigArray makeBigArray(int initial_capacity, size_t elem_sz)
{
  BigArray array;
  array.capacity = initial_capacity;
  array.size = 0;
  array.elem_sz = elem_sz;
  array.elems = calloc(1, sizeof(BigArrayElem));
  if(array.elems == NULL)
  {
    print_to_log("Error (makeArray): malloc failure.\n");
    exit(1);
  }
  array.elems->size = initial_capacity;
  array.elems->items = calloc(initial_capacity, elem_sz);
  if(array.elems->items == NULL)
  {
    print_to_log("Error (makeArray): malloc failure.\n");
    exit(1);
  }
  array.holes = makeIntArray(16);
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
  new_elem->items = calloc(array->capacity, array->elem_sz);
  if(new_elem->items == NULL)
  {
    print_to_log("Error (doubleArray): malloc failure.\n");
    exit(1);
  }
  new_elem->next = array->elems;
  array->elems = new_elem;
  array->capacity *= 2;
}

int genFreeBigArrayPos(BigArray *array)
{
  if(array->holes.size == 0)
  {
    if(array->size == array->capacity) doubleBigArray(array);
    return array->size++;
  }
  else
  {
    array->holes.size--;
    assert(array->holes.items[array->holes.size] >= 0);
    int val = array->holes.items[array->holes.size];
    array->holes.items[array->holes.size] = -1;
    return val;
  }
}

void *getBigArrayValue(BigArray array, int index)
{
  BigArrayElem *curr = array.elems;
  int curr_min_index = array.capacity - curr->size;
  for(; curr != NULL && index < curr_min_index; curr = curr->next)
    curr_min_index -= curr->size;
  int loc = ((int) curr->items) + index * array.elem_sz;
  return (void *) loc;
}

void removeFromBigArray(BigArray *array, int index)
{
  // First elem in linked list will represent last one in array;
  // must go backwards in indices.
  if(index == array->size - 1) array->size--;
  else
  {
    BigArrayElem *curr = array->elems;
    int curr_min_index = array->capacity - curr->size;
    for(; curr != NULL && index < curr_min_index; curr = curr->next)
      curr_min_index -= curr->size;

    addToIntArray(&(array->holes), index);
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
  free(array->holes.items);
}
