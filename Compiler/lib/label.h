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
  Label Module
  ============

  Defines data types and operations host labels. Host lists are implemented 
  as doubly-linked lists, and are stored in a hash table to avoid duplication
  of lists that occur multiple times in a graph over the course of a program
  execution.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_LABEL_H
#define INC_LABEL_H

#define LIST_TABLE_SIZE 100003

#include "common.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

typedef enum {NONE = 0, RED, GREEN, BLUE, GREY, DASHED, ANY} MarkType; 

typedef struct HostLabel {
   MarkType mark;
   int length;
   struct HostList *list;
} HostLabel;

extern struct HostLabel blank_label;

typedef struct HostList {
   int hash;
   struct HostListItem *first;
   struct HostListItem *last;
} HostList;

typedef struct HostAtom {
   char type; /* (i)nteger or (s)tring */
   union {
      int num;
      string str;
   };
} HostAtom;

typedef struct HostListItem {
   struct HostAtom atom;
   struct HostListItem *next;
   struct HostListItem *prev;
} HostListItem;

typedef struct Bucket {
   HostList *list;
   int reference_count;
   struct Bucket *next;
   struct Bucket *prev;
} Bucket;

/* Hash table to store lists at runtime. Collisions are handled by separate chaining
 * implemented by singly-linked lists ("buckets" as defined above). Lists are added
 * to the host table by making an array of HostAtoms representing the list and 
 * passing it to makeHostList. In this way, each specific list is allocated to heap
 * exactly once and has a single point of reference. */
extern Bucket **list_store;

/* If list hashing is enabled, makeHostList returns a pointer to the HostList represented 
 * by the passed array from the hash table (list_store). If not, the function returns a
 * pointer to a newly-allocated HostList. */
HostList *makeHostList(HostAtom *array, int length, bool free_strings);
/* Expects the passed pointer to exist in the list hash table. Increments the reference
 * count of the list's bucket. */
void addHostList(HostList *list);
/* Expects the passed pointer to exist in the list hash table. Decrements the reference
 * count of the list's bucket. Deletes/frees the list and its containing bucket if
 * the new reference count is 0. */
void removeHostList(HostList *list);

/* Called at runtime to build labels. */
HostLabel makeEmptyLabel(MarkType mark);
HostLabel makeHostLabel(MarkType mark, int length, HostList *list);

/* Used to determine whether a node or edge needs relabelling, and to evaluate
 * the edge predicate if a label argument is provided. */
bool equalHostLabels(HostLabel label1, HostLabel label2);
/* Used to evaluate list comparison predicates. */
bool equalHostLists(HostAtom *left_list, HostAtom *right_list,
                    int left_length, int right_length);
/* Used when adding list assignments to the morphism and when copying the host graph. */
HostList *copyHostList(HostList *list);

void printHostLabel(HostLabel label, FILE *file);
void printHostList(HostListItem *item, FILE *file);

void freeHostList(HostList *list);
void freeHostListStore(void);

#endif /* INC_LABEL_H */
