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

// 1 byte
typedef enum {
  NONE = 0, RED, GREEN, BLUE, GREY, DASHED, ANY
} __attribute__ ((__packed__)) MarkType;

// 11 bytes
typedef struct HostLabel {
   struct HostList *list;
   unsigned short length;
   MarkType mark;
} HostLabel;

extern struct HostLabel blank_label;

// 20 bytes
typedef struct HostList {
   struct HostListItem *first;
   struct HostListItem *last;
   int hash;
} HostList;

// 9 bytes
typedef struct HostAtom {
   union {
      int num; // TODO: LONG
      string str;
   };
   char type; /* (i)nteger or (s)tring */ // TODO: ENUM
} HostAtom;

// 24 bytes
typedef struct HostListItem {
   struct HostAtom atom;
   struct HostListItem *next;
   struct HostListItem *prev;
} HostListItem;

// 28 bytes
typedef struct Bucket {
   HostList *list;
   struct Bucket *next;
   struct Bucket *prev;
   int reference_count; // TODO: UNSIGNED
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
bool equalHostLabelsModMarks(HostLabel label1, HostLabel label2);
/* Used to evaluate list comparison predicates. */
bool equalHostLists(HostAtom *left_list, HostAtom *right_list, int left_length, int right_length);
/* Used when adding list assignments to the morphism and when copying the host graph. */
HostList *copyHostList(HostList *list);

void printHostLabel(HostLabel label, FILE *file);
void printHostList(HostListItem *item, FILE *file);

void freeHostList(HostList *list);
void freeHostListStore(void);

#endif /* INC_LABEL_H */
