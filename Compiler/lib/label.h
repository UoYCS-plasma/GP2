/* ///////////////////////////////////////////////////////////////////////////

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

#define LIST_TABLE_SIZE 400

#include "globals.h"

/* The data structure for GP2 lists allows singleton lists, namely a single
 * integer or a single string, to be stored directly as the C value. The
 * empty list and lists of length greater than 1 are stored in a doubly-linked
 * list structure. */
typedef struct GP2List {
   char type; /* (n)ot assigned, (i)nteger, (s)tring, (l)ist */
   union {
      int num;
      string str;
      struct HostList *list;
   };
} GP2List;

typedef struct HostLabel {
   MarkType mark;
   int length;
   struct GP2List list;
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
 * passing it to addHostList. In this way, each specific list is allocated to heap
 * exactly once and has a single point of reference. */
extern Bucket **list_store;

/* If list hashing is enabled, addHostList returns a pointer to the HostList represented 
 * by the passed array from the hash table (list_store). If not, the function returns a
 * pointer to a newly-allocated HostList. */
HostList *addHostList(HostAtom *array, int length, bool free_strings);
void removeHostList(HostList *list);
void removeHostLabel(HostLabel label);

/* Called at runtime to build labels. */
HostLabel makeEmptyLabel(MarkType mark);
HostLabel makeListLabel(MarkType mark, int length, HostList *list);
HostLabel makeIntegerLabel(MarkType mark, int num);
HostLabel makeStringLabel(MarkType mark, string str);

/* Used to determine whether a node or edge needs relabelling, and to evaluate
 * the edge predicate if a label argument is provided. */
bool equalHostLabels(HostLabel label1, HostLabel label2);
/* Used to evaluate list comparison predicates. */
bool equalHostLists(HostAtom *left_list, HostAtom *right_list,
                    int left_length, int right_length);
/* Used in rule application to get the length of the value matched by a list variable. */
int getListVariableLength(GP2List list);
/* Used when adding list assignments to the morphism and when copying the host graph. */
HostList *copyHostList(HostList *list);

void printHostLabel(HostLabel label, FILE *file);
void printHostList(HostListItem *item, FILE *file);

void freeHostList(HostList *list);
void freeHostListStore(void);

#endif /* INC_LABEL_H */
