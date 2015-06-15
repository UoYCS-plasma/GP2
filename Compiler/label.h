/* ///////////////////////////////////////////////////////////////////////////

  ============
  Label Module
  ============

  Data structures and functions for GP2's labels.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_LABEL_H
#define INC_LABEL_H

#include "error.h"
#include "globals.h"

/* AtomType defined in globals.h. I place the enumerated type here for reference.
 * {INTEGER_CONSTANT = 0, STRING_CONSTANT, VARIABLE, LENGTH, INDEGREE,
 *  OUTDEGREE, NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomType; */
typedef struct Atom { 
   AtomType type;
   union {
      int number;
      string string;
      struct {
         string name;
         GPType type;
      } variable;
      /* The index of the node in the RHS of the rule. */
      int node_id;   
      struct Atom *neg_exp;
      struct {
         struct Atom *left_exp;
         struct Atom *right_exp;
      } bin_op;
   };
} Atom;

/* The length of the list in a label is fixed at compile time in the
 * transformation phase. If length > 0, then an array of length atoms is 
 * allocated to heap. The array is populated with the appropriate atoms. */
typedef struct Label {
   MarkType mark;
   int length;
   struct GPList *first;
   struct GPList *last;
} Label;

extern struct Label blank_label;

typedef struct GPList {
   Atom atom;
   struct GPList *next;
   struct GPList *prev;
} GPList;
   
/* Called at runtime to build labels. */
Label makeEmptyLabel(MarkType mark);
Label makeHostLabel(MarkType mark, int length, GPList *list);
GPList *appendList(GPList *list, GPList *list_to_append);
GPList *appendAtom(GPList *list, Atom atom);
GPList *appendIntegerAtom(GPList *list, int value);
/* Duplicates value. */
GPList *appendStringAtom(GPList *list, string value);
GPList *getLastElement(GPList *list);
int getListLength(GPList *list);

/* Used to compare LHS labels with RHS labels to check if a node or edge is
 * relabelled by the rule. Also used at runtime to evaluate conditions. */
bool equalLabels(Label left_label, Label right_label);
bool equalAtoms(Atom *left_atom, Atom *right_atom);
bool hasListVariable(Label label);

/* Creates a copy of source and assigns it to target. The assumption is that 
 * target points to a Label-sized portion of heap. Heap memory is allocated
 * in this function only if the source label contains pointers to heap, namely 
 * strings and nested Atoms. */
void copyLabel(Label *source, Label *target);
/* Pass tail == NULL to copy the whole list from head. */
GPList *copyList(GPList *head, GPList *tail);
//Atom *copyList(Atom *list, int length);
Atom *copyAtom(Atom *atom);

void printLabel(Label label, FILE *file);
void printList(GPList *list, FILE *file);
void printAtom(Atom *atom, bool nested, FILE *file);
void printOperation(Atom *left_exp, Atom *right_exp, string const operation,
                    bool nested, FILE *file);
void printMark(MarkType mark, FILE *file);

void freeLabel(Label label);
void freeList(GPList *list);
/* freeAtom is called on both the atoms in the label's list (array) and on any
 * nested atoms. The array is a contiguous block of memory containing some
 * number of atoms: atoms in this array shoud not be freed individually. Nested
 * atoms are allocated on demand, so it is safe to free them individually. This
 * is the reason for the free_atom boolean argument. */
void freeAtom(Atom *atom, bool free_atom);

#endif /* INC_LABEL_H */
