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
   /* Array of Atoms with length elements. */
   Atom *list;
} Label;

extern struct Label blank_label;

/* A partition of the set of all GP 2 host labels for guided querying of host 
 * items during label matching. 
 *
 * The label classes are as follows: 
 * - The empty list (EMPTY_L)
 * - Integer constant (INT_L)
 * - String constant (STRING_L)
 * - List of length 2, 3, and 4 (LIST2_L, LIST3_L, LIST4_L).
 * - List of length > 4 (LONG_LIST_L). */

typedef enum {EMPTY_L = 0, INT_L, STRING_L, LIST2_L, LIST3_L, LIST4_L, 
              LONG_LIST_L} LabelClass;

/* Called at runtime to build labels. makeHostLabel expects an atom array in
 * heap as its third argument. */
Label makeEmptyLabel(MarkType mark);
Label makeHostLabel(MarkType mark, int length, Atom *list);

/* Compares a LHS label with a RHS label of the same rule for syntactic equality. 
 * Used in rule generation to determine if an item is relabelled. */
bool equalRuleLabels(Label left_label, Label right_label);
/* Called by equalRuleLabels. Since left_atom is an atom in a LHS label, it must 
 * be a constant, a variable, a negated variable or a concatenated string. */
bool equalRuleAtoms(Atom *left_atom, Atom *right_atom);

/* Returns the label class of a host graph label. */
LabelClass getLabelClass(Label label);
bool hasListVariable(Label label);

/* Allocates memory for an array with <length> number of atoms. */
Atom *makeList(int length);
void addAtom(Atom atom, Label label, int position);

/* Creates a copy of source and assigns it to target. The assumption is that 
 * target points to a Label-sized portion of heap. Heap memory is allocated
 * in this function only if the source label contains pointers to heap, namely 
 * strings and nested Atoms. */
void copyLabel(Label *source, Label *target);
Atom *copyList(Atom *list, int length);
Atom *copyAtom(Atom *atom);

void printLabel(Label label, FILE *file);
void printList(Atom *atom, int length, FILE *file);
void printAtom(Atom *atom, bool nested, FILE *file);
void printOperation(Atom *left_exp, Atom *right_exp, string const operation,
                    bool nested, FILE *file);
void printMark(MarkType mark, FILE *file);

void freeLabel(Label label);
void freeList(Atom *atom, int length);
/* freeAtom is called on both the atoms in the label's list (array) and on any
 * nested atoms. The array is a contiguous block of memory containing some
 * number of atoms: atoms in this array shoud not be freed individually. Nested
 * atoms are allocated on demand, so it is safe to free them individually. This
 * is the reason for the free_atom boolean argument. */
void freeAtom(Atom *atom, bool free_atom);

#endif /* INC_LABEL_H */
