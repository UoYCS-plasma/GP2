/* ///////////////////////////////////////////////////////////////////////////

  ============
  Label Module
  ============

  Module for GP2's labels and lists. Defines a doubly-linked list structure
  to support GP2 lists.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_LABEL_H
#define INC_LABEL_H

#define LABEL_CLASSES 10

#include "error.h"
#include "globals.h"

/* Classes of GP 2 labels for querying by label. This is a partition of the
 * set of all GP 2 labels. 
 * The label classes are the empty list, an integer constant, a string constant,
 * constant, a variable of atomic type (int, string, char or atom), 
 * fixed-length lists of length 2 up to a fixed bound, and any list containing 
 * a list variable. */

typedef enum {EMPTY_L = 0, INT_L, STRING_L, ATOMIC_VAR_L, LIST2_L, LIST3_L,
              LIST4_L, LIST5_L, LONG_LIST_L, LISTVAR_L} LabelClass;

typedef struct GP2Atom {
   /* Abstract data type for atomic expressions. From globals.h. 
   typedef enum {VARIABLE = 0, INTEGER_CONSTANT, CHARACTER_CONSTANT,
                 STRING_CONSTANT, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH,
                 NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomExpType; */
   AtomExpType type;
   union {
      string name;                /* VARIABLE */
      int number;                 /* INTEGER_CONSTANT */
      string string;              /* STRING_CONSTANT */
      string node_id;             /* INDEGREE, OUTDEGREE */
      struct GP2List *list_arg;   /* LIST_LENGTH */
      struct GP2Atom *str_arg;    /* STRING_LENGTH */
      struct GP2Atom *exp; 	  /* NEG */
      struct { 
         struct GP2Atom *left_exp;
         struct GP2Atom *right_exp;
      } bin_op;           	  /* ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT */
   } value;
   struct GP2Atom *prev;
   struct GP2Atom *next;
} GP2Atom;

typedef struct GP2List {
   struct GP2Atom *first;
   struct GP2Atom *last;
} GP2List;

void append(GP2List *list, GP2Atom *atom);
/* Cleans <destination> by NULLing its first and last pointers, then walks
 * <source>, copying the atoms and appending them to <destination>. */
void copyGP2List(GP2List source, GP2List *destination);
/* Only called when copying the host graph, hence this function only concerns
 * atoms with constant values. */
GP2Atom *copyGP2Atom(GP2Atom *atom);

typedef struct Label {
   /* Abstract data type for GP2's marks defined in globals.h.
    * typedef enum {NONE = 0, RED, GREEN, BLUE, GREY, DASHED, ANY} MarkType; */
   MarkType mark;
   struct GP2List list;
   /* Metadata set while the label is being constructed from the AST. */
   int list_length; 
   bool list_variable;
} Label;

/* Global structure for the labels with empty lists. */
extern struct Label blank_label;
extern struct Label red_label;
extern struct Label green_label;
extern struct Label blue_label;
extern struct Label grey_label;
extern struct Label dashed_label;

LabelClass getLabelClass(Label *label);
Label *copyLabel(Label *label);
void freeLabel(Label *label);

bool isConstantLabel(Label *label);

/* Compares two labels for syntactic equality. Used in rule generation to
 * determine whether an item is relabelled or not. */
bool equalLabels(Label *left_label, Label *right_label);
/* For now, this only tests mark equality. */
bool labelMatch(Label *rule_label, Label *host_label);
bool marksMatch(MarkType rule_mark, MarkType host_mark);
Label *makeEmptyList(MarkType mark);

/* Do not pass a NULL pointer! To either argument! */
void printGP2List(GP2List list, FILE *file);
void printGP2Atom(GP2Atom *atom, FILE *file);
void printMark(MarkType mark, bool verbose, FILE *file);
void freeGP2List(GP2List list);
void freeGP2Atom(GP2Atom *atom);


#endif /* INC_LABEL_H */
