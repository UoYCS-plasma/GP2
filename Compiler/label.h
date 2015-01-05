/* ///////////////////////////////////////////////////////////////////////////

  ============
  Label Module
  ============

  Module for GP2's labels and lists. Defines a doubly-linked list structure
  to support GP2 lists.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_LABEL_H
#define INC_LABEL_H

#include "error.h"
#include "globals.h"

/* Classes of GP 2 labels for querying by label. This is a partition of the
 * set of all GP 2 labels. 
 * The label classes are the empty list, an integer constant, a string constant,
 * constant, a variable of atomic type (int, string, char or atom), 
 * fixed-length lists of length 2 up to a fixed bound, and any list containing 
 * a list variable. */

typedef enum {EMPTY_L = 0, INT_L, STRING_L, ATOMIC_VAR_L, LIST2_L, LIST3_L,
              LIST4_L, LIST5_L, LISTVAR_L} LabelClass;

              
/* Abstract data type for GP2's marks defined in globals.h.
 * typedef enum {NONE = 0, RED, GREEN, BLUE, GREY, DASHED, CYAN} MarkType;
 * list is the NULL pointer for the empty list. Otherwise, the list contains
 * at least one element. Hence list != NULL implies list->first != NULL and 
 * list->last != NULL. */

typedef struct Label {
   MarkType mark;
   struct GP2List *list;
   /* Metadata set while the label is being constructed from the AST. */
   int list_length; 
   bool has_list_variable;
} Label;

/* Global structure for blank labels. */
extern struct Label blank_label;
void freeLabel(Label *label);

typedef struct GP2List {
   struct GP2ListElement *first;
   struct GP2ListElement *last;
} GP2List;

GP2List *newGP2List(void);
void freeGP2List(GP2List *list);

typedef struct GP2ListElement {
   struct GP2Atom *atom;
   struct GP2ListElement *prev;
   struct GP2ListElement *next;
} GP2ListElement;

/* Abstract data type for atomic expressions. From globals.h. 
typedef enum {VARIABLE = 0, INTEGER_CONSTANT, CHARACTER_CONSTANT,
              STRING_CONSTANT, INDEGREE, OUTDEGREE, LIST_LENGTH, STRING_LENGTH,
              NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomExpType; */

typedef struct GP2Atom {
   AtomExpType type;		  
   union {
    string name;		  /* VARIABLE */
    int number; 	 	  /* INTEGER_CONSTANT */
    string string;		  /* CHARACTER_CONSTANT, STRING_CONSTANT */
    string node_id; 		  /* INDEGREE, OUTDEGREE */
    GP2List *list_arg;	 	  /* LIST_LENGTH */
    struct GP2Atom *str_arg;      /* STRING_LENGTH */
    struct GP2Atom *exp; 	  /* NEG */
    struct { 
      struct GP2Atom *left_exp;
      struct GP2Atom *right_exp;
    } bin_op; 		   	  /* ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT */
  } value;
} GP2Atom;

void freeGP2Atom(GP2Atom *atom);

LabelClass getLabelClass(Label *label);
Label *copyLabel(Label *label);

/* Do not pass a NULL pointer! */
void append(GP2List *list, GP2Atom *atom);
GP2List *copyGP2List(GP2List *list);
/* Called when copying the host graph: this function only concerns atoms
 * with constant values, namely VARIABLE, INTEGER_CONSTANT, CHARACTER_CONSTANT,
 * STRING_CONSTANT, NEG and CONCAT. */
GP2Atom *copyGP2Atom(GP2Atom *atom);

void printGP2List(GP2List *list);
void printGP2Atom(GP2Atom *atom);
void printMark(MarkType mark, bool verbose);

#endif /* INC_LABEL_H */
