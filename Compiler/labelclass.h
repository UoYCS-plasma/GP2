/* ///////////////////////////////////////////////////////////////////////////

  ==================
  Label Class Module
  ==================

  Defines the label class data structures and operations.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_LABEL_CLASS_H
#define LABEL_CLASS_H

#include "error.h"
#include "globals.h"
#include "label.h"

#define MARKS 7 
#define LABEL_CLASSES 7

/* TODO: Create label class table only for the host graph. No label class
 * functions for rule graphs. Probably need to improve control over this
 * in that the functions should not be called via the graph functions.
 * Separate the calls to addNode and addLabelClassTable via genHost. 
 *
 * Access requirements:
 * 'Any' mark: An entire column (all rows == all marks)
 * List variable: An entire row (all column == all label classes).
 * 'Any' and list variable: Iterate over the node/edge array. Maybe.
 * Other mark/structure combinations map to exactly one label class table element.

/* An array of indices with a certain mark and label class. */
typedef struct LabelClassTable {
   int pool_size;
   int index;
   int *items;
} LabelClassTable;

/* 2D arrays of node and edge indices. Rows are marks. Columns are label classes. */
extern struct LabelClassTable *nodes_by_label;
extern struct LabelClassTable *edges_by_label;

/* Allocates an array of MARKS * LABEL_CLASSES pointers. The intention is for
 * this to act as a 2-dimensional array whose elements can be indexed by 
 * pointer arithmetic. */
LabelClassTable *makeLabelClassTable(void);

/* Adds an index to the label class table at array entry [mark, label_class]. 
 * If the pointer is NULL, a label class table of pool size 4 is created,
 * otherwise the index is added to the existing table, whose items array is
 * grown if necessary. */
int addLabelClassIndex(bool node, int mark, int label_class, int index);

/* Only called by removeNode and removeEdge. */
void removeLabelClassIndex(bool node, int mark, int label_class, int index);

LabelClassTable *getNodesByLabel(Graph *graph, LabelClass label_class);
LabelClassTable *getEdgesByLabel(Graph *graph, LabelClass label_class);

#endif /* LABEL_CLASS_H */
