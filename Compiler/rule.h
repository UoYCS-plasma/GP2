/* ///////////////////////////////////////////////////////////////////////////

  ===========
  Rule Module 
  ===========

  Defines an intermediate structure for rules along with data structures
  used during the transformation of rules and their graphs from the AST
  to the intermediate form.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_STRUCTURES_H
#define INC_STRUCTURES_H

#include "error.h"
#include "globals.h"
#include "graph.h"

/* The parameter list of a rule. Each variable has one of the five GP 2 types 
 * according to the rule declaration. Used in the matching algorithm to check
 * the type of a variable for label matching.
 */
typedef enum {INTEGER_VAR = 0, CHARACTER_VAR, STRING_VAR, ATOM_VAR, LIST_VAR} 
  GPType;

typedef struct VariableList {
  string variable;
  GPType type; 
  struct VariableList *next;
} VariableList;

VariableList *addVariable(VariableList *variable_list, string name, GPType type);
GPType lookupType(VariableList *variable_list, string name);
void freeVariableList(VariableList *variable_list);


/* When processing a rule's AST, two lists of index maps (one for nodes and one 
 * for edges) are maintained. They store the ID of the item, its indices in the 
 * LHS and RHS graphs, and the source and target IDs of edges. 
 *
 * The lists of index maps are used to obtain the correct source and targets 
 * when creating edges and to obtain information about edges created by the 
 * rule. */
typedef struct IndexMap {
   string id;
   bool root;
   int left_index;
   int right_index;
   string source_id;
   string target_id;
   Label *label;
   struct IndexMap *next;
} IndexMap;

/* Prepends a new map with the passed information to the given list and returns
 * a pointer to the new first map in the list. */
IndexMap *addIndexMap(IndexMap *map, string id, bool root, int left_index, 
                      int right_index, string source_id, string target_id,
                      Label *label);
int findLeftIndexFromId(IndexMap *map, string id);                      
IndexMap *findMapFromId(IndexMap *map, string id);
/* Used to find a map for an edge with the passed source and target IDs. */
IndexMap *findMapFromSrcTgt(IndexMap *map, string source, string target);
IndexMap *removeMap(IndexMap *map, IndexMap *map_to_remove);
void freeIndexMap(IndexMap *map);

/* A simple linked list to store node indices. */
typedef struct ItemList {
   int index;
   struct ItemList *next;
} ItemList;

ItemList *addItem(ItemList *item_list, int index);
bool queryItemList(ItemList *item_list, int index);
void freeItemList(ItemList *item_list);

/* A linked list of items that are preserved by the rule. If the preserved
 * item's label does not change on rule application, new_label is NULL,
 * otherwise it is the label of the corresponding RHS item. */
typedef struct PreservedItemList {
   int left_index;
   bool rhs_root;
   Label *new_label;
   struct PreservedItemList *next;
} PreservedItemList;

PreservedItemList *addPreservedItem(PreservedItemList *list, int left_index, 
                                    bool change_root, Label *new_label);
PreservedItemList *queryPItemList(PreservedItemList *list, int left_index);                                
void freePItemList(PreservedItemList *list);


/* A linked list of structures describing edges created by the rule. The 
 * edge's incident nodes may be preserved by the rule, in which case the
 * LHS index of the node is stored. Alternatively, the nodes could be created
 * by the rule, in which case the RHS index of the node is stored. This is
 * specified by the characters source_location and target_location. */
typedef struct NewEdgeList {
   int edge_index;
   char source_location; /* 'l' or 'r' */
   int source_index;
   char target_location; /* 'l' or 'r' */
   int target_index; 
   struct NewEdgeList *next;
} NewEdgeList;

NewEdgeList *addNewEdge(NewEdgeList *edge, int index, char source_loc, 
                        int source_index, char target_loc, char target_index);
void freeNewEdgeList(NewEdgeList *new_edge);


typedef struct Condition {
  CondExpType exp_type;		/* globals.h */
  union {
    string var; 		/* INT_CHECK, CHAR_CHECK, STRING_CHECK, 
				 * ATOM_CHECK */
    struct {
      string source; 
      string target; 
      Label *label;
    } edge_pred; 		/* EDGE_PRED */

    struct { 
      GP2List *left_list;
      GP2List *right_list; 
    } list_cmp; 		/* EQUAL, NOT_EQUAL */

    struct { 
      GP2List *left_exp; 
      GP2List *right_exp; 
    } atom_cmp; 		/* GREATER, GREATER_EQUAL, LESS, LESS_EQUAL */

    struct Condition *not_exp;  /* BOOL_NOT */

    struct { 
      struct Condition *left_exp; 
      struct Condition *right_exp; 
    } bin_exp; 			/* BOOL_OR, BOOL_AND */
  } value;
} Condition;

typedef struct Rule {
   string name; 
   VariableList *variables;
   int number_of_variables;
   Graph *lhs;
   Graph *rhs; 
   PreservedItemList *preserved_nodes;
   PreservedItemList *preserved_edges;
   /* Deleted LHS items are precisely those that do not occur in the 
    * PreservedItems list. I explicitly store the deleted nodes because
    * the dangling condition places a stronger requirement on the degrees
    * of candidate host nodes which is exploited in the rule matching code. */
   ItemList *deleted_nodes;
   ItemList *added_nodes;
   NewEdgeList *added_edges;
   Condition *condition;
   bool is_rooted;
} Rule;

/* Checks if a rule does not modify the host graph: the rule neither adds nor
 * deletes nor relabels any items. */
bool isPredicate(Rule *rule);
void printRule(Rule *rule);
void freeRule(Rule *rule);

#endif /* INC_RULE_H */
