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

  ===========
  Rule Module 
  ===========

  Defines a data structure for rules, storing the information necessary to 
  facilitate the generation of code to match and apply the rule.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_STRUCTURES_H
#define INC_STRUCTURES_H

#include <inc/common.h>
#include <inc/gp2enums.h>

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

/* The pointer <variables> points to a static array of struct Variable. 
 * variables is the first unused array index: once the array is populated, 
 * it stores the size of the array.
 *
 * The pointers <lhs> and <rhs> point to a single struct RuleGraph, defined below.
 *
 * The pointer <condition> points to a single struct Condition, which is a 
 * binary tree of struct Predicates. See the definition below. <predicate_count>
 * is used to allocate memory for Predicate pointer arrays in nodes and variables. */
typedef struct Rule {
   string name; 
   bool is_rooted, adds_nodes, adds_edges;
   struct Variable *variable_list; 
   int variables;
   struct RuleGraph *lhs; 
   struct RuleGraph *rhs;
   struct Condition *condition;
   int predicate_count;
} Rule;

/* The variable structure stores information about a variable declared by a rule:
 * - The variable's name.
 * - The variable's type.
 * - Pointers to the predicate in which it participates. If the variable does not
 *   occur in any predicates, this pointer is NULL, otherwise it is a pointer array
 *   with <predicate_count> elements.
 * - A flag set to true if the variable's value is needed for rule application. */ 
typedef struct Variable {
   string name;
   GPType type;
   struct Predicate **predicates;
   int predicate_count;
   bool used_by_rule;
} Variable;


typedef struct RuleLabel {
   MarkType mark;
   int length;
   struct RuleList *list;
} RuleLabel;

typedef struct RuleList {
   struct RuleListItem *first;
   struct RuleListItem *last;
} RuleList;

/* AtomType defined in globals.h. I place the enumerated type here for reference.
 * {INTEGER_CONSTANT = 0, STRING_CONSTANT, VARIABLE, LENGTH, INDEGREE,
 *  OUTDEGREE, NEG, ADD, SUBTRACT, MULTIPLY, DIVIDE, CONCAT} AtomType; */
typedef struct RuleAtom { 
   AtomType type;
   union {
      int number;
      string string;
      struct {
         int id;
         GPType type;
      } variable;
      int node_id;  /* The index of the node in the RHS of the rule. */
      struct RuleAtom *neg_exp;
      struct {
         struct RuleAtom *left_exp;
         struct RuleAtom *right_exp;
      } bin_op;
   };
} RuleAtom;

typedef struct RuleListItem {
   struct RuleAtom *atom;
   struct RuleListItem *next;
   struct RuleListItem *prev;
} RuleListItem;

/* A rule graph contains a static array for its nodes and one for its edges. */
typedef struct RuleGraph {
   struct RuleNode *nodes;
   struct RuleEdge *edges;
   int node_index, edge_index;
} RuleGraph;

/* A rule node contains its index in the graph's pointer array, some flags,
 * pointers to related graph components, a label, pointers to the predicates
 * in which it participates (as in struct Variable), and degree information. */
typedef struct RuleNode {
   int index; 
   /* Root - true if the node is rooted.
    * Remarked - true if the node's mark is changed by the rule.
    * Relabelled - true if the node's list is changed by the rule.
    * Root changed - true if the node's root status may be changed by the rule
    *                (in some cases, this can only be determined at runtime).
    * Degree flags - true if the node's indegree or outdegree is required
    *                by the rule during rule application. */
   bool root, remarked, relabelled, root_changed, indegree_arg, outdegree_arg;
   /* If the node is in the interface of the rule, this points to the
    * corresponding node in the other rule graph. Otherwise, it is NULL. */
   struct RuleNode *interface; 
   /* Linked lists of edge pointers. */
   struct RuleEdges *outedges, *inedges;
   struct RuleLabel label;
   struct Predicate **predicates;
   int predicate_count;
   int indegree, outdegree, bidegree;
} RuleNode;

typedef struct RuleEdges {
   struct RuleEdge *edge;
   struct RuleEdges *next;
} RuleEdges;

/* A rule node contains its index in the graph's pointer array, some flags,
 * pointers to related graph components, and a label. */
typedef struct RuleEdge {
   int index;
   /* Root - true if the node is rooted.
    * Remarked - true if the edge's mark is changed by the rule.
    * Relabelled - true if the edge's list is changed by the rule. */
   bool bidirectional, remarked, relabelled;
   /* If the edge is preserved by the rule, this points to the corresponding
    * edge in the other rule graph. Otherwise, it is NULL. */
   struct RuleEdge *interface;
   struct RuleNode *source, *target; 
   struct RuleLabel label;
} RuleEdge;

/* The condition is stored as a binary tree of predicates. */
typedef struct Condition {
   char type; /* (e)xpression, (n)egated expression, (o)r, (a)nd. */
   union {
      struct Predicate *predicate;
      struct Condition *neg_condition;
      struct {
         struct Condition *left_condition;
         struct Condition *right_condition;
      };
   };
} Condition;

/* Representation of all the predicate expressions used in GP 2 conditions. 
 * These are the leaves of the condition tree. Each predicate has a unique
 * integer identifier, used to generate unique boolean variables to store
 * the results of each predicate at runtime. Nodes and variables contain
 * pointers to any predicates taking that node or variable as an argument. */
typedef struct Predicate {
   int bool_id;
   bool negated;
   ConditionType type;
   union {
      int variable_id; /* Subtype predicates. */
      struct {
         int source;
         int target;
         struct RuleLabel label;
      } edge_pred; /* Edge predicate. */
      struct {
         struct RuleLabel left_label;
         struct RuleLabel right_label;
      } list_comp; /* Relational operators over lists. */
      struct {
         struct RuleAtom *left_atom;
         struct RuleAtom *right_atom;
      } atom_comp; /* Relational operators over atoms. */
   };
} Predicate;

/* Rule Building Functions *
 * ======================= */

/* Allocates memory for a rule structure and its graphs.
 * The arguments are the sizes of the appropriate arrays. */
Rule *makeRule(int variables, int left_nodes, int left_edges, int right_nodes, 
               int right_edges);

/* Populates the <rule->variable_index>th element of the rule's variable array
 * and increments the variable index. */
void addVariable(Rule *rule, int index, string name, GPType type);

/* Initialises the array entry of the appropriate graph array and returns the
 * index of the added item. */
int addRuleNode(RuleGraph *graph, bool root, RuleLabel label);
int addRuleEdge(RuleGraph *graph, bool bidirectional, RuleNode *source,
                RuleNode *target, RuleLabel label);

/* Allocates memory for a Condition. */
Condition *makeCondition(void);

/* Allocates memory for a Predicate and populates its fields according to the
 * type of the predicate and the arguments passed to the function. */
Predicate *makeTypeCheck(int bool_id, bool negated, ConditionType type, int variable_id);
Predicate *makeEdgePred(int bool_id, bool negated, int source, int target, RuleLabel label);
Predicate *makeListComp(int bool_id, bool negated, ConditionType type,
                        RuleLabel left_label, RuleLabel right_label);
Predicate *makeAtomComp(int bool_id, bool negated, ConditionType type,
                        RuleAtom *left_atom, RuleAtom *right_atom);

/* Adds the passed predicate pointer to the predicate pointer array of the node
 * or variable. If the pointer array does not exist, an array of size <size> is
 * allocated. These functions also increment the node/variable's predicate count
 * if the passed predicate is not already in the array. The <predicate_count>
 * field of the rule should be passed as the third argument. */
void addNodePredicate(RuleNode *node, Predicate *predicate, int size);
void addVariablePredicate(Variable *variable, Predicate *predicate, int size);

/* Rule Operations and Queries *
 * =========================== */
/* Checks if a rule does not modify the host graph: the rule neither adds nor
 * deletes nor relabels any items. */
bool isPredicate(Rule *rule);

Variable *getVariable(Rule *rule, string name);
int getVariableId(Rule *rule, string name);
RuleNode *getRuleNode(RuleGraph *graph, int index);
RuleEdge *getRuleEdge(RuleGraph *graph, int index);

/* Used to build the rule labels when creating the rule data structure via AST
 * transformation. */
RuleList *appendRuleAtom(RuleList *list, RuleAtom *atom);
/* Used to compare LHS labels with RHS labels to check if a node or edge is
 * relabelled by the rule. */
bool equalRuleLists(RuleLabel left_label, RuleLabel right_label);
/* Used to determine the appropriate function call to generate label matching code. */
bool hasListVariable(RuleLabel label);

void printRule(Rule *rule, FILE *file);
void freeRule(Rule *rule);

#endif /* INC_RULE_H */
