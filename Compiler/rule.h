/* ///////////////////////////////////////////////////////////////////////////

  ===========
  Rule Module 
  ===========

  Defines a data structure for rules, storing the information necessary to 
  facilitate the generation of code to match and apply the rule.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_STRUCTURES_H
#define INC_STRUCTURES_H

#include "error.h"
#include "globals.h"
#include "graph.h"

/* The pointer <variables> points to a static array of struct Variable. 
 * variable_index is the first unused array index: once the array is populated, 
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
   struct Variable *variables; 
   int variable_index;
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
   /* Root flag - true if the node is rooted.
    * Relabelled flag - true if the node is relabelled by the rule.
    * Degree flags - true if the node's indegree or outdegree is required
    *                by the rule during rule application. */
   bool root, relabelled, indegree_arg, outdegree_arg;
   /* If the node is in the interface of the rule, this points to the
    * corresponding node in the other rule graph. Otherwise, it is NULL. */
   struct RuleNode *interface; 
   /* Linked lists of edge pointers. */
   struct RuleEdges *outedges, *inedges;
   Label label;
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
   /* Root flag - true if the node is rooted.
    * Relabelled flag - true if the node is relabelled by the rule. */
   bool bidirectional, relabelled;
   /* If the edge is preserved by the rule, this points to the corresponding
    * edge in the other rule graph. Otherwise, it is NULL. */
   struct RuleEdge *interface;
   struct RuleNode *source, *target; 
   Label label;
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
      string variable; /* Subtype predicates. */
      struct {
         int source;
         int target;
         Label *label;
      } edge_pred; /* Edge predicate. */
      struct {
         Label left_label;
         Label right_label;
      } list_comp; /* Relational operators over lists. */
      struct {
         Atom left_atom;
         Atom right_atom;
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
void addVariable(Rule *rule, string name, GPType type);

/* Initialises the array entry of the appropriate graph array and returns the
 * index of the added item. */
int addRuleNode(RuleGraph *graph, bool root, Label label);
int addRuleEdge(RuleGraph *graph, bool bidirectional, RuleNode *source,
                RuleNode *target, Label label);

/* Allocates memory for a Condition. */
Condition *makeCondition(void);

/* Allocates memory for a Predicate and populates its fields according to the
 * type of the predicate and the arguments passed to the function. */
Predicate *makeTypeCheck(int bool_id, bool negated, ConditionType type, string variable);
Predicate *makeEdgePred(int bool_id, bool negated, int source, int target, Label *label);
Predicate *makeListComp(int bool_id, bool negated, ConditionType type,
                        Label left_label, Label right_label);
Predicate *makeAtomComp(int bool_id, bool negated, ConditionType type,
                        Atom left_atom, Atom right_atom);

/* Adds the passed predicate pointer to the passed predicate pointer array.
 * Passing NULL as the argument will create a Predicate pointer array with <size>
 * elements. The <predicate_count> field of the rule should be passed as the
 * third argument. */
Predicate **addPredicate(Predicate **predicates, Predicate *predicate, int size);

/* Rule Operations and Queries *
 * =========================== */

/* Checks if a rule does not modify the host graph: the rule neither adds nor
 * deletes nor relabels any items. */
bool isPredicate(Rule *rule);

Variable *getVariable(Rule *rule, string name);
RuleNode *getRuleNode(RuleGraph *graph, int index);
RuleEdge *getRuleEdge(RuleGraph *graph, int index);

void printRule(Rule *rule, FILE *file);
void freeRule(Rule *rule);

#endif /* INC_RULE_H */
