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
   bool is_rooted;
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
   struct RuleEdges *outedges, *inedges, *biedges;
   Label label;
   struct Predicate **predicates;
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
   char type; /* (e)xpression, (n)egated expression, (o)r, (a)nd */
   union {
      struct Predicate *predicate;
      struct Condition *neg_predicate;
      struct {
         struct Condition *left_predicate;
         struct Condition *right_predicate;
      };
   };
} Condition;

Condition *makeCondition(void);

/* Representation of all the predicate expressions used in GP 2 conditions. 
 * These are the leaves of the condition tree. Each predicate has a unique
 * integer identifier, used to generate unique boolean variables to store
 * the results of each predicate at runtime. Nodes and variables contain
 * pointers to any predicates taking that node or variable asn argument. */
typedef struct Predicate {
   int bool_id;
   ConditionType type;
   union {
      string variable; /* Subtype predicates. */
      struct {
         int source;
         int target;
         Label *label;
      } edge_pred; /* Edge predicate. */
      struct {
         Atom *left_list;
         int left_length;
         Atom *right_list;
         int right_length;
      } comparison; /* Relational operators over lists/atoms. */
   };
} Predicate;

/* Rule Building Functions *
 * ======================= */

/* Allocates memory for a rule structure and its graphs.
 * The arguments are the sizes of the appropriate arrays. */
Rule *makeRule(int variables, int left_nodes, int left_edges, int right_nodes, 
               int right_edges);

/* Allocates memory for a RuleGraph, its node array, and its edge array. 
 * Returns a pointer to the RuleGraph. */
RuleGraph *makeRuleGraph(int nodes, int edges);

/* Populates the <rule->variable_index>th element of the rule's variable array
 * and increments the variable index. */
void addVariable(Rule *rule, string name, GPType type);

/* Populates the array entry of the appropriate graph array and returns the
 * index of the added item. */
int addRuleNode(RuleGraph *graph, bool root, Label label);
int addRuleEdge(RuleGraph *graph, bool bidirectional, RuleNode *source,
                RuleNode *target, Label label);

/* Prepends a RuleEdge pointer to the passed RuleEdges list.
 * Called by addRuleEdge on the incident edge array of the edge's source and target. */           
RuleEdges *addIncidentEdge(RuleEdges *edges, RuleEdge *edge);

/* Allocates memory for a Predicate and populates its fields according to the
 * type of the predicate and the arguments passed to the function. */
Predicate *makeTypeCheck(int bool_id, ConditionType type, string variable);
Predicate *makeEdgePred(int bool_id, int source, int target, Label *label);
Predicate *makeRelationalCheck(int bool_id, ConditionType type, Atom *left_list,
                               int left_length, Atom *right_list, int right_length);

/* Searches for the variable <name> in the rule's variable array and calls 
 * addPredicate on its predicate pointer array. */
void addVariablePredicate(Rule *rule, string name, Predicate *predicate);

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

/* Returns the type of a variable in the rule's variable list. */
GPType lookupType(Rule *rule, string name);

RuleNode *getRuleNode(RuleGraph *graph, int index);
RuleEdge *getRuleEdge(RuleGraph *graph, int index);

void printRule(Rule *rule, FILE *file);
void printRuleGraph(RuleGraph *graph, FILE *file);
void printCondition(Condition *condition, FILE *file);

void freeRule(Rule *rule);
void freeRuleGraph(RuleGraph *graph);
void freeRuleEdges(RuleEdges *edges);
void freeCondition(Condition *condition);
void freePredicate(Predicate *predicate);

/* When processing a rule's AST, two lists of index maps (one for nodes and one 
 * for edges) are maintained. They store the ID of the item, its indices in the 
 * LHS and RHS graphs, and the source and target IDs of edges. 
 *
 * These are used to obtain the correct source and targets when creating edges
 * and to obtain information about edges created by the rule. */
typedef struct IndexMap {
   string id;
   bool root;
   int left_index;
   int right_index;
   string source_id;
   string target_id;
   struct IndexMap *next;
} IndexMap;

/* Prepends a new map with the passed information to the given list and returns
 * a pointer to the new first map in the list. */
IndexMap *addIndexMap(IndexMap *map, string id, bool root, int left_index, 
                      int right_index, string source_id, string target_id);
int findLeftIndexFromId(IndexMap *map, string id);                      
IndexMap *findMapFromId(IndexMap *map, string id);
/* Used to find a map for an edge with the passed source and target IDs. */
IndexMap *findMapFromSrcTgt(IndexMap *map, string source, string target);
IndexMap *removeMap(IndexMap *map, IndexMap *map_to_remove);
void freeIndexMap(IndexMap *map);

#endif /* INC_RULE_H */
