/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.h - Chris Bak (14/08/2014)
  ================================
                             
  This is a library of structures and functions for the runtime system.
  The modules for each compiled GP2 rule include this header file.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_MATCH_H
#define INC_MATCH_H

#include "../globals.h"
#include "../graph.h"
#include "../label.h"
#include "../rule.h"

typedef struct RewriteData {
   bool remove_item;
   bool rhs_root;
   Label *new_label; /* NULL if the label does not change. */
   int host_index;
} RewriteData;

/* Association list to represent variable-value mappings. */
typedef struct Assignment {
  string variable;
  GP2List value;
} Assignment;

/* Create a new assignment specified by the last two arguments and prepends it
 * to the assignment given by the first argument. Returns a pointer to the 
 * start of the new assignment.
 
Assignment *addAssignment(Assignment *assignment, string name, GP2List *value);

* Deletes and frees the first element of the assignment.       
Assignment *removeAssignment(Assignment *assignment);

* Given an assignment and the name of a variable, lookupValue returns the value
 * of the variable if it exists in the assignment. Otherwise it returns NULL.
 
GP2List *lookupValue(Assignment *assignment, string name);

void freeAssignment(Assignment *assignment); */

typedef struct Map {
   int left_index;
   int host_index;
   /* The number of variable-value assignments added by this node map.
    * Needed when matching backtracks in order to remove the appropriate
    * number of assignments from the morphism. */
   int added_variables;
} Map;

/* typedef struct Map {
   int right_index;
   Node *host_node;
   struct Map *next;
} Map;

Map *addMap(Map *map, int right_index, Node *host_node); 
Node *findHostNode(Map *map, int right_index);
void freeMap(Map *map); */

/* A graph morphism is a set of node-to-node mappings, a set of edge-to-edge
 * mappings and a variable-value assignment. */

typedef struct Morphism {
   int nodes;
   int node_map_index;
   Map *node_map;

   int edges;
   int edge_map_index;
   Map *edge_map;

   int variables;
   int assignment_index;
   Assignment *assignment;
} Morphism;

Morphism *makeMorphism(int nodes, int edges, int variables);
void clearMorphism(Morphism *morphism);
void addNodeMap(Morphism *morphism, int left_index, int host_index);
void addEdgeMap(Morphism *morphism, int left_index, int host_index);
void addAssignment(Morphism *morphism, string variable, GP2List value);
void removeNodeMap(Morphism *morphism);
void removeEdgeMap(Morphism *morphism);
void removeAssignment(Morphism *morphism);
int findHostIndex(Morphism *morphism, int left_index);
void printMorphism(Morphism *morphism);
void freeMorphism(Morphism *morphism);
 
#endif /* INC_MATCH_H */
