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

/* Association list to represent variable-value mappings. */
typedef struct Assignment {
  string variable;
  int length;
  Atom *value;
} Assignment;

typedef struct Map {
   int left_index;
   int host_index;
   /* The number of variable-value assignments added by this node map.
    * Needed when matching backtracks in order to remove the appropriate
    * number of assignments from the morphism. */
   int added_variables;
} Map;

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
/* addAssignment calls copyList on value. */
void addAssignment(Morphism *morphism, string variable, int length, Atom *value);
void removeNodeMap(Morphism *morphism);
void removeEdgeMap(Morphism *morphism);
void removeAssignments(Morphism *morphism, int number);
int lookupVariable(Morphism *morphism, string variable);
int findHostIndex(Morphism *morphism, int left_index);
void printMorphism(Morphism *morphism);
void freeMorphism(Morphism *morphism);
 
#endif /* INC_MATCH_H */
