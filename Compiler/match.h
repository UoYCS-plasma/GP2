/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.h - Chris Bak (14/08/2014)
  ================================
                             
  Header file for the rule matching module. Defines the data structures for
  variables, conditions, rules and graph morphisms.
  
/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_MATCH_H
#define INC_MATCH_H

#include "globals.h"
#include "graph.h"
#include "rule.h"

/* Association list to represent variable-value mappings. */

typedef struct Assignment {
  string variable;
  GList *value;
  struct Assignment *next;
} Assignment;


/* Create a new assignment specified by the last two arguments and prepends it
 * to the assignment given by the first argument. Returns a pointer to the 
 * start of the new assignment.
 */
Assignment *addAssignment(Assignment *assignment, string name, GList *value);

/* Deletes and frees the first element of the assignment. */        
                
Assignment *removeAssignment(Assignment *assignment);

/* Given an assignment and the name of a variable, lookupValue returns the value
 * of the variable if it exists in the assignment. Otherwise it returns NULL.
 */
GList *lookupValue(Assignment *assignment, string name);

void freeAssignment(Assignment *assignment);


/* A graph morphism is a set of node-to-node mappings, a set of edge-to-edge
 * mappings and a variable-value assignment. */

typedef struct Morphism {
   Stack *node_images;
   Stack *edge_images;
   Assignment *assignment;
} Morphism;

Morphism *makeMorphism(void);
void printMorphism(Morphism *morphism);
void freeMorphism(Morphism *morphism);
 
#endif /* INC_MATCH_H */
