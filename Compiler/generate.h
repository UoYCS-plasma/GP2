/* ///////////////////////////////////////////////////////////////////////////

  ===================================
  generate.h - Chris Bak (27/10/2014)
  ===================================
                             
  Module for generating C code to execute GP2 programs.

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GENERATE_H
#define INC_GENERATE_H

#define printToHeader(code, ...)	              \
  do { fprintf(match_header, code, ##__VA_ARGS__); }  \
  while(0) 

#define printToSource(code, ...)	              \
  do { fprintf(match_source, code, ##__VA_ARGS__); }  \
  while(0) 

#define printToSourceI(code, indent, ...)	              		 \
  do { fprintf(match_source, "%*s" code, indent, " ", ##__VA_ARGS__); }  \
  while(0) 

#include "globals.h"
#include "match.h"

extern FILE *match_header;
extern FILE *match_source;
extern struct Searchplan *searchplan;

/* Search operations are categorised by a character as follows:
 * 'n': Non-root node.
 * 'r': Root node.
 * 'i': Non-root node matched from its incoming edge.
 * 'o': Non-root node matched from its outgoing edge.
 * 'b': Non-root matched from an incident bidirectional edge.
 * 'e': Edge.
 * 's': Edge matched from its source.
 * 't': Edge matched from its target.
 */
typedef struct SearchOp {
   bool is_node;
   char type;
   int index;
   struct SearchOp *next;
} SearchOp;

typedef struct Searchplan {
   SearchOp *first;
   SearchOp *last;
} Searchplan;

/* Appends the search operation (type, index) to plan. */
Searchplan *initialiseSearchplan(void);
void addSearchOp(Searchplan *plan, char type, int index);
void printSearchplan(Searchplan *searchplan);
void freeSearchplan(Searchplan *searchplan);

void generateSearchplan(Graph *lhs);
void traverseNode(Node *node, char match_from, bool *discovered_item, 
	          int offset);
void traverseEdge(Edge *edge, char match_from, bool *discovered_item,
		  int offset);

void generateMatchingCode(Graph *lhs, string rule_name);
void emitMainFunction(string rule_name, SearchOp *first_op);

/* Emits a function that iterates over a list of host nodes and tries to label 
 * match it with left_node's label. The list of host nodes is determined by
 * type and a position. The position is a parameter to the emitted function. 
 * The function takes the address of the position and updates it if a match
 * is found. */
void emitNodeMatcher(Node *left_node, bool is_root, SearchOp *next_op);
void emitNodeFromEdgeMatcher(Node *left_node, char type, SearchOp *next_op);
void emitEdgeMatcher(Edge *left_edge, SearchOp *next_op);
void emitEdgeFromNodeMatcher(Edge *left_edge, char type, SearchOp *next_op);
bool emitNextMatcherCall(SearchOp* next_op);

#endif /* INC_GENERATE_H */

