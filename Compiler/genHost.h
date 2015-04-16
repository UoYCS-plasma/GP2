/* ///////////////////////////////////////////////////////////////////////////

  =====================
  Generate Host Module
  =====================    

  Generates the runtime module init_runtime which is responsible for building
  the host graph and performing runtime analysis. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_HOST_H
#define INC_GEN_HOST_H

/* source is the file handle for init_source.c. */
#define printToInitSource(code, ...)	        \
  do { fprintf(source, code, ##__VA_ARGS__); }  \
  while(0) 

#define PTIS printToInitSource

/* If the host graph contains fewer than MIN_HOST_NODE_SIZE nodes, the host
 * graph is allocated memory for that number of nodes. Similarly for edges. */
#define MIN_HOST_NODE_SIZE 256
#define MIN_HOST_EDGE_SIZE 256

/* The size of the five runtime arrays that support the building of the host
 * graph. */
#define BUFFER_SIZE 4096

#include "ast.h"
#include "error.h"
#include "globals.h"
#include "transform.h"

/* generateHostGraphCode creates the module init_runtime. It uses the AST of
 * the host graph to emit code to create nodes and edges and add them to
 * the host graph. This code is written to a function called makeHostGraph
 * that is called at runtime. */ 
void generateHostGraphCode(GPGraph *ast_host_graph);

#endif /* INC_GEN_HOST_H */
