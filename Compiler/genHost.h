/* ///////////////////////////////////////////////////////////////////////////

  =====================
  Generate Host Module
  =====================    

  Generates the runtime module buildHost which is responsible for building
  the host graph and performing runtime analysis. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_HOST_H
#define INC_GEN_HOST_H

#include "ast.h"
#include "error.h"
#include "globals.h"

/* Emit code to build the host graph at runtime. For sufficiently small graphs,
 * all the code is emitted to the file runtime/buildHost.c. Otherwise,
 * new source files are created, each adding BUFFER_SIZE nodes or edges.
 * For each node (edge) in the graph, a call to addNode (addEdge) is written.
 * The name of the label passed to addNode (addEdge) is returned by calls to
 * generateLabelCode. */
void generateHostGraphCode(GPGraph *ast_host_graph);

#endif /* INC_GEN_HOST_H */
