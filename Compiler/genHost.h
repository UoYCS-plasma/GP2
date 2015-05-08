/* ///////////////////////////////////////////////////////////////////////////

  =====================
  Generate Host Module
  =====================    

  Generates the runtime module init_runtime which is responsible for building
  the host graph and performing runtime analysis. 

/////////////////////////////////////////////////////////////////////////// */

#ifndef INC_GEN_HOST_H
#define INC_GEN_HOST_H

/* If the host graph contains fewer than MIN_HOST_NODE_SIZE nodes, the host
 * graph is allocated memory for that number of nodes. Similarly for edges. */
#define MIN_HOST_NODE_SIZE 256
#define MIN_HOST_EDGE_SIZE 256

/* For large host graphs, compile time is significantly shorter if the host
 * graph build is distributed among many source files. Hence BUFFER_SIZE is
 * the maximum amount of nodes/edges added to the graph in one source file. */
#define BUFFER_SIZE 2000

#include "ast.h"
#include "error.h"
#include "genLabel.h"
#include "globals.h"
#include "transform.h"

/* Emit code to build the host graph at runtime. For sufficiently small graphs,
 * all the code is emitted to the file runtime/buildHost.c. Otherwise,
 * new source files are created, each adding BUFFER_SIZE nodes or edges.
 * For each node (edge) in the graph, a call to addNode (addEdge) is written.
 * The name of the label passed to addNode (addEdge) is returned by calls to
 * generateLabelCode. */
void generateHostGraphCode(GPGraph *ast_host_graph);

/* Generates code to build a host label from the label's AST representation.
 *
 * Argument 1: The AST subtree representing the label.
 * Argument 2: An integer used to uniquely identify list variables created 
 *             during label building at runtime.
 * Argument 3: Handle to the file where the label building code is printed. */
void generateLabelCode(GPLabel *ast_label, int list_count, FILE *file);

#endif /* INC_GEN_HOST_H */
