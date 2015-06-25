#ifndef INC_GEN_MACROS_H
#define INC_GEN_MACROS_H

/* The host node does not match the rule node if:
 * (1) The host node's indegree is strictly less than the rule node's indegree.
 * (2) The host node's outdegree is strictly less than the rule node's outdegree.
 * (3) The number of edges incident to the host node is strictly less than the
 *     number of edges incident to the rule node. */
#define IF_INVALID_NODE(indeg, outdeg, bideg)          \
   if(host_node->indegree < (indeg) ||                 \
      host_node->outdegree < (outdeg) ||               \
      ((host_node->outdegree + host_node->indegree     \
        - (indeg) - (outdeg) - (bideg)) < 0))          \

/* As above, but with a stricter third condition:
 * (3) The number of edges incident to the host node is not equal to the 
 *     number of edges incident to the rule node. Indeed, if it is less,
 *     then standard matching is violated (above). If it is greater,
 *     then the dangling condition is violated. */
#define IF_INVALID_DANGLING_NODE(indeg, outdeg, bideg)  \
   if(host_node->indegree < (indeg) ||                  \
      host_node->outdegree < (outdeg) ||                \
      ((host_node->outdegree + host_node->indegree      \
        - (indeg) - (outdeg) - (bideg)) != 0))          \

#endif /* INC_GEN_MACROS */
