#include "searchplan.h"

Searchplan *makeSearchplan(void)
{
   Searchplan *plan = malloc(sizeof(Searchplan));
   if(plan == NULL)
   {
     print_to_log("Error (makeSearchplan): malloc failure.\n");
     exit(1);
   }   
   plan->first = NULL;
   plan->last = NULL;
   return plan;
}

void addSearchOp(Searchplan *plan, char type, int index)
{
   SearchOp *new_op = malloc(sizeof(SearchOp));
   if(new_op == NULL)
   {
     print_to_log("Error (makeSearchplan): malloc failure.\n");
     exit(1);
   }   
    if(type == 'e' || type == 's' || type == 't' || type == 'l')
        new_op->is_node = false;
   else new_op->is_node = true;
   new_op->type = type;
   new_op->index = index;

   if(plan->last == NULL)
   {
      new_op->next = NULL;
      plan->first = new_op;
      plan->last = new_op;  
   }
   else
   {
      new_op->next = NULL;
      plan->last->next = new_op;
      plan->last = new_op;
   }
}  

void printSearchplan(Searchplan *plan)
{ 
   if(plan->first == NULL) printf("Empty searchplan.\n");
   else
   {
      SearchOp *iterator = plan->first;
      while(iterator != NULL)
      {
         if(iterator->is_node) printf("Node\n");
         else printf("Edge\n");
         printf("Type: %c\nIndex: %d\n\n", iterator->type, iterator->index);
         iterator = iterator->next;  
      }
   }
}

void freeSearchplan(Searchplan *plan)
{
   if(plan == NULL) return;
   if(plan->first == NULL) free(plan);
   else
   {
      SearchOp *iterator = plan->first;
      while(iterator != NULL)
      {
         SearchOp *temp = iterator;
         iterator = iterator->next;  
         free(temp);
      }
      free(plan);
   }
}

/* Global bool arrays to tag nodes and edges after they have been examined during
 * the graph traversal. */
bool *tagged_nodes = NULL;
bool *tagged_edges = NULL;

Searchplan *generateSearchplan(RuleGraph *lhs)
{
   int lhs_nodes = lhs->node_index;
   int lhs_edges = lhs->edge_index;

   Searchplan *searchplan = makeSearchplan();
   tagged_nodes = calloc(lhs_nodes, sizeof(bool));
   if(tagged_nodes == NULL)
   {
     print_to_log("Error (generateSearchplan): malloc failure.\n");
     exit(1);
   }   
   tagged_edges = calloc(lhs_edges, sizeof(bool));
   if(tagged_edges == NULL)
   {
     print_to_log("Error (generateSearchplan): malloc failure.\n");
     exit(1);
   }   
   int index;
   for(index = 0; index < lhs_nodes; index++) tagged_nodes[index] = false;
   for(index = 0; index < lhs_edges; index++) tagged_edges[index] = false;

   /* Perform a depth-first traversal of the graph from its root nodes. */
   for(index = 0; index < lhs_nodes; index++)
   {
      RuleNode *node = getRuleNode(lhs, index);
      if(node->root && !tagged_nodes[node->index]) 
         traverseNode(searchplan, lhs, node, 'r', lhs_nodes);
   }

   /* Search for undiscovered nodes, namely nodes that are not reachable 
    * from a root node. */
   for(index = 0; index < lhs_nodes; index++)
   {
      if(!tagged_nodes[index]) 
      {
         RuleNode *node = getRuleNode(lhs, index);
         traverseNode(searchplan, lhs, node, 'n', lhs_nodes);
      }
   }
   return searchplan;
}

void traverseNode(Searchplan *searchplan, RuleGraph *lhs, RuleNode *node, char type, int offset)
{
   tagged_nodes[node->index] = true;
   addSearchOp(searchplan, type, node->index);
   int index;
   /* Search the node's incident edges for an untagged edge. Outedges
    * are arbitrarily examined first. If no such edges exist, the function
    * exits and control passes to the caller. */
   RuleEdges *iterator = node->outedges;
   while(iterator != NULL)
   {
      RuleEdge *edge = iterator->edge;
      if(!tagged_edges[edge->index])
      {
         if(edge->source == edge->target) 
              traverseEdge(searchplan, lhs, edge, 'l', offset);
         else traverseEdge(searchplan, lhs, edge, 's', offset);
      }
      iterator = iterator->next;
   }
   iterator = node->inedges;
   for(index = 0; index < node->indegree; index++)
   {
      RuleEdge *edge = iterator->edge;
      if(!tagged_edges[edge->index])
      {
         if(edge->source == edge->target)
              traverseEdge(searchplan, lhs, edge, 'l', offset);
         else traverseEdge(searchplan, lhs, edge, 't', offset);
      }
   }
   iterator = node->biedges;
   for(index = 0; index < node->bidegree; index++)
   {
      RuleEdge *edge = iterator->edge;
      if(!tagged_edges[edge->index])
      {
         if(edge->source == edge->target)
              traverseEdge(searchplan, lhs, edge, 'l', offset);
         else traverseEdge(searchplan, lhs, edge, 't', offset);
      }
   }
}

void traverseEdge(Searchplan *searchplan, RuleGraph *lhs, RuleEdge *edge, char type, int offset)
{
   tagged_edges[edge->index] = true;
   addSearchOp(searchplan, type, edge->index);

   /* If the edge is a loop, its incident node has already been examined. 
    * Backtrack by returning control to the caller. */
   if(type == 'l') return;

   /* Continue the graph traversal of any untagged nodes incident to the edge. */
   if(type == 's')
   {
      RuleNode *target = edge->target;
      if(!tagged_nodes[target->index])
      {
         if(edge->bidirectional) 
              traverseNode(searchplan, lhs, target, 'b', offset);
         else traverseNode(searchplan, lhs, target, 'i', offset);
      }
   }      
   else /* type == 't' */ 
   {
      RuleNode *source = edge->source;
      if(!tagged_nodes[source->index])
      {
         if(edge->bidirectional) 
              traverseNode(searchplan, lhs, source, 'b', offset);
         else traverseNode(searchplan, lhs, source, 'o', offset);  
      }
   }
}

