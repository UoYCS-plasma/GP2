#include "searchplan.h"

Searchplan *initialiseSearchplan(void)
{
   Searchplan *new_plan = malloc(sizeof(Searchplan));

   if(new_plan == NULL)
   {
     print_to_log("Error: Memory exhausted during searchplan "
                  "construction.\n");
     exit(1);
   }   

   new_plan->first = NULL;
   new_plan->last = NULL;

   return new_plan;
}

void addSearchOp(Searchplan *plan, char type, int index)
{
   SearchOp *new_op = malloc(sizeof(SearchOp));

   if(new_op == NULL)
   {
     print_to_log("Error: Memory exhausted during search operation "
                  "construction.\n");
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


Searchplan *generateSearchplan(Graph *lhs)
{
   int lhs_nodes = lhs->number_of_nodes;
   int lhs_size = lhs_nodes + lhs->number_of_edges;
   int index;

   Searchplan *searchplan = initialiseSearchplan();

   /* Indices 0 to |V_L|-1 are the nodes. Indices |V_L| to |V_L|+|E_L|-1 are the
    * edges. Index |V_L| refers to the LHS edge with index 0. */
   bool *tagged_items = calloc(lhs_size, sizeof(bool));
   if(tagged_items == NULL)
   {
      print_to_log("Error: Memory exhausted during tagged items "
                   "array construction.\n");
      exit(1);
   }
   GSList *left_roots = getRootNodes(lhs); 
   GSList *iterator = left_roots;
   
   /* Add the root nodes to the search plan before traversing the graph.
    * Optimisation: if the rule has no root nodes, this and the next 
    * while loop are unnecessary. */ 
   while(iterator != NULL)
   {
      Node *root = (Node *)iterator->data;
      addSearchOp(searchplan, 'r', root->index);      
      iterator = iterator->next;
   }  
 
   /* Perform a dePTRH-first traversal of the graph from its root nodes. */
   while(left_roots != NULL)
   {
      Node *root = (Node *)left_roots->data;
      if(!tagged_items[root->index]) 
         traverseNode(searchplan, root, 'r', tagged_items, lhs_nodes);
      left_roots = left_roots->next;
   }

   /* Search for undiscovered nodes. These are nodes not reachable from a root
    * node.
    * Optimisation: check rules beforehand to see if they are left-root-connected.
    * If so, this code fragment is unnecessary. */
   for(index = 0; index < lhs->next_node_index; index++)
   {
      if(!tagged_items[index]) 
      {
         Node *node = getNode(lhs, index);
         traverseNode(searchplan, node, 'n', tagged_items, lhs_nodes);
      }
   }
   free(tagged_items);

   return searchplan;
}

void traverseNode(Searchplan *searchplan, Node *node, char type, 
                  bool *tagged_items, int offset)
{
   tagged_items[node->index] = true;

   /* Root nodes are already in the searchplan. */
   if(type != 'r') 
   {      
      addSearchOp(searchplan, type, node->index);
   }
   
   int index;
   /* Search the node's incident edges for an untagged edge. Outedges
    * are arbitrarily examined first. If no such edges exist, the function
    * exits and control passes to the caller. */
   for(index = 0; index < node->next_out_edge_index; index++)
   {
      Edge *edge = getOutEdge(node, index);
      if(!tagged_items[offset + edge->index])
      {
         if(edge->source == edge->target)
            traverseEdge(searchplan, edge, 'l', tagged_items, offset);
         else traverseEdge(searchplan, edge, 's', tagged_items, offset);
      }
   }

   for(index = 0; index < node->next_in_edge_index; index++)
   {
      Edge *edge = getInEdge(node, index);
      if(!tagged_items[offset + edge->index]) 
      {
         if(edge->source == edge->target)
            traverseEdge(searchplan, edge, 'l', tagged_items, offset);
         else traverseEdge(searchplan, edge, 't', tagged_items, offset);
      }
   }
}


void traverseEdge(Searchplan *searchplan, Edge *edge, char type, 
                  bool *tagged_items, int offset)
{
   tagged_items[offset + edge->index] = true;

   addSearchOp(searchplan, type, edge->index);

   /* If the edge is a loop, nothing is gained from examining the edge's
    * incident node as it was examined by the caller. */
   if(type == 'l') return;

   /* Check if the edge's source and target are untagged. The target is
    * arbitrarily examined first. If both nodes are tagged, the function exits
    * and control passes to the caller. */
   if(type == 's')
   {
      Node *target = edge->target;
      if(!tagged_items[target->index])
      {
         if(edge->bidirectional) 
              traverseNode(searchplan, target, 'b', tagged_items, offset);
         else traverseNode(searchplan, target, 'i', tagged_items, offset);
      }
   }      
   else 
   {
      Node *source = edge->source;
      if(!tagged_items[source->index])
      {
         if(edge->bidirectional) 
              traverseNode(searchplan, source, 'b', tagged_items, offset);
         else traverseNode(searchplan, source, 'o', tagged_items, offset);  
      }
   }
}

