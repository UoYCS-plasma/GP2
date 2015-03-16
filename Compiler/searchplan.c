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
   bool tagged_items[lhs_size];
   for(index = 0; index < lhs_size; index++) tagged_items[index] = false;

   RootNodes *iterator = getRootNodeList(lhs);
   RootNodes *left_roots = iterator; 
   while(iterator != NULL)
   {
      addSearchOp(searchplan, 'r', iterator->index);      
      iterator = iterator->next;
   }  
   /* Perform a depth-first traversal of the graph from its root nodes. */
   while(left_roots != NULL)
   {
      if(!tagged_items[left_roots->index]) 
      {
         Node *root = getNode(lhs, left_roots->index);
         traverseNode(searchplan, lhs, root, 'r', tagged_items, lhs_nodes);
      }
      left_roots = left_roots->next;
   }

   /* Search for undiscovered nodes. These are nodes not reachable from a root
    * node.
    * TODO (optimisation): check rules beforehand to see if they are 
    * left-root-connected. If so, this code fragment is unnecessary. */
   for(index = 0; index < lhs->node_index; index++)
   {
      if(!tagged_items[index]) 
      {
         Node *node = getNode(lhs, index);
         traverseNode(searchplan, lhs, node, 'n', tagged_items, lhs_nodes);
      }
   }
   return searchplan;
}

void traverseNode(Searchplan *searchplan, Graph *lhs, Node *node, char type, 
                  bool *tagged_items, int offset)
{
   tagged_items[node->index] = true;

   /* Root nodes are already in the searchplan. */
   if(type != 'r') addSearchOp(searchplan, type, node->index);
   
   int index;
   /* Search the node's incident edges for an untagged edge. Outedges
    * are arbitrarily examined first. If no such edges exist, the function
    * exits and control passes to the caller. */
   for(index = 0; index < node->out_index; index++)
   {
      Edge *edge = getEdge(lhs, getOutEdge(node, index));
      if(edge == NULL) continue;
      if(!tagged_items[offset + edge->index])
      {
         if(edge->source == edge->target)
            traverseEdge(searchplan, lhs, edge, 'l', tagged_items, offset);
         else traverseEdge(searchplan, lhs, edge, 's', tagged_items, offset);
      }
   }

   for(index = 0; index < node->in_index; index++)
   {
      Edge *edge = getEdge(lhs, getInEdge(node, index));
      if(edge == NULL) continue;
      if(!tagged_items[offset + edge->index]) 
      {
         if(edge->source == edge->target)
            traverseEdge(searchplan, lhs, edge, 'l', tagged_items, offset);
         else traverseEdge(searchplan, lhs, edge, 't', tagged_items, offset);
      }
   }
}


void traverseEdge(Searchplan *searchplan, Graph *lhs, Edge *edge, char type, 
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
      Node *target = getNode(lhs, edge->target);
      if(!tagged_items[target->index])
      {
         if(edge->bidirectional) 
              traverseNode(searchplan, lhs, target, 'b', tagged_items, offset);
         else traverseNode(searchplan, lhs, target, 'i', tagged_items, offset);
      }
   }      
   else 
   {
      Node *source = getNode(lhs, edge->source);
      if(!tagged_items[source->index])
      {
         if(edge->bidirectional) 
              traverseNode(searchplan, lhs, source, 'b', tagged_items, offset);
         else traverseNode(searchplan, lhs, source, 'o', tagged_items, offset);  
      }
   }
}

