/* ///////////////////////////////////////////////////////////////////////////

  ========================================
  staticsearch.c - Chris Bak (14/08/2014)
  ========================================

/////////////////////////////////////////////////////////////////////////// */

#include "staticsearch.h"

/* There are three searchplan/matching operations in order of highest
 * priority to lowest.
 *
 * - Match a root node.
 * - Match an edge incident to an already matched node and match its other
 *   incident node.
 * - Match a non-root node.
 *
 * The structure of the algorithm is as follows:
 *
 * (1) Match all root nodes one by one by searching the root node list of the
 *     host graph. If the first root node has no match, no total match exists,
 *     return NULL. If any other root node has no match, set the backtracking 
 *     flag. If backtracking is on, get the last root node matched and try to
 *     match it to another root node. Repeat until all root nodes are matched
 *     or the stack is empty.
 * 
 * In case there are no root nodes, the match count will be 0, so a do-while
 * loop is used below to kick off the matching process.
 *
 * Do while (0 < match_count < number_of_nodes + number_of_edges)
 *
 * If backtracking is on, repeat (2) and (3) until a successful match is found,
 * which turns backtracking off, or match_count == 0.
 *
 * (2) If the last matched item is a node and it was not matched as the source
 *     or target of a matched edge, find another match for that node.
 *     A boolean flag in the list of node matches is set if the node should
 *     be matched again.
 *
 * (3) If the last matched item is a edge, find another match for that edge.
 *
 * If backtracking is off:
 *
 * (4) Match an edge incident to an already matched node. This is done by
 *     iterating through the inedge and outedge lists of matched nodes.
 *     If no such edges exist, go to (6). If an unmatched edge does exist, 
 *     search for a candidate edge in the appropriate edge list (inedges or
 *     outedges) of the appropriate host node. 
 * 
 * (5) Match the other node incident to the edge matched in (4). That is,
 *     if the edge was matched from its source (target), find a match for the
 *     edge's target (source). 
 *     If the node in question has already been matched, check if it has
 *     been matched to the source or target of the appropriate host edge. 
 *     If so, then nothing else needs to be done. If not, then the morphism
 *     condition is violated and backtracking is triggered.
 *     If the node in question has not been matched, try to match it against
 *     the source or target of the appropriate host edge.
 *
 * (6) Match a non-root node by searching all nodes of the host graph. 
 *
 * End while
 *
 * If match_count = number_of_nodes + number_of_edges, a complete match has been
 * found. Create a morphism with the generated graph mappings and assignment.
 * Otherwise return NULL.     
 */

/* Third argument determined when traversing the AST. */

Morphism *staticSearchplan(Graph *lhs, Graph *host, VariableList *variables) 
{
   GraphMapping *node_matches = NULL;
   GraphMapping *edge_matches = NULL;
   Assignment *assignment = NULL;
  
   int number_of_nodes = lhs->next_node_index;
   int number_of_edges = lhs->next_edge_index;
   int nodes_matched = 0, edges_matched = 0;

   bool backtracking = false;
   
   char matches[number_of_nodes + number_of_edges]; 
   int match_count = 0;

   /* (1) Match all root nodes. */
   GSList *rule_roots = getRootNodes(lhs);   

   while(rule_roots != NULL)
   {
      if(backtracking)
      {
         /* At this point I know that only node matches have been made. No need
          * to test the stack for its match_count character. */
         Node *node = getNode(lhs, node_matches->rule_item);
         int host_index = matchRootNode(node, host, node_matches->host_item, 
                                        node_matches, variables, &assignment);
         if(host_index >= 0)
         {
            /* Replace the old mapping with the new one. 
             * match_count and nodes_matched remain the same. */
            node_matches = removeMapping(node_matches);
            node_matches = addMapping(node_matches, node->index, host_index, 
				      true);
            backtracking = false;
         }
         else 
         { 
            /* No alternate match is found. Remove the mapping and decrement
             * the match_counts. */
            node_matches = removeMapping(node_matches);
            nodes_matched--;
            match_count--;
            if(match_count == 0) return NULL;
         }
      }   
      else
      {
         Node *node = (Node*)rule_roots->data;
         int host_index = matchRootNode(node, host, -1, node_matches,
                                        variables, &assignment);
         if(host_index >= 0)
         {
            node_matches = addMapping(node_matches, node->index, host_index, true);
            matches[match_count] = 'n';
            match_count++;
            nodes_matched++;           
         }
         else 
         {
            /* If this is the first root node, then no morphism exists,
             * otherwise set the backtracking flag. */
            if(match_count == 0) return NULL;
            else backtracking = true; 
         }         
      }
      rule_roots = rule_roots->next;
   } 

   do  
   {
      if(backtracking)
      { 
         if(matches[match_count] == 'n')
         {
            /* (2) If the last matched item is a node, find another
             *     match for that node. */
            if(node_matches->flag)
            {
               /* The first item in node_matches is the last node matched. */
               Node *node = getNode(lhs, node_matches->rule_item);
               int host_index = 0;
               /* The matching function used depends on whether the node
                * in question is a root node. */
               if(node->root)
                  host_index = matchRootNode(node, host, node_matches->host_item,
                                             node_matches, variables, &assignment);
               else
                  host_index = matchNode(node, host, node_matches->host_item,
                                         node_matches, variables, &assignment);

               if(host_index >= 0)
               {
                  /* Replace the old mapping with the new one. 
                   * match_count and nodes_matched remain the same. */
                  node_matches = removeMapping(node_matches);
                  node_matches = addMapping(node_matches, node->index,       
                                            host_index, true);
                  backtracking = false;
               }
               else 
               {
                  /* No alternate match is found. Remove the mapping and
                   * decrement the match_counts. */
                  node_matches = removeMapping(node_matches);
                  nodes_matched--;
                  match_count--;
               }
            }
            else
            {
                /* node_matches->flag == false. The node was matched as
                 * the source or target of a match, hence there was only
                 * one option and we should backtrack once more. */
                nodes_matched--;
                match_count--;
            }
         }

         else /* matches[match_count= != 'n'] */
         {
            /* (3) If the last matched item is a edge, find another
             *     match for that edge. 
             *
             * The first item in edge_matches is the last edge matched.    
             * edge_matches->flag is true if the edge was matched from its
             * source in the rule graph. */

            Edge *rule_edge = getEdge(lhs, edge_matches->rule_item);

            int host_index = 
               matchEdge(rule_edge, host, edge_matches->flag, 
                         edge_matches->host_item, edge_matches, variables, 
                         &assignment);
            
            if(host_index >= 0)
            {
               /* Replace the old mapping with the new one. 
                * match_count and edges_matched remain the same. */
               edge_matches = removeMapping(edge_matches);
               edge_matches = addMapping(edge_matches, rule_edge->index,       
                                         host_index, true);

               /* Try to match the other incident node. The flag determines
                * whether the node to query is the source or target of the
                * edge. */
               Edge *host_edge = getEdge(host, host_index);
               int result = matchIncidentNode(rule_edge, host_edge,
                                              edge_matches->flag, node_matches,
                                              variables, &assignment);
               if(result >= 0)
               {
                  /* Add the new node mapping. */
                  int index = 0;
                  if(edge_matches->flag) index = rule_edge->target->index;
                  else index = rule_edge->source->index;

                  node_matches = addMapping(node_matches, index, result, false);
                  matches[match_count] = 'n';
                  match_count++;
                  nodes_matched++;
                  backtracking = false;
               }
               else
               { 
                  if(result == -1) backtracking = true;
               }
               /* Otherwise matchIncidentNode returned -2 and nothing needs
                * to be done. */
            }
            else 
            {
              /* No alternate match is found. Remove the mapping and
               * decrement the match_counts. */
               edge_matches = removeMapping(edge_matches);
               edges_matched--;
               match_count--;
            }
         }
         /* Restart the do-while loop. */
         break;   

      /* end if(backtracking) */
      }

      /* No need to explicitly check if backtracking is off as the backtracking
       * loop always breaks at its conclusion. */

      /* (4) Match an edge incident to an already matched node.
       */
      if(edges_matched < number_of_edges && nodes_matched > 0) 
      {
         bool match_from_source = false;
         Edge *edge = NULL;
         GraphMapping *iterator = node_matches;

         /* Attempt to find an unmatched edge incident to an unmatched node.
          * If one is found, it will be assigned to edge. 
          * If no such edge is found, then another node must be matched. */
         while(iterator != NULL)
         {           
            Node *node = getNode(lhs, node_matches->rule_item);

            /* First try searching the node's outedges. */
            GSList *out_edges = getOutEdges(node);
            while(out_edges != NULL) 
            {
               Edge *candidate_edge = (Edge *)out_edges->data;
               if(lookupFromRule(edge_matches, candidate_edge->index) == -1)
               {
                 /* candidate_edge is unmatched. Assign it to edge and break. */
                 edge = candidate_edge;
                 break;
               }
               out_edges = out_edges->next;
            }
         
            if(edge != NULL) {
               match_from_source = true;
               break;
            }

            /* No unmatched edge has been found among the outedges; try 
             * the inedges. */
            GSList *in_edges = getInEdges(node);
            while(in_edges != NULL) 
            {
               Edge *candidate_edge = (Edge *)in_edges->data;
               if(lookupFromRule(edge_matches, candidate_edge->index) == -1)
               {
                 /* candidate_edge is unmatched. Assign it to edge and break. */
                 edge = candidate_edge;
                 break;
               }
               in_edges = in_edges->next;
            }
           
            /* If an edge has been found, break the iterator while loop. */
            if(edge != NULL) break;

            iterator = iterator->next;
         }   
            
         if(edge != NULL) 
         {
            int host_index = matchEdge(edge, host, match_from_source, -1,
                                       edge_matches, variables, &assignment);
            if(host_index >= 0)
            {
               edge_matches = addMapping(edge_matches, edge->index, host_index, 
                                         match_from_source);
               matches[match_count] = 'e';
               match_count++;
               edges_matched++;

               /* (5) Match the other node incident to the edge matched in (4) */
               Edge *host_edge = getEdge(host, host_index);

               int result = matchIncidentNode(edge, host_edge, match_from_source,
                                              node_matches, variables, &assignment);
               if(result >= 0)
               {
                  int index = 0;
                  if(match_from_source) index = edge->target->index;
                  else index = edge->source->index;

                  node_matches = addMapping(node_matches, index, result, false);
                  matches[match_count] = 'n';
                  match_count++;
                  nodes_matched++;
               }
               else
               { 
                  if(result == -1)
                     {
                        backtracking = true;
                        break;
                     }
               }
               /* Otherwise matchTarget/matchSource returned -2 and nothing needs
                * to be done. */
            }
            else 
            {
               /* The node has no match. Turn backtracking on and break the 
                * main loop. */
               backtracking = true;
               break; 
            }
         /* No unmatched edge has been found. Breaking here would result
          * in an infinite loop of trying to find an edge to match. 
          * Instead, do not break, so that the node matching code below
          * is executed once before restarting the loop. */
         }
      }

      /* (6) Match a non-root node. */
      if(nodes_matched < number_of_nodes) 
      {
         int counter;
         /* Iterate over the LHS nodes to find an unmatched node. */
         for(counter = 0; counter < number_of_nodes; counter++) 
         {
            Node *rule_node = g_ptr_array_index(lhs->nodes, counter);
            /* If the rule node has not been matched, try to match it. */
            if(lookupFromRule(node_matches, rule_node->index) == -1) 
            {
               int host_index = matchNode(rule_node, host, -1, node_matches,
                                          variables, &assignment);
               if(host_index >= 0)
               {
                  node_matches = addMapping(node_matches, rule_node->index,
                                            host_index, true);
                  matches[match_count] = 'n';
                  match_count++;
                  nodes_matched++;
               }
               else 
               {
                  backtracking = true;
                  break; 
               }
            }
         }
      /* Restart the while loop to check the loop condition. */
      break;

      }
   }
   while(match_count > 0 || match_count < number_of_nodes + number_of_edges);
       
   /* Check if a complete match has been found. */    

   if(match_count == 0)
   {
      if(node_matches) freeMapping(node_matches);
      if(edge_matches) freeMapping(edge_matches);
      if(assignment) freeAssignment(assignment); 
      return NULL;
   }

   assert(match_count == number_of_nodes + number_of_edges);
 
   Morphism *morphism = malloc(sizeof(Morphism));

   if(morphism == NULL) 
   {
      print_to_log("Memory exhausted during morphism construction.\n");
      exit(1);
   }
   morphism->node_matches = node_matches;
   morphism->edge_matches = edge_matches; 
   morphism->assignment = assignment;

   return morphism;
}


int matchRootNode(Node *rule_root, Graph *host, int index, 
                  GraphMapping *node_matches, VariableList *variables,
		  Assignment **assignment)
{
   GSList *host_roots = getRootNodes(host);

   /* If index is not -1, find the previously matched host root node
    * in the list so that search can be started from that point. */
   if(index >= 0) 
   {
      Node *old_host_root = getNode(host, index);
      host_roots = g_slist_find(host_roots, old_host_root);
      host_roots = host_roots->next;
   }

   while(host_roots != NULL) 
   {
      Node *host_root = (Node*)host_roots->data;

      /* If host_root has not already been matched, try and match 
       * rule_root with host_root. */
      if(lookupFromHost(node_matches, host_root->index) == -1) 
      {
         if(labelMatch(rule_root->label, host_root->label, variables, 
		       assignment)) 
            return host_root->index;
      }
      /* If host_root has already been matched, move to the next host root. */
      host_roots = host_roots->next;
   }    
   /* No match has been found for the current rule_root. */
   return -1;
}         
    

int matchEdge(Edge *rule_edge, Graph *host, bool match_from_source, int index, 
              GraphMapping *edge_matches, VariableList *variables,
              Assignment **assignment)
{
   LabelClass label_class = rule_edge->label_class;
   Node *host_node = NULL;
   GSList *host_edges = NULL;

   /* Get the appropriate edge list according to the match_from_source flag. */
   if(match_from_source)
   {
      int node_index = rule_edge->source->index;         
      host_node = getNode(host, node_index);
      host_edges = getOutEdgesByLabel(host_node, label_class);
   }
   else
   {
      int node_index = rule_edge->target->index;         
      host_node = getNode(host, node_index); 
      host_edges = getInEdgesByLabel(host_node, label_class);    
   } 

   /* If index is not -1, find the previously matched host root node
    * in the list so that search can be started from that point. */
   if(index >= 0) 
   {
      Edge *old_host_edge = getEdge(host, index);
      host_edges = g_slist_find(host_edges, old_host_edge);
      host_edges = host_edges->next;
   }

   while(host_edges != NULL) 
   {
      Edge *host_edge = (Edge*)(host_edges->data);

      if(lookupFromHost(edge_matches, host_edge->index) == -1)
      {
         if(labelMatch(rule_edge->label, host_edge->label, variables,
                       assignment))
            return host_edge->index;
      }
      host_edges = host_edges->next;
   }

   /* No match found. */
   return -1;
}


int matchIncidentNode(Edge *rule_edge, Edge *host_edge, bool match_target, 
                      GraphMapping *node_matches, VariableList *variables,
		      Assignment **assignment)
{
   if(match_target)
   {
      Node *rule_target = getTarget(rule_edge);
      Node *host_target = getTarget(host_edge);
      
      int rule_target_image = lookupFromRule(node_matches, rule_target->index);

      if(rule_target_image >= 0) 
      {
         /* Check if the target of the rule edge has been mapped to the target
          * of the host edge. If not, the match is invalid. If so, the targets
          * are already matched and nothing more needs to be done. */
         if(rule_target_image == host_target->index) return -2;
	 else return -1;
      }

      if(labelMatch(rule_target->label, host_target->label, variables,
                    assignment)) 
         return host_target->index;
      else return -1;
   }

   else 
   {
      Node *rule_source = getSource(rule_edge);
      Node *host_source = getSource(host_edge);

      int rule_source_image = lookupFromRule(node_matches, rule_source->index);
      
      if(rule_source_image >= 0) 
      {
         /* Check if the source of the rule edge has been mapped to the source
          * of the host edge. If not, the match is invalid. If so, the sources
          * are already matched and nothing more needs to be done. */
         if(rule_source_image == host_source->index) return -2;
         else return -1;
      }

      if(labelMatch(rule_source->label, host_source->label, variables,
                    assignment)) 
         return host_source->index;
      else return -1;
   }
}


int matchNode(Node *rule_node, Graph *host, int index, 
              GraphMapping *node_matches, VariableList *variables,
              Assignment **assignment)
{
   LabelClass label_class = rule_node->label_class;
   GSList *host_nodes = getNodes(host, label_class);

   /* If index is not -1, find the previously matched host root node
    * in the list so that search can be started from that point. */
   if(index >= 0) 
   {
      Node *old_host_node = getNode(host, index);
      host_nodes = g_slist_find(host_nodes, old_host_node);
      host_nodes = host_nodes->next;
   }

   while(host_nodes != NULL) 
   {
      Node *host_node = (Node *)(host_nodes->data);
      /* If the host node has not been matched, check it against the rule
       * node. */
      if(lookupFromHost(node_matches, host_node->index) == -1) 
      {
         if(labelMatch(rule_node->label, host_node->label, variables,
                       assignment))
            return host_node->index;
      host_nodes = host_nodes->next;
      }
   }
   return -1;
}
