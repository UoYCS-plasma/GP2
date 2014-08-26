/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.c - Chris Bak (14/08/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "match.h"

Morphism *makeMorphism (Graph *lhs, Graph *host, GraphMapping *node_matches,
                        GraphMapping *edge_matches, Stack *assignment) {

   Morphism *morphism = malloc(sizeof(Morphism));

   if(morphism == NULL) {
      print_to_log("Memory exhausted during morphism construction.\n");
      exit(1);
   }

   morphism->node_matches = newStack(lhs->next_node_index);
   morphism->edge_matches = newStack(lhs->next_edge_index);


   GraphMapping *iterator;

   for(iterator = node_matches; iterator != NULL; iterator = iterator->next)
   {
      NodeMorphism *node_morphism = malloc(sizeof(NodeMorphism));

      if(node_morphism == NULL) {
         print_to_log("Memory exhausted during morphism construction.\n");
         exit(1);
      }

      node_morphism->rule_node = g_ptr_array_index(lhs->nodes, iterator->rule_item);
      node_morphism->host_node = g_ptr_array_index(host->nodes, iterator->host_item);

      push(morphism->node_matches, node_morphism);
   }

   for(iterator = edge_matches; iterator != NULL; iterator = iterator->next)
   {
      EdgeMorphism *edge_morphism = malloc(sizeof(EdgeMorphism));

      if(edge_morphism == NULL) {
         print_to_log("Memory exhausted during morphism construction.\n");
         exit(1);
      }

      edge_morphism->rule_edge = g_ptr_array_index(lhs->edges, iterator->rule_item);
      edge_morphism->host_edge = g_ptr_array_index(host->edges, iterator->host_item);

      push(morphism->edge_matches, edge_morphism);
   }

   morphism->assignment   = assignment;

   return morphism;
}


GraphMapping *node_matches = NULL;
GraphMapping *edge_matches = NULL;


/* To quickly check if the total morphism is found. */
int nodes_matched = 0, edges_matched = 0;

Stack *assignment = NULL;

/* Third argument determined when traversing the AST. */
Morphism *findMatch (Graph *lhs, Graph *host, int number_of_variables) {

   int number_of_nodes = lhs->next_node_index;
   int number_of_edges = lhs->next_edge_index;
   assignment = newStack(number_of_variables);

   /* Match root nodes */
   if(lhs->root_nodes) {
      if(!matchRootNodes(lhs, host)) return NULL;
   }

   /* Repeat searchplan operations until a total match is found or it is 
    * demonstrated that one does not exist. The loop will repeat after
    * the execution of a single operation, hence there is always a break 
    * statement after each function call. */
   while(nodes_matched != number_of_nodes && edges_matched != number_of_edges) {
      
      /* Find the source and target of matched edges. */
      if(nodes_matched < number_of_nodes && edge_matches)
      {
         if(!matchSourceAndTargets(lhs, host)) return NULL;
         break;
      }
      
      /* If not all edges have been matched, match edges with a matched 
       * source or target. */
      if(edges_matched < number_of_edges) 
      {
         int counter;
         for(counter = 0; counter < number_of_edges; counter++) 
         {
            Edge *rule_edge = g_ptr_array_index(lhs->edges, counter);
            /* If the rule edge has not been matched, try to match it. */
            if(lookupFromRule(edge_matches, rule_edge->index) == -1) 
            {
               if(!matchEdge(rule_edge, lhs, host)) return NULL;
               /* The call to matchEdge may match an edge, hence I check here
                * to see if all the left edges have been matched. If so, no 
                * need to iterate further through the left edges. */
               else if(edges_matched >= number_of_edges) break;
            }
         }
         break;
      }

      /* Match a single node. */
      if(nodes_matched < number_of_nodes) {
         int counter;
         /* Iterate over the left nodes to find one that has not been matched. */
         for(counter = 0; counter < number_of_nodes; counter++) {
            Node *rule_node = g_ptr_array_index(lhs->edges, counter);
            /* If the rule node has not been matched, try to match it. */
            if(lookupFromRule(node_matches, rule_node->index) == -1) 
            {
               if(!matchNode(rule_node, lhs, host)) return NULL;
               /* A node has been matched. Exit the for loop. */
               else break;
            }
         }
         /* Restart the while loop to check the loop condition. */
         break;
      }
   }

   /* If this point has been reached, then a total morphism has been found,
    * otherwise the function will have hit one of the return NULLs. */
   Morphism *match = makeMorphism(lhs, host, node_matches, edge_matches, assignment);

   /* Using the not-yet-implemented association lists for node and edge matches, create
    * the stacks of node matches and edge matches. */
   match->assignment = assignment;

   return match;
}


bool matchRootNodes(Graph *lhs, Graph *host) {

   GSList *lhs_roots = lhs->root_nodes;
   bool match_exists = false;

   do {
      Node *rule_root = (Node*)lhs_roots->data;
      GSList *host_roots = host->root_nodes;

      while(host_roots != NULL || match_exists) {
         Node *host_root = (Node*)host_roots->data;

         /* If host_root has not already been matched, try and match 
          * rule_root with host_root. */
         if(lookupFromHost(node_matches, host_root->index) == -1) 
         {
            if(labelMatch(assignment, rule_root->label, host_root->label)) 
            {
                addMap(node_matches, rule_root->index, host_root->index);
                nodes_matched++;
                match_exists = true;
                /* Match found. Exit the while loop. */
                break;
            }
         }
         /* If host_root has already been matched, move to the next host root. */
         host_roots = host_roots->next;
      }    
      /* No match has been found for the current rule_root, therefore a morphism
       * does not exist. */
      if(!match_exists) return false;
      lhs_roots = lhs_roots->next;
   } 
   while(lhs_roots != NULL); 
   
   return match_exists;
}         
     

bool matchSourceAndTargets(Graph *lhs, Graph *host) {
   
   bool match_exists = false;
   GraphMapping *iterator = edge_matches;

   /* Iterate over the matched rule edges. */
   do 
   {
      int rule_edge_index = iterator->rule_item;
      Edge *rule_edge = g_ptr_array_index(lhs->edges, rule_edge_index);

      /* Get the image of the rule edge according to the mapping. */
      int host_edge_index = lookupFromRule(edge_matches, rule_edge_index); 
      Edge *host_edge = g_ptr_array_index(host->edges, host_edge_index);

      Node *rule_source = rule_edge->source;
      Node *host_source = host_edge->source;

      int rule_source_image = lookupFromRule(node_matches, rule_source->index);

      if(rule_source_image >= 0) 
      {
         /* Check if the source of the rule edge has been mapped to the source
          * of the host edge. If not, the match is invalid. If so, the sources
          * are already matched and nothing more needs to be done. */
         if(rule_source_image != host_source->index) return false;
      }
      else  
      {
         /* No match exists for the rule source, check if one exists for the
          * host source. If so, the match is invalid. If not, try to match
          * the rule source with the host source. */
         if(lookupFromHost(node_matches, host_source->index) >= 0) return false;
         else
         {
            if(labelMatch(assignment, rule_source->label, host_source->label)) 
            {
               addMap(node_matches, rule_source->index, host_source->index);
               nodes_matched++;
               match_exists = true;
            }
         }    
      }

      /* If at this point match_exists is false, then no match has been found
       * for the rule edge's source. Therefore a match does not exist. */
      if(!match_exists) return false;

      /* Repeat for the target nodes. */
      Node *rule_target = rule_edge->target;
      Node *host_target = host_edge->target;

      int rule_target_image = lookupFromRule(node_matches, rule_target->index);

      if(rule_target_image >= 0) 
      {
         /* Check if the target of the rule edge has been mapped to the target
          * of the host edge. If not, the match is invalid. If so, the targets
          * are already matched and nothing more needs to be done. */
         if(rule_target_image != host_target->index) return false;
      }
      else  
      {
         /* No match exists for the rule target, check if one exists for the
          * host target. If so, the match is invalid. If not, try to match
          * the rule target with the host target. */
         if(lookupFromHost(node_matches, host_target->index) >= 0) return false;
         else
         {
            if(labelMatch(assignment, rule_target->label, host_target->label)) 
            {
               addMap(node_matches, rule_target->index, host_target->index);
               nodes_matched++;
               match_exists = true;
            }
         }    
      }

      /* If at this point match_exists is false, then no match has been found
       * for the rule edge's target. Therefore a match does not exist. */
      if(!match_exists) return false;

      iterator = iterator->next;
   } 
   while(iterator != NULL); 

   return match_exists;
}  


bool matchEdge(Edge *rule_edge, Graph *lhs, Graph *host) {

   Node *source = getSource(rule_edge);
   Node *target = getTarget(rule_edge);
   LabelClass label_class = rule_edge->label_class;

   /* Get the image of rule_edge's source and target in the mapping if they
    * exist. */
   int source_index = lookupFromRule(node_matches, source->index);
   int target_index = lookupFromRule(node_matches, target->index);

   /* If neither source nor target have been matched, do not match the edge. */
   if(source_index == -1 && target_index == -1) return true;

   bool match_exists = false;
   
   if(source_index >= 0 && target_index >= 0) {

      /* Get the corresponding host source and host target. */
      Node *host_source = g_ptr_array_index(host->nodes, source_index);
      Node *host_target = g_ptr_array_index(host->nodes, target_index);

      GSList *candidate_outedges = getOutEdges(host_source, label_class);
      GSList *candidate_inedges = getInEdges(host_target, label_class);

      /* Need to find edges present in both lists. I will iterate over the
       * outedges and check if each edge exists in the inedges. My choice of 
       * list to iterate over is arbitrary. */
      while(candidate_outedges != NULL) {
         Edge *host_edge = (Edge*)(candidate_outedges->data);
         if(g_slist_find(candidate_inedges, host_edge)) {
            if(labelMatch(assignment, rule_edge->label, host_edge->label)) 
            {
               addMap(edge_matches, rule_edge->index, host_edge->index);
               edges_matched++;
               match_exists = true;
               /* Match found. Exit the while loop. */
               break;
            } 
         }
         /* If the outedge is not in the inedge list, try the next outedge. */
         candidate_outedges = candidate_outedges->next;
      } 
   }
        
   else if(source_index) {
      /* Get the corresponding host source. */
      Node *host_source = g_ptr_array_index(host->nodes, source_index);

      GSList *candidate_edges = getOutEdges(host_source, label_class);

      while(candidate_edges != NULL) {
         Edge *host_edge = (Edge*)(candidate_edges->data);
         if(labelMatch(assignment, rule_edge->label, host_edge->label)) 
         {
            addMap(edge_matches, rule_edge->index, host_edge->index);
            edges_matched++;
            match_exists = true;
            /* Match found. Exit the while loop. */
            break;
         } 
      }
      candidate_edges = candidate_edges->next;
   } 
   

   else if(target_index) {
      /* Get the corresponding host target. */
      Node *host_target = g_ptr_array_index(host->nodes, target_index);

      GSList *candidate_edges = getInEdges(host_target, label_class);

      while(candidate_edges != NULL) {
         Edge *host_edge = (Edge*)(candidate_edges->data);
         if(labelMatch(assignment, rule_edge->label, host_edge->label)) 
         {
            addMap(edge_matches, rule_edge->index, host_edge->index);
            edges_matched++;
            match_exists = true;
            /* Match found. Exit the while loop. */
            break;
         } 
      }
      candidate_edges = candidate_edges->next;
   }

   return match_exists;
}


bool matchNode(Node *rule_node, Graph *lhs, Graph *host) {

   GSList *candidate_nodes = getNodes(host, rule_node->label_class);
   bool match_exists = false;

   while(candidate_nodes != NULL) {
      Node *host_node = (Node *)(candidate_nodes->data);
      /* If the host node has not been matched, check it against the rule
       * node. */
      if(lookupFromHost(node_matches, host_node->index) == -1) 
      {
         if(labelMatch(assignment, rule_node->label, host_node->label))
         {
            addMap(node_matches, rule_node->index, host_node->index);
            nodes_matched++;
            match_exists = true;
            /* Match found. Exit the while loop. */
            break;
         }
      candidate_nodes = candidate_nodes->next;
      }
   }
   return match_exists;
}
         
  


