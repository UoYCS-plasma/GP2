/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.c - Chris Bak (14/08/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "match.h"

Stack *newStack (int max_size) {

   Stack *new_stack = malloc(sizeof(Stack));

   if(new_stack == NULL) {
      print_to_log("Memory exhausted during stack construction.\n");
      exit(1);
   }
 
   void **items = calloc(max_size, sizeof(void*));

   if(items == NULL) {
      print_to_log("Memory exhausted during stack construction.\n");
      exit(1);
   } 

   /* Array indexing starts at 0, so an empty stack has -1 top */
   new_stack->top = -1;
   new_stack->max_size = max_size;
   new_stack->items = items;

   return new_stack;
}

void push (Stack *stack, void *data) {

   if(stackIsFull(stack)) print_to_log("Warning: Trying to push to a full stack.\n");
   else {
      stack->top++;
      stack->items[stack->top] = data;
   }
}

void *pop (Stack *stack) {

   void *popped_item = NULL;

   if(stackIsEmpty(stack)) {
      print_to_log("Warning: Trying to pop from an empty stack.\n");
      return NULL;
   }
   else {
      popped_item = stack->items[stack->top];
      stack->top--;
   }
   
   return popped_item;
}


bool stackIsFull (Stack *stack) {
   return stack->top == stack->max_size;
}

bool stackIsEmpty (Stack *stack) {
   return stack->top == -1;
}

void freeStack (Stack *stack) {
   /* Need to free the items in the stack first. */
   free(stack->items);
   free(stack);
}



Morphism *newMorphism (int nodes, int edges, int variables) {

   Morphism *new_morphism = malloc(sizeof(Morphism));

   if(new_morphism == NULL) {
      print_to_log("Memory exhausted during morphism construction.\n");
      exit(1);
   }

   /* newStack should take the number of nodes in the rule, the number of edges
    * in the rule and the number of variables in the rule respectively. Determined
    * when traversing the AST. */
   new_morphism->node_matches = newStack(nodes);
   new_morphism->edge_matches = newStack(edges);
   new_morphism->assignment   = newStack(variables);

   return new_morphism;
}


/* Stores the indices of matched nodes and edges. 
 * Reimplement these as association lists and use them to construct
 * the morphism at the end when it is known that a match exists. */
GSList *matched_rule_nodes = NULL;
GSList *matched_rule_edges = NULL;

GSList *matched_host_nodes = NULL;
GSList *matched_host_edges = NULL;

/* To quickly check if the total morphism is found. */
int node_matches = 0, edge_matches = 0;

Stack *assignment;


Morphism *findMatch (Graph *lhs, Graph *host, int number_of_variables) {

   int number_of_nodes = lhs->next_node_index;
   int number_of_edges = lhs->next_edge_index;

   /* Match root nodes */
   if(lhs->root_nodes) {
      if(!matchRootNodes(lhs, host)) return NULL;
   }

   /* Repeat searchplan operations until a total match is found or it is 
    * demonstrated that one does not exist. */
   while(node_matches != number_of_nodes && edge_matches != number_of_edges) {
      
      /* Find the source and target of matched edges. */
      if(node_matches < number_of_nodes && matched_rule_edges)
      {
         if(!matchSourceAndTargets(lhs, host)) return NULL;
         /* Restart the while loop to check the loop condition. */
         break;
      }
      
      /* If not all edges have been matched, match edges with a matched 
       * source or target. */
      if(edge_matches < number_of_edges) 
      {
         int counter;
         for(counter = 0; counter < number_of_edges; counter++) 
         {
            Edge *rule_edge = g_ptr_array_index(lhs->edges, counter);
            /* If the edge has not been matched, try to match it. */
            if(g_slist_find(matched_rule_edges, &(rule_edge->index)) == NULL) 
            {
               if(!matchEdge(rule_edge, lhs, host)) return NULL;
               /* The call to matchEdge may match an edge, hence I check here
                * to see if all the left edges have been matched. */
               else if(edge_matches >= number_of_edges) break;
            }
         }
         /* Restart the while loop to check the loop condition. */
         break;
      }

      /* Match a single node. */
      if(node_matches < number_of_nodes) {
         int counter;
         /* Iterate over the left nodes to find one that has not been matched. */
         for(counter = 0; counter < number_of_nodes; counter++) {
            Node *rule_node = g_ptr_array_index(lhs->edges, counter);
            /* If the node has not been matched, try to match it. */
            if(g_slist_find(matched_rule_nodes, &(rule_node->index)) == NULL) 
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
   Morphism *match = newMorphism(number_of_nodes, number_of_edges, number_of_variables);

   /* Using the not-yet-implemented association lists for node and edge matches, create
    * the stacks of node matches and edge matches. */
   match->assignment = assignment;

   return match;
}

      
     
bool matchNode(Node *rule_node, Graph *lhs, Graph *host) {

   GSList *candidate_nodes = getNodes(host, rule_node->label_class);
   bool match_exists = false;

   while(candidate_nodes != NULL) {
      Node *host_node = (Node *)(candidate_nodes->data);
      if(g_slist_find(matched_rule_nodes, &(host_node->index)) == NULL) 
      {
         if(labelMatch(assignment, rule_node->label, host_node->label))
         {
            addNodeMatch(assignment, rule_node->index, host_node>index);
            g_slist_prepend(matched_rule_nodes, &(rule_node->index));
            g_slist_prepend(matched_host_nodes, &(host_node->index));
            node_matches++;
            match_exists = true;
            /* Match found. Exit the while loop. */
            break;
         }
      candidate_nodes->candidate_nodes->next;
      }
   }
   return match_exists;
}
         
    

bool matchEdge(Edge *rule_edge, Graph *lhs, Graph *host) {

   Node *source = getSource(rule_edge);
   Node *target = getTarget(rule_edge);
   LabelClass label_class = rule_edge->label_class;

   int *source_index = (g_slist_find(matched_rule_nodes, &(source->index))); 
   int *target_index = (g_slist_find(matched_rule_nodes, &(target->index))); 

   /* If neither source nor target have been matched, do not match the edge. */
   if(source_index != NULL && target_index != NULL) return true;

   bool match_exists = false;
   
   if(source_index && target_index) {

      /* Get the corresponding host source and host target. */
      int source_position = g_slist_position(matched_rule_nodes, source_index);
      int target_position = g_slist_position(matched_rule_nodes, target_index);
      Node *host_source = g_slist_nth_data(matched_host_edges, source_position);
      Node *host_target = g_slist_nth_data(matched_host_edges, target_position);

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
               addEdgeMatch(assignment, rule_edge->index, host_edge>index);
               g_slist_prepend(matched_rule_edges, &(rule_edge->index));
               g_slist_prepend(matched_host_edges, &(host_edge->index));
               edge_matches++;
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
      int source_position = g_slist_position(matched_rule_nodes, source_index);
      Node *host_source = g_slist_nth_data(matched_host_edges, source_position);

      GSList *candidate_edges = getOutEdges(host_source, label_class);

      while(candidate_edges != NULL) {
         Edge *host_edge = (Edge*)(candidate_edges->data);
         if(labelMatch(assignment, rule_edge->label, host_edge->label)) 
         {
            addEdgeMatch(assignment, rule_edge->index, host_edge>index);
            g_slist_prepend(matched_rule_edges, &(rule_edge->index));
            g_slist_prepend(matched_host_edges, &(host_edge->index));
            edge_matches++;
            match_exists = true;
            /* Match found. Exit the while loop. */
            break;
         } 
      }
      candidate_edges = candidate_edges->next;
   } 
   

   else if(target_index) {
      /* Get the corresponding host target. */
      int target_position = g_slist_position(matched_rule_nodes, target_index);
      Node *host_target = g_slist_nth_data(matched_host_edges, target_position);

      GSList *candidate_edges = getInEdges(host_target, label_class);

      while(candidate_edges != NULL) {
         Edge *host_edge = (Edge*)(candidate_edges->data);
         if(labelMatch(assignment, rule_edge->label, host_edge->label)) 
         {
            addEdgeMatch(match, rule_edge->index, host_edge>index);
            g_slist_prepend(matched_rule_edges, &(rule_edge->index));
            g_slist_prepend(matched_host_edges, &(host_edge->index));
            edge_matches++;
            match_exists = true;
            /* Match found. Exit the while loop. */
            break;
         } 
      }
      candidate_edges = candidate_edges->next;
   }

   return match_exists;
}


bool matchSourceAndTargets(Graph *lhs, Graph *host) {
   
   bool match_exists = false;

   do {
      int *edge_index = matched_rule_edges->data;
      Edge *rule_edge = g_ptr_array_index(lhs->edges, *edge_index);
      Node *rule_source = rule_edge->source;
      Node *rule_target = rule_edge->target;

      /* If the source is already matched, it is not possible to construct
       * a valid morphism. */
      if(g_slist_find(matched_host_nodes, &(rule_source)) != NULL) 
         return false;
      else {
         /* Try to match the rule edge's source with the source of the 
          * corresponding host edge's source. 
          *
          * I use the fact that the rule edge and its matched host edge are in
          * the same position in the matched edges lists. */
         int position = g_slist_position(matched_rule_edges, edge_index);
         Edge *host_edge = g_slist_nth_data(matched_host_edges, position);    
         Node *host_source = host_edge->source;

         if(g_slist_find(matched_host_nodes, &(host_source->index)) != NULL) 
         {
            if(labelMatch(assignment, rule_source->label, 
                           host_source->label)) 
            {
               addNodeMatch(assignment, rule_source->index, host_source->index);
               g_slist_prepend(matched_rule_nodes, &(rule_source->index));
               g_slist_prepend(matched_host_nodes, &(host_source->index));
               node_matches++;
               match_exists = true;
            }
         }    
      }
      /* No match has been found for the rule edge's source, therefore a
       * morphism does not exist. */
      if(!match_exists) return false;
      match_rule_edges = match_rule_edges->next;
   } 
   while(matched_rule_edges != NULL); 

   return match_exists;
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
         if(g_slist_find(matched_host_nodes, &(host_root->index)) == NULL) 
         {
            if(labelMatch(assignment, rule_root->label, host_root->label)) 
            {
                addNodeMatch(assignment, rule_root->index, host_root->index);
                g_slist_prepend(matched_rule_nodes, &(rule_root->index));
                g_slist_prepend(matched_host_nodes, &(host_root->index));
                node_matches++;
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

  
      


