/* ///////////////////////////////////////////////////////////////////////////

  ================================
  match.c - Chris Bak (14/08/2014)
  ================================

/////////////////////////////////////////////////////////////////////////// */

#include "match.h"

static Assignment *assignment;

/* Third argument determined when traversing the AST. */
Morphism *staticSearchplan(Graph *lhs, Graph *host, VariableList *variables) 
{
   GraphMapping *node_matches = NULL;
   GraphMapping *edge_matches = NULL;
  
   int number_of_nodes = lhs->next_node_index;
   int number_of_edges = lhs->next_edge_index;
   int nodes_matched = 0, edges_matched = 0;

   /* Set to true when a fail and stack is popped. If backtrack true,
    * matchers should examine the top of the stack and act accordingly. */
   bool backtracking = false;
   
   char match_stack[number_of_nodes + number_of_edges]; 
   int top = 0;

   /* Match root nodes before entering the main loop. */
   GSList *rule_roots = getRootNodes(lhs);

   while(rule_roots != NULL)
   {
      Node *node = (Node*)rule_roots->data;
      int host_index = matchRootNode(node, host, variables, 0, node_matches);
      if(host_index >= 0)
      {
         node_matches = addMapping(node_matches, node->index, host_index, true);
         match_stack[top] = 'n';
         top++;
         nodes_matched++;           
      }
      else 
      {
         /* If this is the first root node, then no morphism exists,
          * otherwise set the backtracking flag and enter the do-while loop.
          */
         if(nodes_matched == 0) return NULL;
         else 
         {
            backtracking = true; 
            break; 
         }
      }
      rule_roots = rule_roots->next;
   }

   /* Repeat searchplan operations until a total match is found or it is 
    * demonstrated that one does not exist. The loop will repeat after
    * the execution of a single operation, hence there is always a break 
    * statement after each function call. */
   do  
   {
      if(backtracking)
      { 
         if(match_stack[top] == 'n')
         {
            if(node_matches->flag)
            {
               Node *node = getNode(lhs, node_matches->rule_item);
               if(node->root)
               {
                  int host_index = 
                     matchRootNode(node, host, variables, 
                                   node_matches->host_item, node_matches);
                  if(host_index >= 0)
                  {
                     node_matches = removeMapping(node_matches);
                     node_matches = addMapping(node_matches, node->index,       
                                               host_index, true);
                     backtracking = false;
                  }
                  else 
                  {
                     node_matches = removeMapping(node_matches);
                     nodes_matched--;
                     top--;
                  }
                  break;
               }
               else
               {
                  int host_index = 
                     matchNode(node, host, variables, node_matches->host_item,
                               node_matches);
                  if(host_index >= 0)
                  {
                     node_matches = removeMapping(node_matches);
                     node_matches = addMapping(node_matches, node->index,       
                                               host_index, true);
                     backtracking = false;
                  }
                  else 
                  {
                     node_matches = removeMapping(node_matches);
                     nodes_matched--;
                     top--;
                  }
                  break;
               }
            }
            else
            {
                nodes_matched--;
                top--;
                break;
            }
         }
         else
         {
            /* flag is true if the edge was matched from the outgoing list
             * of the rule node. */
               
            Edge *rule_edge = getEdge(lhs, edge_matches->rule_item);

            int index = edge_matches->host_item;
            int host_index = 
               matchEdge(rule_edge, host, variables, edge_matches->flag, 
                         index, edge_matches);
            
            if(host_index >= 0)
            {
               edge_matches = removeMapping(edge_matches);
               edge_matches = addMapping(edge_matches, rule_edge->index,       
                                         host_index, true);

               /* Now try to match the other node */
               int rule_index = 0;

               if(edge_matches->flag) rule_index = rule_edge->target->index;
               else rule_index = rule_edge->source->index;

               Edge *host_edge = getEdge(host, host_index);
               int result = matchIncidentNode(rule_edge, host_edge, variables,
                                              node_matches, edge_matches->flag);

               if(result >= 0)
               {
                  node_matches = 
                     addMapping(node_matches, rule_index, result, false);
                  match_stack[top] = 'n';
                  top++;
                  nodes_matched++;
                  backtracking = false;
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
               edge_matches = removeMapping(edge_matches);
               edges_matched--;
               top--;
            }
            break;
         }
      }   




      /* If not all edges have been matched, search matched nodes for incident
       * unmatched edges. */
      if(edges_matched < number_of_edges && nodes_matched > 0) 
      {
         bool match_from_source = false;
         Edge *edge = NULL;
         GraphMapping *iterator = node_matches;

         /* Attempt to find an unmatched edge incident to an unmatched node.
          * If one is found, it will be assigned to edge_to_match. 
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
                 edge = candidate_edge;
                 break;
               }
               in_edges = in_edges->next;
            }
           
            if(edge != NULL) break;

            iterator = iterator ->next;
         }   
            
         if(edge != NULL) 
         {
            int host_index = matchEdge(edge, host, variables, 
                                       match_from_source, -1, edge_matches);
            if(host_index >= 0)
            {
               edge_matches = addMapping(edge_matches, edge->index, host_index, 
                                         match_from_source);
               match_stack[top] = 'e';
               top++;
               edges_matched++;

               /* Now try to match the other node. */
               Edge *host_edge = getEdge(host, host_index);
               int index = 0;
               if(match_from_source) index = edge->target->index;
               else index = edge->source->index;

               int result = matchIncidentNode(edge, host_edge, variables,
                                              node_matches, -1);

               if(result >= 0)
               {
                  node_matches = addMapping(node_matches, index, result, false);
                  match_stack[top] = 'n';
                  top++;
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
               backtracking = true;
               break; 
            }
         /* No unmatched edge has been found. Breaking here would result
          * in an infinite loop of trying to find an edge to match. 
          * Instead, do not break, so that the node matching code below
          * is executed once before restarting the loop. */
         }
      }

      /* Match a single node. */
      if(nodes_matched < number_of_nodes) 
      {
         int counter;
         /* Iterate over the LHS nodes to find an unmatched node. */
         for(counter = 0; counter < number_of_nodes; counter++) 
         {
            Node *rule_node = g_ptr_array_index(lhs->edges, counter);
            /* If the rule node has not been matched, try to match it. */
            if(lookupFromRule(node_matches, rule_node->index) == -1) 
            {
               int host_index = matchNode(rule_node, host, variables, -1,
                                          node_matches);
               if(host_index >= 0)
               {
                  node_matches = addMapping(node_matches, rule_node->index,
                                            host_index, true);
                  match_stack[top] = 'n';
                  top++;
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
   while(top > 0 || top < number_of_nodes + number_of_edges);
       
   /* Check if complete match found. */    

   if(top == 0) return NULL;

   assert(top == number_of_nodes + number_of_edges);
 
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

int matchRootNode(Node *rule_root, Graph *host, VariableList *variables,
                  int position, GraphMapping *node_matches)
{
   GSList *host_roots = host->root_nodes;
   host_roots = g_slist_nth(host_roots, position);

   while(host_roots != NULL) 
   {
      Node *host_root = (Node*)host_roots->data;

      /* If host_root has not already been matched, try and match 
       * rule_root with host_root. */
      if(lookupFromHost(node_matches, host_root->index) == -1) 
      {
         if(labelMatch(variables, rule_root->label, host_root->label)) 
            return host_root->index;
      }
      /* If host_root has already been matched, move to the next host root. */
      host_roots = host_roots->next;
   }    
   /* No match has been found for the current rule_root. */
   return -1;
}         
    

int matchEdge(Edge *rule_edge, Graph *host, VariableList *variables,
              bool match_from_source, int index, GraphMapping *edge_matches) 
{
   if(match_from_source)
   {
      int source_index = rule_edge->source->index;
      LabelClass label_class = rule_edge->label_class;
      
      /* Get the corresponding host source. */
      Node *host_source = getNode(host, source_index);

      Edge *old_host_edge = getEdge(host, index);

      GSList *candidate_edges = getOutEdgesByLabel(host_source, label_class);
      if(index >= 0) 
      {
         candidate_edges = g_slist_find(candidate_edges, old_host_edge);
         candidate_edges = candidate_edges->next;
      }

      while(candidate_edges != NULL) 
      {
         Edge *host_edge = (Edge*)(candidate_edges->data);

         if(lookupFromHost(edge_matches, host_edge->index) == -1)
         {
            if(labelMatch(variables, rule_edge->label, host_edge->label))
               return host_edge->index;
         }
         candidate_edges = candidate_edges->next;
      }
   }
  
   else
   {
      int target_index = rule_edge->target->index;
      LabelClass label_class = rule_edge->label_class;
      
      /* Get the corresponding host target. */
      Node *host_target = getNode(host, target_index);

      Edge *old_host_edge = getEdge(host, index);

      GSList *candidate_edges = getInEdgesByLabel(host_target, label_class);
      if(index >= 0)
      {
         candidate_edges = g_slist_find(candidate_edges, old_host_edge);
         candidate_edges = candidate_edges->next;
      }

      while(candidate_edges != NULL) 
      {
         Edge *host_edge = (Edge*)(candidate_edges->data);

         if(lookupFromHost(edge_matches, host_edge->index) == -1)
         {
            if(labelMatch(variables, rule_edge->label, host_edge->label))
               return host_edge->index;
         }
         candidate_edges = candidate_edges->next;
      }
   }
   /* No match found. */
   return -1;
}


int matchIncidentNode(Edge *rule_edge, Edge *host_edge, VariableList *variables,
                      GraphMapping *node_matches, bool match_from_source)
{
   if(match_from_source)
   {
      Node *rule_source = getSource(rule_edge);
      Node *host_source = getSource(host_edge);

      int rule_source_image = lookupFromRule(node_matches, rule_source->index);
      
      if(rule_source_image >= 0) 
      {
         /* Check if the source of the rule edge has been mapped to the source
          * of the host edge. If not, the match is invalid. If so, the sources
          * are already matched and nothing more needs to be done. */
         if(rule_source_image != host_source->index) return -2;
      }

      if(labelMatch(variables, rule_source->label, host_source->label)) 
         return host_source->index;
   }

   else 
   {
      Node *rule_target = getTarget(rule_edge);
      Node *host_target = getTarget(host_edge);
      
      int rule_target_image = lookupFromRule(node_matches, rule_target->index);

      if(rule_target_image >= 0) 
      {
         /* Check if the target of the rule edge has been mapped to the target
          * of the host edge. If not, the match is invalid. If so, the targets
          * are already matched and nothing more needs to be done. */
         if(rule_target_image != host_target->index) return -2;
      }

      if(labelMatch(variables, rule_target->label, host_target->label)) 
         return host_target->index;
   }
   return -1;
}

int matchNode(Node *rule_node, Graph *host, VariableList *variables,
              int index, GraphMapping *node_matches) 
{
   LabelClass label_class = rule_node->label_class;
   Node *old_host_node = getNode(host, index);

   GSList *candidate_nodes = getNodes(host, label_class);
   if(index >= 0) 
   {
      candidate_nodes = g_slist_find(candidate_nodes, old_host_node);
      candidate_nodes = candidate_nodes->next;
   }

   while(candidate_nodes != NULL) 
   {
      Node *host_node = (Node *)(candidate_nodes->data);
      /* If the host node has not been matched, check it against the rule
       * node. */
      if(lookupFromHost(node_matches, host_node->index) == -1) 
      {
         if(labelMatch(variables, rule_node->label, host_node->label))
            return host_node->index;
      candidate_nodes = candidate_nodes->next;
      }
   }
   return -1;
}
         

bool labelMatch (VariableList *variables, Label rule_label, Label host_label) 
{
   /* First check if the marks line up. */
   if(rule_label.mark == CYAN && rule_label.mark != host_label.mark) return false;

   GList *rule_list = g_list_first(rule_label.list);
   GList *host_list = g_list_first(host_label.list);

   string list_variable = NULL;

   /* Check if both lists are empty. */
   if(rule_list == NULL && host_list == NULL) return true;

   /* This loop compares elements from the starts of the lists until a list
    * variable is found in the rule list, in which case the elements are
    * compared from the ends of the lists, or a failure condition is met.
    */
   while(rule_list != NULL) 
   {
      /* If the host list is empty, the lists cannot match since the remaining
       * rule lists items cannot be paired up with anything. */
      if(host_list == NULL) return false;
 
      ListElement *rule_atom = (ListElement*)rule_list->data;
      ListElement *host_atom = (ListElement*)host_list->data;

      /* Break if a list variable is encountered. */
      if(rule_atom->type == VARIABLE) 
      {
         if(lookupType(variables, rule_atom->value.name) == LIST_VAR) 
         {   
            list_variable = rule_atom->value.name;
            break;
         }
      }
      
      if(!compareAtoms(rule_atom, host_atom, variables)) return false;

      rule_list = rule_list->next;
      host_list = host_list->next;
   }

   /* The while loop breaks if the rule list contains a list variable, but this
    * is not guaranteed. Hence we must explicitly check if the end of the rule
    * list has been reached. If so, the return value depends on whether the end
    * of the host list has also been reached. 
    * If so, then the lists have equal length. Return true.
    * If not, then some host list elements have not been matched, but there are
    * no rule elements left for them to be matched against. Return false.
    */
   if(rule_list == NULL) return host_list == NULL;

   /* If this point is reached then a list variable has been encountered in
    * the rule list. We need to record the current position in the host list. 
    * This will be the first element of the value assigned to the list 
    * variable. It is also used for length checking in the subsequent loop.
    */
   GList *marker = host_list;
   int marker_position = g_list_position(host_label.list, marker);

   rule_list = g_list_last(rule_label.list);
   host_list = g_list_last(host_label.list);
   
   while(rule_list != NULL) 
   {
      int host_position = g_list_position(host_label.list, host_list);

      ListElement *rule_atom = (ListElement*)rule_list->data;

      /* When the list variable is encountered, assign it the unchecked
       * segment of the host list. 
       */
      if(rule_atom->type == VARIABLE) 
      {
         if(lookupType(variables, rule_atom->value.name) == LIST_VAR) 
         {
            /* Create a new list and assign it to list_variable. */
            GList *value = NULL;
            int counter;
            /* Iterate backwards through the host list from its last element
             * to marker. This enables me to prepend values to the list at
             * each step which is cheaper than appending.
             */
            for(counter = host_position; counter > marker_position; counter--)
            {
               ListElement *buffer = NULL;
               ListElement *atom = g_list_nth_data(host_label.list, counter);
               buffer = memcpy(buffer, atom, sizeof(ListElement));
               value = g_list_prepend(value, buffer);
            }  
            if(!verifyList(assignment, list_variable, value)) return false;
            break;
         }
      }
  
      /* Check if the current host element occurs earlier in the list than the 
       * marker. If so, then it is not possible for the lists to match because
       * the host list has too few elements. 
       */
      if(host_position < marker_position - 1) return false;      
        
      ListElement *host_atom = (ListElement*)host_list->data;

      if(!compareAtoms(rule_atom, host_atom, variables)) return false;

      rule_list = rule_list->prev;
      host_list = host_list->prev;
   }

   return true;
}
    

bool compareAtoms(ListElement *rule_atom, ListElement *host_atom,  
                  VariableList *variables) 
{
   switch(rule_atom->type) 
   {

      case VARIABLE:
      {
         string variable = rule_atom->value.name;

         switch(lookupType(variables, variable)) 
         {
            case INT_VAR:
            
               if(host_atom->type == INTEGER_CONSTANT)
               {
                  return verifyAtom(assignment, variable, host_atom);
               }
               else return false;
               
               break;

            case CHAR_VAR:

               if(host_atom->type == CHARACTER_CONSTANT)
               {
                  return verifyAtom(assignment, variable, host_atom);
               }
               else return false;
               
               break;

            case STRING_VAR:

               if(host_atom->type == CHARACTER_CONSTANT || 
                  host_atom->type == STRING_CONSTANT)
               {
                  return verifyAtom(assignment, variable, host_atom);
               }
               else return false;
               
               break;

            case ATOM_VAR:

               if(host_atom->type == INTEGER_CONSTANT)
               {
                  return verifyAtom(assignment, variable, host_atom);
               }
               else 
               {
                  if(host_atom->type == CHARACTER_CONSTANT || 
                     host_atom->type == STRING_CONSTANT)
                     {
                        return verifyAtom(assignment, variable, host_atom);
                     }
                  else 
                  {
                     print_to_log("Warning: Host atom does not have an "
				  "appropriate type.\n");
                     return false;
                  }
               }
               
               break;

            case LIST_VAR:
        
                print_to_log("Error: A list variable has appeared in the "
                             "compareAtoms function!\n");
            
                break;
         }

      }
      break; 
 
      case INTEGER_CONSTANT:

           if(host_atom->type == INTEGER_CONSTANT)
              return(rule_atom->value.number == host_atom->value.number);
           else return false;
           
           break;
          

      case CHARACTER_CONSTANT:

           if(host_atom->type == CHARACTER_CONSTANT)
              return(strcmp(rule_atom->value.string, host_atom->value.string) == 0);
           else return false;
           
           break;


      case STRING_CONSTANT:

           if(host_atom->type == CHARACTER_CONSTANT ||
              host_atom->type == STRING_CONSTANT)
              return(strcmp(rule_atom->value.string, host_atom->value.string) == 0);
           else return false;
           
           break;

           
      case NEG:

           if(host_atom->type == INTEGER_CONSTANT)
              return(rule_atom->value.number == -host_atom->value.number);
           else return false;
           
           break;
           

      /* I assume that expressions involving empty strings and the concat of
       * two constant strings are simplified in the translation phase.
       * I also assume a right-associative representation.
       * Hence the possibilities are:
       * (1) concat(stringVar, stringConst)
       * (2) concat(stringConst, stringVar)
       * (3) concat(stringConst, concat(stringVar, stringConst))
       */
      case CONCAT:
      
           if(host_atom->type == CHARACTER_CONSTANT ||
              host_atom->type == STRING_CONSTANT)
           {
              /* Possibility (1) */
              if(rule_atom->value.bin_op.left_exp->type == VARIABLE) 
              {
                 string variable = rule_atom->value.bin_op.left_exp->value.name;
                 /* Semantic checking ensures that right_exp is a string or 
                  * character constant. Need to check that right_exp is a 
                  * suffix of host_string. */
                 string host_string = host_atom->value.string;
                 string rule_string = 
                    rule_atom->value.bin_op.right_exp->value.string;
                 
                 string result = isSuffix(rule_string, host_string);
                 
                 if(result != NULL)  
                 {  
                    ListElement *atom = NULL;
                    atom->type = STRING_CONSTANT;
                    atom->value.string = result;
                    return verifyAtom(assignment, variable, atom);
                    free(result);
                 }
                 else return false;
              }
              else 
              {
                 /* Check if left_exp is a prefix of the host string. */
                 string host_string = host_atom->value.string;
                 string rule_string = 
                    rule_atom->value.bin_op.left_exp->value.string;

                 string host_suffix = isPrefix(rule_string, host_string); 
                 
                 if(host_suffix != NULL)
                 {
                    /* Possibility (2) */
                    if(rule_atom->value.bin_op.right_exp->type == VARIABLE) 
                    {                 
                       string variable = rule_atom->value.bin_op.right_exp->value.name;
                       ListElement *atom = NULL;
                       atom->type = STRING_CONSTANT;
                       atom->value.string = host_suffix;
                       return verifyAtom(assignment, variable, atom);
                    }
                    /* Possibility (3) */
                    else /* right_exp is a concat */
                    {
                       ListElement *str = rule_atom->value.bin_op.right_exp; 

                       assert(str->type == CONCAT);
                       assert(str->value.bin_op.left_exp->type == VARIABLE);
                       assert(str->value.bin_op.right_exp->type == CHARACTER_CONSTANT ||
                              str->value.bin_op.right_exp->type == STRING_CONSTANT);

                       /* Check str->right_exp is a suffix of host_remainder. */
                       rule_string = str->value.bin_op.right_exp->value.string;

                       string result = isSuffix(rule_string, host_suffix);
                       
                       if(result != NULL) 
                       {
                          string variable = rule_atom->value.bin_op.left_exp->value.name;
                          ListElement *atom = NULL;
                          atom->type = STRING_CONSTANT;
                          atom->value.string = result;
                          return verifyAtom(assignment, variable, atom);
                          free(result);
                       }
                       else return false;
                    }
                 }
                 else return false;
              }
           }
           else return false;
           
           break;
           
      default: print_to_log("Error: Unexpected list element type %d "
                            "encountered during label matching.\n", 
                            rule_atom->type);
           break;
   }
   return false;
}   


/* Check if test is a prefix of str. If so, return the rest of str after the
 * matched prefix. Otherwise return NULL.
 */

string isPrefix(const string test, const string str)
{
   int test_length = strlen(test);
   int str_length = strlen(str);
   if(str_length < test_length) return NULL;
   /* strncmp will compare test against the first test_length characters
    * of str. */
   if(strncmp(str, test, test_length) == 0) return str + test_length;
   else return NULL;
}

/* Check if test is a suffix of str. If so, return the rest of str before the
 * matched suffix. Otherwise return NULL.
 */

string isSuffix(const string test, const string str)
{
   int offset = strlen(test) - strlen(str);
   if(offset < 0) return NULL;
   /* compare the last test_length characters of str with test. */
   if(strcmp(str + offset, test) == 0) 
   {
      /* Make a string containing the first offset characters of str. */
      string rest = malloc(offset + 1);
      if(rest != NULL)
      {
         print_to_log("Memory exhausted during prefix construction.\n");
         exit(1);
      }
      strncpy(rest, str, offset);
      rest[offset] = '\0';
      return rest;
   }
   else return NULL;
}
  


