#include "rule.h"

VariableList *addVariable(VariableList *variable_list, string name, GPType type) 
{
   VariableList *new_variable_list = malloc(sizeof(VariableList));

   if(new_variable_list == NULL) 
   {
      print_to_log("Memory exhausted during rule construction.\n");
      exit(1);
   }

   new_variable_list->variable = strdup(name);
   new_variable_list->type = type;   
   new_variable_list->next = variable_list;

   return new_variable_list;
}

/* I assume this called only when a variable is known to be in the list. */
GPType lookupType(VariableList *variable_list, string name) 
{
   while(variable_list != NULL) 
   {
     if(strcmp(variable_list->variable, name) == 0) return variable_list->type;
     variable_list = variable_list->next;
   }
   print_to_log("Error: lookupType called incorrectly.");
   exit(0);         
}

void freeVariableList(VariableList *variable_list)
{
   if(variable_list == NULL) return;
   if(variable_list->variable) free(variable_list->variable);
   if(variable_list->next) freeVariableList(variable_list->next);  
   free(variable_list); 
}

IndexMap *addIndexMap(IndexMap *map, string id, int left_index, 
                      int right_index, string source_id, string target_id)
{
   IndexMap *new_map = malloc(sizeof(IndexMap));
   if(new_map == NULL)
   {
      print_to_log("Error: Memory exhausted during map construction.\n");
      exit(1);
   }
   new_map->id = strdup(id);
   new_map->left_index = left_index;
   new_map->right_index = right_index;
   if(source_id == NULL) new_map->source_id = NULL;
   else new_map->source_id = strdup(source_id);
   if(target_id == NULL) new_map->target_id = NULL;
   else new_map->target_id = strdup(target_id);
   new_map->next = map;

   return new_map;
}
  
IndexMap *findMapFromId(IndexMap *map, string id)
{
   while(map != NULL)
   {
      if(!strcmp(map->id, id)) return map;
      else map = map->next;
   }
   return NULL; 
}

IndexMap *findMapFromSrcTgt(IndexMap *map, string source, string target)
{
   while(map != NULL)
   {
      if(!strcmp(map->source_id, source) && !strcmp(map->target_id, target))
         return map;
      else map = map->next;
   }
   return NULL; 
}


IndexMap *removeMap(IndexMap *map, IndexMap *map_to_remove)
{
   if(map == map_to_remove)
   {
      IndexMap *temp = map;
      map = map->next;
      if(temp->id) free(temp->id);
      if(temp->source_id) free(temp->source_id);
      if(temp->target_id) free(temp->target_id);
      free(temp);
   }
   else
   {
      IndexMap *iterator = map;
      while(iterator != NULL)
      {
         if(iterator->next == NULL) break;
         if(iterator->next == map_to_remove)
         {
            IndexMap *temp = iterator->next;
            iterator->next = iterator->next->next;
            if(temp->id) free(temp->id);
            if(temp->source_id) free(temp->source_id);
            if(temp->target_id) free(temp->target_id);
            free(temp);
            break;
         }
         else iterator = iterator->next;
      }
   }
   return map;
}


void freeIndexMap(IndexMap *map)
{
   if(map == NULL) return;
   
   if(map->id) free(map->id);
   if(map->source_id) free(map->source_id);
   if(map->target_id) free(map->target_id);
   if(map->next) freeIndexMap(map->next);
   free(map);
}

ItemList *addItem(ItemList *item_list, int index)
{
   ItemList *new_item = malloc(sizeof(ItemList));

   if(new_item == NULL) 
   {
      print_to_log("Error: Memory exhausted during rule construction.\n");
      exit(1);
   }

   new_item->index = index;
   new_item->next = item_list;

   return new_item;
}

bool queryItemList(ItemList *item_list, int index)
{
   while(item_list != NULL)
   {
      if(index == item_list->index) return true;
      else item_list = item_list->next;
   }
   return false;
}

void freeItemList(ItemList *item_list)
{
   if(item_list == NULL) return;
   if(item_list->next) freeItemList(item_list->next);
   free(item_list);
}

PreservedItemList *addPreservedItem(PreservedItemList *list, bool label_change,
                                    int left_index, int right_index)
{
   PreservedItemList *new_list = malloc(sizeof(PreservedItemList));

   if(new_list == NULL) 
   {
      print_to_log("Error: Memory exhausted during rule construction.\n");
      exit(1);
   }

   new_list->label_change = label_change;
   new_list->left_index = left_index;   
   new_list->right_index = right_index;
   new_list->next = list;

   return new_list;
}

PreservedItemList *queryPItemList(PreservedItemList *list, int left_index)
{
   while(list != NULL)
   {
      if(left_index == list->left_index) return list;
      else list = list->next;
   }
   return NULL;
}

int findRightIndex(PreservedItemList *list, int left_index)     
{
   while(list != NULL)
   {
      if(left_index == list->left_index) return list->right_index;
      else list = list->next;
   }
   return -1;
}

void freePItemList(PreservedItemList *list)
{
   if(list == NULL) return;
   if(list->next) freePItemList(list->next);
   free(list);
}

NewEdgeList *addNewEdge(NewEdgeList *edge, int index, char source_loc, int source_index,
                    char target_loc, char target_index)
{
   NewEdgeList *new_edge = malloc(sizeof(NewEdgeList));

   if(new_edge == NULL) 
   {
      print_to_log("Error: Memory exhausted during rule construction.\n");
      exit(1);
   }

   new_edge->edge_index = index;
   new_edge->source_location = source_loc;
   new_edge->source_index = source_index;   
   new_edge->target_location = target_loc;
   new_edge->target_index = target_index;
   new_edge->next = edge;

   return new_edge;
}

void freeNewEdgeList(NewEdgeList *edge)
{
   if(edge == NULL) return;
   if(edge->next) freeNewEdgeList(edge->next);
   free(edge);
}


void printRule(Rule *rule)
{
   if(rule == NULL) 
   {
      printf("printRule passed a NULL pointer.\n\n");
      return;
   }

   printf("%s\n\n", rule->name);
   printGraph(rule->lhs);
   printf("=>\n\n");
   printGraph(rule->rhs);
}



void freeRule(Rule *rule)
{
   if(rule == NULL) return;
   if(rule->name) free(rule->name);
   if(rule->variables) freeVariableList(rule->variables);
   if(rule->lhs) freeGraph(rule->lhs);
   if(rule->rhs) freeGraph(rule->rhs);
   if(rule->preserved_nodes) freePItemList(rule->preserved_nodes);
   if(rule->preserved_edges) freePItemList(rule->preserved_edges);
   if(rule->deleted_nodes) freeItemList(rule->deleted_nodes);
   if(rule->added_nodes) freeItemList(rule->added_nodes);
   if(rule->added_edges) freeNewEdgeList(rule->added_edges);
   /* Conditions currently ignored. No function exists to free a condition. */
   if(rule->condition) free(rule->condition);
   free(rule);
}
