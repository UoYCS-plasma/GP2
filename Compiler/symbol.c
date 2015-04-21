#include "symbol.h"

Symbol *makeSymbol(SymbolType type, string scope, string containing_rule,
                   bool is_var, bool in_lhs, bool wildcard, bool bidirectional)
{
   Symbol *symbol = malloc(sizeof(Symbol));
   if(symbol == NULL) {
      print_to_log("Error (makeSymbol): malloc failure.\n");
      exit(1);
   }
   symbol->type = type;
   symbol->scope = scope == NULL ? NULL : strdup(scope);
   symbol->containing_rule = containing_rule == NULL ? NULL : strdup(containing_rule);
   symbol->is_var = is_var;
   symbol->in_lhs = in_lhs;
   symbol->wildcard = wildcard;
   symbol->bidirectional = bidirectional;
   return symbol;
}

bool symbolInScope(Symbol *symbol, string scope, string rule_name)
{
   return !strcmp(symbol->scope, scope) && 
          !strcmp(symbol->containing_rule, rule_name);
}

void freeSymbolList(gpointer key, gpointer value, gpointer data) 
{
   /* iterator keeps a pointer to the current GSList node */
   GSList *iterator = (GSList*)value;
   while(iterator != NULL) 
   {
      iterator = iterator->next;
      Symbol *symbol = (Symbol*)iterator->data;
      if(symbol == NULL) continue;
      if(symbol->scope) free(symbol->scope); 
      if(symbol->containing_rule) free(symbol->containing_rule); 
      free(symbol);
   }
   g_slist_free((GSList*)value);
}

void addBiEdge(BiEdgeList *list, string scope, string containing_rule, 
               char graph, string source, string target)
{
   BiEdgeList *new_edge = malloc(sizeof(BiEdgeList));
   if(new_edge == NULL)
   {
      print_to_log("Error (addBiEdge): malloc failure.\n");
      exit(1);
   }
   new_edge->value.scope = strdup(scope); 
   new_edge->value.containing_rule = strdup(containing_rule);
   new_edge->value.graph = graph;
   new_edge->value.source = strdup(source);
   new_edge->value.target = strdup(target);
   new_edge->next = NULL;

   /* Append new_edge to bidirectional_edges. */
   if(list == NULL) list = new_edge;
   else 
   { 
      BiEdgeList *iterator = list;
      while(iterator->next) iterator = iterator->next; 
      iterator->next = new_edge;
   }
}

void freeBiEdgeList(BiEdgeList *list) 
{
    if(list == NULL) return;
    BiEdge bi_edge = list->value;
    if(bi_edge.scope) free(bi_edge.scope);
    if(bi_edge.containing_rule) free(bi_edge.containing_rule);
    if(bi_edge.source) free(bi_edge.source);
    if(bi_edge.target) free(bi_edge.target);
      
    if(list->next) freeBiEdgeList(list->next);             
    free(list);
}
