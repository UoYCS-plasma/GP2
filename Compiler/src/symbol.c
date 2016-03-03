/* Copyright 2015-2016 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>. */

#include "symbol.h"

SymbolList *addSymbol(SymbolList *list, SymbolType type, string scope, string rule_name,
                      bool is_var, bool in_lhs, bool wildcard, bool bidirectional)
{
   SymbolList *symbol = malloc(sizeof(SymbolList));
   if(symbol == NULL) {
      print_to_log("Error (makeSymbol): malloc failure.\n");
      exit(1);
   }
   symbol->type = type;
   symbol->scope = scope == NULL ? NULL : strdup(scope);
   symbol->rule_name = rule_name == NULL ? NULL : strdup(rule_name);
   symbol->is_var = is_var;
   symbol->in_lhs = in_lhs;
   symbol->wildcard = wildcard;
   symbol->bidirectional = bidirectional;
   symbol->next = list;
   return symbol;
}

bool symbolInScope(SymbolList *symbol, string scope, string rule_name)
{
   return !strcmp(symbol->scope, scope) && !strcmp(symbol->rule_name, rule_name);
}

void freeSymbolList(gpointer key, gpointer value, gpointer user_data) 
{
   SymbolList *list = (SymbolList*)value;
   while(list != NULL)
   {
      if(list->scope) free(list->scope);    
      if(list->rule_name) free(list->rule_name);
      SymbolList *temp = list;
      list = list->next;
      free(temp);
   }
}


BiEdgeList *addBiEdge(BiEdgeList *list, string scope, string rule_name,
                      char graph, string source, string target)
{
   BiEdgeList *new_edge = malloc(sizeof(BiEdgeList));
   if(new_edge == NULL)
   {
      print_to_log("Error (addBiEdge): malloc failure.\n");
      exit(1);
   }
   new_edge->value.scope = strdup(scope); 
   new_edge->value.rule_name = strdup(rule_name);
   new_edge->value.graph = graph;
   new_edge->value.source = strdup(source);
   new_edge->value.target = strdup(target);
   new_edge->next = list;
   return new_edge;
}

void freeBiEdgeList(BiEdgeList *list) 
{
    if(list == NULL) return;
    BiEdge bi_edge = list->value;
    if(bi_edge.scope) free(bi_edge.scope);
    if(bi_edge.rule_name) free(bi_edge.rule_name);
    if(bi_edge.source) free(bi_edge.source);
    if(bi_edge.target) free(bi_edge.target);
      
    if(list->next) freeBiEdgeList(list->next);             
    free(list);
}
