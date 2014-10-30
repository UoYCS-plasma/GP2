/* ///////////////////////////////////////////////////////////////////////////

  ===================================
  generate.c - Chris Bak (28/10/2014)
  ===================================

/////////////////////////////////////////////////////////////////////////// */

#define PC printCode
#define PIC printICode

#include "generate.h"

FILE *match_source = NULL;

void generateMatchingCode(Graph *lhs, string rule_name)
{
   /* Create a file match_<rule_name>.c */
   int length = strlen(rule_name) + 8;
   char file_name[length];
   strcpy(file_name, "match_");
   strcat(file_name, rule_name);
   strcat(file_name, ".c");

   match_source = fopen(file_name, "w");
   if(match_source == NULL) { 
     perror(file_name);
     exit(1);
   } 

   PC("#include \"globals.h\"\n"
      "#include \"graph.h\"\n"
      "#include \"match.h\"\n\n"
      "#define LEFT_NODES %d\n"
      "#define LEFT_EDGES %d\n\n" 

      "Morphism *match_%s(Graph *host)\n"
      "{\n" 
      "   if(LEFT_NODES > host->number_of_nodes ||\n"
      "      LEFT_EDGES > host->number_of_edges)\n"
      "      return NULL;\n\n"
      "   /* Initialise variables. */\n"
      "   Stack *node_images = newStack();\n"
      "   Stack *edge_images = newStack();\n"
      "   int nodes_matched = 0, edges_matched = 0, match_count = 0;\n"
      "   bool backtracking = false, match_found = false; set_root_list = false;\n"
      "   Stack *matched_items = NULL;\n\n"
      "   GSList *roots = getRootNodes(host);\n\n",
      lhs->number_of_nodes, lhs->number_of_edges, rule_name);

   GSList *left_roots = getRootNodes(lhs); 
   Node *left_node = (Node *)left_roots->data;

   while(left_roots != NULL)
   {
      PIC("/* Match a root node. */\n", 3);
      PIC("while(roots != NULL)\n", 3);
      PIC("{\n", 3);
      PIC("if(backtracking)\n", 6);
      PIC("{\n", 6);
      PIC("/* At this point the match stack contains only nodes, \n", 9);
      PIC(" * so the top of the matched_items stack is discarded.\n", 9);
      PIC(" */\n", 9);
      PIC("void *p = pop(matched_items);\n", 9);
      PIC("Node *node = (Node *)pop(node_images);\n\n", 9);
      PIC("if(node == NULL) \n", 9);
      PIC("{\n", 9);
      PIC("print_to_log(\"Error: Unexpected NULL pointer from node "
                 "image stack.\\n\");\n", 12);
      PIC("exit(1);\n", 12);
      PIC("}\n\n", 9);
      PIC("/* Navigate to the node's location in the root list if not already "
          "done so. */\n", 9);
      PIC("if(!set_root_list)\n", 9);
      PIC("{\n", 9);
      PIC("roots = g_slist_find(roots, node);\n", 12);
      PIC("roots = roots->next;\n", 12);
      PIC("set_root_list = true;\n", 12);
      PIC("}\n\n", 9);
      PIC("node = (Node *)roots->data;\n\n", 9);
      emitNodeMatchingCode(left_node, true, 9);
      PIC("}\n", 6);

      PIC("else\n", 6); 
      PIC("{\n", 6);
      PIC("Node *node = (Node *)roots->data;\n\n", 9);
      emitNodeMatchingCode(left_node, false, 9);
      PIC("}\n", 6);
      PIC("roots = roots->next;\n", 6);
      PIC("}\n\n", 3);

      left_roots = left_roots->next;
   }
   
   PIC("}\n", 0);
}

void emitNodeMatchingCode(Node *left_node, bool backtracking, int indent)
{
    PIC("/* Check if the node is already matched. */\n", indent);
    PIC("/* Search node matches stack or something. */\n\n", indent);

    PIC("/* Check label class, mark and degrees. */\n", indent);

    PIC("if(node->label_class != %d)\n", indent, left_node->label_class);
    PIC("{\n", indent);
    PIC("roots = roots->next;\n", indent + 3);
    PIC("continue;\n", indent + 3);
    PIC("}\n\n", indent); 

    PIC("if(node->label->mark != %d)\n", indent, left_node->label->mark);
    PIC("{\n", indent);
    PIC("roots = roots->next;\n", indent + 3);
    PIC("continue;\n", indent + 3);
    PIC("}\n\n", indent); 

    PIC("if(node->indegree < %d)\n", indent, left_node->indegree);
    PIC("{\n", indent);
    PIC("roots = roots->next;\n", indent + 3);
    PIC("continue;\n", indent + 3);
    PIC("}\n\n", indent); 

    PIC("if(node->outdegree < %d)\n", indent, left_node->outdegree);
    PIC("{\n", indent);
    PIC("roots = roots->next;\n", indent + 3);
    PIC("continue;\n", indent + 3);
    PIC("}\n\n", indent); 

    /* Generating label matching code is currently a chasm of confusion. */
    PIC("if(labelMatch(node))\n", indent);
    PIC("{\n", indent);
    PIC("push(node_images, node);\n", indent + 3);
    PIC("push(matched_items, \"n\");\n", indent + 3);
    PIC("nodes_matches++;\n", indent + 3);
    PIC("match_count++;\n", indent + 3);
    PIC("backtracking = false;\n", indent + 3);
    PIC("match_found = true;\n", indent + 3);
    PIC("break;\n", indent + 3);
    PIC("}\n", indent);

    if(backtracking)
    {
       PIC("else\n", indent);
       PIC("{\n", indent);
       PIC("/* If the first root node fails to match, then no morphism \n", indent + 3); 
       PIC(" * exists. Otherwise set the backtracking flag. */\n", indent + 3);
       PIC("nodes_matched--;\n", indent + 3);
       PIC("match_count--;\n", indent + 3);
       PIC("if(match_count == 0) return NULL;\n", indent + 3);
       PIC("else\n", indent + 3);
       PIC("{\n", indent + 3);
       PIC("roots = roots->next;\n", indent + 6);
       PIC("continue;\n", indent + 6);
       PIC("}\n", indent + 3);
       PIC("}\n", indent);
    }
    else
    {
       PIC("else /* (!backtracking) */\n", indent);
       PIC("{\n", indent);
       PIC("/* If the first root node fails to match, then no morphism \n", indent + 3); 
       PIC(" * exists. Otherwise set the backtracking flag. */\n", indent + 3);
       PIC("if(match_count == 0) return NULL;\n", indent + 3);
       PIC("}\n", indent);
    }
}
