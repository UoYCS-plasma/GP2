#include "genProgram.h"

void generateHostGraphCode(GPGraph *ast_host_graph)
{
   FILE *header = fopen("init_runtime.h", "w");
   if(header == NULL) { 
     perror("init_runtime.h");
     exit(1);
   }  

   FILE *source = fopen("init_runtime.c", "w");
   if(source == NULL) { 
     perror("init_runtime.c");
     exit(1);
   }
     
   fprintf(header, "#include \"graph.h\"\n"
                   "#include \"macros.h\"\n"
                   "#include \"rule.h\"\n\n"
 		   "Graph *makeHostGraph(void);\n");

   PTIS("#include \"init_runtime.h\"\n\n"
        "Graph *makeHostGraph(void)\n"
        "{\n"
        "   Graph *host = newGraph();\n"
        "   Node *node, *source, *target = NULL;\n"
        "   Edge *edge = NULL;\n"
        "   IndexMap *node_map = NULL;\n\n");

   List *nodes = ast_host_graph->nodes;

   /* For each node in the AST, create a data structure for its label and
    * call ADD_HOST_NODE. ADD_HOST_NODE is a macro that creates the node,
    * adds it to the graph, and adds a new record to the node index map. */
   while(nodes != NULL)
   {
      GPNode *ast_node = nodes->value.node;
      //TODO: Incorporate Label *label = transformLabel(ast_node->label);
      PTIS("   ADD_HOST_NODE(%d, \"%s\")\n", ast_node->root, ast_node->name);
      nodes = nodes->next;   
   }
   PTIS("\n");
   List *edges = ast_host_graph->edges;

   /* For each edge in the AST, look up its source and target. For loops,
    * only the source needs to be looked up. GET_HOST_SOURCE and 
    * GET_HOST_TARGET are macros that search node_map for a node with the 
    * appropriate string ID. */
   while(edges != NULL)
   {
      GPEdge *ast_edge = edges->value.edge;

      //TODO: Incorporate Label *label = transformLabel(ast_edge->label);
      PTIS("   GET_HOST_SOURCE(\"%s\")\n"
           "   /* If the edge is a loop, no need to get the target node. */\n"
           "   if(!strcmp(\"%s\", \"%s\"))\n"
           "   {\n"
           "      edge = newEdge(%d, NULL, source, source);\n"
           "      addEdge(host, edge);\n"
           "   }\n",
           ast_edge->source, ast_edge->source, ast_edge->target, 
           ast_edge->bidirectional);

      PTIS("   else\n"
           "   {\n"
           "      GET_HOST_TARGET(\"%s\")\n"
           "      edge = newEdge(%d, NULL, source, target);\n"
           "      addEdge(host, edge);\n\n"
           "   }\n",
           ast_edge->target, ast_edge->bidirectional);

      edges = edges->next;   
   }
   PTIS("   if(node_map) freeIndexMap(node_map);\n"
        "   return host;\n"
        "}\n");
}

static FILE *main_header = NULL;
static FILE *main_source = NULL;

void generateRuntimeCode(List *declarations)
{
   main_header = fopen("runtime.h", "w");
   if(main_header == NULL) { 
     perror("runtime.h");
     exit(1);
   }  

   main_source = fopen("runtime.c", "w");
   if(main_source == NULL) { 
     perror("runtime.c");
     exit(1);
   }

   PTMH("#include \"error.h\"\n"
        "#include \"debug.h\"\n"
        "#include \"graph.h\"\n"
        "#include \"match.h\"\n"
        "#include \"init_runtime.h\"\n\n");

   PTMS("#include \"runtime.h\"\n\n");

   generateDeclarationCode(declarations);

   fclose(main_header);
   fclose(main_source);
}

void generateDeclarationCode(List *declarations)
{
   while(declarations != NULL)
   {
      GPDeclaration *decl = declarations->value.declaration;
     
      switch(decl->decl_type)
      {
         /* This case should be reached exactly once, including recursive calls.
          * The semantic analysis ensures that the passed AST contains exactly one
          * MAIN_DECLARATION node. */
         case MAIN_DECLARATION:

              PTMS("Graph *host = NULL;\n"
                   "Morphism *morphism = NULL;\n"
                   "string result = NULL;\n"
                   "bool success = true, copy = false;\n\n"
                   "int main(void)\n"
                   "{\n"               
                   "   openLogFileR();\n"
                   "   host = makeHostGraph();\n\n");

              generateProgramCode(decl->value.main_program, TOP_LEVEL, 3);

              PTMS("   printGraph(host);\n"
                   "   freeGraph(host);\n"
                   "   closeLogFile();\n\n"
                   "   return 0;\n"
                   "}\n\n");

              break;

         case PROCEDURE_DECLARATION:

              PTMH("int proc_%s(void);\n", decl->value.procedure->name);
              PTMS("int proc_%s(void)\n"
                   "{\n", decl->value.procedure->name);

              generateProgramCode(decl->value.procedure->cmd_seq, PROC_BODY, 3);

              PTMS("   return 0;\n"
                   "}\n\n");

              if(decl->value.procedure->local_decls != NULL)
                 generateDeclarationCode(decl->value.procedure->local_decls);
              
              break;

         case RULE_DECLARATION:
         {

              PTMH("#include \"%s.h\"\n", decl->value.rule->name);

              Rule *rule = makeRule(decl->value.rule);
              generateRuleCode(rule);
              freeRule(rule);

              break;
         }

         default: print_to_log("Error (generateRuntimeCode): Unexpected "
                               "declaration type %d at AST node %d\n", 
                               decl->decl_type, decl->node_id);
              break;
      }
      declarations = declarations->next;
   }
}

void generateProgramCode(GPStatement *statement, ContextType context, int indent)
{
   switch(statement->statement_type)
   {
      case COMMAND_SEQUENCE:

           generateCommandSequence(statement->value.cmd_seq, context, indent);
           break;

      case RULE_CALL:

           generateRuleCall(statement->value.rule_name, context, indent);
           break;

      case RULE_SET_CALL:

           generateRuleSetCall(statement->value.rule_set, context, indent);
           break;

      case PROCEDURE_CALL:
          
           generateProcedureCall(statement->value.proc_name, context, indent);
           break;

      case IF_STATEMENT:

           generateProgramCode(statement->value.cond_branch.condition, 
                               IF_BODY, indent);
           PTMSI("if(copy)\n", indent);
           PTMSI("{\n", indent);
           PTMSI("host = restoreGraph;\n", indent + 3);
           PTMSI("copy = false;\n", indent + 3);
           PTMSI("}\n", indent);
           PTMSI("if(success)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.then_stmt, 
                               context, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.else_stmt, 
                               context, indent + 3);
           PTMSI("}\n\n", indent);
           PTMSI("success = true;\n\n", indent);
           break;

      case TRY_STATEMENT:

           generateProgramCode(statement->value.cond_branch.condition,
                               TRY_BODY, indent);
           PTMSI("if(success)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.then_stmt, 
                               context, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           PTMSI("if(copy) host = restoreGraph;\n", indent + 3);
           generateProgramCode(statement->value.cond_branch.else_stmt,
                               context, indent + 3);
           PTMSI("}\n\n", indent);
           PTMSI("copy = false;\n", indent);
           PTMSI("success = true;\n\n", indent);
           break;

      case ALAP_STATEMENT:
           /* If loop statement is rule_call or rule_set_call, don't 
            * copy the graph. */
           if(statement->value.loop_stmt->statement_type != RULE_CALL &&
              statement->value.loop_stmt->statement_type != RULE_SET_CALL)
                PTMSI("copyGraph(host);\n", indent);

           PTMSI("while(true)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.loop_stmt, LOOP_BODY, indent);
           PTMSI("}\n\n", indent);
           break;

      case PROGRAM_OR:
           
           /* TODO: random seed thingy */
           generateProgramCode(statement->value.or_stmt.left_stmt, context, indent);
           generateProgramCode(statement->value.or_stmt.right_stmt, context, indent);
           break;

      case SKIP_STATEMENT:

           PTMSI("/* skip */\n\n", indent);
           break;
           
      case FAIL_STATEMENT:

           generateFailureCode(NULL, context, indent);
           break;
           
      default: print_to_log("Error (generateProgramCode): Unexpected "
                            "statement type %d at AST node %d\n", 
                            statement->statement_type,
                            statement->node_id);
           break;
   }
}

void generateCommandSequence(List *commands, ContextType context, int indent)
{
   /* Nothing special to be done for top level and procedure body context:
    * just generate the commands in sequence/ */
   if(context == TOP_LEVEL || context == PROC_BODY)
   {
      while(commands != NULL)
      {
         generateProgramCode(commands->value.command, context, indent);
         commands = commands->next;
      }
   }
   /* In other contexts, the graph may need to be copied. The else branch
    * manually generates code for the first command depending its type,
    * then it generates code with generateProgramCode for the remainder of 
    * the command list. */
   else
   {
      string rule_name = commands->value.command->value.rule_name;

      if(commands->value.command->statement_type == RULE_CALL)
      {
         PTMSI("morphism = match%s(host);\n", indent, rule_name);
         PTMSI("if(morphism != NULL)\n", indent);
         PTMSI("{\n", indent);
         PTMSI("copyGraph(host);\n", indent + 3);
         PTMSI("copy = true;\n", indent + 3);
         PTMSI("apply%s(host);\n", indent + 3, rule_name);
         PTMSI("}\n", indent);
         PTMSI("else\n", indent);
         PTMSI("{\n", indent);
         generateFailureCode(rule_name, context, indent + 3);
         PTMSI("}\n\n", indent);
      }

      if(commands->value.command->statement_type == RULE_SET_CALL)
      {
         List *rules = commands->value.command->value.rule_set;

         PTMSI("while(true)\n", indent);
         PTMSI("{\n", indent);

         while(rules != NULL)
         {  
            PTMSI("morphism = match%s(host);\n", indent, rules->value.rule_name);
            PTMSI("if(morphism != NULL)\n", indent);
            PTMSI("{\n", indent);
            PTMSI("copyGraph(host);\n", indent + 3);
            PTMSI("copy = true;\n", indent + 3);
            PTMSI("apply%s(host);\n", indent + 3, rules->value.rule_name);
            PTMSI("break;\n", indent + 3);
            PTMSI("}\n", indent);
            
            if(rules->next == NULL) 
            {
               PTMSI("else\n", indent + 3);
               PTMSI("{\n", indent + 3);
               generateFailureCode(rules->value.rule_name, context, indent + 6);         
               PTMSI("}\n", indent + 3);
               break;
            }
            rules = rules->next;
         }
      }

      if(commands->value.command->statement_type != RULE_CALL &&
         commands->value.command->statement_type != RULE_SET_CALL)
      {
         PTMSI("copyGraph(host);\n", indent);
         PTMSI("copy = true;\n", indent);
         generateProgramCode(commands->value.command, context, indent);
      }

      commands = commands->next;
      while(commands != NULL)
      {
         generateProgramCode(commands->value.command, context, indent);
         commands = commands->next;
      }
   }
}


void generateRuleCall(string rule_name, ContextType context, int indent)
{
   PTMSI("morphism = match%s(host);\n", indent, rule_name);

   /* No need to apply the rule in an if statement since the original graph is
    * kept for the then or else branch. */
   if(context == IF_BODY) 
   {
      PTMSI("if(morphism == NULL)\n", indent);
      generateFailureCode(NULL, IF_BODY, indent + 3);
   }
   /* In all other contexts, the rule is applied. In particular, the try statement
    * retains changes made to the host graph by the condition when taking the 
    * then branch. */
   else 
   {
      PTMSI("if(morphism != NULL) apply%s(morphism, host);\n", indent, rule_name);
      PTMSI("else\n", indent);
      PTMSI("{\n", indent);
      generateFailureCode(rule_name, context, indent + 3);
      PTMSI("}\n\n", indent);
   }
}

void generateRuleSetCall(List *rules, ContextType context, int indent)
{
   PTMSI("while(true)\n", indent);
   PTMSI("{\n", indent);
   while(rules != NULL)
   {  
      PTMSI("morphism = match%s(host);\n", indent + 3, rules->value.rule_name);
      
      /* No need to apply the rule in an if statement since the original graph is
       * kept for the then or else branch. */
      if(context == IF_BODY)
      {
         PTMSI("if(morphism != NULL) break;\n", indent + 3);
      }
      /* In all other contexts, the rule is applied. In particular, the try statement
       * retains changes made to the host graph by the condition when taking the 
       * then branch. */
      else 
      {
         PTMSI("if(morphism != NULL)\n", indent + 3);
         PTMSI("{\n", indent + 3);
         PTMSI("apply%s(morphism, host);\n", indent + 6,
               rules->value.rule_name);
         PTMSI("break;\n", indent + 6);
         PTMSI("}\n", indent + 3);
      }
      if(rules->next == NULL) 
      {
         PTMSI("else\n", indent + 3);
         PTMSI("{\n", indent + 3);
         generateFailureCode(rules->value.rule_name, context, indent + 6);         
         PTMSI("}\n", indent + 3);
         break;
      }
      rules = rules->next;
   }
}

void generateProcedureCall(string proc_name, ContextType context, int indent)
{
   PTMSI("result = proc_%s(host);\n", indent, proc_name);

   if(context == TOP_LEVEL)
   {
      PTMSI("if(!success)", indent);
      PTMSI("{\n", indent);
      PTMSI("print_to_console(\"No output graph: rule \%s not applicable.\\n\", "
            "result);\n", indent + 3);
      PTMSI("if(graph_stack) freeGraphStack(graph_stack);\n", indent + 3);
      PTMSI("freeGraph(host);\n", indent + 3);
      PTMSI("return 0;\n", indent + 3);
      PTMSI("}\n\n", indent);
   }

   if(context == PROC_BODY) PTMSI("return result;\n", indent);

   if(context == LOOP_BODY) PTMSI("if(!success) break;\n", indent);
}

void generateFailureCode(string rule_name, ContextType context, int indent)
{
   if(context == TOP_LEVEL)
   {
      if(rule_name != NULL)
         PTMSI("print_to_console(\"No output graph: rule %s not "
               "applicable.\\n\");\n", indent, rule_name);
      else PTMSI("print_to_console(\"No output graph: Fail statement "
                 "invoked.\\n\");\n", indent);
      PTMSI("if(graph_stack) freeGraphStack(graph_stack);\n", indent);
      PTMSI("freeGraph(host);\n", indent);
      PTMSI("return 0;\n", indent);
   }

   if(context == PROC_BODY)
   {
      PTMSI("success = false;\n", indent);
      PTMSI("return \"%s\";\n", indent, rule_name);
   }

   if(context == IF_BODY || context == TRY_BODY) 
      PTMSI("success = false;\n", indent);

   if(context == LOOP_BODY) PTMSI("break;\n", indent);
}


