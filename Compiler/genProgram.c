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
           "      addEdge(host, edge);\n"
           "   }\n\n",
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

   PTMH("#include <time.h>\n"
        "#include \"error.h\"\n"
        "#include \"debug.h\"\n"
        "#include \"graph.h\"\n"
        "#include \"init_runtime.h\"\n"
        "#include \"match.h\"\n"
        "#include \"stack.h\"\n\n");

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
                   "bool success = true, copy = false;\n"
                   "int copy_count = 0;\n\n"
                   "int main(void)\n"
                   "{\n"               
                   "   srand(time(NULL));\n"
                   "   openLogFileR();\n"
                   "   host = makeHostGraph();\n\n");
                   /* Debug code 
                   "   printGraph(host);\n\n"); */

              generateProgramCode(decl->value.main_program, MAIN_BODY, 3);

              PTMS("   printGraph(host);\n"
                   "   freeGraph(host);\n"
                   "   if(graph_stack) freeGraphStack(graph_stack);\n"
                   "   closeLogFile();\n\n"
                   "   return 0;\n"
                   "}\n\n");

              break;

         case PROCEDURE_DECLARATION:

              PTMH("string proc_%s(void);\n", decl->value.procedure->name);
              PTMS("string proc_%s(void)\n"
                   "{\n", decl->value.procedure->name);

              generateProgramCode(decl->value.procedure->cmd_seq, PROC_BODY, 3);

              PTMS("   return NULL;\n"
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
 
           /* If the command sequence consists of only one command, call 
            * generateProgramCode, otherwise call generateCommandSequence. */
           if(statement->value.cmd_seq->next == NULL)
              generateProgramCode(statement->value.cmd_seq->value.command,
                                  context, indent);
           else generateCommandSequence(statement->value.cmd_seq, context, 
                                        indent);
           break;

      case RULE_CALL:

           PTMSI("/* Rule Call */\n", indent);
           generateRuleCall(statement->value.rule_name, context, indent);
           break;

      case RULE_SET_CALL:

           PTMSI("/* Rule Set Call */\n", indent);
           generateRuleSetCall(statement->value.rule_set, context, indent);
           break;

      case PROCEDURE_CALL:
          
           generateProcedureCall(statement->value.proc_name, context, indent);
           break;

      case IF_STATEMENT:

           PTMSI("/* If Statement */\n", indent);
           PTMSI("/* Condition */\n", indent);
           generateProgramCode(statement->value.cond_branch.condition, 
                               IF_BODY, indent);
           PTMSI("\n", indent);
           /* In an if statement, the then/else command sequence is executed on
            * the host graph state before the execution of the condition. If 
            * the graph was copied in the condition, restore the graph before
            * taking the branch. */
           PTMSI("if(copy)\n", indent);
           PTMSI("{\n", indent);
           PTMSI("host = restoreGraph(host);\n", indent + 3);
           PTMSI("copy_count--;\n", indent + 3);
           PTMSI("copy = false;\n", indent + 3);
           PTMSI("}\n", indent);
           PTMSI("/* Then Branch */\n", indent);
           PTMSI("if(success)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.then_stmt, 
                               context, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("/* Else Branch */\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.else_stmt, 
                               context, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("success = true;\n\n", indent);
           break;

      case TRY_STATEMENT:

           PTMSI("/* Try Statement */\n", indent);
           PTMSI("/* Condition */\n", indent);
           generateProgramCode(statement->value.cond_branch.condition,
                               TRY_BODY, indent);
           /* In a try statement, only the else command sequence is executed on
            * the host graph state before the execution of the condition. If
            * the condition succeeds, the graph resulting from the execution
            * of the condition is the new working graph for the then branch.
            * Hence the restoreGraph code is emitted within the else branch of
            * the generated code. */
           PTMSI("/* Then Branch */\n", indent);
           PTMSI("if(success)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.then_stmt, 
                               context, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("/* Else Branch */\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           PTMSI("if(copy)\n", indent + 3);
           PTMSI("{\n", indent + 3);
           PTMSI("host = restoreGraph(host);\n", indent + 6);
           PTMSI("copy_count--;\n", indent + 6);
           PTMSI("copy = false;\n", indent + 6);
           PTMSI("}\n", indent + 3);
           generateProgramCode(statement->value.cond_branch.else_stmt,
                               context, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("success = true;\n\n", indent);
           break;

      case ALAP_STATEMENT:
           /* If loop statement is rule_call, rule_set_call or proc_call, don't 
            * copy the graph. */
           if(statement->value.loop_stmt->statement_type != RULE_CALL &&
              statement->value.loop_stmt->statement_type != RULE_SET_CALL &&
              statement->value.loop_stmt->statement_type != PROCEDURE_CALL)
           {
                PTMSI("copyGraph(host);\n", indent);
                PTMSI("copy_count++;\n", indent);
           }
           PTMSI("/* Looped Statement */\n", indent);
           PTMSI("while(success)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.loop_stmt, LOOP_BODY, 
                               indent + 3);
           PTMSI("}\n", indent);
           PTMSI("success = true;\n", indent);
           break;

      case PROGRAM_OR:

           /* Emit code to nondeterministically choose between the two 
            * subprograms. */
           PTMSI("/* OR Statement */\n", indent);
           PTMSI("int random = rand();\n", indent);
           PTMSI("if((random %% 2) == 0)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.or_stmt.left_stmt, context, 
                               indent + 3);
           PTMSI("}\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.or_stmt.right_stmt, context, 
                               indent + 3);
           PTMSI("}\n", indent);
           break;

      case SKIP_STATEMENT:

           PTMSI("/* skip */\n", indent);
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
   /* Nothing special to be done for the main body: just generate the commands 
    * in sequence. */
   if(context == MAIN_BODY)
   {
      while(commands != NULL)
      {
         generateProgramCode(commands->value.command, context, indent);
         commands = commands->next;
      }
   }
   /* In other contexts, the graph may need to be copied.
    * First emit tailored code to process the first command. It then iterates
    * over the rest of the command sequence, calling generateProgramCode on
    * each command.
    *
    * If the first command in the sequence is a rule call or a rule set call, 
    * the graph is copied only when necessary, namely if a match is found for
    * a rule. For other first commands, the graph is copied without any 
    * preliminary tests. */
   else
   {
      string rule_name = commands->value.command->value.rule_name;

      if(commands->value.command->statement_type == RULE_CALL)
      {
         PTMSI("morphism = match%s(host);\n", indent, rule_name);
         PTMSI("if(morphism != NULL)\n", indent);
         PTMSI("{\n", indent);
         PTMSI("copyGraph(host);\n", indent + 3);
         PTMSI("copy_count++;\n", indent + 3);
         if(context == IF_BODY || context == TRY_BODY)
            PTMSI("copy = true;\n", indent + 3);
         PTMSI("apply%s(morphism, host);\n", indent + 3, rule_name);
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
            PTMSI("copy_count++;\n", indent + 3);
            if(context == IF_BODY || context == TRY_BODY)
               PTMSI("copy = true;\n", indent + 3);
            PTMSI("apply%s(morphism, host);\n", indent + 3, rules->value.rule_name);
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
         PTMSI("copy_count++;\n", indent);
         if(context == IF_BODY || context == TRY_BODY)
            PTMSI("copy = true;\n", indent + 3);
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
      PTMSI("if(morphism == NULL) success = false;\n", indent);
      PTMSI("else freeMorphism(morphism);\n", indent);
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
      PTMSI("}\n", indent);
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
         PTMSI("if(morphism != NULL)\n", indent + 3);
         PTMSI("{\n", indent + 3);
         PTMSI("freeMorphism(morphism);\n", indent + 6);
         PTMSI("break;\n", indent + 6);
         PTMSI("}\n", indent + 3);
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
         PTMSI("}\n\n", indent + 3);
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
   PTMSI("}\n", indent);
}

void generateProcedureCall(string proc_name, ContextType context, int indent)
{
   PTMSI("result = proc_%s();\n", indent, proc_name);

   if(context == MAIN_BODY)
   {
      PTMSI("if(!success)\n", indent);
      PTMSI("{\n", indent);
      /* Debug code: print the graph before announcing failure. */
      PTMSI("printGraph(host);\n", indent + 3);
      PTMSI("print_to_console(\"No output graph: rule %%s not applicable.\\n\", "
            "result);\n", indent + 3);
      PTMSI("if(graph_stack) freeGraphStack(graph_stack);\n", indent + 3);
      PTMSI("freeGraph(host);\n", indent + 3);
      PTMSI("return 0;\n", indent + 3);
      PTMSI("}\n", indent);
   }

   if(context == PROC_BODY) PTMSI("return result;\n", indent);

   if(context == LOOP_BODY) PTMSI("if(!success) break;\n", indent);
}

void generateFailureCode(string rule_name, ContextType context, int indent)
{
   /* A failure in the main body ends the execution. Emit code to report the 
    * failure, garbage collect and return 0. */
   if(context == MAIN_BODY)
   {
      /* Debug code: print the graph before announcing failure. */
      PTMSI("printGraph(host);\n", indent);
      if(rule_name != NULL)
         PTMSI("print_to_console(\"No output graph: rule %s not "
               "applicable.\\n\");\n", indent, rule_name);
      else PTMSI("print_to_console(\"No output graph: Fail statement "
                 "invoked.\\n\");\n", indent);
      PTMSI("if(graph_stack) freeGraphStack(graph_stack);\n", indent);
      PTMSI("freeGraph(host);\n", indent);
      PTMSI("return 0;\n", indent);
   }
      
   /* In other contexts, set the success flag to false. Nothing more needs
    * to be done in an IF_BODY and a TRY_BODY. */
   PTMSI("success = false;\n", indent);

   if(context == PROC_BODY) 
   {
      if(rule_name == NULL) PTMSI("return NULL;\n", indent);
      else PTMSI("return \"%s\";\n", indent, rule_name);
   }

   if(context == LOOP_BODY) PTMSI("break;\n", indent);
}


