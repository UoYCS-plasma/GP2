#include "genProgram.h"

#undef RULE_TRACE
#undef GRAPH_TRACE
#undef BACKTRACK_TRACE

static FILE *main_source = NULL;
int undo_point_count = 0;

void generateRuntimeCode(List *declarations)
{
   FILE *host_file = fopen("runtime/host.h", "w");
   if(host_file == NULL) { 
     perror("runtime/host.h");
     exit(1);
   }  
   fprintf(host_file, "extern Graph *host;\n");
   fclose(host_file);

   main_source = fopen("runtime/main.c", "w");
   if(main_source == NULL) { 
     perror("runtime/main.c");
     exit(1);
   }

   PTMS("#include <time.h>\n"
        "#include \"../error.h\"\n"
        "#include \"../debug.h\"\n"
        "#include \"../graph.h\"\n"
        "#include \"../graphStacks.h\"\n"
        "#include \"init_runtime.h\"\n"
        "#include \"host.h\"\n"
        "#include \"match.h\"\n\n");
   generateMorphismCode(declarations, 'd');
   PTMS("Graph *host = NULL;\n"
        "bool success = true;\n\n"
        "void garbageCollect(void);\n"
        "void freeMorphisms(void);\n\n");
   generateDeclarationCode(declarations);
   fclose(main_source);
}

void generateDeclarationCode(List *declarations)
{
   PTMS("int main(void)\n{\n");
   PTMSI("srand(time(NULL));\n", 3);
   PTMSI("openLogFile();\n", 3);
   PTMSI("host = makeHostGraph();\n\n", 3);
   #ifdef GRAPH_TRACE
      PTMSI("print_to_log(\"/* Start Graph. */\\n\");\n", 3);
      PTMSI("printGraph(host, log_file);\n\n", 3);
   #endif
   generateMorphismCode(declarations, 'm');
   List *iterator = declarations;
   while(iterator != NULL)
   {
      GPDeclaration *decl = iterator->declaration;
     
      switch(decl->decl_type)
      {
         /* This case should be reached exactly once, including recursive calls.
          * The semantic analysis ensures that the passed AST contains exactly one
          * MAIN_DECLARATION node. */
         case MAIN_DECLARATION:
              generateProgramCode(decl->main_program, MAIN_BODY, -1, -1, 3);
              break;

         case PROCEDURE_DECLARATION:
              if(decl->procedure->local_decls != NULL)
                 generateDeclarationCode(decl->procedure->local_decls);
              break;

         case RULE_DECLARATION:
              break;

         default: 
              print_to_log("Error (generateDeclarationCode): Unexpected "
                           "declaration type %d at AST node %d\n", 
                           decl->decl_type, decl->id);
              break;
      }
      iterator = iterator->next;
   }
   PTMS("   /* Output Graph. */\n"
        "   printGraph(host, stdout);\n"
        "   garbageCollect();\n"
        "   return 0;\n"
        "}\n\n");

   PTMS("void garbageCollect(void)\n"
        "{\n"
        "   freeGraph(host);\n"
        "   freeMorphisms();\n"
        "   freeGraphStack();\n"
        "   freeGraphChangeStack();\n"
        "   closeLogFile();\n"
        "}\n\n");

   PTMS("void freeMorphisms(void)\n{\n");
   generateMorphismCode(declarations, 'f');
   PTMS("}\n");
}

void generateMorphismCode(List *declarations, char type)
{
   if(type != 'm' && type != 'f' && type != 'd') 
   {
      print_to_log("Error: generateMorphismCode called with invalid type %c.\n", type);
      exit(1);
   }
   while(declarations != NULL)
   {
      GPDeclaration *decl = declarations->declaration;
      switch(decl->decl_type)
      {
         case MAIN_DECLARATION:
              break;

         case PROCEDURE_DECLARATION:
              if(decl->procedure->local_decls != NULL)
                 generateMorphismCode(decl->procedure->local_decls, type);
              break;

         case RULE_DECLARATION:
         {
              GPRule *rule = decl->rule;
              if(type == 'd')
              {
                 PTMS("#include \"%s.h\"\n", rule->name);
                 PTMS("Morphism *M_%s = NULL;\n", rule->name);
              }
              if(type == 'm')
                 PTMSI("M_%s = makeMorphism(%d, %d, %d);\n", 3,
                       rule->name, rule->left_nodes, rule->left_edges,
                       rule->variable_count);
              if(type == 'f')
                 PTMSI("freeMorphism(M_%s);\n", 3, rule->name);
              break;
         }
         default: 
              print_to_log("Error (generateMorphismCode): Unexpected "
                           "declaration type %d at AST node %d\n", 
                           decl->decl_type, decl->id);
              break;
      }
      declarations = declarations->next;
   }
   if(type == 'd' || type == 'm') PTMS("\n");
}


void generateProgramCode(GPCommand *command, ContextType context, 
                         int restore_point, int undo_point, int indent)
{
   switch(command->command_type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = command->commands;
           while(commands != NULL)
           {
              generateProgramCode(commands->command, context, restore_point,
                                  undo_point, indent);
              if(context == LOOP_BODY && commands->next != NULL)
                 PTMSI("if(!success) break;\n", indent);             
              commands = commands->next;
           }           
           break;
      }
      case RULE_CALL:

           PTMSI("/* Rule Call */\n", indent);
           generateRuleCall(command->rule_call.rule_name, 
                            command->rule_call.rule->empty_lhs,
                            command->rule_call.rule->is_predicate,
                            context, restore_point, undo_point, true, indent);
           break;

      case RULE_SET_CALL:

           PTMSI("/* Rule Set Call */\n", indent);
           generateRuleSetCall(command->rule_set, context, restore_point,
                               undo_point, indent);
           break;

      case PROCEDURE_CALL:
      {
           GPProcedure *procedure = command->proc_call.procedure;
           generateProgramCode(procedure->commands, context, restore_point,
                               undo_point, indent);
           break;
      }

      case IF_STATEMENT:

      case TRY_STATEMENT:
           generateBranchStatement(command, context, restore_point, undo_point, indent);
           break;

      case ALAP_STATEMENT:
           generateLoopStatement(command, undo_point, indent);
           break;

      case PROGRAM_OR:

           /* Emit code to nondeterministically choose between the two 
            * subprograms. */
           PTMSI("/* OR Statement */\n", indent);
           PTMSI("int random = rand();\n", indent);
           PTMSI("if((random %% 2) == 0)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(command->or_stmt.left_command, context, 
                               restore_point, undo_point, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(command->or_stmt.right_command, context, 
                               restore_point, undo_point, indent + 3);
           PTMSI("}\n", indent);
           if(context == IF_BODY || context == TRY_BODY) 
              PTMSI("break;\n", indent);
           break;

      case SKIP_STATEMENT:

           PTMSI("/* Skip Statement */\n", indent);
           PTMSI("success = true;\n", indent);
           break;
           
      case FAIL_STATEMENT:

           PTMSI("/* Fail Statement */\n", indent);
           generateFailureCode(NULL, context, restore_point, undo_point, indent);
           break;

      case BREAK_STATEMENT:
           PTMSI("/* Break Statement */\n", indent);
           if(restore_point >= 0)
           {
              PTMSI("/* The graph copy is no longer needed. */\n", indent);
              #ifdef BACKTRACK_TRACE
                 PTMSI("print_to_log(\"(%d) Discarding graphs.\\n\\n\");\n",
                       indent + 3, restore_point);
              #endif
              PTMSI("discardGraphs(%d);\n", indent, restore_point);
           }
           if(undo_point >= 0)
           {
              PTMSI("/* Graph changes from loop body not required.\n", indent);
              PTMSI("   Discard them so that future graph roll backs are "
                    "uncorrupted. */\n", indent);
              #ifdef BACKTRACK_TRACE
                 PTMSI("print_to_log(\"(%d) Discarding graph changes.\\n\\n\");\n",
                       indent + 3, undo_point);
              #endif
              PTMSI("discardChanges(undo_point%d);\n", indent, undo_point);
           }
           PTMSI("break;\n", indent);
           break;
           
      default: 
           print_to_log("Error (generateProgramCode): Unexpected command type "
                        "%d at AST node %d\n", command->command_type, command->id);
           break;
   }
}

void generateRuleCall(string rule_name, bool empty_lhs, bool predicate,
                      ContextType context, int restore_point,
                      int undo_point, bool last_rule, int indent)
{
   if(empty_lhs)
   {
      #ifdef RULE_TRACE
         PTMSI("print_to_log(\"Matched %s. (empty rule)\\n\\n\");\n", indent, rule_name);
      #endif
      if(undo_point >= 0) PTMSI("apply%s(true);\n", indent, rule_name);
      else PTMSI("apply%s(false);\n", indent, rule_name);
      #ifdef GRAPH_TRACE
         PTMSI("printGraph(host, log_file);\n\n", indent);
      #endif
      PTMSI("success = true;\n\n", indent);
   }
   else
   {
      #ifdef RULE_TRACE
         PTMSI("print_to_log(\"Matching %s...\\n\");\n", indent, rule_name);
      #endif
      PTMSI("if(match%s(M_%s))\n", indent, rule_name, rule_name);
      PTMSI("{\n", indent);
      #ifdef RULE_TRACE
         PTMSI("print_to_log(\"Matched %s.\\n\\n\");\n", indent + 3, rule_name);
      #endif
      if(!predicate)
      {
         /* Optimisation: Don't have to apply the last rule in an if condition.
          * However, finding such rules is non-trivial. The condition below
          * suffices for contexts where applying the rule is incorrect, namely
          * where there is no restore point and hence the graph is not copied
          * in the GP2 if condition. */
         if(context != IF_BODY || restore_point >= 0)
         { 
            if(undo_point >= 0) 
               PTMSI("apply%s(M_%s, true);\n", indent + 3, rule_name, rule_name);
            else PTMSI("apply%s(M_%s, false);\n", indent + 3, rule_name, rule_name);
            #ifdef GRAPH_TRACE
               PTMSI("printGraph(host, log_file);\n\n", indent + 3);
            #endif
         }
      }
      else PTMSI("clearMorphism(M_%s);\n", indent + 3, rule_name);
      PTMSI("success = true;\n", indent + 3);
      if(!last_rule) PTMSI("break;\n", indent + 3);
      PTMSI("}\n", indent);
      /* Only generate failure code if the last rule in the set fails. */ 
      if(last_rule)
      {
         PTMSI("else\n", indent);
         PTMSI("{\n", indent);
         #ifdef RULE_TRACE
            PTMSI("print_to_log(\"Failed to match %s.\\n\\n\");\n", indent + 3, rule_name);
         #endif
         generateFailureCode(rule_name, context, restore_point, undo_point, indent + 3);
         PTMSI("}\n", indent);  
      }
      else 
      {
         #ifdef RULE_TRACE
            PTMSI("print_to_log(\"Failed to match %s.\\n\\n\");\n", indent + 3, rule_name);
         #endif
      }
   }
}

void generateRuleSetCall(List *rules, ContextType context, int restore_point, 
                         int undo_point, int indent)
{
   PTMSI("do\n", indent);
   PTMSI("{\n", indent);
   while(rules != NULL)
   {  
      string rule_name = rules->rule_call.rule_name;
      bool empty_lhs = rules->rule_call.rule->empty_lhs;
      bool predicate = rules->rule_call.rule->is_predicate;
      if(rules->next == NULL)
           generateRuleCall(rule_name, empty_lhs, predicate, context,
                            restore_point, undo_point, true, indent + 3);
      else generateRuleCall(rule_name, empty_lhs, predicate, context, 
                            restore_point, undo_point, false, indent + 3);
      rules = rules->next;
   }
   PTMSI("} while(false);\n", indent);
}

void generateBranchStatement(GPCommand *command, ContextType context,
                             int restore_point, int undo_point, int indent)
{
   int new_restore_point = command->cond_branch.restore_point;
   bool roll_back = command->cond_branch.roll_back;
   int new_undo_point = -1;
   if(roll_back) new_undo_point = undo_point_count++;
   else if(undo_point >= 0) new_undo_point = undo_point;

   ContextType new_context = command->command_type == IF_STATEMENT ?
                             IF_BODY : TRY_BODY;
   if(new_context == IF_BODY) PTMSI("/* If Statement */\n", indent);
   else PTMSI("/* Try Statement */\n", indent);
   PTMSI("/* Condition */\n", indent);
   if(roll_back)
   {
      #ifdef BACKTRACK_TRACE
         PTMSI("print_to_log(\"(%d) Recording graph changes.\\n\\n\");\n",
               indent, new_undo_point);
      #endif
      PTMSI("int undo_point%d = graph_change_index;\n", indent, new_undo_point);
   }
   PTMSI("do\n", indent);
   PTMSI("{\n", indent);
   if(new_restore_point >= 0) 
   {
      #ifdef BACKTRACK_TRACE
         PTMSI("print_to_log(\"(%d) Copying host graph.\\n\\n\");\n",
               indent + 3, new_restore_point);
      #endif
      PTMSI("copyGraph(host);\n", indent + 3);
   }
   generateProgramCode(command->cond_branch.condition, new_context,
                       new_restore_point, new_undo_point, indent + 3);
   PTMSI("} while(false);\n\n", indent);
   if(new_context == IF_BODY)
   {
      if(new_restore_point >= 0) 
      {
         #ifdef BACKTRACK_TRACE
            PTMSI("print_to_log(\"(%d) Restoring graph.\\n\\n\");\n",
                  indent, new_restore_point);
         #endif
         PTMSI("host = popGraphs(host, %d);\n", indent, new_restore_point);
      }
      if(roll_back)
      {
         #ifdef BACKTRACK_TRACE
            PTMSI("print_to_log(\"(%d) Undoing graph changes.\\n\\n\");\n",
                  indent, new_restore_point);
         #endif
         PTMSI("undoChanges(host, undo_point%d);\n", indent, new_undo_point);
      }
   }
   PTMSI("/* Then Branch */\n", indent);
   PTMSI("if(success)\n", indent);
   PTMSI("{\n", indent);
   generateProgramCode(command->cond_branch.then_command, 
                       context, restore_point, undo_point, indent + 3);
   PTMSI("}\n", indent);
   PTMSI("/* Else Branch */\n", indent);
   PTMSI("else\n", indent);
   PTMSI("{\n", indent);
   if(new_context == TRY_BODY)
   {
      if(new_restore_point >= 0) 
      {
         PTMSI("host = popGraphs(host, %d);\n", indent + 3, new_restore_point);
         #ifdef BACKTRACK_TRACE
            PTMSI("print_to_log(\"(%d) Restoring graph.\\n\\n\");\n",
                  indent + 3, new_restore_point);
            PTMSI("printGraph(host, log_file);\n", indent + 3);
         #endif
      }
      if(roll_back)
      {
         PTMSI("undoChanges(host, undo_point%d);\n", indent + 3, new_undo_point);
         #ifdef BACKTRACK_TRACE
            PTMSI("print_to_log(\"(%d) Undoing graph changes.\\n\\n\");\n",
                  indent + 3, new_undo_point);
            PTMSI("printGraph(host, log_file);\n", indent + 3);
         #endif
      }
   }
   generateProgramCode(command->cond_branch.else_command, 
                       context, restore_point, undo_point, indent + 3);
   PTMSI("}\n", indent);
   if(context == IF_BODY || context == TRY_BODY) PTMSI("break;\n", indent);
   return;
}

void generateLoopStatement(GPCommand *command, int undo_point, int indent)
{
   int new_restore_point = command->loop_stmt.restore_point;
   bool roll_back = command->loop_stmt.roll_back;   
   int new_undo_point = -1;
   if(roll_back) new_undo_point = undo_point_count++;
   else if(undo_point >= 0 && !command->loop_stmt.stop_recording)
            new_undo_point = undo_point;
               
   PTMSI("/* Loop Statement */\n", indent);
   PTMSI("success = true;\n", indent);
   if(roll_back) PTMSI("int undo_point%d = graph_change_index;\n", 
                        indent, new_undo_point);
   PTMSI("while(success)\n", indent);
   PTMSI("{\n", indent);
   if(new_restore_point >= 0) 
   {
      #ifdef BACKTRACK_TRACE
         PTMSI("print_to_log(\"(%d) Copying host graph.\\n\\n\");\n",
               indent + 3, new_restore_point);
      #endif
      PTMSI("copyGraph(host);\n", indent + 3);
   }
   if(roll_back)
   {
      #ifdef BACKTRACK_TRACE
         PTMSI("print_to_log(\"(%d) Recording graph changes.\\n\\n\");\n",
               indent + 3, new_undo_point);
      #endif
   }
   generateProgramCode(command->loop_stmt.loop_body, LOOP_BODY,
                        new_restore_point, new_undo_point, indent + 3);
   if(new_restore_point >= 0)
   {
      PTMSI("/* If the body has succeeded, the graph copy is no "
            "longer needed. */\n", indent + 3);
      #ifdef BACKTRACK_TRACE
         PTMSI("print_to_log(\"(%d) Discarding graphs.\\n\\n\");\n",
               indent + 3, new_restore_point);
      #endif
      PTMSI("if(success) discardGraphs(%d);\n", indent + 3, 
            new_restore_point);
   }
   if(roll_back)
   {
      PTMSI("/* Graph changes from loop body may not have been used.\n", 
            indent + 3);
      PTMSI("   Discard them so that future graph roll backs are "
            "uncorrupted. */\n", indent + 3);
      #ifdef BACKTRACK_TRACE
         PTMSI("print_to_log(\"(%d) Discarding graph changes.\\n\\n\");\n",
               indent + 3, new_undo_point);
      #endif
      PTMSI("if(success) discardChanges(undo_point%d);\n", 
            indent + 3, new_undo_point);
   }
   PTMSI("}\n", indent);
   PTMSI("success = true;\n", indent);
}

void generateFailureCode(string rule_name, ContextType context, 
                         int restore_point, int undo_point, int indent)
{
   /* A failure in the main body ends the execution. Emit code to report the 
    * failure, garbage collect and return 0. */
   if(context == MAIN_BODY)
   {
      #ifdef GRAPH_TRACE
         PTMSI("print_to_log(\"Program failed. Final graph below.\\n\");\n", indent);
         PTMSI("printGraph(host, log_file);\n", indent);
      #endif
      if(rule_name != NULL)
         PTMSI("print_to_console(\"No output graph: rule %s not "
               "applicable.\\n\");\n", indent, rule_name);
      else PTMSI("print_to_console(\"No output graph: Fail statement "
                 "invoked.\\n\");\n", indent);
      PTMSI("garbageCollect();\n", indent);
      PTMSI("return 0;\n", indent);
   }
   /* In other contexts, set the success flag to false. */
   else PTMSI("success = false;\n", indent);

   if(context == IF_BODY || context == TRY_BODY) PTMSI("break;\n", indent);

   if(context == PROC_BODY) 
   {
      if(rule_name == NULL) PTMSI("return NULL;\n", indent);
      else PTMSI("return \"%s\";\n", indent, rule_name);
   }
   if(context == LOOP_BODY) 
   {
      if(restore_point >= 0)
      {
         PTMSI("host = popGraphs(host, %d);\n", indent, restore_point);
         #ifdef BACKTRACK_TRACE
            PTMSI("print_to_log(\"(%d) Restoring host graph.\\n\\n\");\n",
                  indent, restore_point);
            PTMSI("printGraph(host, log_file);\n", indent);
         #endif
      }
      if(undo_point >= 0) 
      {
         PTMSI("undoChanges(host, undo_point%d);\n", indent, undo_point);
         #ifdef BACKTRACK_TRACE
            PTMSI("print_to_log(\"(%d) Undoing graph changes.\\n\\n\");\n",
                  indent, undo_point);
            PTMSI("printGraph(host, log_file);\n", indent);
         #endif
      }
   }
}

