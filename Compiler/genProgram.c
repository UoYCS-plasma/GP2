#include "genProgram.h"

#undef RULE_TRACE
#undef GRAPH_TRACE
#undef BACKTRACK_TRACE

static FILE *file = NULL;
int undo_point_count = 0;

void generateRuntimeCode(List *declarations)
{
   file = fopen("runtime/main.c", "w");
   if(file == NULL) { 
     perror("runtime/main.c");
     exit(1);
   }
   PTF("#include <time.h>\n"
       "#include \"../error.h\"\n"
       "#include \"../debug.h\"\n"
       "#include \"../graph.h\"\n"
       "#include \"../graphStacks.h\"\n"
       "#include \"buildHost.h\"\n"
       "#include \"host/host.h\"\n"
       "#include \"morphism.h\"\n\n");
   generateMorphismCode(declarations, 'd');
   PTF("Graph *host = NULL;\n"
       "bool success = true;\n\n"
       "void garbageCollect(void);\n"
       "void freeMorphisms(void);\n\n");
   generateDeclarationCode(declarations);
   fclose(file);
}

void generateDeclarationCode(List *declarations)
{
   PTF("int main(void)\n{\n");
   PTFI("srand(time(NULL));\n", 3);
   PTFI("openLogFile();\n", 3);
   PTFI("host = buildHostGraph();\n\n", 3);
   #ifdef GRAPH_TRACE
      PTFI("print_to_log(\"/* Start Graph. */\\n\");\n", 3);
      PTFI("printGraph(host, log_file);\n\n", 3);
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
   PTF("   /* Output Graph. */\n"
       "   printGraph(host, stdout);\n"
       "   garbageCollect();\n"
       "   return 0;\n"
       "}\n\n");

   PTF("void garbageCollect(void)\n"
       "{\n"
       "   freeGraph(host);\n"
       "   freeMorphisms();\n"
       "   freeGraphStack();\n"
       "   freeGraphChangeStack();\n"
       "   closeLogFile();\n"
       "}\n\n");

   PTF("void freeMorphisms(void)\n{\n");
   generateMorphismCode(declarations, 'f');
   PTF("}\n");
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
                 PTF("#include \"%s.h\"\n", rule->name);
                 PTF("Morphism *M_%s = NULL;\n", rule->name);
              }
              if(type == 'm')
                 PTFI("M_%s = makeMorphism(%d, %d, %d);\n", 3,
                       rule->name, rule->left_nodes, rule->left_edges,
                       rule->variable_count);
              if(type == 'f')
                 PTFI("freeMorphism(M_%s);\n", 3, rule->name);
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
   if(type == 'd' || type == 'm') PTF("\n");
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
                 PTFI("if(!success) break;\n", indent);             
              commands = commands->next;
           }           
           break;
      }
      case RULE_CALL:

           PTFI("/* Rule Call */\n", indent);
           generateRuleCall(command->rule_call.rule_name, 
                            command->rule_call.rule->empty_lhs,
                            command->rule_call.rule->is_predicate,
                            context, restore_point, undo_point, true, indent);
           break;

      case RULE_SET_CALL:

           PTFI("/* Rule Set Call */\n", indent);
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
           PTFI("/* OR Statement */\n", indent);
           PTFI("int random = rand();\n", indent);
           PTFI("if((random %% 2) == 0)\n", indent);
           PTFI("{\n", indent);
           generateProgramCode(command->or_stmt.left_command, context, 
                               restore_point, undo_point, indent + 3);
           PTFI("}\n", indent);
           PTFI("else\n", indent);
           PTFI("{\n", indent);
           generateProgramCode(command->or_stmt.right_command, context, 
                               restore_point, undo_point, indent + 3);
           PTFI("}\n", indent);
           if(context == IF_BODY || context == TRY_BODY) 
              PTFI("break;\n", indent);
           break;

      case SKIP_STATEMENT:

           PTFI("/* Skip Statement */\n", indent);
           PTFI("success = true;\n", indent);
           break;
           
      case FAIL_STATEMENT:

           PTFI("/* Fail Statement */\n", indent);
           generateFailureCode(NULL, context, restore_point, undo_point, indent);
           break;

      case BREAK_STATEMENT:
           PTFI("/* Break Statement */\n", indent);
           if(restore_point >= 0)
           {
              PTFI("/* The graph copy is no longer needed. */\n", indent);
              #ifdef BACKTRACK_TRACE
                 PTFI("print_to_log(\"(%d) Discarding graphs.\\n\\n\");\n",
                      indent + 3, restore_point);
              #endif
              PTFI("discardGraphs(%d);\n", indent, restore_point);
           }
           if(undo_point >= 0)
           {
              PTFI("/* Graph changes from loop body not required.\n", indent);
              PTFI("   Discard them so that future graph roll backs are "
                   "uncorrupted. */\n", indent);
              #ifdef BACKTRACK_TRACE
                 PTFI("print_to_log(\"(%d) Discarding graph changes.\\n\\n\");\n",
                      indent + 3, undo_point);
              #endif
              PTFI("discardChanges(undo_point%d);\n", indent, undo_point);
           }
           PTFI("break;\n", indent);
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
         PTFI("print_to_log(\"Matched %s. (empty rule)\\n\\n\");\n", indent, rule_name);
      #endif
      if(undo_point >= 0) PTFI("apply%s(true);\n", indent, rule_name);
      else PTFI("apply%s(false);\n", indent, rule_name);
      #ifdef GRAPH_TRACE
         PTFI("printGraph(host, log_file);\n\n", indent);
      #endif
      PTFI("success = true;\n\n", indent);
   }
   else
   {
      #ifdef RULE_TRACE
         PTFI("print_to_log(\"Matching %s...\\n\");\n", indent, rule_name);
      #endif
      PTFI("if(match%s(M_%s))\n", indent, rule_name, rule_name);
      PTFI("{\n", indent);
      #ifdef RULE_TRACE
         PTFI("print_to_log(\"Matched %s.\\n\\n\");\n", indent + 3, rule_name);
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
               PTFI("apply%s(M_%s, true);\n", indent + 3, rule_name, rule_name);
            else PTFI("apply%s(M_%s, false);\n", indent + 3, rule_name, rule_name);
            #ifdef GRAPH_TRACE
               PTFI("printGraph(host, log_file);\n\n", indent + 3);
            #endif
         }
      }
      else PTFI("initialiseMorphism(M_%s);\n", indent + 3, rule_name);
      PTFI("success = true;\n", indent + 3);
      if(!last_rule) PTFI("break;\n", indent + 3);
      PTFI("}\n", indent);
      /* Only generate failure code if the last rule in the set fails. */ 
      if(last_rule)
      {
         PTFI("else\n", indent);
         PTFI("{\n", indent);
         #ifdef RULE_TRACE
            PTFI("print_to_log(\"Failed to match %s.\\n\\n\");\n", indent + 3, rule_name);
         #endif
         generateFailureCode(rule_name, context, restore_point, undo_point, indent + 3);
         PTFI("}\n", indent);  
      }
      else 
      {
         #ifdef RULE_TRACE
            PTFI("print_to_log(\"Failed to match %s.\\n\\n\");\n", indent + 3, rule_name);
         #endif
      }
   }
}

void generateRuleSetCall(List *rules, ContextType context, int restore_point, 
                         int undo_point, int indent)
{
   PTFI("do\n", indent);
   PTFI("{\n", indent);
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
   PTFI("} while(false);\n", indent);
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
   if(new_context == IF_BODY) PTFI("/* If Statement */\n", indent);
   else PTFI("/* Try Statement */\n", indent);
   PTFI("/* Condition */\n", indent);
   if(roll_back)
   {
      #ifdef BACKTRACK_TRACE
         PTFI("print_to_log(\"(%d) Recording graph changes.\\n\\n\");\n",
               indent, new_undo_point);
      #endif
      PTFI("int undo_point%d = graph_change_index;\n", indent, new_undo_point);
   }
   PTFI("do\n", indent);
   PTFI("{\n", indent);
   if(new_restore_point >= 0) 
   {
      #ifdef BACKTRACK_TRACE
         PTFI("print_to_log(\"(%d) Copying host graph.\\n\\n\");\n",
               indent + 3, new_restore_point);
      #endif
      PTFI("copyGraph(host);\n", indent + 3);
   }
   generateProgramCode(command->cond_branch.condition, new_context,
                       new_restore_point, new_undo_point, indent + 3);
   PTFI("} while(false);\n\n", indent);
   if(new_context == IF_BODY)
   {
      if(new_restore_point >= 0) 
      {
         #ifdef BACKTRACK_TRACE
            PTFI("print_to_log(\"(%d) Restoring graph.\\n\\n\");\n",
                  indent, new_restore_point);
         #endif
         PTFI("host = popGraphs(host, %d);\n", indent, new_restore_point);
      }
      if(roll_back)
      {
         #ifdef BACKTRACK_TRACE
            PTFI("print_to_log(\"(%d) Undoing graph changes.\\n\\n\");\n",
                  indent, new_restore_point);
         #endif
         PTFI("undoChanges(host, undo_point%d);\n", indent, new_undo_point);
      }
   }
   PTFI("/* Then Branch */\n", indent);
   PTFI("if(success)\n", indent);
   PTFI("{\n", indent);
   generateProgramCode(command->cond_branch.then_command, 
                       context, restore_point, undo_point, indent + 3);
   PTFI("}\n", indent);
   PTFI("/* Else Branch */\n", indent);
   PTFI("else\n", indent);
   PTFI("{\n", indent);
   if(new_context == TRY_BODY)
   {
      if(new_restore_point >= 0) 
      {
         PTFI("host = popGraphs(host, %d);\n", indent + 3, new_restore_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_to_log(\"(%d) Restoring graph.\\n\\n\");\n",
                 indent + 3, new_restore_point);
            PTFI("printGraph(host, log_file);\n", indent + 3);
         #endif
      }
      if(roll_back)
      {
         PTFI("undoChanges(host, undo_point%d);\n", indent + 3, new_undo_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_to_log(\"(%d) Undoing graph changes.\\n\\n\");\n",
                 indent + 3, new_undo_point);
            PTFI("printGraph(host, log_file);\n", indent + 3);
         #endif
      }
   }
   generateProgramCode(command->cond_branch.else_command, 
                       context, restore_point, undo_point, indent + 3);
   PTFI("}\n", indent);
   if(context == IF_BODY || context == TRY_BODY) PTFI("break;\n", indent);
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
               
   PTFI("/* Loop Statement */\n", indent);
   PTFI("success = true;\n", indent);
   if(roll_back) PTFI("int undo_point%d = graph_change_index;\n", 
                       indent, new_undo_point);
   PTFI("while(success)\n", indent);
   PTFI("{\n", indent);
   if(new_restore_point >= 0) 
   {
      #ifdef BACKTRACK_TRACE
         PTFI("print_to_log(\"(%d) Copying host graph.\\n\\n\");\n",
              indent + 3, new_restore_point);
      #endif
      PTFI("copyGraph(host);\n", indent + 3);
   }
   if(roll_back)
   {
      #ifdef BACKTRACK_TRACE
         PTFI("print_to_log(\"(%d) Recording graph changes.\\n\\n\");\n",
              indent + 3, new_undo_point);
      #endif
   }
   generateProgramCode(command->loop_stmt.loop_body, LOOP_BODY,
                       new_restore_point, new_undo_point, indent + 3);
   if(new_restore_point >= 0)
   {
      PTFI("/* If the body has succeeded, the graph copy is no "
            "longer needed. */\n", indent + 3);
      #ifdef BACKTRACK_TRACE
         PTFI("print_to_log(\"(%d) Discarding graphs.\\n\\n\");\n",
              indent + 3, new_restore_point);
      #endif
      PTFI("if(success) discardGraphs(%d);\n", indent + 3, 
           new_restore_point);
   }
   if(roll_back)
   {
      PTFI("/* Graph changes from loop body may not have been used.\n", 
           indent + 3);
      PTFI("   Discard them so that future graph roll backs are "
           "uncorrupted. */\n", indent + 3);
      #ifdef BACKTRACK_TRACE
         PTFI("print_to_log(\"(%d) Discarding graph changes.\\n\\n\");\n",
              indent + 3, new_undo_point);
      #endif
      PTFI("if(success) discardChanges(undo_point%d);\n", 
           indent + 3, new_undo_point);
   }
   PTFI("}\n", indent);
   PTFI("success = true;\n", indent);
}

void generateFailureCode(string rule_name, ContextType context, 
                         int restore_point, int undo_point, int indent)
{
   /* A failure in the main body ends the execution. Emit code to report the 
    * failure, garbage collect and return 0. */
   if(context == MAIN_BODY)
   {
      #ifdef GRAPH_TRACE
         PTFI("print_to_log(\"Program failed. Final graph below.\\n\");\n", indent);
         PTFI("printGraph(host, log_file);\n", indent);
      #endif
      if(rule_name != NULL)
         PTFI("print_to_console(\"No output graph: rule %s not "
              "applicable.\\n\");\n", indent, rule_name);
      else PTFI("print_to_console(\"No output graph: Fail statement "
                "invoked.\\n\");\n", indent);
      PTFI("garbageCollect();\n", indent);
      PTFI("return 0;\n", indent);
   }
   /* In other contexts, set the success flag to false. */
   else PTFI("success = false;\n", indent);

   if(context == IF_BODY || context == TRY_BODY) PTFI("break;\n", indent);

   if(context == PROC_BODY) 
   {
      if(rule_name == NULL) PTFI("return NULL;\n", indent);
      else PTFI("return \"%s\";\n", indent, rule_name);
   }
   if(context == LOOP_BODY) 
   {
      if(restore_point >= 0)
      {
         PTFI("host = popGraphs(host, %d);\n", indent, restore_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_to_log(\"(%d) Restoring host graph.\\n\\n\");\n",
                 indent, restore_point);
            PTFI("printGraph(host, log_file);\n", indent);
         #endif
      }
      if(undo_point >= 0) 
      {
         PTFI("undoChanges(host, undo_point%d);\n", indent, undo_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_to_log(\"(%d) Undoing graph changes.\\n\\n\");\n",
                 indent, undo_point);
            PTFI("printGraph(host, log_file);\n", indent);
         #endif
      }
   }
}

