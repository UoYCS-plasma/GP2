#include "genProgram.h"

static FILE *main_header = NULL;
static FILE *main_source = NULL;

void generateRuntimeCode(List *declarations)
{
   FILE *host_file = fopen("runtime/host.h", "w");
   if(host_file == NULL) { 
     perror("runtime/host.h");
     exit(1);
   }  
   fprintf(host_file, "extern Graph *host;\n");
   fclose(host_file);

   main_header = fopen("runtime/runtime.h", "w");
   if(main_header == NULL) { 
     perror("runtime/runtime.h");
     exit(1);
   }  

   main_source = fopen("runtime/main.c", "w");
   if(main_source == NULL) { 
     perror("runtime/main.c");
     exit(1);
   }

   PTMH("#include <time.h>\n"
        "#include \"../error.h\"\n"
        "#include \"../debug.h\"\n"
        "#include \"../graph.h\"\n"
        "#include \"host.h\"\n"
        "#include \"match.h\"\n"
        "#include \"../stack.h\"\n"
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
                   "bool success = true;\n\n"
                   "int main(void)\n"
                   "{\n"               
                   "   srand(time(NULL));\n"
                   "   openLogFileR();\n"
                   "   host = makeHostGraph();\n\n");
                   /* Debug code 
                   "   printGraph(host);\n\n"); */

              generateProgramCode(decl->value.main_program, MAIN_BODY, -1, 3);

              PTMS("   printGraph(host);\n"
                   "   freeGraph(host);\n"
                   "   if(graph_stack) freeGraphStack(graph_stack);\n"
                   "   closeLogFile();\n\n"
                   "   return 0;\n"
                   "}\n\n");

              break;

         case PROCEDURE_DECLARATION:

              if(decl->value.procedure->local_decls != NULL)
                 generateDeclarationCode(decl->value.procedure->local_decls);
              
              break;

         case RULE_DECLARATION:

              PTMH("#include \"%s.h\"\n", decl->value.rule->name);
              break;

         default: print_to_log("Error (generateRuntimeCode): Unexpected "
                               "declaration type %d at AST node %d\n", 
                               decl->decl_type, decl->node_id);
              break;
      }
      declarations = declarations->next;
   }
}

void generateProgramCode(GPStatement *statement, ContextType context, 
                         int restore_point, int indent)
{
   switch(statement->statement_type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = statement->value.commands;
           while(commands != NULL)
           {
              generateProgramCode(commands->value.command, context, restore_point, indent);
              if(context == LOOP_BODY && commands->next != NULL)
                 PTMSI("if(!success) break;\n", indent);             
              commands = commands->next;
           }           
           break;
      }
      case RULE_CALL:

           PTMSI("/* Rule Call */\n", indent);
           generateRuleCall(statement->value.rule_call.rule_name, 
                            statement->value.rule_call.rule->empty_lhs,
                            statement->value.rule_call.rule->is_predicate,
                            context, restore_point, 
                            statement->value.rule_call.copy_point, false, indent);
           break;

      case RULE_SET_CALL:

           PTMSI("/* Rule Set Call */\n", indent);
           generateRuleSetCall(statement->value.rule_set, context, restore_point, indent);
           break;

      case PROCEDURE_CALL:
      {
           GPProcedure *procedure = statement->value.proc_call.procedure;
           generateProgramCode(procedure->commands, context, 
                               procedure->restore_point, indent);
           break;
      }

      case IF_STATEMENT:
      {
           int if_restore_point = statement->value.cond_branch.restore_point;
           PTMSI("/* If Statement */\n", indent);
           PTMSI("/* Condition */\n", indent);
           PTMSI("do\n", indent);
           PTMSI("{\n", indent);
           if(statement->value.cond_branch.copy_point && if_restore_point >= 0)
              PTMSI("copyGraph(host, %d);\n", indent + 3, if_restore_point);
           generateProgramCode(statement->value.cond_branch.condition, 
                               IF_BODY, if_restore_point, indent + 3);
           PTMSI("} while(false);\n\n", indent);
           if(if_restore_point >= 0)
              PTMSI("host = restoreGraph(host, %d);\n", indent, if_restore_point);
           PTMSI("/* Then Branch */\n", indent);
           PTMSI("if(success)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.then_stmt, 
                               context, restore_point, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("/* Else Branch */\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.else_stmt, 
                               context, restore_point, indent + 3);
           PTMSI("}\n", indent);
           if(context == IF_BODY || context == TRY_BODY) 
              PTMSI("break;\n", indent);
           break;
      }
      case TRY_STATEMENT:
      {
           int try_restore_point = statement->value.cond_branch.restore_point;
           PTMSI("/* Try Statement */\n", indent);
           PTMSI("/* Condition */\n", indent);
           PTMSI("do\n", indent);
           PTMSI("{\n", indent);
           if(statement->value.cond_branch.copy_point && try_restore_point >= 0)
              PTMSI("copyGraph(host, %d);\n", indent + 3, try_restore_point);
           generateProgramCode(statement->value.cond_branch.condition,
                               TRY_BODY, try_restore_point, indent + 3);
           /* In a try statement, only the else command sequence is executed on
            * the host graph state before the execution of the condition. If
            * the condition succeeds, the graph resulting from the execution
            * of the condition is the new working graph for the then branch.
            * Hence the restoreGraph code is emitted within the else branch of
            * the generated code. */
           PTMSI("} while(false);\n\n", indent);
           PTMSI("/* Then Branch */\n", indent);
           PTMSI("if(success)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.then_stmt, 
                               context, restore_point, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("/* Else Branch */\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           if(try_restore_point >= 0)
              PTMSI("host = restoreGraph(host, %d);\n", indent + 3, try_restore_point);
           generateProgramCode(statement->value.cond_branch.else_stmt,
                               context, restore_point, indent + 3);
           PTMSI("}\n\n", indent);
           if(context == IF_BODY || context == TRY_BODY) 
              PTMSI("break;\n", indent);
           break;
      }
      case ALAP_STATEMENT:
      {
           int loop_restore_point = statement->value.loop_stmt.restore_point;
           PTMSI("/* Loop Statement */\n", indent);
           PTMSI("while(success)\n", indent);
           PTMSI("{\n", indent);
           if(statement->value.loop_stmt.copy_point && loop_restore_point >= 0)
              PTMSI("copyGraph(host, %d);\n", indent + 3, loop_restore_point);
           generateProgramCode(statement->value.loop_stmt.loop_body, LOOP_BODY, 
                               loop_restore_point, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("success = true;\n", indent);
           break;
      }
      case PROGRAM_OR:

           /* Emit code to nondeterministically choose between the two 
            * subprograms. */
           PTMSI("/* OR Statement */\n", indent);
           PTMSI("int random = rand();\n", indent);
           PTMSI("if((random %% 2) == 0)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.or_stmt.left_stmt, context, 
                               restore_point, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.or_stmt.right_stmt, context, 
                               restore_point, indent + 3);
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
           generateFailureCode(NULL, context, restore_point, indent);
           break;
           
      default: print_to_log("Error (generateProgramCode): Unexpected "
                            "statement type %d at AST node %d\n", 
                            statement->statement_type,
                            statement->node_id);
           break;
   }
}

void generateRuleCall(string rule_name, bool empty_lhs, bool predicate,
                      ContextType context, int restore_point, bool copy_point,
                      bool in_rule_set, int indent)
{
   bool copy_graph = restore_point >= 0 && copy_point;
   if(empty_lhs)
   {
      if(copy_graph) PTMSI("copyGraph(host, %d);\n", indent, restore_point);
      PTMSI("apply%s();\n", indent, rule_name);
      PTMSI("success = true;\n\n", indent);
   }
   else
   {
      PTMSI("morphism = match%s();\n", indent, rule_name);
      PTMSI("if(morphism != NULL)\n", indent);
      PTMSI("{\n", indent);
      if(copy_graph) PTMSI("copyGraph(host, %d);\n", indent + 3, restore_point);
      if(predicate) PTMSI("freeMorphism(morphism);\n", indent + 3);
      else
      {
         /* Optimisation: Don't have to apply the last rule in an if condition.
         * However, finding such rules is non-trivial. The condition below
         * suffices for contexts where applying the rule is incorrect, namely
         * where there is no restore point and hence the graph is not copied
         * in the GP2 if condition. */
         if(context != IF_BODY || restore_point >= 0)
            PTMSI("apply%s(morphism);\n", indent + 3, rule_name);
         else PTMSI("freeMorphism(morphism);\n", indent + 3);
      }
      PTMSI("success = true;\n", indent + 3);
      if(in_rule_set) PTMSI("break;\n", indent + 3);
      PTMSI("}\n", indent);
      /* No failure code in a rule set: the generated code follows through to the
      * next rule in the set. This flag is not set for the last rule in a set. */
      if(!in_rule_set)
      {
         PTMSI("else\n", indent);
         PTMSI("{\n", indent);
         /* Do not restore the graph on a loop if this is a copy point, otherwise 
         * the working graph will be the graph before this rule was applied on the
         * previous iteration, effectively neglecting an entire loop iteration. */
         if(context == LOOP_BODY && copy_graph) 
            PTMSI("success = false;\n", indent + 3);
         else generateFailureCode(rule_name, context, restore_point, indent + 3);
         PTMSI("}\n", indent);   
      }
   }
}

void generateRuleSetCall(List *rules, ContextType context, int restore_point, 
                         int indent)
{
   PTMSI("do\n", indent);
   PTMSI("{\n", indent);
   while(rules != NULL)
   {  
      string rule_name = rules->value.rule_call.rule_name;
      bool empty_lhs = rules->value.rule_call.rule->empty_lhs;
      bool predicate = rules->value.rule_call.rule->is_predicate;
      if(rules->next == NULL)
           generateRuleCall(rule_name, empty_lhs, predicate, context,
                            restore_point, rules->value.rule_call.copy_point, 
                            false, indent + 3);
      else generateRuleCall(rule_name, empty_lhs, predicate, context, 
                            restore_point, rules->value.rule_call.copy_point, 
                            true, indent + 3);
      rules = rules->next;
   }
   PTMSI("} while(false);\n", indent);
}

void generateFailureCode(string rule_name, ContextType context, int restore_point, int indent)
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
      PTMSI("closeLogFile();\n", indent);
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
   if(context == LOOP_BODY && restore_point >= 0) 
      PTMSI("host = restoreGraph(host, %d);\n", indent, restore_point);
}


