#include "genProgram.h"

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
   PTMS("#define DEBUG\n\n");
   generateMorphismCode(declarations, 'd');
   PTMS("Graph *host = NULL;\n"
        "bool success = true;\n\n"
        "void freeMorphisms(void);\n\n");
   generateDeclarationCode(declarations);
   fclose(main_source);
}

void generateDeclarationCode(List *declarations)
{
   PTMS("int main(void)\n"
        "{\n"               
        "   srand(time(NULL));\n"
        "   openLogFile();\n"
        "   host = makeHostGraph();\n\n");
   /* Debug code 
   PTMS("   printGraph(host);\n\n"); */
   generateMorphismCode(declarations, 'm');
   List *iterator = declarations;
   while(iterator != NULL)
   {
      GPDeclaration *decl = iterator->value.declaration;
     
      switch(decl->decl_type)
      {
         /* This case should be reached exactly once, including recursive calls.
          * The semantic analysis ensures that the passed AST contains exactly one
          * MAIN_DECLARATION node. */
         case MAIN_DECLARATION:
              generateProgramCode(decl->value.main_program, MAIN_BODY, -1, false, 3);
              break;

         case PROCEDURE_DECLARATION:
              if(decl->value.procedure->local_decls != NULL)
                 generateDeclarationCode(decl->value.procedure->local_decls);
              break;

         case RULE_DECLARATION:
              break;

         default: print_to_log("Error (generateDeclarationCode): Unexpected "
                               "declaration type %d at AST node %d\n", 
                               decl->decl_type, decl->node_id);
              break;
      }
      iterator = iterator->next;
   }
   PTMS("   printGraph(host);\n"
        "   freeGraph(host);\n"
        "   freeMorphisms();\n"
        "   freeGraphStack(graph_stack);\n"
        "   freeGraphChangeStack(graph_change_stack);\n"
        "   closeLogFile();\n\n"
        "   return 0;\n"
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
      GPDeclaration *decl = declarations->value.declaration;
      switch(decl->decl_type)
      {
         case MAIN_DECLARATION:
              break;

         case PROCEDURE_DECLARATION:
              if(decl->value.procedure->local_decls != NULL)
                 generateMorphismCode(decl->value.procedure->local_decls, type);
              break;

         case RULE_DECLARATION:
         {
              GPRule *rule = decl->value.rule;
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
         default: print_to_log("Error (generateMorphismCode): Unexpected "
                               "declaration type %d at AST node %d\n", 
                               decl->decl_type, decl->node_id);
              break;
      }
      declarations = declarations->next;
   }
   if(type == 'd' || type == 'm') PTMS("\n");
}


void generateProgramCode(GPStatement *statement, ContextType context, 
                         int restore_point, int roll_back_point, int indent)
{
   switch(statement->statement_type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = statement->value.commands;
           while(commands != NULL)
           {
              generateProgramCode(commands->value.command, context, restore_point,
                                  roll_back_point, indent);
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
                            context, restore_point, roll_back_point, false, indent);
           break;

      case RULE_SET_CALL:

           PTMSI("/* Rule Set Call */\n", indent);
           generateRuleSetCall(statement->value.rule_set, context, restore_point,
                               roll_back_point, indent);
           break;

      case PROCEDURE_CALL:
      {
           GPProcedure *procedure = statement->value.proc_call.procedure;
           generateProgramCode(procedure->commands, context, restore_point,
                               roll_back_point, indent);
           break;
      }

      case IF_STATEMENT:
      {
           int if_restore_point = statement->value.cond_branch.restore_point;
           int if_roll_back_point = statement->value.cond_branch.roll_back_point;
           PTMSI("/* If Statement */\n", indent);
           PTMSI("/* Condition */\n", indent);
           PTMSI("do\n", indent);
           PTMSI("{\n", indent);
           if(if_restore_point >= 0) PTMSI("copyGraph(host);\n", indent + 3);
           generateProgramCode(statement->value.cond_branch.condition, IF_BODY,
                               if_restore_point, if_roll_back_point, indent + 3);
           PTMSI("} while(false);\n\n", indent);
           if(if_restore_point >= 0) 
              PTMSI("host = restoreGraph(host, %d);\n", indent, if_restore_point);
           if(if_roll_back_point >= 0)
              PTMSI("rollBackGraph(host, %d);\n", indent, if_roll_back_point);
           PTMSI("/* Then Branch */\n", indent);
           PTMSI("if(success)\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.then_stmt, 
                               context, restore_point, roll_back_point, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("/* Else Branch */\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.cond_branch.else_stmt, 
                               context, restore_point, roll_back_point, indent + 3);
           PTMSI("}\n", indent);
           if(context == IF_BODY || context == TRY_BODY) 
              PTMSI("break;\n", indent);
           break;
      }
      case TRY_STATEMENT:
      {
           int try_restore_point = statement->value.cond_branch.restore_point;
           int try_roll_back_point = statement->value.cond_branch.roll_back_point;
           PTMSI("/* Try Statement */\n", indent);
           PTMSI("/* Condition */\n", indent);
           PTMSI("do\n", indent);
           PTMSI("{\n", indent);
           if(try_restore_point >= 0) PTMSI("copyGraph(host);\n", indent + 3);
           generateProgramCode(statement->value.cond_branch.condition, TRY_BODY,
                               try_restore_point, try_roll_back_point, indent + 3);
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
                               context, restore_point, roll_back_point, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("/* Else Branch */\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           if(try_restore_point >= 0) 
              PTMSI("host = restoreGraph(host, %d);\n", indent + 3, try_restore_point);
           if(try_roll_back_point >= 0)
              PTMSI("rollBackGraph(host, %d);\n", indent + 3, try_roll_back_point);
           generateProgramCode(statement->value.cond_branch.else_stmt,
                               context, restore_point, roll_back_point, indent + 3);
           PTMSI("}\n\n", indent);
           if(context == IF_BODY || context == TRY_BODY) 
              PTMSI("break;\n", indent);
           break;
      }
      case ALAP_STATEMENT:
      {
           int loop_restore_point = statement->value.loop_stmt.restore_point;
           int loop_roll_back_point = statement->value.loop_stmt.roll_back_point;
           PTMSI("/* Loop Statement */\n", indent);
           PTMSI("success = true;\n", indent);
           PTMSI("while(success)\n", indent);
           PTMSI("{\n", indent);
           if(loop_restore_point >= 0) 
           {
              PTMSI("host = restoreGraph(host, %d);\n", indent + 3, 
                    loop_restore_point);
              PTMSI("copyGraph(host);\n", indent + 3);
           }
           if(loop_roll_back_point >= 0)
              PTMSI("discardChanges(%d);\n", indent + 3, loop_roll_back_point);
           else 
           {
              if(statement->value.loop_stmt.stop_recording) loop_roll_back_point = -1;
           }
           generateProgramCode(statement->value.loop_stmt.loop_body, LOOP_BODY,
                               loop_restore_point, loop_roll_back_point, indent + 3);
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
                               restore_point, roll_back_point, indent + 3);
           PTMSI("}\n", indent);
           PTMSI("else\n", indent);
           PTMSI("{\n", indent);
           generateProgramCode(statement->value.or_stmt.right_stmt, context, 
                               restore_point, roll_back_point, indent + 3);
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
           generateFailureCode(NULL, context, restore_point, roll_back_point,
                               indent);
           break;
           
      default: print_to_log("Error (generateProgramCode): Unexpected "
                            "statement type %d at AST node %d\n", 
                            statement->statement_type,
                            statement->node_id);
           break;
   }
}

void generateRuleCall(string rule_name, bool empty_lhs, bool predicate,
                      ContextType context, int restore_point,
                      int roll_back_point, bool in_rule_set, int indent)
{
   if(empty_lhs)
   {
      PTMSI("#ifdef DEBUG\n", indent);
      PTMSI("print_to_console(\"Matched %s.\\n\");\n", indent + 3, rule_name);
      PTMSI("#endif\n", indent);
      PTMSI("apply%s(%d);\n", indent, rule_name, roll_back_point >= 0);
      PTMSI("success = true;\n\n", indent);
   }
   else
   {
      PTMSI("if(match%s(M_%s))\n", indent, rule_name, rule_name);
      PTMSI("{\n", indent);
      PTMSI("#ifdef DEBUG\n", indent + 3);
      PTMSI("print_to_console(\"Matched %s.\\n\");\n", indent + 6, rule_name);
      PTMSI("#endif\n", indent + 3);
      if(!predicate)
      {
         /* Optimisation: Don't have to apply the last rule in an if condition.
          * However, finding such rules is non-trivial. The condition below
          * suffices for contexts where applying the rule is incorrect, namely
          * where there is no restore point and hence the graph is not copied
          * in the GP2 if condition. */
         if(context != IF_BODY || restore_point >= 0)
            PTMSI("apply%s(M_%s, %d);\n", indent + 3, rule_name, rule_name,
                  roll_back_point >= 0);
      }
      else PTMSI("clearMorphism(M_%s);\n", indent + 3, rule_name);
      PTMSI("success = true;\n", indent + 3);
      if(in_rule_set) PTMSI("break;\n", indent + 3);
      PTMSI("}\n", indent);
      /* No failure code in a rule set: the generated code follows through to the
       * next rule in the set. This flag is not set for the last rule in a set. */
      if(!in_rule_set)
      {
         PTMSI("else\n", indent);
         PTMSI("{\n", indent);
         generateFailureCode(rule_name, context, restore_point, 
                             roll_back_point, indent + 3);
         PTMSI("}\n", indent);   
      }
   }
}

void generateRuleSetCall(List *rules, ContextType context, int restore_point, 
                         int roll_back_point, int indent)
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
                            restore_point, roll_back_point, false, indent + 3);
      else generateRuleCall(rule_name, empty_lhs, predicate, context, 
                            restore_point, roll_back_point, true, indent + 3);
      rules = rules->next;
   }
   PTMSI("} while(false);\n", indent);
}

void generateFailureCode(string rule_name, ContextType context, 
                         int restore_point, int roll_back_point, int indent)
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
      PTMSI("freeMorphisms();\n", indent);
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
   if(context == LOOP_BODY) 
   {
      if(restore_point >= 0)
         PTMSI("host = restoreGraph(host, %d);\n", indent, restore_point);
      if(roll_back_point >= 0) 
         PTMSI("rollBackGraph(host, %d);\n", indent, roll_back_point);
   }
}


