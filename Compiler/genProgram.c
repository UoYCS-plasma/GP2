#include "genProgram.h"

#define RULE_TRACE
#define GRAPH_TRACE
#define BACKTRACK_TRACE

static FILE *file = NULL;

/* At compile time, the AST is annotated with restore points and roll back flags
 * signalling that the current host graph needs to be retained while executing
 * a particular program fragment.
 *
 * There are two ways in which the host graph is retained. 
 * (1) The host graph is copied and pushed onto the graph stack. This copy is
 *     performed once per restore point, so it is known at compile time the
 *     stack depth from which the host graph is retrieved if necessary. 
 * (2) The changes made to the host graph are recorded during execution of the
 *     program fragment. The amount of changes that need to be rolled back cannot be
 *     determined at compile time, so variables to store undo points are defined
 *     at runtime. The global variable defined below is incremented when one of
 *     these variables is generated to ensure that the runtime system has a
 *     unique variable identifier for each undo point. */
int undo_point_count = 0;

/* The contexts of a GP2 program determine the code that is generated. In
 * particular, the code generated when a rule match fails is determined by
 * its context. The context also has some impact on graph copying. */
typedef enum {MAIN_BODY, IF_BODY, TRY_BODY, LOOP_BODY} ContextType;

/* Structure containing data to pass between code generation functions.
 * context - The context of the current command.
 * undo_point - A non-negative integer if the command is part of a command
 *              sequence that is recording host graph changes and -1 otherwise.
 *              Its value is assigned the value of the global undo_point_count
 *              if a command's roll_back flag is set. The count is incremented
 *              when assigned to ensure unique undo point names at runtime.
 * stop_recording - True if a recorded command sequence has passed a loop
 *                  with its stop_recording flag set.
 * restore_point - The restore point of a conditional branch or a loop.
 * indent - For formatting the printed C code. */
 typedef struct CommandData {
   ContextType context;
   int undo_point;
   bool stop_recording;
   int restore_point;
   int indent;
} CommandData;


static void generateMorphismCode(List *declarations, char type);
static void generateProgramCode(GPCommand *command, CommandData data);
static void generateRuleCall(string rule_name, bool empty_lhs, bool predicate,
                             bool last_rule, CommandData data);
static void generateBranchStatement(GPCommand *command, CommandData data);
static void generateLoopStatement(GPCommand *command, CommandData data);
static void generateFailureCode(string rule_name, CommandData data);

void generateRuntimeMain(List *declarations)
{
   file = fopen("runtime/main.c", "w");
   if(file == NULL) { 
     perror("runtime/main.c");
     exit(1);
   }

   PTF("#include <time.h>\n");
   PTF("#include \"../error.h\"\n");
   PTF("#include \"../debug.h\"\n");
   PTF("#include \"../graph.h\"\n");
   PTF("#include \"../graphStacks.h\"\n");
   PTF("#include \"buildHost.h\"\n");
   PTF("#include \"host/host.h\"\n");
   PTF("#include \"morphism.h\"\n\n");

   /* Declare the global morphism variables for each rule. */
   generateMorphismCode(declarations, 'd');

   /* Declare the runtime global variables and functions. */
   PTF("Graph *host = NULL;\n");
   PTF("bool success = true;\n\n");
   PTF("static void garbageCollect(void);\n");
   PTF("static void freeMorphisms(void);\n\n");

   /* Open the runtime's main function and set up the execution environment. */
   PTF("int main(void)\n{\n");
   PTFI("srand(time(NULL));\n", 3);
  
   #if defined GRAPH_TRACE || defined RULE_TRACE || defined BACKTRACK_TRACE
      PTFI("openTraceFile(\"../gp2.trace\");\n", 3);
   #endif

   PTFI("host = buildHostGraph();\n\n", 3);

   #ifdef GRAPH_TRACE
      PTFI("print_trace(\"Start Graph: \\n\");\n", 3);
      PTFI("printGraph(host, trace_file);\n\n", 3);
   #endif
 
   /* Print the calls to allocate memory for each morphism. */
   generateMorphismCode(declarations, 'm');

   /* Find the main declaration and generate code from its command sequence. */
   List *iterator = declarations;
   while(iterator != NULL)
   {
      GPDeclaration *decl = iterator->declaration;
      if(decl->type == MAIN_DECLARATION)
      {
         CommandData initialData = {MAIN_BODY, -1, false, -1, 3}; 
         generateProgramCode(decl->main_program, initialData);
      }
      iterator = iterator->next;
   }

   /* Print the clean-up phase, including the garbage collection functions. */
   PTF("   /* Output Graph. */\n");
   PTF("   printGraph(host, stdout);\n");
   PTF("   garbageCollect();\n");
   PTF("   return 0;\n");
   PTF("}\n\n");

   PTF("void garbageCollect(void)\n");
   PTF("{\n");
   PTF("   freeGraph(host);\n");
   PTF("   freeMorphisms();\n");
   PTF("   freeGraphStack();\n");
   PTF("   freeGraphChangeStack();\n");
   #if defined GRAPH_TRACE || defined RULE_TRACE || defined BACKTRACK_TRACE
      PTF("   closeTraceFile();\n");
   #endif
   PTF("}\n\n");

   generateMorphismCode(declarations, 'f');
   fclose(file);
}

/* For each rule declaration, generate code to handle the morphism variables at
 * runtime. The variables are named M_<rule_name>. This function is called three
 * times with different 'type' arguments:
 *
 * Type (d)eclarations switches on the printing of the declaration of the global 
 * morphism variables and the include directives to the <rule_name>.h headers.
 * This is called before the definition of the main function is printed.
 *
 * Type (m)akeMorphism switches on the printing of the definition and allocation 
 * of the morphism structures by the makeMorphism function. At runtime this is done
 * at the start of the main function. Data from the rule declaration is used to print 
 * the correct arguments for calls to makeMorphism. 
 *
 * Type (f)reeMorphism switches on the printing of the freeMorphisms function.
 * For each rule declaration, a call to freeMorphism is printed. */

static void generateMorphismCode(List *declarations, char type)
{
   assert(type == 'm' || type == 'f' || type == 'd');
   if(type == 'f') PTF("void freeMorphisms(void)\n{\n");
   while(declarations != NULL)
   {
      GPDeclaration *decl = declarations->declaration;
      switch(decl->type)
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
                 PTFI("M_%s = makeMorphism(%d, %d, %d);\n", 3, rule->name, 
                      rule->left_nodes, rule->left_edges, rule->variable_count);
              if(type == 'f')
                 PTFI("freeMorphism(M_%s);\n", 3, rule->name);
              break;
         }
         default: 
              print_to_log("Error (generateMorphismCode): Unexpected "
                           "declaration type %d at AST node %d\n", 
                           decl->type, decl->id);
              break;
      }
      declarations = declarations->next;
   }
   if(type == 'd' || type == 'm') PTF("\n");
   else PTF("}\n");
}


static void generateProgramCode(GPCommand *command, CommandData data)
{
   switch(command->type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = command->commands;
           CommandData new_data = data;
           while(commands != NULL)
           {
              GPCommand *command = commands->command;
              if(command->type == ALAP_STATEMENT && command->loop_stmt.stop_recording)
                 new_data.stop_recording = true;
              generateProgramCode(command, new_data);
              /* TODO: is this necessary? */
              if(data.context == LOOP_BODY && commands->next != NULL)
                 PTFI("if(!success) break;\n", data.indent);             
              commands = commands->next;
           }           
           break;
      }
      case RULE_CALL:
           PTFI("/* Rule Call */\n", data.indent);
           generateRuleCall(command->rule_call.rule_name, command->rule_call.rule->empty_lhs,
                            command->rule_call.rule->is_predicate, true, data);
           break;

      case RULE_SET_CALL:
      {
           PTFI("/* Rule Set Call */\n", data.indent);
           PTFI("do\n", data.indent);
           PTFI("{\n", data.indent);
           CommandData new_data = data;
           new_data.indent = data.indent + 3;
           List *rules = command->rule_set;
           while(rules != NULL)
           {  
              string rule_name = rules->rule_call.rule_name;
              bool empty_lhs = rules->rule_call.rule->empty_lhs;
              bool predicate = rules->rule_call.rule->is_predicate;
              generateRuleCall(rule_name, empty_lhs, predicate, rules->next == NULL, new_data);
              rules = rules->next;
           }
           PTFI("} while(false);\n", data.indent);
           break;
      }
      case PROCEDURE_CALL:
      {
           GPProcedure *procedure = command->proc_call.procedure;
           generateProgramCode(procedure->commands, data);
           break;
      }
      case IF_STATEMENT:
      case TRY_STATEMENT:
           generateBranchStatement(command, data);
           break;

      case ALAP_STATEMENT:
           generateLoopStatement(command, data);
           break;

      case PROGRAM_OR:
      {
           /* Emit code to nondeterministically choose between the two subprograms. */
           CommandData new_data = data;
           new_data.indent = data.indent + 3;
           PTFI("/* OR Statement */\n", data.indent);
           PTFI("int random = rand();\n", data.indent);
           PTFI("if((random %% 2) == 0)\n", data.indent);
           PTFI("{\n", data.indent);
           generateProgramCode(command->or_stmt.left_command, new_data);
           PTFI("}\n", data.indent);
           PTFI("else\n", data.indent);
           PTFI("{\n", data.indent);
           generateProgramCode(command->or_stmt.right_command, new_data);
           PTFI("}\n", data.indent);
           if(data.context == IF_BODY || data.context == TRY_BODY) 
              PTFI("break;\n", data.indent);
           break;
      }
      case SKIP_STATEMENT:
           PTFI("/* Skip Statement */\n", data.indent);
           PTFI("success = true;\n", data.indent);
           break;
           
      case FAIL_STATEMENT:
           PTFI("/* Fail Statement */\n", data.indent);
           generateFailureCode(NULL, data);
           break;

      case BREAK_STATEMENT:
           PTFI("/* Break Statement */\n", data.indent);
           if(data.restore_point >= 0)
           {
              PTFI("/* The graph copy is no longer needed. */\n", data.indent);
              #ifdef BACKTRACK_TRACE
                 PTFI("print_trace(\"(%d) Discarding graphs.\\n\\n\");\n",
                      data.indent + 3, data.restore_point);
              #endif
              PTFI("discardGraphs(%d);\n", data.indent, data.restore_point);
           }
           if(data.undo_point >= 0)
           {
              PTFI("/* Graph changes from loop body not required.\n", data.indent);
              PTFI("   Discard them so that future graph roll backs are uncorrupted",
                   data.indent);
              #ifdef BACKTRACK_TRACE
                 PTFI("print_trace(\"(%d) Discarding graph changes.\\n\\n\");\n",
                      data.indent + 3, data.undo_point);
              #endif
              PTFI("discardChanges(undo_point%d);\n", data.indent, data.undo_point);
           }
           PTFI("break;\n", data.indent);
           break;
           
      default: 
           print_to_log("Error (generateProgramCode): Unexpected command type "
                        "%d at AST node %d\n", command->type, command->id);
           break;
   }
}

/* That's a lot of arguments! What do they achieve?
 * rule_name: Used to print variables and functions named after their rule,
 *            specifically the morphism, the rule matching function and the
 *            rule application function.
 * empty_lhs: If this flag is set, only the call to the rule application
 *            function is printed. 
 * predicate: If this flag is set, code to apply the rule is not generated.
 * last_rule: Set if this is the last rule in a rule set call. Controls the
 *            generation of failure code. 
 * data:      CommandData passed from the calling command. */
static void generateRuleCall(string rule_name, bool empty_lhs, bool predicate,
                             bool last_rule, CommandData data)
{
   if(empty_lhs)
   {
      #ifdef RULE_TRACE
         PTFI("print_trace(\"Matched %s. (empty rule)\\n\\n\");\n", 
              data.indent, rule_name);
      #endif
      if(predicate) return;
      if(data.undo_point >= 0 && !data.stop_recording) 
         PTFI("apply%s(true);\n", data.indent, rule_name);
      else PTFI("apply%s(false);\n", data.indent, rule_name);
      #ifdef GRAPH_TRACE
         PTFI("print_trace(\"Graph after applying rule %s:\\n\");\n",
              data.indent, rule_name);
         PTFI("printGraph(host, trace_file);\n\n", data.indent);
      #endif
      PTFI("success = true;\n\n", data.indent);
   }
   else
   {
      #ifdef RULE_TRACE
         PTFI("print_trace(\"Matching %s...\\n\");\n", data.indent, rule_name);
      #endif
      PTFI("if(match%s(M_%s))\n", data.indent, rule_name, rule_name);
      PTFI("{\n", data.indent);
      #ifdef RULE_TRACE
         PTFI("print_trace(\"Matched %s.\\n\\n\");\n", data.indent + 3, rule_name);
      #endif
      if(!predicate)
      {
         /* Optimisation: Don't have to apply the last rule in an if condition.
          * However, finding such rules is non-trivial. The condition below
          * suffices for contexts where applying the rule is incorrect, namely
          * where there is no restore point and hence the graph is not copied
          * in the GP2 if condition. */
         if(data.context != IF_BODY || data.restore_point >= 0)
         { 
            if(data.undo_point >= 0 && !data.stop_recording) 
                 PTFI("apply%s(M_%s, true);\n", data.indent + 3, rule_name, rule_name);
            else PTFI("apply%s(M_%s, false);\n", data.indent + 3, rule_name, rule_name);
            #ifdef GRAPH_TRACE
               PTFI("print_trace(\"Graph after applying rule %s:\\n\");\n",
                    data.indent + 3, rule_name);
               PTFI("printGraph(host, trace_file);\n\n", data.indent + 3);
            #endif
         }
      }
      else PTFI("initialiseMorphism(M_%s);\n", data.indent + 3, rule_name);
      PTFI("success = true;\n", data.indent + 3);
      /* If this rule call is within a rule set, and it is not the last rule in that
       * set, print a break statement to exit the containing do-while loop of the rule
       * set call. */
      if(!last_rule) PTFI("break;\n", data.indent + 3);
      PTFI("}\n", data.indent);
      /* Only generate failure code if the last rule in the set fails. */ 
      if(last_rule)
      {
         PTFI("else\n", data.indent);
         PTFI("{\n", data.indent);
         #ifdef RULE_TRACE
            PTFI("print_trace(\"Failed to match %s.\\n\\n\");\n",
                 data.indent + 3, rule_name);
         #endif
         CommandData new_data = data;
         new_data.indent = data.indent + 3;
         generateFailureCode(rule_name, new_data);
         PTFI("}\n", data.indent);  
      }
      else 
      {
         #ifdef RULE_TRACE
            PTFI("else print_trace(\"Failed to match %s.\\n\\n\");\n",
                 data.indent, rule_name);
         #endif
      }
   }
}

/* generateBranchStatement passes on the command data passed by the caller to
 * the calls to generate code for the then and else branches.
 * The flags from the GPCommand structure are used onlt to generate code for
 * the condition subprogram . */
static void generateBranchStatement(GPCommand *command, CommandData data)
{
   CommandData condition_data = data;
   /* If restore_point != -1, then the host graph is copied before the conditional
    * subprogram is executed. If the roll_back flag is set, then changes to the host
    * graph are recorded during the execution of the conditional subprogram, marked
    * by setting the undo_point to a non-negative value. These two events are mutually 
    * exclusive. */
   condition_data.context = command->type == IF_STATEMENT ? IF_BODY : TRY_BODY;
   if(command->cond_branch.roll_back) condition_data.undo_point = undo_point_count++;
   else condition_data.undo_point = -1;
   condition_data.restore_point = command->cond_branch.restore_point;
   assert(condition_data.restore_point == -1 || condition_data.undo_point == -1);
   condition_data.indent = data.indent + 3;

   if(condition_data.context == IF_BODY) PTFI("/* If Statement */\n", data.indent);
   else PTFI("/* Try Statement */\n", data.indent);
   PTFI("/* Condition */\n", data.indent);
   if(condition_data.undo_point >= 0)
   {
      #ifdef BACKTRACK_TRACE
         PTFI("print_trace(\"(%d) Recording graph changes.\\n\\n\");\n",
              data.indent, condition_data.undo_point);
      #endif
      PTFI("int undo_point%d = graph_change_index;\n", data.indent, 
           condition_data.undo_point);
   }
   PTFI("do\n", data.indent);
   PTFI("{\n", data.indent);
   if(condition_data.restore_point >= 0) 
   {
      #ifdef BACKTRACK_TRACE
         PTFI("print_trace(\"(%d) Copying host graph.\\n\\n\");\n",
              data.indent + 3, condition_data.restore_point);
      #endif
      PTFI("copyGraph(host);\n", data.indent + 3);
   }
   generateProgramCode(command->cond_branch.condition, condition_data);

   PTFI("} while(false);\n\n", data.indent);
   if(condition_data.context == IF_BODY)
   {
      if(condition_data.restore_point >= 0) 
      {
         PTFI("host = popGraphs(host, %d);\n", data.indent,
              condition_data.restore_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_trace(\"(%d) Restoring graph.\\n\\n\");\n",
                 data.indent, condition_data.restore_point);
         #endif
         #ifdef GRAPH_TRACE
            PTFI("print_trace(\"Restored graph:\\n\");\n", data.indent);
            PTFI("printGraph(host, trace_file);\n", data.indent);
         #endif
      }
      if(condition_data.undo_point >= 0)
      {
         PTFI("undoChanges(host, undo_point%d);\n", data.indent, 
              condition_data.undo_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_trace(\"(%d) Undoing graph changes.\\n\\n\");\n",
                 data.indent, condition_data.undo_point);
         #endif
         #ifdef GRAPH_TRACE
            PTFI("print_trace(\"Restored graph:\\n\");\n", data.indent);
            PTFI("printGraph(host, trace_file);\n", data.indent);
         #endif
      }
   }
   CommandData new_data = data;
   new_data.indent = data.indent + 3;
   PTFI("/* Then Branch */\n", data.indent);
   PTFI("if(success)\n", data.indent);
   PTFI("{\n", data.indent);
   generateProgramCode(command->cond_branch.then_command, new_data);
   PTFI("}\n", data.indent);
   PTFI("/* Else Branch */\n", data.indent);
   PTFI("else\n", data.indent);
   PTFI("{\n", data.indent);
   if(condition_data.context == TRY_BODY)
   {
      if(condition_data.restore_point >= 0) 
      {
         PTFI("host = popGraphs(host, %d);\n", data.indent + 3, 
              condition_data.restore_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_trace(\"(%d) Restoring graph.\\n\\n\");\n",
                 data.indent + 3, condition_data.restore_point);
         #endif
         #ifdef GRAPH_TRACE
            PTFI("print_trace(\"Restored graph:\\n\");\n", data.indent + 3);
            PTFI("printGraph(host, trace_file);\n", data.indent + 3);
         #endif
      }
      if(condition_data.undo_point >= 0)
      {
         PTFI("undoChanges(host, undo_point%d);\n", data.indent + 3, 
              condition_data.undo_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_trace(\"(%d) Undoing graph changes.\\n\\n\");\n",
                 data.indent + 3, condition_data.undo_point);
         #endif
         #ifdef GRAPH_TRACE
            PTFI("print_trace(\"Restored graph:\\n\");\n", data.indent + 3);
            PTFI("printGraph(host, trace_file);\n", data.indent + 3);
         #endif
      }
   }
   generateProgramCode(command->cond_branch.else_command, new_data);
   PTFI("}\n", data.indent);
   if(data.context == IF_BODY || data.context == TRY_BODY) PTFI("break;\n", data.indent);
   return;
}

void generateLoopStatement(GPCommand *command, CommandData data)
{
   CommandData loop_data = data;
   /* If restore_point != -1, then the host graph is copied before the conditional
    * subprogram is executed. If the roll_back flag is set, then changes to the host
    * graph are recorded during the execution of the conditional subprogram. These
    * two events are mutually exclusive. */
   loop_data.context = LOOP_BODY;
   if(command->loop_stmt.roll_back) loop_data.undo_point = undo_point_count++;
   else loop_data.undo_point = -1;
   loop_data.restore_point = command->loop_stmt.restore_point;
   assert(loop_data.restore_point == -1 || loop_data.undo_point == -1);
   loop_data.indent = data.indent + 3;

   PTFI("/* Loop Statement */\n", data.indent);
   PTFI("success = true;\n", data.indent);
   if(loop_data.undo_point >= 0)
   {
      #ifdef BACKTRACK_TRACE
         PTFI("print_trace(\"(%d) Recording graph changes.\\n\\n\");\n",
              data.indent, loop_data.undo_point);
      #endif
      PTFI("int undo_point%d = graph_change_index;\n", 
          data.indent, loop_data.undo_point);
   }
   PTFI("while(success)\n", data.indent);
   PTFI("{\n", data.indent);
   if(loop_data.restore_point >= 0) 
   {
      #ifdef BACKTRACK_TRACE
         PTFI("print_trace(\"(%d) Copying host graph.\\n\\n\");\n",
              data.indent + 3, loop_data.restore_point);
      #endif
      PTFI("copyGraph(host);\n", data.indent + 3);
   }
   if(loop_data.restore_point >= 0)
   {
      #ifdef BACKTRACK_TRACE
         PTFI("print_trace(\"(%d) Recording graph changes.\\n\\n\");\n",
              data.indent + 3, loop_data.undo_point);
      #endif
   }
   generateProgramCode(command->loop_stmt.loop_body, loop_data);
   if(loop_data.restore_point >= 0)
   {
      PTFI("/* If the body has succeeded, the graph copy is no longer needed. */\n",
           data.indent + 3);
      #ifdef BACKTRACK_TRACE
         PTFI("print_trace(\"(%d) Discarding graphs.\\n\\n\");\n",
              data.indent + 3, loop_data.restore_point);
      #endif
      PTFI("if(success) discardGraphs(%d);\n", data.indent + 3, loop_data.restore_point);
   }
   if(loop_data.restore_point >= 0)
   {
      PTFI("/* Graph changes from loop body may not have been used.\n", 
           data.indent + 3);
      PTFI("   Discard them so that future graph roll backs are uncorrupted. */\n",
           data.indent + 3);
      #ifdef BACKTRACK_TRACE
         PTFI("print_trace(\"(%d) Discarding graph changes.\\n\\n\");\n",
              data.indent + 3, loop_data.undo_point);
      #endif
      PTFI("if(success) discardChanges(undo_point%d);\n", 
           data.indent + 3, loop_data.undo_point);
   }
   PTFI("}\n", data.indent);
   PTFI("success = true;\n", data.indent);
}

/* Generates code to handle failure, which is context-dependent. There are two
 * kinds of failure: 
 *
 * (1) A rule fails to match. The name of the rule is passed as the first 
 *     argument. 
 * (2) The fail statement is called. NULL is passed as the first argument. */

static void generateFailureCode(string rule_name, CommandData data)
{
   /* A failure in the main body ends the execution. Emit code to report the 
    * failure, garbage collect and return 0. */
   if(data.context == MAIN_BODY)
   {
      #ifdef GRAPH_TRACE
         PTFI("print_trace(\"Program failed. Final graph:\\n\");\n", data.indent);
         PTFI("printGraph(host, trace_file);\n", data.indent);
      #endif
      if(rule_name != NULL)
         PTFI("print_to_console(\"No output graph: rule %s not "
              "applicable.\\n\");\n", data.indent, rule_name);
      else PTFI("print_to_console(\"No output graph: Fail statement "
                "invoked.\\n\");\n", data.indent);
      PTFI("garbageCollect();\n", data.indent);
      PTFI("return 0;\n", data.indent);
   }
   /* In other contexts, set the runtime success flag to false. */
   else PTFI("success = false;\n", data.indent);

   if(data.context == IF_BODY || data.context == TRY_BODY) PTFI("break;\n", data.indent);
   if(data.context == LOOP_BODY) 
   {
      if(data.restore_point >= 0)
      {
         PTFI("host = popGraphs(host, %d);\n", data.indent, data.restore_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_trace(\"(%d) Restoring host graph.\\n\\n\");\n",
                 data.indent, data.restore_point);
         #endif
         #ifdef GRAPH_TRACE
            PTFI("print_trace(\"Restored graph:\\n\");\n", data.indent);
            PTFI("printGraph(host, trace_file);\n", data.indent);
         #endif
      }
      if(data.undo_point >= 0) 
      {
         PTFI("undoChanges(host, undo_point%d);\n", data.indent, data.undo_point);
         #ifdef BACKTRACK_TRACE
            PTFI("print_trace(\"(%d) Undoing graph changes.\\n\\n\");\n",
                 data.indent, data.undo_point);
         #endif
         #ifdef GRAPH_TRACE
            PTFI("print_trace(\"Restored graph:\\n\");\n", data.indent);
            PTFI("printGraph(host, trace_file);\n", data.indent);
         #endif
      }
   }
}

