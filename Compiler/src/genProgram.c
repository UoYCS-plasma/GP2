#include "genProgram.h"

#undef RULE_TRACING
#undef GRAPH_TRACING
#undef BACKTRACK_TRACING

static FILE *file = NULL;

/* At compile time, the AST is annotated with 'roll back' flags to signal that
 * changes to the host graph are to be recorded while executing a particular
 * program fragment. See the analysis module for the implementation of this
 * annotation.
 *
 * The changes made to the host graph are recorded during execution of the
 * program fragment. The amount of changes that need to be rolled back cannot be
 * determined at compile time, so variables to store restore points (the number of
 * a frame on the graph change stack) are defined at runtime.
 * The global variable defined below is incremented when one of these variables
 * is generated to ensure that the runtime system has a unique variable identifier
 * for each restore point. */
int restore_point_count = 0;

/* The contexts of a GP2 program determine the code that is generated. In
 * particular, the code generated when a rule match fails is determined by
 * its context. The context also has some impact on graph copying. */
typedef enum {MAIN_BODY, IF_BODY, TRY_BODY, LOOP_BODY} ContextType;

/* Structure containing data to pass between code generation functions.
 * context - The context of the current command.
 * record_changes - Set to true if the command is a branch statement whose
 *                  condition requires graph recording, or if the command is
 *                  a loop whose body requires graph recording.
 * restore_point - A non-negative integer if the command is part of a command
 *                 sequence that is recording host graph changes and -1 otherwise.
 *                 Its value is assigned the value of the global restore_point_count
 *                 if a command's rrecord_changes flag is set. The count is incremented
 *                 when assigned to ensure unique restore point names at runtime.
 * indent - For formatting the printed C code. */
 typedef struct CommandData {
   ContextType context;
   bool record_changes;
   int restore_point;
   int indent;
} CommandData;

/* If the host graph contains fewer than MIN_HOST_NODE_SIZE nodes, the host
 * graph is allocated memory for that number of nodes. Similarly for edges. */
#define MIN_HOST_NODE_SIZE 128
#define MIN_HOST_EDGE_SIZE 128

/* Generates an appropriate initial node/edge array size for a graph. 
 * Returns the maximum of minimum_size and the smallest power of 2 greater 
 * than the number_of_items in the passed graph. number_of_items is obtained
 * from a call to countNodes or countEdges. */
static int getArraySize(int number_of_items, int minimum_size)
{
   if(number_of_items < minimum_size) return minimum_size;
   /* Return the smallest power of 2 greater than number_of_items. */
   number_of_items--;
   number_of_items |= number_of_items >> 1;
   number_of_items |= number_of_items >> 2;
   number_of_items |= number_of_items >> 4;
   number_of_items |= number_of_items >> 8;
   number_of_items |= number_of_items >> 16;
   return number_of_items + 1;
}

static void generateMorphismCode(List *declarations, char type);
static void generateProgramCode(GPCommand *command, CommandData data);
static void generateRuleCall(string rule_name, bool empty_lhs, bool predicate,
                             bool last_rule, CommandData data);
static void generateBranchStatement(GPCommand *command, CommandData data);
static void generateLoopStatement(GPCommand *command, CommandData data);
static void generateFailureCode(string rule_name, CommandData data);

void generateRuntimeMain(List *declarations, int host_nodes, int host_edges,
                         string host_file, string output_file)
{
   file = fopen("../runtime/main.c", "w");
   if(file == NULL) { 
     perror("../runtime/main.c");
     exit(1);
   }

   PTF("#include <time.h>\n");
   PTF("#include \"debug.h\"\n");
   PTF("#include \"graph.h\"\n");
   PTF("#include \"graphStacks.h\"\n");
   PTF("#include \"hostParser.h\"\n");
   PTF("#include \"morphism.h\"\n\n");

   /* Declare the global morphism variables for each rule. */
   generateMorphismCode(declarations, 'd');

   /* Declare the runtime global variables and functions. */
   generateMorphismCode(declarations, 'f');

   PTF("static void garbageCollect(void)\n");
   PTF("{\n");
   PTF("   freeGraph(host);\n");
   #ifdef LIST_HASHING
      PTF("   freeHostListStore();\n");
   #endif
   PTF("   freeMorphisms();\n");
   if(graph_copying) PTF("   freeGraphStack();\n");
   else PTF("   freeGraphChangeStack();\n");
   PTF("   closeLogFile();\n");
   #if defined GRAPH_TRACING || defined RULE_TRACING || defined BACKTRACK_TRACING
      PTF("   closeTraceFile();\n");
   #endif
   PTF("}\n\n");

   PTF("Graph *host = NULL;\n");
   PTF("int *node_map = NULL;\n\n");

   /* Print the function that builds the host graph through the host graph parser. */
   PTF("static Graph *buildHostGraph(void)\n");
   PTF("{\n");
   if(host_file == NULL)
   {
      PTFI("return newGraph(%d, %d);\n", 3, MIN_HOST_NODE_SIZE, MIN_HOST_EDGE_SIZE);
      PTF("}\n\n");
      return;
   }
   PTFI("yyin = fopen(\"%s\", \"r\");\n", 3, host_file);
   PTFI("if(yyin == NULL)\n", 3);
   PTFI("{\n", 3);
   PTFI("perror(\"%s\");\n", 6, host_file);
   PTFI("return NULL;\n", 6);
   PTFI("}\n\n", 3);
   int host_node_size = getArraySize(host_nodes, MIN_HOST_NODE_SIZE);
   int host_edge_size = getArraySize(host_edges, MIN_HOST_EDGE_SIZE);
   PTFI("host = newGraph(%d, %d);\n", 3, host_node_size, host_edge_size);
   PTFI("node_map = calloc(%d, sizeof(int));\n", 3, host_node_size);
   PTFI("if(node_map == NULL)\n", 3);
   PTFI("{\n", 3);
   PTFI("freeGraph(host);\n", 6);
   PTFI("return NULL;\n", 6);
   PTFI("}\n", 3);
   PTFI("/* The parser populates the host graph using node_map to add edges with\n", 3);
   PTFI(" * the correct source and target indices. */\n", 3);
   PTFI("int result = yyparse();\n", 3);
   PTFI("free(node_map);\n", 3);
   PTFI("fclose(yyin);\n", 3);
   PTFI("if(result == 0) return host;\n", 3);
   PTFI("else\n", 3);
   PTFI("{\n", 3);
   PTFI("freeGraph(host);\n", 6);
   PTFI("return NULL;\n", 6);
   PTFI("}\n", 3);
   PTF("}\n\n");
   
   PTF("bool success = true;\n\n");

   /* Open the runtime's main function and set up the execution environment. */
   PTF("int main(void)\n");
   PTF("{\n");
   PTFI("srand(time(NULL));\n", 3);
   PTFI("openLogFile(\"../gp2.log\");\n", 3);
   #if defined GRAPH_TRACING || defined RULE_TRACING || defined BACKTRACK_TRACING
      PTFI("openTraceFile(\"../gp2.trace\");\n", 3);
   #endif

   PTFI("host = buildHostGraph();\n", 3);
   PTFI("if(host == NULL)\n", 3);
   PTFI("{\n", 3);
   PTFI("fprintf(stderr, \"Error parsing host graph file. Execution aborted.\\n\");\n", 6);
   PTFI("return 0;\n", 6);
   PTFI("}\n", 3);

   if(output_file == NULL) PTFI("string output = \"../gp2.output\";\n", 3);
   else PTFI("string output = \"%s\";\n", 3, output_file);
   PTFI("FILE *output_file = fopen(output, \"w\");\n", 3);
   PTFI("if(output_file == NULL)\n", 3);
   PTFI("{\n", 3);
   PTFI("perror(output);\n", 6);
   PTFI("exit(1);\n", 6);
   PTFI("}\n", 3);

   #ifdef GRAPH_TRACING
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
         CommandData initialData = {MAIN_BODY, false, -1, 3}; 
         generateProgramCode(decl->main_program, initialData);
      }
      iterator = iterator->next;
   }
   PTF("   printGraph(host, output_file);\n");
   PTF("   printf(\"Output graph saved to file %%s\\n\", output);\n");
   PTF("   garbageCollect();\n");
   PTF("   printf(\"Graph changes recorded: %%d\\n\", graph_change_count);\n");
   PTF("   fclose(output_file);\n");
   PTF("   return 0;\n");
   PTF("}\n\n");
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
   if(type == 'f') PTF("static void freeMorphisms(void)\n{\n");
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
   else PTF("}\n\n");
}


static void generateProgramCode(GPCommand *command, CommandData data)
{
   switch(command->type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = command->commands;
           CommandData new_data = data;
           bool stop_recording = false;
           while(commands != NULL)
           {
              GPCommand *command = commands->command;
              if(command->type == ALAP_STATEMENT && command->loop_stmt.stop_recording)
                 stop_recording = true;
              /* If stop_recording is set to true above, every future iteration of this
               * loop will unset the record_changes flag in the data it passes to
               * generateProgramCode. */
              if(stop_recording) new_data.record_changes = false;
              generateProgramCode(command, new_data);
              if(data.context == LOOP_BODY && commands->next != NULL)
                 PTFI("if(!success) break;\n\n", data.indent);             
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
              PTFI("/* Graph changes from loop body not required.\n", data.indent);
              PTFI("   Discard them so that future graph roll backs are uncorrupted. */\n",
                   data.indent);
              #ifdef BACKTRACK_TRACING
                 PTFI("print_trace(\"(%d) Discarding graph changes.\\n\\n\");\n",
                      data.indent + 3, data.restore_point);
              #endif
              if(graph_copying) 
              {
                 PTFI("Graph *copy = popGraphs(%d);\n", data.indent, data.restore_point);
                 PTFI("freeGraph(copy);\n", data.indent);
              }
              else PTFI("discardChanges(restore_point%d);\n", data.indent, data.restore_point);
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
      #ifdef RULE_TRACING
         PTFI("print_trace(\"Matched %s. (empty rule)\\n\\n\");\n", 
              data.indent, rule_name);
      #endif
      if(predicate) return;
      if(data.record_changes && !graph_copying) 
         PTFI("apply%s(true);\n", data.indent, rule_name);
      else PTFI("apply%s(false);\n", data.indent, rule_name);
      #ifdef GRAPH_TRACING
         PTFI("print_trace(\"Graph after applying rule %s:\\n\");\n",
              data.indent, rule_name);
         PTFI("printGraph(host, trace_file);\n\n", data.indent);
      #endif
      PTFI("success = true;\n\n", data.indent);
   }
   else
   {
      #ifdef RULE_TRACING
         PTFI("print_trace(\"Matching %s...\\n\");\n", data.indent, rule_name);
      #endif
      PTFI("if(match%s(M_%s))\n", data.indent, rule_name, rule_name);
      PTFI("{\n", data.indent);
      #ifdef RULE_TRACING
         PTFI("print_trace(\"Matched %s.\\n\\n\");\n", data.indent + 3, rule_name);
      #endif
      if(!predicate)
      {
         /* It is incorrect to apply the rule in a program such as "if r1 then P else Q",
          * even if the match has succeeded. This situation occurs only when the context 
          * is IF_BODY and there is no graph recording. 
          * Hence, only generate rule application if the context is not IF_BODY or
          * graph recording is on (signified by a restore_point >= 0). */
         if(data.context != IF_BODY || data.restore_point >= 0)
         { 
            if(data.record_changes && !graph_copying) 
                 PTFI("apply%s(M_%s, true);\n", data.indent + 3, rule_name, rule_name);
            else PTFI("apply%s(M_%s, false);\n", data.indent + 3, rule_name, rule_name);
            #ifdef GRAPH_TRACING
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
         #ifdef RULE_TRACING
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
         #ifdef RULE_TRACING
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
   condition_data.context = command->type == IF_STATEMENT ? IF_BODY : TRY_BODY;
   condition_data.indent = data.indent + 3;
   if(command->cond_branch.record_changes)
   {
      condition_data.record_changes = true;
      condition_data.restore_point = restore_point_count++;
   }
   else condition_data.restore_point = -1;

   if(condition_data.context == IF_BODY) PTFI("/* If Statement */\n", data.indent);
   else PTFI("/* Try Statement */\n", data.indent);
   PTFI("/* Condition */\n", data.indent);
   if(condition_data.restore_point >= 0)
   {
      #ifdef BACKTRACK_TRACING
         PTFI("print_trace(\"(%d) Recording graph changes.\\n\\n\");\n",
              data.indent, condition_data.restore_point);
      #endif
      if(graph_copying) PTFI("copyGraph(host);\n", data.indent);
      else PTFI("int restore_point%d = graph_change_stack == NULL ? 0 : topOfGraphChangeStack();\n",
                data.indent, condition_data.restore_point);
   }
   PTFI("do\n", data.indent);
   PTFI("{\n", data.indent);
   generateProgramCode(command->cond_branch.condition, condition_data);
   PTFI("} while(false);\n\n", data.indent);

   if(condition_data.context == IF_BODY)
   {
      if(condition_data.restore_point >= 0)
      {
         if(graph_copying) PTFI("host = popGraphs(%d);\n", data.indent, 
                                condition_data.restore_point);
         else PTFI("undoChanges(host, restore_point%d);\n", data.indent, 
                   condition_data.restore_point);
         #ifdef BACKTRACK_TRACING
            PTFI("print_trace(\"(%d) Undoing graph changes.\\n\\n\");\n",
                 data.indent, condition_data.restore_point);
         #endif
         #ifdef GRAPH_TRACING
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
         if(graph_copying) PTFI("host = popGraphs(%d);\n", data.indent, 
                                condition_data.restore_point);
         else PTFI("undoChanges(host, restore_point%d);\n", data.indent, 
                   condition_data.restore_point);
         #ifdef BACKTRACK_TRACING
            PTFI("print_trace(\"(%d) Undoing graph changes.\\n\\n\");\n",
                 new_data.indent, condition_data.restore_point);
         #endif
         #ifdef GRAPH_TRACING
            PTFI("print_trace(\"Restored graph:\\n\");\n", new_data.indent);
            PTFI("printGraph(host, trace_file);\n", new_data.indent);
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
   loop_data.context = LOOP_BODY;
   loop_data.indent = data.indent + 3;
   if(command->loop_stmt.record_changes)
   {
      loop_data.record_changes = true;
      loop_data.restore_point = restore_point_count++;
   }
   else loop_data.restore_point = -1;

   PTFI("/* Loop Statement */\n", data.indent);
   if(loop_data.restore_point >= 0)
   {
      #ifdef BACKTRACK_TRACING
         PTFI("print_trace(\"(%d) Recording graph changes.\\n\\n\");\n",
              data.indent, loop_data.restore_point);
      #endif
      if(graph_copying) PTFI("copyGraph(host);\n", data.indent);
      else PTFI("int restore_point%d = graph_change_stack == NULL ? 0 : topOfGraphChangeStack();\n",
                data.indent, loop_data.restore_point);
   }
   PTFI("while(success)\n", data.indent);
   PTFI("{\n", data.indent);
   generateProgramCode(command->loop_stmt.loop_body, loop_data);
   if(loop_data.restore_point >= 0)
   {
      PTFI("/* Graph changes from loop body may not have been used.\n", data.indent + 3);
      PTFI("   Discard them so that future graph roll backs are uncorrupted. */\n",
           data.indent + 3);
      #ifdef BACKTRACK_TRACING
         PTFI("print_trace(\"(%d) Discarding graph changes.\\n\\n\");\n",
              data.indent + 3, loop_data.restore_point);
      #endif
      if(graph_copying)
      {
         PTFI("if(success)\n", data.indent + 3);
         PTFI("{\n", data.indent + 3);
         PTFI("Graph *copy = popGraphs(%d);\n", data.indent + 6, loop_data.restore_point);
         PTFI("freeGraph(copy);\n", data.indent + 6);
         PTFI("}\n", data.indent + 3);
      }
      else PTFI("if(success) discardChanges(restore_point%d);\n", 
                data.indent + 3, loop_data.restore_point);
   }
   PTFI("}\n", data.indent);
   PTFI("success = true;\n", data.indent);
}

/* Generates code to handle failure, which is context-dependent. There are two
 * kinds of failure: 
 * (1) A rule fails to match. The name of the rule is passed as the first 
 *     argument. 
 * (2) The fail statement is called. NULL is passed as the first argument. */

static void generateFailureCode(string rule_name, CommandData data)
{
   /* A failure in the main body ends the execution. Emit code to report the 
    * failure, garbage collect and return 0. */
   if(data.context == MAIN_BODY)
   {
      #ifdef GRAPH_TRACING
         PTFI("print_trace(\"Program failed. Final graph:\\n\");\n", data.indent);
         PTFI("printGraph(host, trace_file);\n", data.indent);
      #endif
      if(rule_name != NULL)
         PTFI("fprintf(output_file, \"No output graph: rule %s not applicable.\\n\");\n",
              data.indent, rule_name);
      else PTFI("fprintf(output_file, \"No output graph: Fail statement invoked\\n\");\n",
                data.indent);
      PTFI("printf(\"Output information saved to file %%s\\n\", output);\n", data.indent);
      PTFI("garbageCollect();\n", data.indent);
      PTFI("printf(\"Graph changes recorded: %%d\\n\", graph_change_count);\n", data.indent);
      PTFI("fclose(output_file);\n", data.indent);
      PTFI("return 0;\n", data.indent);
   }
   /* In other contexts, set the runtime success flag to false. */
   else PTFI("success = false;\n", data.indent);

   if(data.context == IF_BODY || data.context == TRY_BODY) PTFI("break;\n", data.indent);
   if(data.context == LOOP_BODY) 
   {
      if(data.restore_point >= 0) 
      {
         if(graph_copying) PTFI("host = popGraphs(%d);\n", data.indent, data.restore_point);
         else PTFI("undoChanges(host, restore_point%d);\n", data.indent, data.restore_point);
         #ifdef BACKTRACK_TRACING
            PTFI("print_trace(\"(%d) Undoing graph changes.\\n\\n\");\n",
                 data.indent, data.restore_point);
         #endif
         #ifdef GRAPH_TRACING
            PTFI("print_trace(\"Restored graph:\\n\");\n", data.indent);
            PTFI("printGraph(host, trace_file);\n", data.indent);
         #endif
      }
   }
}

