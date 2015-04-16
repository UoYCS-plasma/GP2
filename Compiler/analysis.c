#include "analysis.h"

void staticAnalysis(List *declarations, bool debug, string prefix)
{
   List *iterator = declarations;
   while(iterator != NULL)
   {
      GPDeclaration *decl = iterator->value.declaration;
      switch(decl->decl_type)
      {
         case MAIN_DECLARATION:
              annotate(decl->value.main_program, 0);
              break;

         case PROCEDURE_DECLARATION:
              if(decl->value.procedure->local_decls != NULL)
                 staticAnalysis(decl->value.procedure->local_decls, false, NULL);
              break;

         case RULE_DECLARATION:
              break;

         default: print_to_log("Error (enterPredicateRules): Unexpected "
                               "declaration type %d at AST node %d\n", 
                               decl->decl_type, decl->node_id);
              break;
      }
      iterator = iterator->next;
   }
   if(debug)
   {
      int length = strlen(prefix) + 2;
      char file_name[length]; 
      strcpy(file_name, prefix);
      strcat(file_name,"_3"); 
      printDotAST(declarations, file_name);
   }
}

void annotate(GPCommand *command, int restore_point)
{
   switch(command->command_type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = command->value.commands;
           while(commands != NULL)
           {            
              /* Note that each command is annotated with the same restore
               * point as each command in the sequence is independent with
               * respect to graph backtracking. */
              annotate(commands->value.command, restore_point);
              commands = commands->next;
           }
           break;
      }

      case RULE_CALL:
      case RULE_SET_CALL:
           break;

      case PROCEDURE_CALL:
      {
           GPProcedure *procedure = command->value.proc_call.procedure;
           annotate(procedure->commands, restore_point);
           break;
      }

      case IF_STATEMENT:
      case TRY_STATEMENT:
      {
           bool if_body = command->command_type == IF_STATEMENT;
           int new_restore_point = restore_point;
           GPCommand *condition = command->value.cond_branch.condition;
           GPCommand *then_command = command->value.cond_branch.then_command;
           GPCommand *else_command = command->value.cond_branch.else_command;
           /* If the if condition below does not hold, then this is a try
            * statement whose condition cannot fail. In that case, no graph
            * backtracking is required.
            * Otherwise, this is an if statement, or a try statement whose 
            * condition can fail, which may require graph backtracking to
            * the state before executing the condition. */
           if(if_body || !neverFails(condition))
           {
              copyType type = getCommandType(condition, 0, if_body, false); 
              if(type == RECORD_CHANGES) 
                 command->value.cond_branch.roll_back = true;
              if(type == COPY)
              {
                 command->value.cond_branch.restore_point = restore_point;
                 new_restore_point++;
              }
           }
           annotate(condition, new_restore_point);
           annotate(then_command, restore_point);
           annotate(else_command, restore_point);
           break;
      }

      case ALAP_STATEMENT:
      {
           GPCommand *loop_body = command->value.loop_stmt.loop_body;
           int loop_restore_point = restore_point;
           /* Only analyse the loop body for backtracking if if it possible
            * for the loop body to fail. */
           if(!neverFails(loop_body))
           {
              copyType type = getCommandType(loop_body, 0, false, false);
              if(type == RECORD_CHANGES)
                 command->value.loop_stmt.roll_back = true;
              if(type == COPY)
              {
                 command->value.loop_stmt.restore_point = restore_point;
                 loop_restore_point++;
              }
           }
           annotate(loop_body, loop_restore_point);
           break;
      }

      case PROGRAM_OR:
      {
           annotate(command->value.or_stmt.left_command, restore_point);
           annotate(command->value.or_stmt.right_command, restore_point);
           break;
      }

      case BREAK_STATEMENT:
      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
           break;

      default:
           print_to_log("Error (findRestorePoints): Unexpected command type %d.\n",
                        command->command_type);
           break;
   }
}

bool neverFails(GPCommand *command)
{
   switch(command->command_type)
   {
      case COMMAND_SEQUENCE:
      { 
           List *commands = command->value.commands;
           while(commands != NULL)
           {
              if(!neverFails(commands->value.command)) return false;
              else commands = commands->next;
           }
           return true;
      }
      case RULE_CALL:
      case RULE_SET_CALL:
           return false;

      case PROCEDURE_CALL:
           return neverFails(command->value.proc_call.procedure->commands);

      case IF_STATEMENT:
      case TRY_STATEMENT:
           if(!neverFails(command->value.cond_branch.then_command)) return false;
           if(!neverFails(command->value.cond_branch.else_command)) return false;
           else return true;

      case ALAP_STATEMENT:
           return true;

      case PROGRAM_OR:
           if(!neverFails(command->value.or_stmt.left_command)) return false;
           if(!neverFails(command->value.or_stmt.right_command)) return false;
           else return true;

      case BREAK_STATEMENT:
      case SKIP_STATEMENT:
           return true;

      case FAIL_STATEMENT:
           return false;

      default:
           print_to_log("Error (neverFails): Unexpected command type %d.\n",
                        command->command_type);
           break;
   }
   return false;
}

/* Depth 0: initial value, not within any command sequence.
 * Depth 1: within the top level command sequence.
 * Depth 2: within a nested command sequence. */
copyType getCommandType(GPCommand *command, int depth, bool if_body,
                        bool last_command)
{
   switch(command->command_type)
   {
      case COMMAND_SEQUENCE:
      { 
           List *commands = command->value.commands;
           /* Special case for sequences containing a single command. 
            * The second expression in the if condition below handles strange
            * programs like "(((r1; r2)); r3)" which would otherwise be treated
            * as "(r1; r2)", because "((r1; r2))" is parsed as a command sequence
            * containing the single command "(r1; r2)". */
           if(commands->next == NULL &&
              commands->value.command->command_type != COMMAND_SEQUENCE)
              return getCommandType(commands->value.command, depth, if_body, true);
           else
           {
              int new_depth = 1;
              /* Set the new depth to 2 if the depth is already 2 or if not at 
               * the last command in the top level command sequence. 
               * Main = "(r1; (r2; r3!))" -> Second command sequence is depth 1.
               * Main = "(r1; (r2; r3!); r4)" -> Second command sequence is depth 2.
               * This is significant for a special case when processing loops. See
               * the case ALAP_STATEMENT below for details. */
              if(depth == 2 || (depth == 1 && !last_command)) new_depth = 2;
              while(commands != NULL)
              {
                 copyType type = getCommandType(commands->value.command, new_depth, if_body,
                                                commands->next == NULL);
                 if(type == COPY) return COPY;
                 else commands = commands->next;
              }
              return RECORD_CHANGES;
           }
      }
      case RULE_CALL:
      case RULE_SET_CALL:
           return NO_BACKTRACK;

      case PROCEDURE_CALL:
           return getCommandType(command->value.proc_call.procedure->commands, 
                                 depth, if_body, last_command);

      case IF_STATEMENT:
      case TRY_STATEMENT:
      {
           copyType cond_type = getCommandType(command->value.cond_branch.condition,
                                               depth, if_body, last_command);
           copyType then_type = getCommandType(command->value.cond_branch.then_command,
                                               depth, if_body, last_command);
           copyType else_type = getCommandType(command->value.cond_branch.else_command, 
                                               depth, if_body, last_command);
           /* No graph backtracking is required in, for example, 
            * "try (if C then r1 else r2) then P else Q" because the inner if
            * statement simplifies to a single rule application on the current
            * graph. This is not guaranteed if the inner statement is a try
            * statement because the changes to the graph performed by the
            * condition may be kept. */
           if(depth == 0 && if_body && then_type == NO_BACKTRACK && 
              else_type == NO_BACKTRACK) return NO_BACKTRACK;                                  
           if(cond_type == COPY || then_type == COPY || else_type == COPY) return COPY;
           else return RECORD_CHANGES;
      }

      case ALAP_STATEMENT:
           if(if_body) return COPY;
           /* Loop statements always succeed, so it is not necessary to cater
            * for backtracking if the loop is the last command in a sequence.
            * For instance, "try (r1; r2; (C!))" only need record graph changes
            * until the start of the loop. After that point, it is certain that
            * the then branch will be taken, so there is no need to record
            * further changes throughout the execution of "C!". */
           else
           {
              /* If not for this condition, the loop in (r1; (r2; r3!); r4)
               * would "stop recording" and NO_BACKTRACK would be returned. 
               * This is why depth 2 is required. */
              if(depth == 2) return COPY;
              if(depth == 1 && !last_command) return COPY;
              if(depth == 1 && last_command)
                 command->value.loop_stmt.stop_recording = true;
           }
           return NO_BACKTRACK;

      /* Return the "max" of the two branches. */
      case PROGRAM_OR:
      {
           copyType left_type = getCommandType(command->value.or_stmt.left_command, 
                                               depth, if_body, last_command);
           copyType right_type = getCommandType(command->value.or_stmt.right_command,
                                                depth, if_body, last_command);
           return left_type > right_type ? left_type : right_type;
      }

      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
      case BREAK_STATEMENT:
           return NO_BACKTRACK;

      default:
           print_to_log("Error (getCommandType): Unexpected command type %d.\n",
                        command->command_type);
           break;
   }
   return COPY;
}

