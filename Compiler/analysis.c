#include "analysis.h"

typedef enum {NO_BACKTRACK = 0, RECORD_CHANGES, COPY} copyType;

static void annotate(GPCommand *command, int restore_point);
static copyType getCommandType(GPCommand *command, bool if_body, bool in_sequence);
static bool neverFails(GPCommand *command);

void staticAnalysis(List *declarations)
{
   List *iterator = declarations;
   while(iterator != NULL)
   {
      GPDeclaration *decl = iterator->declaration;
      switch(decl->type)
      {
         case MAIN_DECLARATION:
              annotate(decl->main_program, 0);
              break;

         case PROCEDURE_DECLARATION:
              if(decl->procedure->local_decls != NULL)
                 staticAnalysis(decl->procedure->local_decls);
              break;

         case RULE_DECLARATION:
              break;

         default: 
              print_to_log("Error (enterPredicateRules): Unexpected "
                           "declaration type %d at AST node %d\n", 
                           decl->type, decl->id);
              break;
      }
      iterator = iterator->next;
   }
}

/* Searches for conditional branching statements and loop bodies so that they 
 * can be analysed with respect to host graph backtracking. */
static void annotate(GPCommand *command, int restore_point)
{
   switch(command->type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = command->commands;
           while(commands != NULL)
           {            
              /* Note that each command is annotated with the same restore
               * point as each command in the sequence is independent with
               * respect to graph backtracking. */
              annotate(commands->command, restore_point);
              commands = commands->next;
           }
           break;
      }
      case RULE_CALL:
      case RULE_SET_CALL:
           break;

      case PROCEDURE_CALL:
      {
           GPProcedure *procedure = command->proc_call.procedure;
           annotate(procedure->commands, restore_point);
           break;
      }
      case IF_STATEMENT:
      case TRY_STATEMENT:
      {
           bool if_body = command->type == IF_STATEMENT;
           int new_restore_point = restore_point;
           GPCommand *condition = command->cond_branch.condition;
           GPCommand *then_command = command->cond_branch.then_command;
           GPCommand *else_command = command->cond_branch.else_command;
           /* If the if condition below does not hold, then this is a try
            * statement whose condition cannot fail. In that case, no graph
            * backtracking is required.
            * Otherwise, this is an if statement, or a try statement whose 
            * condition can fail, which may require graph backtracking to
            * the state before executing the condition. */
           if(if_body || !neverFails(condition))
           {
              copyType type = getCommandType(condition, if_body, false); 
              if(type == RECORD_CHANGES) command->cond_branch.roll_back = true;
              if(type == COPY)
              {
                 command->cond_branch.restore_point = restore_point;
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
           GPCommand *loop_body = command->loop_stmt.loop_body;
           int loop_restore_point = restore_point;
           /* Only analyse the loop body for backtracking if if it possible
            * for the loop body to fail. */
           if(!neverFails(loop_body))
           {
              copyType type = getCommandType(loop_body, false, false);
              if(type == RECORD_CHANGES) command->loop_stmt.roll_back = true;
              if(type == COPY)
              {
                 command->loop_stmt.restore_point = restore_point;
                 loop_restore_point++;
              }
           }
           annotate(loop_body, loop_restore_point);
           break;
      }
      case PROGRAM_OR:
           annotate(command->or_stmt.left_command, restore_point);
           annotate(command->or_stmt.right_command, restore_point);
           break;

      case BREAK_STATEMENT:
      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
           break;

      default:
           print_to_log("Error (findRestorePoints): Unexpected command type %d.\n",
                        command->type);
           break;
   }
}

static copyType getCommandType(GPCommand *command, bool if_body, bool in_sequence)
{
   switch(command->type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = command->commands;
           int command_position = 1;
           copyType type = NO_BACKTRACK;
           while(commands != NULL)
           {
              if(commands->command->type != ALAP_STATEMENT)
              {
                 copyType type = getCommandType(commands->command, if_body, true);
                 if(type == COPY) return COPY;
                 command_position++;
              }
              else
              { 
                 /* The host graph before entering the loop always needs to be kept
                  * for if statements. */
                 if(if_body) return COPY;

                 /* Check if the remainder of the command sequence consists of 
                  * non-failing commands. If so, recording of graph changes stops here:
                  * the context is a try body or a loop body, where the host graph
                  * is not backtracked on success. Otherwise, there is a possibility of
                  * failure, requiring the host graph to be copied before entering the
                  * command sequence. */
                 GPCommand *first_loop = commands->command;
                 commands = commands->next;
                 while(commands != NULL)
                 {
                    if(!neverFails(commands->command)) return COPY;
                    else commands = commands->next;
                 }
                 /* The sequence from first_loop to the end cannot fail. Switch on
                  * the stop recording flag of the first loop. */
                 first_loop->loop_stmt.stop_recording = true;
                 /* The loop is the second item in a never-failing command sequence
                  * e.g. (r1; r2!; r3!). If the first command requires no backtracking 
                  * by itself, then no backtracking is required overall. Note that 
                  * this does not apply if there is more than one command before the
                  * start of the never-failing sequence. */ 
                 if(command_position <= 2)
                 {
                    /* For command_position == 2, type is set by the previous loop
                     * iteration. */
                    if(type == NO_BACKTRACK) return NO_BACKTRACK;
                    else return RECORD_CHANGES;
                 }
                 else return RECORD_CHANGES;
              }
              commands = commands->next;
           }
           return RECORD_CHANGES;
      }
      case RULE_CALL:
      case RULE_SET_CALL:
           return NO_BACKTRACK;

      case PROCEDURE_CALL:
           return getCommandType(command->proc_call.procedure->commands, 
                                 if_body, in_sequence);

      case IF_STATEMENT:
      case TRY_STATEMENT:
      {
           copyType cond_type = getCommandType(command->cond_branch.condition,
                                               if_body, in_sequence);
           copyType then_type = getCommandType(command->cond_branch.then_command,
                                               if_body, in_sequence);
           copyType else_type = getCommandType(command->cond_branch.else_command, 
                                               if_body, in_sequence);
           /* No graph backtracking is required in, for example, 
            * "try (if C then r1 else r2) then P else Q" because the inner if
            * statement simplifies to a single rule application on the current
            * graph. This is not guaranteed if the inner statement is a try
            * statement because the changes to the graph performed by the
            * condition may be kept. */
           if(!in_sequence && if_body && then_type == NO_BACKTRACK && 
              else_type == NO_BACKTRACK) return NO_BACKTRACK;                                  
           if(cond_type == COPY || then_type == COPY || else_type == COPY) return COPY;
           else return RECORD_CHANGES;
      }

      case ALAP_STATEMENT:
           /* Loops within a command sequence are handled in the COMMAND_SEQUENCE case.
            * Therefore this case only deals with a single looped statement. */
           if(if_body) return COPY; else return NO_BACKTRACK;

      case PROGRAM_OR:
      {
           /* Return the "max" of the two branches. */
           copyType left_type = getCommandType(command->or_stmt.left_command, 
                                               if_body, in_sequence);
           copyType right_type = getCommandType(command->or_stmt.right_command,
                                                if_body, in_sequence);
           return left_type > right_type ? left_type : right_type;
      }

      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
      case BREAK_STATEMENT:
           return NO_BACKTRACK;

      default:
           print_to_log("Error (getCommandType): Unexpected command type %d.\n",
                        command->type);
           break;
   }
   return COPY;
}

/* Returns true if the passed GP 2 command always succeeds. Used to test
 * conditions and loop bodies: if these always succeed, then backtracking
 * is not necessary for try statements and loops. */
static bool neverFails(GPCommand *command)
{
   switch(command->type)
   {
      case COMMAND_SEQUENCE:
      { 
           List *commands = command->commands;
           while(commands != NULL)
           {
              if(!neverFails(commands->command)) return false;
              else commands = commands->next;
           }
           return true;
      }
      case RULE_CALL:
           if(command->rule_call.rule->empty_lhs) return true;
           else return false;

      case RULE_SET_CALL:
           if(command->rule_set->rule_call.rule->empty_lhs) return true;
           else return false;

      case PROCEDURE_CALL:
           return neverFails(command->proc_call.procedure->commands);

      case IF_STATEMENT:
      case TRY_STATEMENT:
           if(!neverFails(command->cond_branch.then_command)) return false;
           if(!neverFails(command->cond_branch.else_command)) return false;
           else return true;

      case ALAP_STATEMENT:
           return true;

      case PROGRAM_OR:
           if(!neverFails(command->or_stmt.left_command)) return false;
           if(!neverFails(command->or_stmt.right_command)) return false;
           else return true;

      case BREAK_STATEMENT:
      case SKIP_STATEMENT:
           return true;

      case FAIL_STATEMENT:
           return false;

      default:
           print_to_log("Error (neverFails): Unexpected command type %d.\n",
                        command->type);
           break;
   }
   return false;
}
