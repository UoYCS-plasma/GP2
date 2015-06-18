#include "analysis.h"

typedef enum {NO_BACKTRACK = 0, RECORD_CHANGES, COPY} copyType;

static void annotate(GPCommand *command, int restore_point);
static copyType getIfCommandType(GPCommand *command);
static copyType getTryLoopCommandType(GPCommand *command, bool first_sequence,
                                      bool first_command);
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
              print_to_log("Error (enterPr`edicateRules): Unexpected "
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
           annotate(command->proc_call.procedure->commands, restore_point);
           break;

      case IF_STATEMENT:
      case TRY_STATEMENT:
      {
           GPCommand *condition = command->cond_branch.condition;
           GPCommand *then_command = command->cond_branch.then_command;
           GPCommand *else_command = command->cond_branch.else_command;
           int new_restore_point = restore_point;
           copyType type;
           if(command->type == IF_STATEMENT) type = getIfCommandType(condition); 
           else type = getTryLoopCommandType(condition, true, true); 
           if(type == RECORD_CHANGES) command->cond_branch.roll_back = true;
           if(type == COPY)
           {
              command->cond_branch.roll_back = true;
              //command->cond_branch.restore_point = restore_point;
              //new_restore_point++;
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
              copyType type = getTryLoopCommandType(loop_body, true, true);
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

static copyType getIfCommandType(GPCommand *command)
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
              type = getIfCommandType(commands->command);
              if(type == COPY) return COPY;
              command_position++;
              commands = commands->next;
           }
           /* If there is only one command in the sequence with type NO_BACKTRACK, 
            * then no backtracking is required overall. */
           if(command_position <= 2 && type == NO_BACKTRACK) return NO_BACKTRACK;
           else return RECORD_CHANGES;
      }
      case RULE_CALL:
      case RULE_SET_CALL:
           return NO_BACKTRACK;

      case PROCEDURE_CALL:
           return getIfCommandType(command->proc_call.procedure->commands);

      case IF_STATEMENT:
      case TRY_STATEMENT:
      {
           copyType then_type = getIfCommandType(command->cond_branch.then_command);
           copyType else_type = getIfCommandType(command->cond_branch.else_command);
           if(then_type == COPY || else_type == COPY) return COPY;
           if(then_type == NO_BACKTRACK && else_type == NO_BACKTRACK) return NO_BACKTRACK;     
           else return RECORD_CHANGES;
      }
      case ALAP_STATEMENT:
           return COPY;

      case PROGRAM_OR:
      {
           /* Return the "max" of the two branches. */
           copyType left_type = getIfCommandType(command->or_stmt.left_command);
           copyType right_type = getIfCommandType(command->or_stmt.right_command);
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

/* For the purpose of this analysis, for a sequence C1; ...; Cn, it suffices
 * to establish whether all the commands C2; ... ; Cn never fail. Then
 * no backtracking is required, no matter what C1 is. This analysis
 * is complicated by the fact that the "defining command sequence"
 * is not straightforward. For instance, the defining command sequence
 * of the loop (if C1 then P1 else Q1; try C2 then P2 else Q2; P3 or Q3)!
 * is (P1 or Q1); (C2; P2 or Q2); (P3 or Q3). 
 *
 * The function below determines whether a GP 2 subprogram requires
 * backtracking or not. It takes as input the root AST node of the
 * command to be analysed, and a pointer to the first simple command
 * in that AST. */
static copyType getTryLoopCommandType(GPCommand *command, bool first_sequence,
                                      bool first_command)
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
              GPCommand *current_command = commands->command;
              List *iterator = commands;
              bool cannot_fail = true;
              while(iterator != NULL)
              {
                 if(!neverFails(iterator->command)) cannot_fail = false;
                 iterator = iterator->next;
              }
              if(cannot_fail)
              {
                  if(current_command->type == ALAP_STATEMENT) 
                     current_command->loop_stmt.stop_recording = true;
                  if(command_position <= 2 && type == NO_BACKTRACK) return NO_BACKTRACK;
                  else return RECORD_CHANGES;
              }
              else 
              {
                 type = getTryLoopCommandType(current_command, first_sequence, 
                                              command_position == 1);
                 if(type == COPY) return COPY;
              }
              command_position++;
              commands = commands->next;
           }
           /* If there is only one command in the sequence with type NO_BACKTRACK, 
            * then no backtracking is required overall. */
           if(command_position <= 2 && type == NO_BACKTRACK) return NO_BACKTRACK;
           else return RECORD_CHANGES;
      }
      case RULE_CALL:
           if(first_sequence && first_command) return NO_BACKTRACK;
           else
           {
              if(command->rule_call.rule->empty_lhs) return NO_BACKTRACK;
              else return RECORD_CHANGES;
           }

      case RULE_SET_CALL:
           if(first_sequence && first_command) return NO_BACKTRACK;
           else
           {
              /* If all the rules in the set have an empty LHS, return NO_BACKTRACK. */
              List *rules = command->rule_set;
              while(rules != NULL)
              {
                 if(!rules->rule_call.rule->empty_lhs) return RECORD_CHANGES;
              }
              return NO_BACKTRACK;
           }

      case PROCEDURE_CALL:
           return getTryLoopCommandType(command->proc_call.procedure->commands, 
                                        first_sequence, first_command);

      case IF_STATEMENT:
      {
           GPCommand *condition = command->cond_branch.condition;
           GPCommand *then_command = command->cond_branch.then_command;
           GPCommand *else_command = command->cond_branch.else_command;
           if(neverFails(then_command) && (neverFails(else_command) || neverFails(condition)))
              return NO_BACKTRACK;
           
           /* No graph backtracking is required in, for example, 
            * "(if C then r1 else r2)!" because the inner if statement 
            * simplifies to a single rule application on the current graph. */
           copyType then_type = getTryLoopCommandType(then_command, first_sequence, first_command);
           copyType else_type = getTryLoopCommandType(else_command, first_sequence, first_command);
                                         
           /* Return the "max" of the two branches. */
           return then_type > else_type ? then_type : else_type;
      }
      case TRY_STATEMENT:
      {
           GPCommand *condition = command->cond_branch.condition;
           GPCommand *then_command = command->cond_branch.then_command;
           GPCommand *else_command = command->cond_branch.else_command;
           if(neverFails(then_command) && (neverFails(else_command) || neverFails(condition)))
              return NO_BACKTRACK;

           copyType cond_type = getTryLoopCommandType(condition, first_sequence, first_command);
           /* A try statement's then_type "follows" the condition. */
           copyType then_type = getTryLoopCommandType(then_command, false, first_command);
           copyType else_type = getTryLoopCommandType(else_command, first_sequence, first_command);

           /* Return the "max" of the three branches. */
           if(cond_type == COPY || then_type == COPY || else_type == COPY) return COPY;
           else if(cond_type == RECORD_CHANGES || then_type == RECORD_CHANGES || 
                   else_type == RECORD_CHANGES) return RECORD_CHANGES;
           else return NO_BACKTRACK;
      }

      case ALAP_STATEMENT:
           return COPY;

      case PROGRAM_OR:
      {
           /* Return the "max" of the two branches. */
           copyType left_type = getTryLoopCommandType(command->or_stmt.left_command, 
                                                      first_sequence, first_command);
           copyType right_type = getTryLoopCommandType(command->or_stmt.right_command,
                                                       first_sequence, first_command);
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

/* A simple command is non-failing (NF) if it never fails. Specifically:
 * 'skip' and 'break' are NF.
 * 'fail' is not NF.
 * A rule R is NF if its LHS is empty.
 * A rule set is NF if all the rules in the set are NF. 
 *
 * The NF status of more complicated commands are defined recursively.
 * A looped subprogram is NF.
 * if/try C then P else Q is NF if both P and Q are NF.
 * P or Q is NF if both P and Q are NF.
 *
 * For completeness, a command sequence C1; ... ; Cn is NF if all its
 * commands are NF. However, command sequences are never tested for
 * non-failing status. */
 
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
