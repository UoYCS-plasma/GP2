/* Copyright 2015-2016 Christopher Bak

  This file is part of the GP 2 Compiler. The GP 2 Compiler is free software: 
  you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  The GP 2 Compiler is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
  more details.

  You should have received a copy of the GNU General Public License
  along with the GP 2 Compiler. If not, see <http://www.gnu.org/licenses/>. */

#include "analysis.h"

typedef enum {NO_BACKTRACK = 0, RECORD_CHANGES, COPY} copyType;

static void annotate(GPCommand *command, bool in_loop);
static bool getIfCommandType(GPCommand *command);
static bool getTryLoopCommandType(GPCommand *command, bool first_sequence,
                                  bool first_command);
static bool neverFails(GPCommand *command);
static bool nullCommand(GPCommand *command);

void staticAnalysis(List *declarations)
{
   List *iterator = declarations;
   while(iterator != NULL)
   {
      GPDeclaration *decl = iterator->declaration;
      switch(decl->type)
      {
         case MAIN_DECLARATION:
              annotate(decl->main_program, false);
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
static void annotate(GPCommand *command, bool in_loop)
{
   switch(command->type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = command->commands;
           while(commands != NULL)
           {            
              /* Note that each command is annotated with the same restore
               * point because each command in the sequence is independent with
               * respect to graph backtracking. */
              annotate(commands->command, in_loop);
              commands = commands->next;
           }
           break;
      }
      case RULE_CALL:
      case RULE_SET_CALL:
           break;

      case PROCEDURE_CALL:
           annotate(command->proc_call.procedure->commands, in_loop);
           break;

      case IF_STATEMENT:
      case TRY_STATEMENT:
      {
           GPCommand *condition = command->cond_branch.condition;
           GPCommand *then_command = command->cond_branch.then_command;
           GPCommand *else_command = command->cond_branch.else_command;

           bool graph_recording;
           if(command->type == IF_STATEMENT) graph_recording = getIfCommandType(condition); 
           else graph_recording = getTryLoopCommandType(condition, true, true); 
           if(graph_recording) command->cond_branch.record_changes = true;

           annotate(condition, in_loop);
           annotate(then_command, in_loop);
           annotate(else_command, in_loop);
           break;
      }
      case ALAP_STATEMENT:
      {
           GPCommand *loop_body = command->loop_stmt.loop_body;
           if(in_loop == true) command->loop_stmt.inner_loop = true;
           if(nullCommand(loop_body)) print_error("Warning: Possible nontermination in loop.\n"); 
           /* Only analyse the loop body for backtracking if if it possible
            * for the loop body to fail. */
           if(!neverFails(loop_body))
           {
              bool graph_recording = getTryLoopCommandType(loop_body, true, true);
              if(graph_recording) command->loop_stmt.record_changes = true;
           }
           annotate(loop_body, true);
           break;
      }
      case PROGRAM_OR:
           annotate(command->or_stmt.left_command, in_loop);
           annotate(command->or_stmt.right_command, in_loop);
           break;

      case BREAK_STATEMENT:
           if(in_loop == true) command->inner_loop = true;
	   break;

      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
           break;

      default:
           print_to_log("Error (findRestorePoints): Unexpected command type %d.\n",
                        command->type);
           break;
   }
}

static bool getIfCommandType(GPCommand *command)
{
   switch(command->type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = command->commands;
           /* Go to the first non-null command in the sequence. */
           while(commands != NULL && nullCommand(commands->command))
              commands = commands->next;
           int command_position = 1;
           bool graph_recording = false;
           while(commands != NULL)
           {
              graph_recording = getIfCommandType(commands->command);
              if(graph_recording) return true;
              command_position++;
              commands = commands->next;
           }
           /* If the sequence only contains one command, and that command does not
            * require graph recording, no recording is required overall. */
           if(command_position <= 2 && !graph_recording) return false;
           else return true;
      }
      case RULE_CALL:
      case RULE_SET_CALL:
           return false;

      case PROCEDURE_CALL:
           return getIfCommandType(command->proc_call.procedure->commands);

      case IF_STATEMENT:
      case TRY_STATEMENT:
      {
           bool then_recording = getIfCommandType(command->cond_branch.then_command);
           bool else_recording = getIfCommandType(command->cond_branch.else_command);
           return then_recording || else_recording;
      }
      case ALAP_STATEMENT:
           return true;

      case PROGRAM_OR:
      {
           bool left_recording = getIfCommandType(command->or_stmt.left_command);
           bool right_recording = getIfCommandType(command->or_stmt.right_command);
           return left_recording || right_recording;
      }
      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
      case BREAK_STATEMENT:
           return false;

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
static bool getTryLoopCommandType(GPCommand *command, bool first_sequence,
                                  bool first_command)
{
   switch(command->type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = command->commands;
           /* Go to the first non-null command in the sequence. */
           while(commands != NULL && nullCommand(commands->command))
              commands = commands->next;
           int command_position = 1;
           bool graph_recording = false;

           while(commands != NULL)
           {
              List *iterator = commands;
              bool cannot_fail = true;
              /* Check if the remaining commands in the sequence collectively
               * never fail. */
              while(iterator != NULL && cannot_fail)
              {
                 if(!neverFails(iterator->command)) cannot_fail = false;
                 iterator = iterator->next;
              }
              if(cannot_fail)
              {
                 /* If the remaining commands never fail, then find the first
                  * loop in the remaining commands. If it exists, and it is not within
                  * an outer loop, set its stop recording flag. */
                 iterator = commands;
                 while(iterator != NULL)
                 {
                    if(iterator->command->type == ALAP_STATEMENT &&
                       !(iterator->command->loop_stmt.inner_loop))
                    {
                       iterator->command->loop_stmt.stop_recording = true;
                       break;
                    }
                    iterator = iterator->next;
                 }
                 /* If we are at the first command in the sequence, no graph
                  * recording is required because the whole sequence cannot fail.
                  * graph_recording's initial value is false which is the desired
                  * return value.
                  * If we are at the second command in the sequence, the return
                  * value is the 'graph recording' status of the first command.
                  * That is, if the first command necessitates graph recording,
                  * then the whole command sequence does, even though the remainder
                  * cannot fail. */
                 if(command_position <= 2) return graph_recording;
              }
              else
                 graph_recording = getTryLoopCommandType(commands->command, first_sequence,
                                                         command_position == 1);
              command_position++;
              commands = commands->next;
           }
           /* If there is only one command in the sequence with type false, 
            * then no backtracking is required overall. */
           if(command_position <= 2 && !graph_recording) return false;
           else return true;
      }
      case RULE_CALL:
           if(first_sequence && first_command) return false;
           else
           {
              if(command->rule_call.rule->empty_lhs) return false;
              else return true;
           }

      case RULE_SET_CALL:
           if(first_sequence && first_command) return false;
           else
           {
              /* If all the rules in the set have an empty LHS, return false. */
              List *rules = command->rule_set;
              while(rules != NULL)
              {
                 if(!rules->rule_call.rule->empty_lhs) return true;
              }
              return false;
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
              return false;
           
           /* No graph backtracking is required in, for example, 
            * "(if C then r1 else r2)!" because the inner if statement 
            * simplifies to a single rule application on the current graph. */
           bool then_recording = getTryLoopCommandType(then_command, first_sequence, first_command);
           bool else_recording = getTryLoopCommandType(else_command, first_sequence, first_command);
           return then_recording || else_recording;
      }
      case TRY_STATEMENT:
      {
           GPCommand *condition = command->cond_branch.condition;
           GPCommand *then_command = command->cond_branch.then_command;
           GPCommand *else_command = command->cond_branch.else_command;
           if(neverFails(then_command) && (neverFails(else_command) || neverFails(condition)))
              return NO_BACKTRACK;

           bool cond_recording = getTryLoopCommandType(condition, first_sequence, first_command);
           /* A try statement's then command "follows" the condition in that it uses the 
            * graph state from the condition, so pass false as the second argument. */
           bool then_recording = getTryLoopCommandType(then_command, false, first_command);
           bool else_recording = getTryLoopCommandType(else_command, first_sequence, first_command);
           return cond_recording || then_recording || else_recording;
      }

      case ALAP_STATEMENT:
           return true;

      case PROGRAM_OR:
      {
           /* Return the "max" of the two branches. */
           bool left_recording = getTryLoopCommandType(command->or_stmt.left_command, 
                                                       first_sequence, first_command);
           bool right_recording = getTryLoopCommandType(command->or_stmt.right_command,
                                                         first_sequence, first_command);
           return left_recording || right_recording;
      }

      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
      case BREAK_STATEMENT:
           return false;

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
      {
           List *rule_set = command->rule_set;
           while(rule_set != NULL)
           {
              if(!rule_set->rule_call.rule->empty_lhs) return false;
              else rule_set = rule_set->next;
           }
           return true;

           if(command->rule_set->rule_call.rule->empty_lhs) return true;
           else return false;
      }

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

/* Returns true if the passed GP 2 command does not change the host graph. */
static bool nullCommand(GPCommand *command)
{
   switch(command->type)
   {
      case COMMAND_SEQUENCE:
      { 
           List *commands = command->commands;
           while(commands != NULL)
           {
              if(!nullCommand(commands->command)) return false;
              else commands = commands->next;
           }
           return true;
      }
      case RULE_CALL:
           if(command->rule_call.rule->is_predicate) return true;
           else return false;

      case RULE_SET_CALL:
      {
           List *rule_set = command->rule_set;
           while(rule_set != NULL)
           {
              if(!rule_set->rule_call.rule->is_predicate) return false;
              else rule_set = rule_set->next;
           }
           return true;
      }

      case PROCEDURE_CALL:
           return nullCommand(command->proc_call.procedure->commands);

      case IF_STATEMENT:
           if(!nullCommand(command->cond_branch.then_command)) return false;
           if(!nullCommand(command->cond_branch.else_command)) return false;
           else return true;

      case TRY_STATEMENT:
           if(!nullCommand(command->cond_branch.condition)) return false;
           if(!nullCommand(command->cond_branch.then_command)) return false;
           if(!nullCommand(command->cond_branch.else_command)) return false;
           else return true;

      case ALAP_STATEMENT:
           return nullCommand(command->loop_stmt.loop_body);

      case PROGRAM_OR:
           if(!nullCommand(command->or_stmt.left_command)) return false;
           if(!nullCommand(command->or_stmt.right_command)) return false;
           else return true;

      case BREAK_STATEMENT:
      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
           return true;

      default:
           print_to_log("Error (neverFails): Unexpected command type %d.\n",
                        command->type);
           break;
   }
   return false;
}
