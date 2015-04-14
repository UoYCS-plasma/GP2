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
      { 
           copyType type = getCommandType(command->value.cond_branch.condition,
                                          true, false, 0); 
           /* If the condition is a normal command, the graph is copied before
            * entering the condition. If the condition is null, the graph does
            * not need to be copied at all. Otherwise, the graph is copied
            * only when required, as determined by the AST annotation. */
           int if_restore_point = restore_point;
           if(type == RECORD_CHANGES) 
              command->value.cond_branch.roll_back = true;
           if(type == COPY)
           {
              command->value.cond_branch.restore_point = restore_point;
              if_restore_point++;
           }
           annotate(command->value.cond_branch.condition, if_restore_point);
           annotate(command->value.cond_branch.then_command, restore_point);
           annotate(command->value.cond_branch.else_command, restore_point);
           break;
      }

      case TRY_STATEMENT:
      {
           copyType type = getCommandType(command->value.cond_branch.condition,
                                            false, false, 0); 
           /* As above, except the graph does not need to be copied if the
            * condition is a loop because loops always succeed and try does
            * not backtrack for the 'then' branch. */
           int try_restore_point = restore_point;
           if(type == RECORD_CHANGES) 
              command->value.cond_branch.roll_back = true;
           if(type == COPY)
           {
              command->value.cond_branch.restore_point = restore_point;
              try_restore_point++;
           }
           annotate(command->value.cond_branch.condition, try_restore_point);
           annotate(command->value.cond_branch.then_command, restore_point);
           annotate(command->value.cond_branch.else_command, restore_point);
           break;
      }

      case ALAP_STATEMENT:
      {
           GPCommand *loop_body = command->value.loop_stmt.loop_body;
           copyType type = getCommandType(loop_body, false, false, 0);
           int loop_restore_point = restore_point;
           if(type == RECORD_CHANGES)
              command->value.loop_stmt.roll_back = true;
           if(type == COPY)
           {
              command->value.loop_stmt.restore_point = restore_point;
              loop_restore_point++;
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

      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
      case BREAK_STATEMENT:
           break;

      default:
           print_to_log("Error (findRestorePoints): Unexpected command type %d.\n",
                        command->command_type);
           break;
   }
}

copyType getSequenceType(List *commands, bool if_body, int depth)
{ 
   while(commands != NULL)
   {
      int new_depth = 2;
      if(depth == 0 || (depth == 1 && commands->next == NULL))
         new_depth = 1;
      copyType type = getCommandType(commands->value.command, if_body,
                                     new_depth, commands->next == NULL);
      if(type == COPY) return COPY;
      else commands = commands->next;
   }
   return RECORD_CHANGES;
}

/* depth argument!
 * 0 - initial value, not within any command sequence.
 * 1 - within the first/main/top command sequence.
 * 2 - within a nested command sequence. */
copyType getCommandType(GPCommand *command, bool if_body, int depth,
                        bool last_command)
{
   switch(command->command_type)
   {
      case COMMAND_SEQUENCE:
      { 
           List *commands = command->value.commands;
           /* Special case for sequences with one command. */
           if(commands->next == NULL)
              return getCommandType(commands->value.command, if_body, 
                                    depth, true);
           else return getSequenceType(commands, if_body, depth);
      }

      case RULE_CALL:
      case RULE_SET_CALL:
           return NO_COPY;

      case PROCEDURE_CALL:
           return getCommandType(command->value.proc_call.procedure->commands, 
                                 if_body, depth, last_command);

      case IF_STATEMENT:
      case TRY_STATEMENT:
      {
           copyType cond_type = getCommandType(command->value.cond_branch.condition,
                                               if_body, depth, last_command);
           copyType then_type = getCommandType(command->value.cond_branch.then_command,
                                               if_body, depth, last_command);
           copyType else_type = getCommandType(command->value.cond_branch.else_command,
                                               if_body, depth, last_command);
           if(cond_type == COPY || then_type == COPY || else_type == COPY) return COPY;
           else return RECORD_CHANGES;
      }

      /* A loop anywhere in an if body necessitates a deep copy. 
       * A loop in a try body or a loop body necessitates a deep copy
       * unless it is the last command in the body. */
      case ALAP_STATEMENT:
           if(if_body) return COPY;
           else
           {
              if(depth == 2) return COPY;
              if(depth == 1 && !last_command) return COPY;
              if(depth == 1 && last_command)
                 command->value.loop_stmt.stop_recording = true;
           }
           return NO_COPY;

      /* Return the "max" of the two branches. */
      case PROGRAM_OR:
      {
           copyType left_type = getCommandType(command->value.or_stmt.left_command, 
                                               if_body, depth, last_command);
           copyType right_type = getCommandType(command->value.or_stmt.right_command,
                                                if_body, depth, last_command);
           return left_type > right_type ? left_type : right_type;
      }

      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
      case BREAK_STATEMENT:
           return NO_COPY;

      default:
           print_to_log("Error (getCommandType): Unexpected command type %d.\n",
                        command->command_type);
           break;
   }
   return COPY;
}
