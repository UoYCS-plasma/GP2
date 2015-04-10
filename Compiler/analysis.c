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
              annotate(decl->value.main_program);
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

int restore_point = 0;
int roll_back_point = 0;

void annotate(GPStatement *statement)
{
   switch(statement->statement_type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = statement->value.commands;
           while(commands != NULL)
           {            
              annotate(commands->value.command);
              commands = commands->next;
           }
           break;
      }

      case RULE_CALL:

      case RULE_SET_CALL:
           break;

      case PROCEDURE_CALL:
      {
           GPProcedure *procedure = statement->value.proc_call.procedure;
           annotate(procedure->commands);
           break;
      }

      case IF_STATEMENT:
      { 
           copyType type = getCopyType(statement->value.cond_branch.condition,
                                       true, false, 0); 
           /* If the condition is a normal command, the graph is copied before
            * entering the condition. If the condition is null, the graph does
            * not need to be copied at all. Otherwise, the graph is copied
            * only when required, as determined by the AST annotation. */
           if(type == RECORD_CHANGES) 
              statement->value.cond_branch.roll_back_point = roll_back_point++;
           if(type == COPY)
              statement->value.cond_branch.restore_point = restore_point++;
           annotate(statement->value.cond_branch.condition);
           annotate(statement->value.cond_branch.then_stmt);
           annotate(statement->value.cond_branch.else_stmt);
           break;
      }

      case TRY_STATEMENT:
      {
           copyType type = getCopyType(statement->value.cond_branch.condition,
                                       false, false, 0); 
           /* As above, except the graph does not need to be copied if the
            * condition is a loop because loops always succeed and try does
            * not backtrack for the 'then' branch. */
           if(type == RECORD_CHANGES) 
              statement->value.cond_branch.roll_back_point = roll_back_point++;
           if(type == COPY)
              statement->value.cond_branch.restore_point = restore_point++;
           annotate(statement->value.cond_branch.condition);
           annotate(statement->value.cond_branch.then_stmt);
           annotate(statement->value.cond_branch.else_stmt);
           break;
      }

      case ALAP_STATEMENT:
      {
           GPStatement *loop_body = statement->value.loop_stmt.loop_body;
           copyType type = getCopyType(loop_body, false, false, 0);
           if(type == RECORD_CHANGES)
              statement->value.loop_stmt.roll_back_point = roll_back_point++;
           if(type == COPY)
              statement->value.loop_stmt.restore_point = restore_point++;
           annotate(loop_body);
           break;
      }

      case PROGRAM_OR:
      {
           annotate(statement->value.or_stmt.left_stmt);
           annotate(statement->value.or_stmt.right_stmt);
           break;
      }

      case SKIP_STATEMENT:

      case FAIL_STATEMENT:
           break;

      default:
           print_to_log("Error (findRestorePoints): Unexpected statement type %d.\n",
                        statement->statement_type);
           break;
   }
}

/* com_seq argument!
 * 0 - initial value, not within any command sequence.
 * 1 - within the first/main/top command sequence.
 * 2 - within a nested command sequence. */
copyType getCopyType(GPStatement *statement, bool if_body, int com_seq,
                     bool last_command)
{
   switch(statement->statement_type)
   {
      case COMMAND_SEQUENCE:
      { 
           List *commands = statement->value.commands;
           while(commands != NULL)
           {
              int new_com_seq = 2;
              if(com_seq == 0 || (com_seq == 1 && last_command)) new_com_seq = 1;
              copyType type = getCopyType(commands->value.command, if_body,
                                          new_com_seq, commands->next == NULL);
              if(type == COPY) return COPY;
              else commands = commands->next;
           }
           return RECORD_CHANGES;
      }

      case RULE_CALL:

      case RULE_SET_CALL:
           return NO_COPY;

      case PROCEDURE_CALL:
           return getCopyType(statement->value.proc_call.procedure->commands, 
                              if_body, com_seq, last_command);

      case IF_STATEMENT:

      case TRY_STATEMENT:
      {
           copyType cond_type = getCopyType(statement->value.cond_branch.condition,
                                            if_body, com_seq, last_command);
           copyType then_type = getCopyType(statement->value.cond_branch.then_stmt,
                                            if_body, com_seq, last_command);
           copyType else_type = getCopyType(statement->value.cond_branch.else_stmt,
                                            if_body, com_seq, last_command);
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
              if(com_seq == 2) return COPY;
              if(com_seq == 1 && !last_command) return COPY;
              if(com_seq == 1 && last_command)
                 statement->value.loop_stmt.stop_recording = true;
           }
           return NO_COPY;

      /* Return the "max" of the two branches. */
      case PROGRAM_OR:
      {
           copyType left_type = getCopyType(statement->value.or_stmt.left_stmt, 
                                            if_body, com_seq, last_command);
           copyType right_type = getCopyType(statement->value.or_stmt.right_stmt,
                                             if_body, com_seq, last_command);
           return left_type > right_type ? left_type : right_type;
      }

      case SKIP_STATEMENT:

      case FAIL_STATEMENT:
           return NO_COPY;

      default:
           print_to_log("Error (simpleStatement): Unexpected statement type %d.\n",
                        statement->statement_type);
           break;
   }
   return COPY;
}

bool nullStatement(GPStatement *statement)
{
   switch(statement->statement_type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = statement->value.commands;
           while(commands != NULL)
           {
              if(!nullStatement(commands->value.command)) return false;
              commands = commands->next;
           }
           return true;
      }
      case RULE_CALL:
           if(statement->value.rule_call.rule->is_predicate) return true;
           break;

      case RULE_SET_CALL:
      {
           /* If one non-predicate rule exists, then the rule set is not a null
            * statement. */
           List *rules = statement->value.rule_set;
           while(rules != NULL)
           {
              if(!rules->value.rule_call.rule->is_predicate) return false;
              rules = rules->next;
           }
           return true;
      }

      case PROCEDURE_CALL:
           return nullStatement(statement->value.proc_call.procedure->commands);

      case IF_STATEMENT:
           if(!nullStatement(statement->value.cond_branch.then_stmt)) return false;
           if(!nullStatement(statement->value.cond_branch.else_stmt)) return false;
           return true;

      case TRY_STATEMENT:
           if(!nullStatement(statement->value.cond_branch.condition)) return false;
           if(!nullStatement(statement->value.cond_branch.then_stmt)) return false;
           if(!nullStatement(statement->value.cond_branch.else_stmt)) return false;
           return true;

      case ALAP_STATEMENT:
           if(nullStatement(statement->value.loop_stmt.loop_body))
           {
              print_to_console("Warning: There may be a nonterminating loop in "
                               "your program!\n\n");
              YYLTYPE location = statement->location;
              print_to_log("%d.%d-%d.%d: Warning: Possible nonterminating loop!\n\n", 
                           location.first_line, location.first_column, 
                           location.last_line, location.last_column);
              return true;
           }
           else return false;

      case PROGRAM_OR:
           if(!nullStatement(statement->value.or_stmt.left_stmt)) return false;
           if(!nullStatement(statement->value.or_stmt.right_stmt)) return false;
           return true;

      case SKIP_STATEMENT:

      case FAIL_STATEMENT:
           return true;

      default:
           print_to_log("Error (nullStatement): Unexpected statement type %d.\n",
                        statement->statement_type);
           break;
   }
   return false;
}

