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
              annotate(decl->value.main_program, -1);
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

/* Goes through a command sequence. NULL commands are ignored. 
 * If the first non-NULL command encountered is either a non-predicate rule call
 * or a rule set containing a non-predicate rule, the copy points of all 
 * non-predicate rules are set to true and SIMPLE_COMMAND is returned.
 * If the first non-NULL command encountered is anything else, return
 * NORMAL_COMMAND. 
 * In this way only the rules that are necessary to annotate with copy points
 * are annotated. */
commandType analyseSequence(List *commands, bool copy)
{
   while(commands != NULL)
   {
      bool last_command = commands->next == NULL;
      GPStatement *command = commands->value.command;
      switch(command->statement_type)
      {
         case COMMAND_SEQUENCE:
         {
              commandType type = analyseSequence(command->value.commands, copy);
              if(type != NULL_COMMAND) return type;
              else break;
         }

         case RULE_CALL:
              if(last_command) break;
              if(!command->value.rule_call.rule->is_predicate)
              {
                 if(copy) command->value.rule_call.copy_point = true;
                 return SIMPLE_COMMAND;
              }
              else break;

         case RULE_SET_CALL:
         {
              if(last_command) break;
              commandType type = NULL_COMMAND;
              List *rules = command->value.rule_set;
              while(rules != NULL)
              {
                 if(!rules->value.rule_call.rule->is_predicate) 
                 {
                    if(copy) rules->value.rule_call.copy_point = true;
                    type = SIMPLE_COMMAND;
                 }
                 rules = rules->next;
              }
              if(type == SIMPLE_COMMAND) return SIMPLE_COMMAND;
              else break;
         }

         case PROCEDURE_CALL:
         {
              commandType type =
                 getCommandType(command->value.proc_call.procedure->commands, copy);
              if(type != NULL_COMMAND) return type;
              else break;
         }

         case IF_STATEMENT:

         case TRY_STATEMENT:
              if(getCommandType(command->value.cond_branch.then_stmt, false)
                 != NULL_COMMAND) return NORMAL_COMMAND;
              if(getCommandType(command->value.cond_branch.else_stmt, false)
                 != NULL_COMMAND) return NORMAL_COMMAND;
              else break;

         case ALAP_STATEMENT:
              if(getCommandType(command->value.loop_stmt.loop_body, false)
                 != NULL_COMMAND) return NORMAL_COMMAND;
              else break;

         case PROGRAM_OR:
         {
            if(getCommandType(command->value.or_stmt.left_stmt, false)
               != NULL_COMMAND) return NORMAL_COMMAND;
            if(getCommandType(command->value.or_stmt.right_stmt, false)
               != NULL_COMMAND) return NORMAL_COMMAND;
            else break;
         }

         case SKIP_STATEMENT:

         case FAIL_STATEMENT:
              break;

         default:
              print_to_log("Error (simpleStatement): Unexpected statement type %d.\n",
                           command->statement_type);
              break;
      }
      commands = commands->next;
   }
   return NULL_COMMAND;
}

void annotate(GPStatement *statement, int restore_point)
{
   switch(statement->statement_type)
   {
      case COMMAND_SEQUENCE:
      {
           List *commands = statement->value.commands;
           while(commands != NULL)
           {            
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
           GPProcedure *procedure = statement->value.proc_call.procedure;
           if(restore_point >= 0) procedure->restore_point = restore_point;
           annotate(procedure->commands, restore_point);
           break;
      }

      case IF_STATEMENT:
      { 
           commandType type = getCommandType(statement->value.cond_branch.condition, true); 
           /* If the condition is a normal command, the graph is copied before
            * entering the condition. If the condition is null, the graph does
            * not need to be copied at all. Otherwise, the graph is copied
            * only when required, as determined by the AST annotation. */
           if(type != NULL_COMMAND && type != SINGLE_RULE_CALL)
           {
              statement->value.cond_branch.restore_point = ++restore_point;
              if(type != SIMPLE_COMMAND ||
                 statement->value.cond_branch.condition->statement_type == ALAP_STATEMENT)
                 statement->value.cond_branch.copy_point = true;
           }
           annotate(statement->value.cond_branch.condition, restore_point);
           annotate(statement->value.cond_branch.then_stmt, restore_point);
           annotate(statement->value.cond_branch.else_stmt, restore_point);
           break;
      }

      case TRY_STATEMENT:
      {
           commandType type = getCommandType(statement->value.cond_branch.condition, true); 
           /* As above, except the graph does not need to be copied if the
            * condition is a loop because loops always succeed and try does
            * not backtrack for the 'then' branch. */
           if(type != NULL_COMMAND && type != SINGLE_RULE_CALL &&
              statement->value.cond_branch.condition->statement_type != ALAP_STATEMENT)
           {
              statement->value.cond_branch.restore_point = ++restore_point;
              if(type != SIMPLE_COMMAND) 
                 statement->value.cond_branch.copy_point = true;
           }
           annotate(statement->value.cond_branch.condition, restore_point);
           annotate(statement->value.cond_branch.then_stmt, restore_point);
           annotate(statement->value.cond_branch.else_stmt, restore_point);
           break;
      }

      case ALAP_STATEMENT:
      {
           GPStatement *loop_body = statement->value.loop_stmt.loop_body;
           commandType type = getCommandType(loop_body, true);
           if(type != NULL_COMMAND && type != SINGLE_RULE_CALL)
           {
              statement->value.loop_stmt.restore_point = ++restore_point;
              if(type != SIMPLE_COMMAND) statement->value.loop_stmt.copy_point = true;
           }
           annotate(loop_body, restore_point);
           break;
      }

      case PROGRAM_OR:
      {
           annotate(statement->value.or_stmt.left_stmt, restore_point);
           annotate(statement->value.or_stmt.right_stmt, restore_point);
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

/* Interrogates if/try conditions and loop bodies to determine their "copy type". 
 * Calls analyseSequence on command sequences. */
commandType getCommandType(GPStatement *statement, bool copy)
{
   switch(statement->statement_type)
   {
      case COMMAND_SEQUENCE:
           return analyseSequence(statement->value.commands, copy);

      case RULE_CALL:
           if(statement->value.rule_call.rule->is_predicate) return NULL_COMMAND;
           else return SINGLE_RULE_CALL;

      case RULE_SET_CALL:
      {
           commandType type = NULL_COMMAND;
           List *rules = statement->value.rule_set;
           while(rules != NULL)
           {
              if(!rules->value.rule_call.rule->is_predicate) type = SINGLE_RULE_CALL;
              rules = rules->next;
           }
           return type;
      }

      case PROCEDURE_CALL:
           return getCommandType(statement->value.proc_call.procedure->commands, copy);

      case IF_STATEMENT:

      case TRY_STATEMENT:
           if(getCommandType(statement->value.cond_branch.then_stmt, false) != NULL_COMMAND)
              return NORMAL_COMMAND;
           if(getCommandType(statement->value.cond_branch.else_stmt, false) != NULL_COMMAND)
              return NORMAL_COMMAND;
           else return NULL_COMMAND;

      case ALAP_STATEMENT:
           if(getCommandType(statement->value.loop_stmt.loop_body, false) != NULL_COMMAND)
              return NORMAL_COMMAND;
           else return NULL_COMMAND;

      case PROGRAM_OR:
           if(getCommandType(statement->value.or_stmt.left_stmt, false) != NULL_COMMAND)
              return NORMAL_COMMAND;
           if(getCommandType(statement->value.or_stmt.right_stmt, false) != NULL_COMMAND)
              return NORMAL_COMMAND;
           else return NULL_COMMAND;

      case SKIP_STATEMENT:

      case FAIL_STATEMENT:
           return NULL_COMMAND;

      default:
           print_to_log("Error (simpleStatement): Unexpected statement type %d.\n",
                        statement->statement_type);
           break;
   }
   return NORMAL_COMMAND;
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
              if(!nullStatement(commands->value.command)) 
              {
                 if(commands->next != NULL) return false;
                 else return nullStatement(commands->value.command);
              }
              else commands = commands->next;
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

      case TRY_STATEMENT:
           return false;

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
           print_to_log("Error (simpleStatement): Unexpected statement type %d.\n",
                        statement->statement_type);
           break;
   }
   return false;
}

