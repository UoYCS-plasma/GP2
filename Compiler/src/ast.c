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

#include "ast.h" 

List *makeGPList(YYLTYPE location, ListType type)
{
   List *list = malloc(sizeof(List));
   if(list == NULL)
   {
     print_to_log("Error (makeGPList): malloc failure.\n");
     exit(1);
   }
   list->location = location;
   list->type = type;
   return list;
}

int getASTListLength(List *list)
{
   int length = 0;
   while(list != NULL)
   {
      length++;
      list = list->next;
   }
   return length;
}

List *addASTDecl(ListType type, YYLTYPE location, GPDeclaration *declaration,
	         List *next)
{ 
    List *new_decl = makeGPList(location, type);
    new_decl->declaration = declaration;
    new_decl->next = next;
    return new_decl;
}

/* The programs r1; r2; r3 and r1; (r2; r3) are semantically equivalent, but they
 * generate different parse trees. This is because the brackets trigger a call
 * to make a new command sequence, which is correct behaviour in most instances,
 * but not in others, such as the above (which could occur indirectly from a
 * procedure call in the program text). The resulting trees are below, where C
 * is a List pointing to a GPCommand and the next List node, and Cs is a
 * GPCommand pointing to the head of a List of GPCommand.
 *
 * C -> C -> C -> NULL   C -> C (-> r1) -> NULL
 * |    |    |           | 
 * r3   r2   r1          Cs -> C -> C -> NULL
 *                             |    |
 *                             r3   r2
 *
 * I would like the tree generated in the second case to match the first one.
 * In the second case, the command pointer argument passed is the pointer to Cs
 * in the right-hand parse tree above. The list pointer argument is the pointer
 * to C (-> r1) -> NULL. The list pointer should be appended to the list on the
 * second row of the right-hand tree if the command argument is a command sequence. */

List *addASTCommand(YYLTYPE location, GPCommand *command, List *next)
{
    /* The if body handles that nasty stuff described in the comment above. */
    if(command->type == COMMAND_SEQUENCE)
    {
       List *commands = command->commands, *iterator = command->commands;
       while(iterator->next != NULL) iterator = iterator->next;
       iterator->next = next;
       /* The child of 'command' (the Cs node in the diagram) is returned
        * to the parser. The parent is no longer needed. Free the parent! */
       free(command);
       return commands;
    }
    List *new_commands = makeGPList(location, COMMANDS);
    new_commands->command = command;
    new_commands->next = next;
    return new_commands;
}

List *addASTRule(YYLTYPE location, string rule_name, List *next)
{ 
    List *new_rule = makeGPList(location, RULES);
    new_rule->rule_call.rule_name = strdup(rule_name);
    new_rule->rule_call.rule = NULL;
    new_rule->next = next;
    return new_rule;
}

List *addASTVariableDecl(ListType type, YYLTYPE location, List *variables,
	                 List *next)
{ 
    List *new_var_decl = makeGPList(location, type);
    new_var_decl->variables = variables;
    new_var_decl->next = next;
    return new_var_decl;
}

List *addASTVariable(YYLTYPE location, string variable_name, List *next)
{ 
    List *new_var = makeGPList(location, VARIABLE_LIST);
    new_var->variable_name = strdup(variable_name);
    new_var->next = next;
    return new_var;
}

List *addASTNodeID(YYLTYPE location, string node_id, List *next)
{ 
    List *new_id = makeGPList(location, INTERFACE_LIST);
    new_id->node_id = strdup(node_id);
    new_id->next = next;
    return new_id;
}

List *addASTNode(YYLTYPE location, GPNode *node, List *next)
{
    List *new_node = makeGPList(location, NODE_LIST);
    new_node->node = node;
    new_node->next = next;
    return new_node;
}
      
List *addASTEdge(YYLTYPE location, GPEdge *edge, List *next)
{
    List *new_edge = makeGPList(location, EDGE_LIST);
    new_edge->edge = edge;
    new_edge->next = next;
    return new_edge;
}

List *addASTAtom(YYLTYPE location, GPAtom *atom, List *next)
{
    List *new_atom = makeGPList(location, GP_LIST);
    new_atom->atom = atom;
    new_atom->next = next;
    return new_atom;
}

GPDeclaration *makeGPDeclaration(YYLTYPE location, DeclType type)
{
   GPDeclaration *declaration = malloc(sizeof(GPDeclaration));
   if(declaration == NULL)
   {
      print_to_log("Error (makeGPDeclaration): malloc failure.\n");
      exit(1);
   }
   declaration->location = location;
   declaration->type = type;
   return declaration;
}

GPDeclaration *newASTMainDecl(YYLTYPE location, GPCommand *main_program)
{
   GPDeclaration *new_main = makeGPDeclaration(location, MAIN_DECLARATION);
   new_main->main_program = main_program;
   return new_main;
}

GPDeclaration *newASTProcedureDecl(YYLTYPE location, GPProcedure *procedure)
{
   GPDeclaration *new_proc = makeGPDeclaration(location, PROCEDURE_DECLARATION);
   new_proc->procedure = procedure;
   return new_proc;
}

GPDeclaration *newASTRuleDecl(YYLTYPE location, GPRule *rule)
{
   GPDeclaration *new_rule = makeGPDeclaration(location, RULE_DECLARATION);
   new_rule->rule = rule;
   return new_rule;
}

GPCommand *makeGPCommand(YYLTYPE location, CommandType type)
{
   GPCommand *command = malloc(sizeof(GPCommand));
   if(command == NULL)
   {
      print_to_log("Error (makeGPCommand): malloc failure.\n");
      exit(1);
   }
   command->location = location;
   command->type = type;
   return command;
}

GPCommand *newASTCommandSequence(YYLTYPE location, List *commands)
{
   GPCommand *command = makeGPCommand(location, COMMAND_SEQUENCE);
   command->commands = commands;
   command->reversed = false;
   return command;
}

GPCommand *newASTRuleCall(YYLTYPE location, string rule_name)
{
   GPCommand *command = makeGPCommand(location, RULE_CALL);
   command->rule_call.rule_name = strdup(rule_name);
   command->rule_call.rule = NULL;
   return command;
}

GPCommand *newASTRuleSetCall(YYLTYPE location, List *rule_set)
{
   GPCommand *command = makeGPCommand(location, RULE_SET_CALL);
   command->rule_set = rule_set;
   return command;
}

GPCommand *newASTProcCall(YYLTYPE location, string proc_name)
{
   GPCommand *command = makeGPCommand(location, PROCEDURE_CALL);
   command->proc_call.proc_name = strdup(proc_name);
   command->proc_call.procedure = NULL;
   return command;
}

GPCommand *newASTCondBranch(CommandType type, YYLTYPE location, 
	                    GPCommand *condition, GPCommand *then_command, 
                            GPCommand *else_command)
{
   GPCommand *command = makeGPCommand(location, type);
   command->cond_branch.condition = condition;
   command->cond_branch.then_command = then_command;
   command->cond_branch.else_command = else_command;
   command->cond_branch.record_changes = false;
   return command;
}

GPCommand *newASTAlap(YYLTYPE location, GPCommand *loop_body)
{
   GPCommand *command = makeGPCommand(location, ALAP_STATEMENT);
   command->loop_stmt.loop_body = loop_body;
   command->loop_stmt.record_changes = false;
   command->loop_stmt.stop_recording = false;
   return command;
}

GPCommand *newASTOrStmt(YYLTYPE location, GPCommand *left_command, 
	                GPCommand *right_command)
{
   GPCommand *command = makeGPCommand(location, PROGRAM_OR);
   command->or_stmt.left_command = left_command;
   command->or_stmt.right_command = right_command;
   return command;
}

GPCommand *newASTSkip(YYLTYPE location)
{
   GPCommand *command = makeGPCommand(location, SKIP_STATEMENT);
   return command;
}

GPCommand *newASTEmptyStatement(YYLTYPE location, CommandType type)
{
   GPCommand *command = makeGPCommand(location, type);
   return command;
}


GPCondition *makeGPCondition(YYLTYPE location, CommandType type)
{
   GPCondition *condition = malloc(sizeof(GPCondition));
   if(condition == NULL)
   {
      print_to_log("Error (makeGPCondition): malloc failure.\n");
      exit(1);
   }
   condition->location = location;
   condition->type = type;
   return condition;
}

GPCondition *newASTSubtypePred(ConditionType type, YYLTYPE location, string var)
{
    GPCondition *condition = makeGPCondition(location, type);
    condition->var = strdup(var);
    return condition;
}

GPCondition *newASTEdgePred(YYLTYPE location, string source, string target, 
	                  GPLabel *label)
{
    GPCondition *condition = makeGPCondition(location, EDGE_PRED);
    condition->edge_pred.source = strdup(source);
    condition->edge_pred.target = strdup(target);
    condition->edge_pred.label = label;
    return condition;
}

GPCondition *newASTListComparison(ConditionType type, YYLTYPE location,
	                          List *left_list, List *right_list)
{
    GPCondition *condition = makeGPCondition(location, type);
    condition->list_cmp.left_list = left_list;
    condition->list_cmp.right_list = right_list;
    return condition;
}

GPCondition *newASTAtomComparison(ConditionType type, YYLTYPE location,
	                          GPAtom *left_exp, GPAtom *right_exp)
{
    GPCondition *condition = makeGPCondition(location, type);
    condition->atom_cmp.left_exp = left_exp;
    condition->atom_cmp.right_exp = right_exp;
    return condition;
}

GPCondition *newASTNotExp(YYLTYPE location, GPCondition *not_exp)
{
    GPCondition *condition = makeGPCondition(location, BOOL_NOT);
    condition->not_exp = not_exp;
    return condition;
}

GPCondition *newASTBinaryExp(ConditionType type, YYLTYPE location, 
                           GPCondition *left_exp, GPCondition *right_exp)
{
    GPCondition *condition = makeGPCondition(location, type);
    condition->bin_exp.left_exp = left_exp;
    condition->bin_exp.right_exp = right_exp;
    return condition;
}

GPAtom *makeGPAtom(YYLTYPE location, AtomType type)
{
   GPAtom *atom = malloc(sizeof(GPAtom));
   if(atom == NULL)
   {
      print_to_log("Error (makeGPAtom): malloc failure.\n");
      exit(1);
   }
   atom->location = location;
   atom->type = type;
   return atom;
}

GPAtom *newASTVariable(YYLTYPE location, string name)
{
    GPAtom *atom = makeGPAtom(location, VARIABLE);
    atom->variable.name = strdup(name);
    atom->variable.type = LIST_VAR;
    return atom;
}

GPAtom *newASTNumber(YYLTYPE location, int number)
{
    GPAtom *atom = makeGPAtom(location, INTEGER_CONSTANT);
    atom->number = number;
    return atom;
}

GPAtom *newASTString(YYLTYPE location, string string)
{
    GPAtom *atom = makeGPAtom(location, STRING_CONSTANT);
    if(string) atom->string = strdup(string);
    else atom->string = NULL;
    return atom;
}

GPAtom *newASTDegreeOp(AtomType type, YYLTYPE location, string node_id)
{
    GPAtom *atom = makeGPAtom(location, type);
    atom->node_id = strdup(node_id);
    return atom;
}

GPAtom *newASTLength(YYLTYPE location, string name)
{
    GPAtom *atom = makeGPAtom(location, LENGTH);
    atom->variable.name = strdup(name);
    atom->variable.type = LIST_VAR;
    return atom;
}

GPAtom *newASTNegExp(YYLTYPE location, GPAtom *neg_exp)
{
    GPAtom *atom = makeGPAtom(location, INTEGER_CONSTANT);
    /* If the passed GPAtom is an integer constant, create a new integer
     * constant GPAtom containing the negation of that constant.
     * The passed GPAtoms is freed. */
    if(neg_exp->type == INTEGER_CONSTANT)
    {
       atom->number = -(neg_exp->number);
       freeASTAtom(neg_exp);
    }
    else
    {
       atom->type = NEG;
       atom->neg_exp = neg_exp;
    }
    return atom;
}

GPAtom *newASTBinaryOp(AtomType type, YYLTYPE location, 
                       GPAtom *left_exp, GPAtom *right_exp)
{
    GPAtom *atom = makeGPAtom(location, INTEGER_CONSTANT);
    if(type == DIVIDE && right_exp->type == INTEGER_CONSTANT)
    {
       if(right_exp->number == 0)
       {
          print_error("Error: You are trying to divide by 0. I cannot allow "
                      "you to destroy the universe. Abort!\n");
          exit(1);
       }
    }
    /* If both of the passed GPAtoms are integer constants, create a new
     * integer constant GPAtom containing the appropriate integer. The passed
     * GPAtoms are freed. */
    if(left_exp->type == INTEGER_CONSTANT && right_exp->type == INTEGER_CONSTANT)
    {
       switch(type)
       {
          case ADD:
               atom->number = left_exp->number + right_exp->number;
               break;
          
          case SUBTRACT:
               atom->number = left_exp->number - right_exp->number;
               break;

          case MULTIPLY:
               atom->number = left_exp->number * right_exp->number;
               break;

          case DIVIDE:
               atom->number = left_exp->number / right_exp->number;
               break;

          default:
               print_to_log("Error (newASTBinaryOp): Unexpected atom type %d.\n",
                            type);
               exit(0);
       }
       freeASTAtom(left_exp);
       freeASTAtom(right_exp);
    }
    else
    {
       atom->type = type; 
       atom->bin_op.left_exp = left_exp;
       atom->bin_op.right_exp = right_exp;
    }
    return atom;
}

GPAtom *newASTConcat(YYLTYPE location, GPAtom *left_exp, GPAtom *right_exp)
{
    GPAtom *atom = makeGPAtom(location, STRING_CONSTANT);
    /* If both of the passed GPAtoms are string constants, create a new string
     * constant GPAtom containing the concatenated string. The passed GPAtoms 
     * are freed. */
    if(left_exp->type == STRING_CONSTANT && right_exp->type == STRING_CONSTANT)
    {
       int length = strlen(left_exp->string) + strlen(right_exp->string) + 1;
       char new_string[length];
       strcpy(new_string, left_exp->string);
       strcat(new_string, right_exp->string);
       atom->string = strdup(new_string);
       freeASTAtom(left_exp);
       freeASTAtom(right_exp);
    }
    else
    {
       atom->type = CONCAT; 
       atom->bin_op.left_exp = left_exp;
       atom->bin_op.right_exp = right_exp;
    }
    return atom;
}

GPProcedure *newASTProcedure(YYLTYPE location, string name, List *local_decls,
                             GPCommand *commands)
{
    GPProcedure *proc = malloc(sizeof(GPProcedure));
    if(proc == NULL)  
    {
      print_to_log("Error (AST): malloc failure.\n");
      exit(1);
    }
    proc->node_type = PROCEDURE;
    proc->location = location;
    proc->name = strdup(name);
    proc->local_decls = local_decls;
    proc->commands = commands;
    return proc;
}
 
GPRule *newASTRule(YYLTYPE location, string name, List *variables,
	           GPGraph *lhs, GPGraph *rhs, List *interface, 
		   GPCondition *condition)
{
    GPRule *rule = malloc(sizeof(GPRule));
    if(rule == NULL) 
    {
      print_to_log("Error (AST): malloc failure.\n");
      exit(1);
    }
    rule->id = 0;
    rule->node_type = RULE;
    rule->location = location;
    rule->name = strdup(name);
    rule->variables = variables;
    rule->lhs = lhs;
    rule->rhs = rhs;
    rule->interface = interface;
    rule->condition = condition;
    rule->left_nodes = 0;
    rule->left_edges = 0;
    rule->variable_count = 0;
    rule->predicate_count = 0;
    rule->empty_lhs = false;
    rule->is_predicate = false;
    return rule;
}    

GPGraph *newASTGraph(YYLTYPE location, List *nodes, List *edges)
{
    GPGraph *graph = malloc(sizeof(GPGraph));
    if(graph == NULL) 
    {
      print_to_log("Error (AST): malloc failure.\n");
      exit(1);
    }
    graph->node_type = GRAPH;
    graph->location = location;
    graph->nodes = nodes;
    graph->edges = edges;
    return graph;
}

int countNodes(GPGraph *graph)
{
   int nodes = 0;
   List *iterator;
   for(iterator = graph->nodes; iterator != NULL; iterator = iterator->next) 
       nodes++;
   return nodes;
}

int countEdges(GPGraph *graph)
{
   int edges = 0;
   List *iterator;
   for(iterator = graph->edges; iterator != NULL; iterator = iterator->next) 
       edges++;
   return edges;
}

GPNode *newASTNode(YYLTYPE location, bool root, string name, GPLabel *label)
{
    GPNode *node = malloc(sizeof(GPNode));
    if(node == NULL) 
    {
      print_to_log("Error (AST): malloc failure.\n");
      exit(1);
    }
    node->node_type = NODE;
    node->location = location;
    node->root = root;
    node->name = strdup(name);
    node->label = label;
    return node;
}

GPEdge *newASTEdge(YYLTYPE location, bool bidirectional, string name, 
                   string source, string target, GPLabel *label)
{
    GPEdge *edge = malloc(sizeof(GPEdge));
    if(edge == NULL) 
    {
      print_to_log("Error (AST): malloc failure.\n");
      exit(1);
    }
    edge->node_type = EDGE;
    edge->location = location;
    edge->bidirectional = bidirectional;
    edge->name = strdup(name);
    edge->source = strdup(source);
    edge->target = strdup(target);
    edge->label = label;
    return edge;
}

GPLabel *newASTLabel(YYLTYPE location, MarkType mark, List *gp_list)
{
    GPLabel *label = malloc(sizeof(GPLabel));
    if(label == NULL) 
    {
      print_to_log("Error (AST): malloc failure.\n");
      exit(1);
    }
    label->node_type = LABEL;
    label->location = location;
    label->mark = mark;
    label->gp_list = gp_list;
    return label;
}

List *reverse (List *head) 
{
   List *currentNode = head;
   List *tempNode = NULL;
   List *previousNode = NULL;

   /* invariant: currentNode points to the node being worked on and
    * previousNode points to the original parent of currentNode. */
   while(currentNode != NULL) 
   {
      /* Maintain a pointer to currentNode->next before reassignment. */
      tempNode = currentNode->next; 
      /* reversing the 'next' pointer of currentNode. */
      currentNode->next = previousNode; 
      /* setting the invariant for the next iteration */
      previousNode = currentNode;
      currentNode = tempNode;
   }
   /* Return the tail of the original list i.e. the head of the reversed 
    * list. */
   return previousNode;
}     

void reverseGraphAST(GPGraph *graph)
{
   if(graph->nodes)
   {
      graph->nodes = reverse(graph->nodes);  
      List *iterator = graph->nodes;
      while(iterator) 
      {
           iterator->node->label->gp_list = 
             reverse(iterator->node->label->gp_list);
           iterator = iterator->next;
      }
   }
   if(graph->edges)
   {
      graph->edges = reverse(graph->edges);
      List *iterator = graph->edges;

      while(iterator)  
      {
           iterator->edge->label->gp_list = 
             reverse(iterator->edge->label->gp_list);
           iterator = iterator->next;
      }
   }
}

void freeAST(List *ast) 
{
   if(!ast) return;
   switch(ast->type) 
   {
	case GLOBAL_DECLARATIONS:
        case LOCAL_DECLARATIONS:
	     if(ast->declaration) 
               freeASTDeclaration(ast->declaration);
	     break;	

	case COMMANDS:
             if(ast->command) freeASTCommand(ast->command);
	     break;	

	case RULES:
             if(ast->rule_call.rule_name) free(ast->rule_call.rule_name);
	     break;

	case INT_DECLARATIONS:
        case CHAR_DECLARATIONS:
	case STRING_DECLARATIONS:
	case ATOM_DECLARATIONS:
	case LIST_DECLARATIONS:
             if(ast->variables) freeAST(ast->variables);
	     break;
	
	case VARIABLE_LIST:
             if(ast->variable_name) free(ast->variable_name);
	     break;
	
	case INTERFACE_LIST:
             if(ast->node_id) free(ast->node_id);
	     break;
	
	case NODE_LIST:
             if(ast->node) freeASTNode(ast->node);
	     break;
	
	case EDGE_LIST:
             if(ast->edge) freeASTEdge(ast->edge);
	     break;

	case GP_LIST:
             if(ast->atom) freeASTAtom(ast->atom);
	     break;

	default: print_to_log("Error (freeAST): Unexpected Type: %d\n",
                              (int)ast->type); 
                 break;	 
	}
   if(ast->next) freeAST(ast->next);
   free(ast);
}

void freeASTDeclaration(GPDeclaration *decl)
{
   if(decl == NULL) return;
   switch(decl->type) 
   {
      case MAIN_DECLARATION:
           if(decl->main_program) 
             freeASTCommand(decl->main_program);
           break;

      case PROCEDURE_DECLARATION:
           if(decl->procedure) freeASTProcedure(decl->procedure);
           break;

      case RULE_DECLARATION:
           if(decl->rule) freeASTRule(decl->rule);
           break;

      default: print_to_log("Error (freeASTDeclaration): Unexpected Type: "
                          "%d\n", (int)decl->type);
                break;
   }
   free(decl);
}

void freeASTCommand(GPCommand *command)
{
   if(command == NULL) return;
   switch(command->type) 
   {
      case COMMAND_SEQUENCE:	
           if(command->commands) freeAST(command->commands);
           break;

      case RULE_CALL:
           if(command->rule_call.rule_name) free(command->rule_call.rule_name);
           break;

      case RULE_SET_CALL:
           if(command->rule_set) freeAST(command->rule_set);
           break;

      case PROCEDURE_CALL:
           if(command->proc_call.proc_name) free(command->proc_call.proc_name);
           break;

      case IF_STATEMENT:
      case TRY_STATEMENT:
           if(command->cond_branch.condition) 
             freeASTCommand(command->cond_branch.condition);
           if(command->cond_branch.then_command) 
             freeASTCommand(command->cond_branch.then_command);
           if(command->cond_branch.else_command) 
             freeASTCommand(command->cond_branch.else_command);
           break;

      case ALAP_STATEMENT:
           if(command->loop_stmt.loop_body) 
              freeASTCommand(command->loop_stmt.loop_body);
           break;

      case PROGRAM_OR:
           if(command->or_stmt.left_command) 
             freeASTCommand(command->or_stmt.left_command);
           if(command->or_stmt.right_command) 
             freeASTCommand(command->or_stmt.right_command);
           break;

      case SKIP_STATEMENT:
      case FAIL_STATEMENT:
      case BREAK_STATEMENT:
           break;
      
      default: print_to_log("Error (freeASTCommand): Unexpected type: %d\n",
                            (int)command->type); 
               break;
      }
   free(command);
}

void freeASTCondition(GPCondition *condition)
{
   if(condition == NULL) return;
   switch(condition->type) 
   {
      case INT_CHECK:
      case STRING_CHECK:
      case ATOM_CHECK:
           if(condition->var) free(condition->var);
           break;

      case EDGE_PRED:
           if(condition->edge_pred.source) free(condition->edge_pred.source);
	   if(condition->edge_pred.target) free(condition->edge_pred.target);
	   if(condition->edge_pred.label) freeASTLabel(condition->edge_pred.label);
           break;

      case EQUAL:
      case NOT_EQUAL:
           if(condition->list_cmp.left_list) freeAST(condition->list_cmp.left_list);
           if(condition->list_cmp.right_list) freeAST(condition->list_cmp.right_list);
           break;

      case GREATER:
      case GREATER_EQUAL:
      case LESS:
      case LESS_EQUAL:
           if(condition->atom_cmp.left_exp) freeASTAtom(condition->atom_cmp.left_exp);
           if(condition->atom_cmp.right_exp) freeASTAtom(condition->atom_cmp.right_exp);
           break;	  

      case BOOL_NOT:
           if(condition->not_exp) freeASTCondition(condition->not_exp);
	   break;

      case BOOL_OR:
      case BOOL_AND:
           if(condition->bin_exp.left_exp) freeASTCondition(condition->bin_exp.left_exp);
           if(condition->bin_exp.right_exp) freeASTCondition(condition->bin_exp.right_exp);
      break;

      default: print_to_log("Error (freeASTCondition): Unexpected type: %d\n",
                              (int)condition->type); 
               break;
	}
   free(condition);
}

void freeASTAtom(GPAtom *atom)
{
   if(atom == NULL) return;
   switch(atom->type) 
   {
      case VARIABLE:
           if(atom->variable.name) free(atom->variable.name);
           break;

      case INTEGER_CONSTANT:
           break;

      case STRING_CONSTANT:
           if(atom->string) free(atom->string);
           break;

      case INDEGREE:
      case OUTDEGREE:
           if(atom->node_id) free(atom->node_id);
           break;

      case LENGTH:
           if(atom->variable.name) free(atom->variable.name);
           break;

      case NEG:
           if(atom->neg_exp) freeASTAtom(atom->neg_exp);
           break;

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case CONCAT:
           if(atom->bin_op.left_exp) freeASTAtom(atom->bin_op.left_exp);
           if(atom->bin_op.right_exp) freeASTAtom(atom->bin_op.right_exp);
           break;

      default: print_to_log("Error (freeASTAtom): Unexpected type: %d\n",
                            (int)atom->type); 
               break;
      }
   free(atom);
}

void freeASTProcedure(GPProcedure *proc)
{
   if(proc == NULL) return;
   if(proc->name) free(proc->name);
   if(proc->local_decls) freeAST(proc->local_decls);
   if(proc->commands) freeASTCommand(proc->commands);
   free(proc);
}

void freeASTRule(GPRule *rule)
{
   if(rule == NULL) return;
   if(rule->name) free(rule->name);
   if(rule->variables) freeAST(rule->variables);
   if(rule->lhs) freeASTGraph(rule->lhs);  
   if(rule->rhs) freeASTGraph(rule->rhs);
   if(rule->interface) freeAST(rule->interface);
   if(rule->condition) freeASTCondition(rule->condition);
   free(rule);
}

void freeASTGraph(GPGraph *graph)
{
   if(graph == NULL) return;
   if(graph->nodes) freeAST(graph->nodes);
   if(graph->edges) freeAST(graph->edges);
   free(graph);
}

void freeASTNode(GPNode *node)
{
   if(node == NULL) return;
   if(node->name) free(node->name);
   if(node->label) freeASTLabel(node->label);
   free(node);
}

void freeASTEdge(GPEdge *edge)
{ 
   if(edge == NULL) return;
   if(edge->name) free(edge->name);
   if(edge->source) free(edge->source);
   if(edge->target) free(edge->target);
   if(edge->label) freeASTLabel(edge->label);
   free(edge);
}

void freeASTLabel(GPLabel *label)
{
   if(label == NULL) return;
   if(label->gp_list) freeAST(label->gp_list);
   free(label);
}
  
