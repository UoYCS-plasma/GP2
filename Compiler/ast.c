/* ///////////////////////////////////////////////////////////////////////////

  ==============================
  ast.c - Chris Bak (22/07/2013)
  ==============================

/////////////////////////////////////////////////////////////////////////// */

#include "ast.h" 

List *addASTDecl(ListType list_type, YYLTYPE location, GPDeclaration *declaration,
	         List *next)
{ 
    List *new_decl = malloc(sizeof(List));
    
    if(new_decl == NULL)
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    /* list_type: GLOBAL_DECLARATIONS, LOCAL_DECLARATIONS */
    new_decl->list_type = list_type; 
    new_decl->location = location;
    new_decl->value.declaration = declaration;
    new_decl->next = next;

    return new_decl;
}

List *addASTCommand(YYLTYPE location, GPStatement *command, List *next)
{ 
    List *new_command = malloc(sizeof(List));
    
    if(new_command == NULL)  
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    new_command->list_type = COMMANDS;
    new_command->location = location;
    new_command->value.command = command;
    new_command->next = next;

    return new_command;
}

List *addASTRule(YYLTYPE location, string rule_name, List *next)
{ 
    List *new_rule = malloc(sizeof(List));
    
    if(new_rule == NULL)
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    new_rule->list_type = RULES;
    new_rule->location = location;
    new_rule->value.rule_call.rule_name = strdup(rule_name);
    new_rule->value.rule_call.copy_point = false;
    new_rule->value.rule_call.rule = NULL;
    new_rule->next = next;

    return new_rule;
}

List *addASTVariableDecl(ListType list_type, YYLTYPE location, List *variables,
	                 List *next)
{ 
    List *new_var_decl = malloc(sizeof(List));
    
    if(new_var_decl == NULL)
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    /* list_type: INT_DECLARATIONS, CHAR_DECLARATIONS, STRING_DECLARATIONS, 
     * ATOM_DECLARATIONS, LIST_DECLARATIONS */
    new_var_decl->list_type = list_type; 
    new_var_decl->location = location;
    new_var_decl->value.variables = variables;
    new_var_decl->next = next;

    return new_var_decl;
}

List *addASTVariable(YYLTYPE location, string variable_name, List *next)
{ 
    List *new_var = malloc(sizeof(List));
    
    if(new_var == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    new_var->list_type = VARIABLE_LIST;
    new_var->location = location;
    new_var->value.variable_name = strdup(variable_name);
    new_var->next = next;

    return new_var;
}

List *addASTNodeID(YYLTYPE location, string node_id, List *next)
{ 
    List *new_pair = malloc(sizeof(List));
    
    if(new_pair == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    new_pair->list_type = INTERFACE_LIST;
    new_pair->location = location;
    new_pair->value.node_id = strdup(node_id);
    new_pair->next = next;

    return new_pair;
}

List *addASTNode(YYLTYPE location, GPNode *node, List *next)
{
     List *new_node = malloc(sizeof(List));
     
     if(new_node == NULL)
     {
	print_to_log("Error: Memory exhausted during AST construction.\n");
        exit(1);
     }

     new_node->list_type = NODE_LIST;
     new_node->location = location;
     new_node->value.node = node;
     new_node->next = next;

     return new_node;
}
      
List *addASTEdge(YYLTYPE location, GPEdge *edge, List *next)
{
     List *new_edge = malloc(sizeof(List));
     
     if(new_edge == NULL)
     {
	print_to_log("Error: Memory exhausted during AST construction.\n");
        exit(1);
     }

     new_edge->list_type = EDGE_LIST;
     new_edge->location = location;
     new_edge->value.edge = edge;
     new_edge->next = next;

     return new_edge;
}

List *addASTAtom(YYLTYPE location, GPAtomicExp *atom, List *next)
{
    List *new_atom = malloc(sizeof(List));
   
    if(new_atom == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    new_atom->list_type = GP_LIST;
    new_atom->location = location;
    new_atom->value.atom = atom;
    new_atom->next = next;

    return new_atom;
}

List *addASTEmptyList(YYLTYPE location)
{
     List *new_empty = malloc(sizeof(List));

     if(new_empty == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     new_empty->list_type = EMPTY_LIST;
     new_empty->location = location;
     new_empty->next = NULL;

     return new_empty;
}



GPDeclaration *newASTMainDecl(YYLTYPE location, GPStatement *main_program)
{
    GPDeclaration *new_main = malloc(sizeof(GPDeclaration));
   
    if(new_main == NULL)
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    new_main->decl_type = MAIN_DECLARATION;
    new_main->location = location;
    new_main->value.main_program = main_program;

    return new_main;
}

GPDeclaration *newASTProcedureDecl(YYLTYPE location, GPProcedure *procedure)
{
    GPDeclaration *new_proc = malloc(sizeof(GPDeclaration));
   
    if(new_proc == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    new_proc->decl_type = PROCEDURE_DECLARATION;
    new_proc->location = location;
    new_proc->value.procedure = procedure;

    return new_proc;
}

GPDeclaration *newASTRuleDecl(YYLTYPE location, GPRule *rule)
{
    GPDeclaration *new_rule = malloc(sizeof(GPDeclaration));
   
    if(new_rule == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    new_rule->decl_type = RULE_DECLARATION;
    new_rule->location = location;
    new_rule->value.rule = rule;

    return new_rule;
}



GPStatement *newASTCommandSequence(YYLTYPE location, List *commands)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    stmt->statement_type = COMMAND_SEQUENCE;
    stmt->location = location;
    stmt->value.commands = commands;

    return stmt;
}

GPStatement *newASTRuleCall(YYLTYPE location, string rule_name)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    stmt->statement_type = RULE_CALL;
    stmt->location = location;
    stmt->value.rule_call.rule_name = strdup(rule_name);
    stmt->value.rule_call.copy_point = false;
    stmt->value.rule_call.rule = NULL;
    return stmt;
}

GPStatement *newASTRuleSetCall(YYLTYPE location, List *rule_set)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    stmt->statement_type = RULE_SET_CALL;
    stmt->location = location;
    stmt->value.rule_set = rule_set;

    return stmt;
}

GPStatement *newASTProcCall(YYLTYPE location, string proc_name)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    stmt->statement_type = PROCEDURE_CALL;
    stmt->location = location;
    stmt->value.proc_call.proc_name = strdup(proc_name);
    stmt->value.proc_call.procedure = NULL;

    return stmt;
}

GPStatement *newASTCondBranch(StatementType statement_type, YYLTYPE location, 
	                      GPStatement *condition, GPStatement *then_stmt, 
                              GPStatement *else_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    /* statement_type: IF_STATEMENT, TRY_STATEMENT */
    stmt->statement_type = statement_type; 
    stmt->location = location;
    stmt->value.cond_branch.condition = condition;
    stmt->value.cond_branch.then_stmt = then_stmt;
    stmt->value.cond_branch.else_stmt = else_stmt;
    stmt->value.cond_branch.restore_point = -1;
    stmt->value.cond_branch.roll_back_point = -1;
    stmt->value.cond_branch.copy_point = false;
    return stmt;
}

GPStatement *newASTAlap(YYLTYPE location, GPStatement *loop_body)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    stmt->statement_type = ALAP_STATEMENT;
    stmt->location = location;
    stmt->value.loop_stmt.loop_body = loop_body;
    stmt->value.loop_stmt.restore_point = -1;
    stmt->value.loop_stmt.roll_back_point = -1;
    stmt->value.loop_stmt.copy_point = false;
    stmt->value.loop_stmt.stop_recording = false;

    return stmt;
}

GPStatement *newASTOrStmt(YYLTYPE location, GPStatement *left_stmt, 
	                  GPStatement *right_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    stmt->statement_type = PROGRAM_OR;
    stmt->location = location;
    stmt->value.or_stmt.left_stmt = left_stmt;
    stmt->value.or_stmt.right_stmt = right_stmt;

    return stmt;
}

GPStatement *newASTSkip(YYLTYPE location)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL)
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    stmt->statement_type = SKIP_STATEMENT;
    stmt->location = location;

    return stmt;
}

GPStatement *newASTFail(YYLTYPE location)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    stmt->statement_type = FAIL_STATEMENT;
    stmt->location = location;

    return stmt;
}



GPCondExp *newASTSubtypePred(CondExpType exp_type, YYLTYPE location, string var)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     /* exp_type: INT_CHECK, STRING_CHECK, ATOM_CHECK */
     cond->exp_type = exp_type; 
     cond->location = location;
     cond->value.var = strdup(var);

     return cond;
}

GPCondExp *newASTEdgePred(YYLTYPE location, string source, string target, 
	                  GPLabel *label)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     cond->exp_type = EDGE_PRED;
     cond->location = location;
     cond->value.edge_pred.source = strdup(source);
     cond->value.edge_pred.target = strdup(target);
     cond->value.edge_pred.label = label;

     return cond;
}

GPCondExp *newASTListComparison(CondExpType exp_type, YYLTYPE location,
	                        List *left_list, List *right_list)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     /* exp_type: EQUAL, NOT_EQUAL */
     cond->exp_type = exp_type; 
     cond->location = location;
     cond->value.list_cmp.left_list = left_list;
     cond->value.list_cmp.right_list = right_list;

     return cond;
}


GPCondExp *newASTAtomComparison(CondExpType exp_type, YYLTYPE location,
	                        GPAtomicExp *left_exp, GPAtomicExp *right_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     /* exp_type: GREATER, GREATER_EQUAL, LESS, LESS_EQUAL */
     cond->exp_type = exp_type; 
     cond->location = location;
     cond->value.atom_cmp.left_exp = left_exp;
     cond->value.atom_cmp.right_exp = right_exp;

     return cond;
}


GPCondExp *newASTNotExp(YYLTYPE location, GPCondExp *not_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL)
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     cond->exp_type = BOOL_NOT;
     cond->location = location;
     cond->value.not_exp = not_exp;

     return cond;
}

GPCondExp *newASTBinaryExp(CondExpType exp_type, YYLTYPE location, 
                           GPCondExp *left_exp, GPCondExp *right_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     /* exp_type: OR, AND */
     cond->exp_type = exp_type; 
     cond->location = location;
     cond->value.bin_exp.left_exp = left_exp;
     cond->value.bin_exp.right_exp = right_exp;

     return cond;
}


GPAtomicExp *newASTVariable(YYLTYPE location, string name)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL)
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     atom->exp_type = VARIABLE;
     atom->location = location;
     atom->value.name = strdup(name);

     return atom;
}

GPAtomicExp *newASTNumber(YYLTYPE location, int number)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     atom->exp_type = INTEGER_CONSTANT;
     atom->location = location;
     atom->value.number = number;

     return atom;
}

GPAtomicExp *newASTString(YYLTYPE location, string string)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL)
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     atom->exp_type = STRING_CONSTANT;
     atom->location = location;
     if(string) atom->value.string = strdup(string);
     else atom->value.string = NULL;

     return atom;
}

GPAtomicExp *newASTDegreeOp(AtomExpType exp_type, YYLTYPE location, 
                            string node_id)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     /* exp_type: INDEGREE, OUTDEGREE */
     atom->exp_type = exp_type; 
     atom->location = location;
     atom->value.node_id = strdup(node_id);

     return atom;
}

GPAtomicExp *newASTListLength(YYLTYPE location, List *list_arg)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL)
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     atom->exp_type = LIST_LENGTH;
     atom->location = location;
     atom->value.list_arg = list_arg;

     return atom;
}

GPAtomicExp *newASTStringLength(YYLTYPE location, GPAtomicExp *str_arg)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     atom->exp_type = STRING_LENGTH;
     atom->location = location;
     atom->value.str_arg = str_arg;

     return atom;
}

GPAtomicExp *newASTNegExp(YYLTYPE location, GPAtomicExp *exp)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     atom->exp_type = NEG;
     atom->location = location;
     atom->value.exp = exp;

     return atom;
}

GPAtomicExp *newASTBinaryOp(AtomExpType exp_type, YYLTYPE location, 
                            GPAtomicExp *left_exp, GPAtomicExp *right_exp)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) 
     {
       print_to_log("Error: Memory exhausted during AST construction.\n");
       exit(1);
     }

     /* exp_type: ADD, SUBTRACT, MULTIPLE, DIVIDE, CONCAT */
     atom->exp_type = exp_type; 
     atom->location = location;
     atom->value.bin_op.left_exp = left_exp;
     atom->value.bin_op.right_exp = right_exp;

     return atom;
}


GPProcedure *newASTProcedure(YYLTYPE location, string name, List *local_decls,
                             GPStatement *commands)
{
    GPProcedure *proc = malloc(sizeof(GPProcedure));
    
    if(proc == NULL)  
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    proc->node_type = PROCEDURE;
    proc->location = location;
    proc->name = strdup(name);
    proc->local_decls = local_decls;
    proc->commands = commands;
    proc->restore_point = -1;

    return proc;
}
 
GPRule *newASTRule(YYLTYPE location, string name, List *variables,
	           GPGraph *lhs, GPGraph *rhs, List *interface, 
		   GPCondExp *condition)
{
    GPRule *rule = malloc(sizeof(GPRule));
    
    if(rule == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

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
    rule->empty_lhs = false;
    rule->is_predicate = false;

    return rule;
}    


GPGraph *newASTGraph(YYLTYPE location, List *nodes, List *edges)
{
    GPGraph *graph = malloc(sizeof(GPGraph));
    
    if(graph == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    graph->node_type = GRAPH;
    graph->location = location;
    graph->nodes = nodes;
    graph->edges = edges;

    return graph;
}


GPNode *newASTNode(YYLTYPE location, bool root, string name, GPLabel *label)
{
    GPNode *node = malloc(sizeof(GPNode));
    
    if(node == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
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
      print_to_log("Error: Memory exhausted during AST construction.\n");
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

/* GPPos *newASTPosition (YYLTYPE location, int x, int y)
{
    GPPos *pos = malloc(sizeof(GPPos));
    
    if(pos == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    pos->node_type = POSITION;
    pos->location = location;
    pos->x = x;
    pos->y = y;

    return pos;
} */

GPLabel *newASTLabel(YYLTYPE location, MarkType mark, List *gp_list)
{
    GPLabel *label = malloc(sizeof(GPLabel));
    
    if(label == NULL) 
    {
      print_to_log("Error: Memory exhausted during AST construction.\n");
      exit(1);
    }

    label->node_type = LABEL;
    label->location = location;
    label->mark = mark;
    label->gp_list = gp_list;

    return label;
}

/* These functions perform a depth-first walk of the AST. They take a pointer
 * to an AST node as their argument. They first free any malloc'd strings 
 * (identifiers and string constants) if present, then they free any
 * substructures. Finally they free themselves. 
 */

void freeAST(List *ast) 
{
   if(!ast) return;
 
   switch(ast->list_type) 
   {
	case GLOBAL_DECLARATIONS:

        case LOCAL_DECLARATIONS:

	     if(ast->value.declaration) 
               freeASTDeclaration(ast->value.declaration);

	     break;	


	case COMMANDS:

             if(ast->value.command) freeASTStatement(ast->value.command);

	     break;	


	case RULES:

             if(ast->value.rule_call.rule_name) free(ast->value.rule_call.rule_name);

	     break;
	

	case INT_DECLARATIONS:
    
        case CHAR_DECLARATIONS:
              
	case STRING_DECLARATIONS:

	case ATOM_DECLARATIONS:

	case LIST_DECLARATIONS:

             if(ast->value.variables) freeAST(ast->value.variables);

	     break;
	

	case VARIABLE_LIST:

             if(ast->value.variable_name) free(ast->value.variable_name);

	     break;
	

	case INTERFACE_LIST:

             if(ast->value.node_id) free(ast->value.node_id);

	     break;
	

	case NODE_LIST:

             if(ast->value.node) freeASTNode(ast->value.node);

	     break;
	

	case EDGE_LIST:

             if(ast->value.edge) freeASTEdge(ast->value.edge);

	     break;


	case GP_LIST:

             if(ast->value.atom) freeASTAtomicExp(ast->value.atom);

	     break;

	
	case EMPTY_LIST:

             break;


	default: print_to_log("Error (freeAST): Unexpected Type: %d\n",
                              (int)ast->list_type); 
                 break;	 

	}

   if(ast->next) freeAST(ast->next);
   free(ast);
}

void freeASTDeclaration(GPDeclaration *decl)
{
   if(decl == NULL) return;

   switch(decl->decl_type) 
   {
      case MAIN_DECLARATION:

           if(decl->value.main_program) 
             freeASTStatement(decl->value.main_program);

           break;

      case PROCEDURE_DECLARATION:

           if(decl->value.procedure) freeASTProcedure(decl->value.procedure);

           break;

      case RULE_DECLARATION:

           if(decl->value.rule) freeASTRule(decl->value.rule);

           break;

      default: print_to_log("Error (freeASTDeclaration): Unexpected Type: "
                          "%d\n", (int)decl->decl_type);
                break;
   }

   free(decl);
}

void freeASTStatement(GPStatement *stmt)
{
   if(stmt == NULL) return;

   switch(stmt->statement_type) 
   {
      case COMMAND_SEQUENCE:	

           if(stmt->value.commands) freeAST(stmt->value.commands);

           break;


      case RULE_CALL:

           if(stmt->value.rule_call.rule_name) free(stmt->value.rule_call.rule_name);

           break;


      case RULE_SET_CALL:

           if(stmt->value.rule_set) freeAST(stmt->value.rule_set);

           break;


      case PROCEDURE_CALL:

           if(stmt->value.proc_call.proc_name) free(stmt->value.proc_call.proc_name);

           break;


      case IF_STATEMENT:

      case TRY_STATEMENT:

           if(stmt->value.cond_branch.condition) 
             freeASTStatement(stmt->value.cond_branch.condition);
           if(stmt->value.cond_branch.then_stmt) 
             freeASTStatement(stmt->value.cond_branch.then_stmt);
           if(stmt->value.cond_branch.else_stmt) 
             freeASTStatement(stmt->value.cond_branch.else_stmt);

           break;


      case ALAP_STATEMENT:

           if(stmt->value.loop_stmt.loop_body) 
              freeASTStatement(stmt->value.loop_stmt.loop_body);
            
           break;


      case PROGRAM_OR:

           if(stmt->value.or_stmt.left_stmt) 
             freeASTStatement(stmt->value.or_stmt.left_stmt);
           if(stmt->value.or_stmt.right_stmt) 
             freeASTStatement(stmt->value.or_stmt.right_stmt);

           break;


      case SKIP_STATEMENT:

      case FAIL_STATEMENT:

           break;

      
      default: print_to_log("Error (freeASTStatement): Unexpected type: %d\n",
                            (int)stmt->statement_type); 
               break;

      }

   free(stmt);
}

void freeASTCondition(GPCondExp *cond)
{
   if(cond == NULL) return;

   switch(cond->exp_type) 
   {
      case INT_CHECK:

      case STRING_CHECK:

      case ATOM_CHECK:

           if(cond->value.var) free(cond->value.var);

           break;


      case EDGE_PRED:

           if(cond->value.edge_pred.source)
              free(cond->value.edge_pred.source);
	   if(cond->value.edge_pred.target)
              free(cond->value.edge_pred.target);
	   if(cond->value.edge_pred.label)
              freeASTLabel(cond->value.edge_pred.label);
                                         
           break;


      case EQUAL:

      case NOT_EQUAL:

           if(cond->value.list_cmp.left_list) 
             freeAST(cond->value.list_cmp.left_list);
           if(cond->value.list_cmp.right_list)  
             freeAST(cond->value.list_cmp.right_list);

           break;
	

      case GREATER:

      case GREATER_EQUAL:

      case LESS:
        
      case LESS_EQUAL:

           if(cond->value.atom_cmp.left_exp)
             freeASTAtomicExp(cond->value.atom_cmp.left_exp);
           if(cond->value.atom_cmp.right_exp) 
             freeASTAtomicExp(cond->value.atom_cmp.right_exp);

           break;	  


      case BOOL_NOT:

           if(cond->value.not_exp) freeASTCondition(cond->value.not_exp);

	   break;


      case BOOL_OR:

      case BOOL_AND:

           if(cond->value.bin_exp.left_exp)
              freeASTCondition(cond->value.bin_exp.left_exp);
           if(cond->value.bin_exp.right_exp) 
              freeASTCondition(cond->value.bin_exp.right_exp);

      break;


      default: print_to_log("Error (freeASTCondition): Unexpected type: %d\n",
                              (int)cond->exp_type); 
               break;

	}

   free(cond);
}

void freeASTAtomicExp(GPAtomicExp *atom)
{
   if(atom == NULL) return;

   switch(atom->exp_type) 
   {
      case VARIABLE:

           if(atom->value.name)
              free(atom->value.name);

           break;


      case INTEGER_CONSTANT:

           break;


      case STRING_CONSTANT:

           if(atom->value.string)
             free(atom->value.string);

           break;


      case INDEGREE:
 
      case OUTDEGREE:

           if(atom->value.node_id) 
             free(atom->value.node_id);

           break;


      case LIST_LENGTH:

           if(atom->value.list_arg)
             freeAST(atom->value.list_arg);
		
           break;


      case STRING_LENGTH:

           if(atom->value.str_arg)
             freeASTAtomicExp(atom->value.str_arg);

           break;

      case NEG:

           if(atom->value.exp) freeASTAtomicExp(atom->value.exp);

           break;


      case ADD:

      case SUBTRACT:

      case MULTIPLY:

      case DIVIDE:

      case CONCAT:

           if(atom->value.bin_op.left_exp)
              freeASTAtomicExp(atom->value.bin_op.left_exp);
           if(atom->value.bin_op.right_exp)  
              freeASTAtomicExp(atom->value.bin_op.right_exp);

           break;


      default: print_to_log("Error (freeASTAtomicExp): Unexpected type: %d\n",
                            (int)atom->exp_type); 
               break;

      }

   free(atom);
}

void freeASTProcedure(GPProcedure *proc)
{
   if(proc == NULL) return;
   if(proc->name) free(proc->name);
   if(proc->local_decls) freeAST(proc->local_decls);
   if(proc->commands) freeASTStatement(proc->commands);
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
   


