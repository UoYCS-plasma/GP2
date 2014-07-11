/* ///////////////////////////////////////////////////////////////////////////

  ==============================
  ast.c - Chris Bak (22/07/2013)
  ==============================

/////////////////////////////////////////////////////////////////////////// */

#include "ast.h" 

List *addDecl (ListType list_type, YYLTYPE location, GPDeclaration *declaration,
	       List *next)
{ 
    List *new_decl = malloc(sizeof(List));
    
    if(new_decl == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    /* list_type: GLOBAL_DECLARATIONS, LOCAL_DECLARATIONS */
    new_decl->list_type = list_type; 
    new_decl->location = location;
    new_decl->value.declaration = declaration;
    new_decl->next = next;

    return new_decl;
}

List *addCommand (YYLTYPE location, GPStatement *command, List *next)
{ 
    List *new_command = malloc(sizeof(List));
    
    if(new_command == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_command->list_type = COMMANDS;
    new_command->location = location;
    new_command->value.command = command;
    new_command->next = next;

    return new_command;
}

List *addRule (YYLTYPE location, string rule_name, List *next)
{ 
    List *new_rule = malloc(sizeof(List));
    
    if(new_rule == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_rule->list_type = RULES;
    new_rule->location = location;
    new_rule->value.rule_name = strdup(rule_name);
    new_rule->next = next;

    return new_rule;
}

List *addVariableDecl (ListType list_type, YYLTYPE location, List *variables,
	               List *next)
{ 
    List *new_var_decl = malloc(sizeof(List));
    
    if(new_var_decl == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    /* list_type: INT_DECLARATIONS, CHAR_DECLARATIONS, STRING_DECLARATIONS, 
     * ATOM_DECLARATIONS, LIST_DECLARATIONS */
    new_var_decl->list_type = list_type; 
    new_var_decl->location = location;
    new_var_decl->value.variables = variables;
    new_var_decl->next = next;

    return new_var_decl;
}

List *addVariable (YYLTYPE location, string variable_name, List *next)
{ 
    List *new_var = malloc(sizeof(List));
    
    if(new_var == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_var->list_type = VARIABLE_LIST;
    new_var->location = location;
    new_var->value.variable_name = strdup(variable_name);
    new_var->next = next;

    return new_var;
}

List *addNodeID (YYLTYPE location, string node_id, List *next)
{ 
    List *new_pair = malloc(sizeof(List));
    
    if(new_pair == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_pair->list_type = INTERFACE_LIST;
    new_pair->location = location;
    new_pair->value.node_id = strdup(node_id);
    new_pair->next = next;

    return new_pair;
}

List *addNode (YYLTYPE location, GPNode *node, List *next)
{
     List *new_node = malloc(sizeof(List));
     
     if(new_node == NULL) {
	print_to_log("Memory exhausted during AST construction.\n");
        exit(0);
     }

     new_node->list_type = NODE_LIST;
     new_node->location = location;
     new_node->value.node = node;
     new_node->next = next;

     return new_node;
}
      
List *addEdge (YYLTYPE location, GPEdge *edge, List *next)
{
     List *new_edge = malloc(sizeof(List));
     
     if(new_edge == NULL) {
	print_to_log("Memory exhausted during AST construction.\n");
        exit(0);
     }

     new_edge->list_type = EDGE_LIST;
     new_edge->location = location;
     new_edge->value.edge = edge;
     new_edge->next = next;

     return new_edge;
}

List *addAtom (YYLTYPE location, GPAtomicExp *atom, List *next)
{
    List *new_atom = malloc(sizeof(List));
   
    if(new_atom == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_atom->list_type = GP_LIST;
    new_atom->location = location;
    new_atom->value.atom = atom;
    new_atom->next = next;

    return new_atom;
}

List *addEmptyList (YYLTYPE location)
{
     List *new_empty = malloc(sizeof(List));

     if(new_empty == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     new_empty->list_type = EMPTY_LIST;
     new_empty->location = location;
     new_empty->next = NULL;

     return new_empty;
}



GPDeclaration *newMainDecl (YYLTYPE location, GPStatement *main_program)
{
    GPDeclaration *new_main = malloc(sizeof(GPDeclaration));
   
    if(new_main == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_main->decl_type = MAIN_DECLARATION;
    new_main->location = location;
    new_main->value.main_program = main_program;

    return new_main;
}

GPDeclaration *newProcedureDecl (YYLTYPE location, GPProcedure *procedure)
{
    GPDeclaration *new_proc = malloc(sizeof(GPDeclaration));
   
    if(new_proc == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_proc->decl_type = PROCEDURE_DECLARATION;
    new_proc->location = location;
    new_proc->value.procedure = procedure;

    return new_proc;
}

GPDeclaration *newRuleDecl (YYLTYPE location, GPRule *rule)
{
    GPDeclaration *new_rule = malloc(sizeof(GPDeclaration));
   
    if(new_rule == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    new_rule->decl_type = RULE_DECLARATION;
    new_rule->location = location;
    new_rule->value.rule = rule;

    return new_rule;
}



GPStatement *newCommandSequence(YYLTYPE location, List *cmd_seq)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = COMMAND_SEQUENCE;
    stmt->location = location;
    stmt->value.cmd_seq = cmd_seq;

    return stmt;
}

GPStatement *newRuleCall(YYLTYPE location, string rule_name)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = RULE_CALL;
    stmt->location = location;
    stmt->value.rule_name = strdup(rule_name);

    return stmt;
}

GPStatement *newRuleSetCall(YYLTYPE location, List *rule_set)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = RULE_SET_CALL;
    stmt->location = location;
    stmt->value.rule_set = rule_set;

    return stmt;
}

GPStatement *newProcCall(YYLTYPE location, string proc_name)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = PROCEDURE_CALL;
    stmt->location = location;
    stmt->value.proc_name = strdup(proc_name);

    return stmt;
}

GPStatement *newCondBranch(StatementType statement_type, YYLTYPE location, 
	      GPStatement *condition, GPStatement *then_stmt, GPStatement *else_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    /* statement_type: IF_STATEMENT, TRY_STATEMENT */
    stmt->statement_type = statement_type; 
    stmt->location = location;
    stmt->value.cond_branch.condition = condition;
    stmt->value.cond_branch.then_stmt = then_stmt;
    stmt->value.cond_branch.else_stmt = else_stmt;

    return stmt;
}

GPStatement *newAlap(YYLTYPE location, GPStatement *loop_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = ALAP_STATEMENT;
    stmt->location = location;
    stmt->value.loop_stmt = loop_stmt;

    return stmt;
}

GPStatement *newOrStmt(YYLTYPE location, GPStatement *left_stmt, 
	      GPStatement *right_stmt)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = PROGRAM_OR;
    stmt->location = location;
    stmt->value.or_stmt.left_stmt = left_stmt;
    stmt->value.or_stmt.right_stmt = right_stmt;

    return stmt;
}

GPStatement *newSkip(YYLTYPE location)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = SKIP_STATEMENT;
    stmt->location = location;

    return stmt;
}

GPStatement *newFail(YYLTYPE location)
{
    GPStatement *stmt = malloc(sizeof(GPStatement));
   
    if(stmt == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    stmt->statement_type = FAIL_STATEMENT;
    stmt->location = location;

    return stmt;
}



GPCondExp *newSubtypePred (CondExpType exp_type, YYLTYPE location, string var)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     /* exp_type: INT_CHECK, STRING_CHECK, ATOM_CHECK */
     cond->exp_type = exp_type; 
     cond->location = location;
     cond->value.var = strdup(var);

     return cond;
}

GPCondExp *newEdgePred (YYLTYPE location, string source, string target, 
	    GPLabel *label)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = EDGE_PRED;
     cond->location = location;
     cond->value.edge_pred.source = strdup(source);
     cond->value.edge_pred.target = strdup(target);
     cond->value.edge_pred.label = label;

     return cond;
}

GPCondExp *newListComparison (CondExpType exp_type, YYLTYPE location,
	    List *left_list, List *right_list)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     /* exp_type: EQUAL, NOT_EQUAL */
     cond->exp_type = exp_type; 
     cond->location = location;
     cond->value.list_cmp.left_list = left_list;
     cond->value.list_cmp.right_list = right_list;

     return cond;
}


GPCondExp *newAtomComparison (CondExpType exp_type, YYLTYPE location,
	    GPAtomicExp *left_exp, GPAtomicExp *right_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     /* exp_type: GREATER, GREATER_EQUAL, LESS, LESS_EQUAL */
     cond->exp_type = exp_type; 
     cond->location = location;
     cond->value.atom_cmp.left_exp = left_exp;
     cond->value.atom_cmp.right_exp = right_exp;

     return cond;
}


GPCondExp *newNotExp (YYLTYPE location, GPCondExp *not_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     cond->exp_type = BOOL_NOT;
     cond->location = location;
     cond->value.not_exp = not_exp;

     return cond;
}

GPCondExp *newBinaryExp (CondExpType exp_type, YYLTYPE location, 
	    GPCondExp *left_exp, GPCondExp *right_exp)
{
     GPCondExp *cond = malloc(sizeof(GPCondExp));
 
     if(cond == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     /* exp_type: OR, AND */
     cond->exp_type = exp_type; 
     cond->location = location;
     cond->value.bin_exp.left_exp = left_exp;
     cond->value.bin_exp.right_exp = right_exp;

     return cond;
}





GPAtomicExp *newVariable (YYLTYPE location, string name)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = VARIABLE;
     atom->location = location;
     atom->value.name = strdup(name);

     return atom;
}

GPAtomicExp *newNumber (YYLTYPE location, int number)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = INT_CONSTANT;
     atom->location = location;
     atom->value.number = number;

     return atom;
}


GPAtomicExp *newCharacter (YYLTYPE location, string character)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));

     if(atom == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = CHARACTER_CONSTANT;
     atom->location = location;
     if(character) atom->value.string = strdup(character);

     return atom;
}



GPAtomicExp *newString (YYLTYPE location, string string)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = STRING_CONSTANT;
     atom->location = location;
     if(string) atom->value.string = strdup(string);

     return atom;
}

GPAtomicExp *newDegreeOp (AtomExpType exp_type, YYLTYPE location, string node_id)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     /* exp_type: INDEGREE, OUTDEGREE */
     atom->exp_type = exp_type; 
     atom->location = location;
     atom->value.node_id = strdup(node_id);

     return atom;
}

GPAtomicExp *newListLength (YYLTYPE location, List *list_arg)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = LIST_LENGTH;
     atom->location = location;
     atom->value.list_arg = list_arg;

     return atom;
}

GPAtomicExp *newStringLength (YYLTYPE location, GPAtomicExp *str_arg)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = STRING_LENGTH;
     atom->location = location;
     atom->value.str_arg = str_arg;

     return atom;
}

GPAtomicExp *newNegExp (YYLTYPE location, GPAtomicExp *exp)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     atom->exp_type = NEG;
     atom->location = location;
     atom->value.exp = exp;

     return atom;
}

GPAtomicExp *newBinaryOp (AtomExpType exp_type, YYLTYPE location, GPAtomicExp *left_exp, GPAtomicExp *right_exp)
{
     GPAtomicExp *atom = malloc(sizeof(GPAtomicExp));
 
     if(atom == NULL) {
       print_to_log("Memory exhausted during AST construction.\n");
       exit(0);
     }

     /* exp_type: ADD, SUBTRACT, MULTIPLE, DIVIDE, CONCAT */
     atom->exp_type = exp_type; 
     atom->location = location;
     atom->value.bin_op.left_exp = left_exp;
     atom->value.bin_op.right_exp = right_exp;

     return atom;
}



GPProcedure *newProcedure(YYLTYPE location, string name, List *local_decls, GPStatement *cmd_seq)
{
    GPProcedure *proc = malloc(sizeof(GPProcedure));
    
    if(proc == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    proc->node_type = PROCEDURE;
    proc->location = location;
    proc->name = strdup(name);
    proc->local_decls = local_decls;
    proc->cmd_seq = cmd_seq;

    return proc;
}
 
GPRule *newRule(YYLTYPE location, string name, List *variables,
	        GPGraph *lhs, GPGraph *rhs, List *interface, 
		GPCondExp *condition)
{
    GPRule *rule = malloc(sizeof(GPRule));
    
    if(rule == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    rule->node_type = RULE;
    rule->location = location;
    rule->name = strdup(name);
    rule->variables = variables;
    rule->lhs = lhs;
    rule->rhs = rhs;
    rule->interface = interface;
    rule->condition = condition;

    return rule;
}    


GPGraph *newGraph (YYLTYPE location, GPPos *position, List *nodes, 
                   List *edges)
{
    GPGraph *graph = malloc(sizeof(GPGraph));
    
    if(graph == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    graph->node_type = GRAPH;
    graph->location = location;
    graph->position = position; 
    graph->nodes = nodes;
    graph->edges = edges;

    return graph;
}


GPNode *newNode (YYLTYPE location, bool root, string name, GPLabel *label, 
                 GPPos *position)
{
    GPNode *node = malloc(sizeof(GPNode));
    
    if(node == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    node->node_type = NODE;
    node->location = location;
    node->root = root;
    node->name = strdup(name);
    node->label = label;
    node->position = position;

    return node;
}

GPEdge *newEdge (YYLTYPE location, bool bidirectional, string name, 
                 string source, string target, GPLabel *label)
{
    GPEdge *edge = malloc(sizeof(GPEdge));
    
    if(edge == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
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

GPPos *newPosition (YYLTYPE location, int x, int y)
{
    GPPos *pos = malloc(sizeof(GPPos));
    
    if(pos == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
    }

    pos->node_type = POSITION;
    pos->location = location;
    pos->x = x;
    pos->y = y;

    return pos;
}

GPLabel *newLabel (YYLTYPE location, MarkType mark, List *gp_list)
{
    GPLabel *label = malloc(sizeof(GPLabel));
    
    if(label == NULL) {
      print_to_log("Memory exhausted during AST construction.\n");
      exit(0);
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
   switch(ast->list_type) {

	case GLOBAL_DECLARATIONS:

        case LOCAL_DECLARATIONS:

	     if(ast->value.declaration) 
               freeDeclaration(ast->value.declaration);

	     break;	


	case COMMANDS:

             if(ast->value.command) freeStatement(ast->value.command);

	     break;	


	case RULES:

             if(ast->value.rule_name) free(ast->value.rule_name);

	     break;
	

	case INT_DECLARATIONS:
              
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

             if(ast->value.node) freeNode(ast->value.node);

	     break;
	

	case EDGE_LIST:

             if(ast->value.edge) freeEdge(ast->value.edge);

	     break;


	case GP_LIST:

             if(ast->value.atom) freeAtomicExp(ast->value.atom);

	     break;

	
	case EMPTY_LIST:

             break;


	default: print_to_log("Unexpected List Type: %d\n",
                              (int)ast->list_type); 
                 break;	 

	}

   if(ast->next) freeAST(ast->next);
   if(ast) free(ast);
}

void freeDeclaration(GPDeclaration *decl)
{
     switch(decl->decl_type) {

	case MAIN_DECLARATION:

             if(decl->value.main_program) 
               freeStatement(decl->value.main_program);

	     break;


	case PROCEDURE_DECLARATION:

             if(decl->value.procedure) freeProcedure(decl->value.procedure);

	     break;


	case RULE_DECLARATION:

             if(decl->value.rule) freeRule(decl->value.rule);

	     break;


	default: print_to_log("Unexpected Declaration Type: %d\n",
                              (int)decl->decl_type); 
                 break;

	}

   if(decl) free(decl);
}

void freeStatement(GPStatement *stmt)
{
     switch(stmt->statement_type) {

	case COMMAND_SEQUENCE:	

             if(stmt->value.cmd_seq) freeAST(stmt->value.cmd_seq);

	     break;


	case RULE_CALL:

             if(stmt->value.rule_name) free(stmt->value.rule_name);

	     break;


	case RULE_SET_CALL:

             if(stmt->value.rule_set) freeAST(stmt->value.rule_set);

	     break;


	case PROCEDURE_CALL:

             if(stmt->value.proc_name) free(stmt->value.proc_name);

	     break;


	case IF_STATEMENT:

        case TRY_STATEMENT:

             if(stmt->value.cond_branch.condition) 
               freeStatement(stmt->value.cond_branch.condition);
             if(stmt->value.cond_branch.then_stmt) 
               freeStatement(stmt->value.cond_branch.then_stmt);
	     if(stmt->value.cond_branch.else_stmt) 
               freeStatement(stmt->value.cond_branch.else_stmt);

	     break;


	case ALAP_STATEMENT:

	     if(stmt->value.loop_stmt) freeStatement(stmt->value.loop_stmt);
             
	     break;


	case PROGRAM_OR:

             if(stmt->value.or_stmt.left_stmt) 
               freeStatement(stmt->value.or_stmt.left_stmt);
             if(stmt->value.or_stmt.right_stmt) 
               freeStatement(stmt->value.or_stmt.right_stmt);

	     break;


	case SKIP_STATEMENT:

 	case FAIL_STATEMENT:

	     break;

	
	default: print_to_log("Unexpected Statement Type: %d\n",
                              (int)stmt->statement_type); 
                 break;

	}

   if(stmt) free(stmt);
}

void freeCondition(GPCondExp *cond)
{
     switch(cond->exp_type) {

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
               freeLabel(cond->value.edge_pred.label);
                                         
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
               freeAtomicExp(cond->value.atom_cmp.left_exp);
             if(cond->value.atom_cmp.right_exp) 
               freeAtomicExp(cond->value.atom_cmp.right_exp);

	     break;	  


	case BOOL_NOT:

	     if(cond->value.not_exp) freeCondition(cond->value.not_exp);

	     break;


	case BOOL_OR:

	case BOOL_AND:

	     if(cond->value.bin_exp.left_exp)
               freeCondition(cond->value.bin_exp.left_exp);
	     if(cond->value.bin_exp.right_exp) 
               freeCondition(cond->value.bin_exp.right_exp);

	     break;


	default: print_to_log("Unexpected Condition Type: %d\n",
                              (int)cond->exp_type); 
                 break;

	}

   if(cond) free(cond);
}

void freeAtomicExp(GPAtomicExp *atom)
{
     switch(atom->exp_type) {


	case VARIABLE:

	     if(atom->value.name)
               free(atom->value.name);

             break;


	case INT_CONSTANT:

             break;


        case CHARACTER_CONSTANT:
          
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
               freeAtomicExp(atom->value.str_arg);

             break;

	case NEG:

	     if(atom->value.exp) freeAtomicExp(atom->value.exp);

             break;


	case ADD:

	case SUBTRACT:

	case MULTIPLY:

	case DIVIDE:

	case CONCAT:

	     if(atom->value.bin_op.left_exp)
                freeAtomicExp(atom->value.bin_op.left_exp);
	     if(atom->value.bin_op.right_exp)  
                freeAtomicExp(atom->value.bin_op.right_exp);

             break;


	default: print_to_log("Unexpected Atomic Expression Type: %d\n",
                             (int)atom->exp_type); 
                 break;

	}

   if(atom) free(atom);
}

void freeProcedure(GPProcedure *proc)
{
   if(proc->name) free(proc->name);
   if(proc->local_decls) freeAST(proc->local_decls);
   if(proc->cmd_seq) freeStatement(proc->cmd_seq);
   if(proc) free(proc);
}

void freeRule(GPRule *rule)
{
   if(rule->name) free(rule->name);
   if(rule->variables) freeAST(rule->variables);
   if(rule->lhs) freeGraph(rule->lhs);  
   if(rule->rhs) freeGraph(rule->rhs);
   if(rule->interface) freeAST(rule->interface);
   if(rule->condition) freeCondition(rule->condition);
   if(rule) free(rule);
}

void freeGraph(GPGraph *graph)
{
   if(graph->position) free(graph->position);
   if(graph->nodes) freeAST(graph->nodes);
   if(graph->edges) freeAST(graph->edges);
   if(graph) free(graph);
}

void freeNode(GPNode *node)
{
   if(node->name) free(node->name);
   if(node->label) freeLabel(node->label);
   if(node->position) free(node->position);
   if(node) free(node);
}

void freeEdge(GPEdge *edge)
{
   if(edge->name) free(edge->name);
   if(edge->source) free(edge->source);
   if(edge->target) free(edge->target);
   if(edge->label) freeLabel(edge->label);
   if(edge) free(edge);
}

void freeLabel(GPLabel *label)
{
   if(label->gp_list) freeAST(label->gp_list);
   if(label) free(label);
}
   


