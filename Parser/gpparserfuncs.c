/* ///////////////////////////////////////////////////////////////////////////////////////////////// */

/*                                       gpparserfuncs.c                                       
* * Contains the functions for creating AST tree nodes, manipulating the AST and symbol table
* management. 
*
* Created on 22/7/2013 by Chris Bak 
* 
* ///////////////////////////////////////////////////////////////////////////////////////////////// */

#include <stdio.h>
#include <stdlib.h>
#include "gpparser.h"

List *addRule (YYLTYPE location, symbol *name, List *next)
{ 
    List *newrule = malloc(sizeof(List));
    
    if(!newrule) {
      yyerror("insufficient space");
      exit(0);
    }
    newrule->listtype = RULES;
    newrule->location = location;
    newrule->value.rulename = rulename;
    newrule->next = next;

    return newrule;
}

List *addVariableDecl (list_t nodetype, YYLTYPE location, List *vars, List *next)
{ 
    List *newvardecl = malloc(sizeof(List));
    
    if(!newvar) {
      yyerror("insufficient space");
      exit(0);
    }
    newvardecl->listtype = VARIABLE;
    newvardecl->location = location;
    newvardecl->value.vars = vars;
    newvardecl->next = next;

    return newvardecl;
}

List *addVariable (YYLTYPE location, symbol *var, List *next)
{ 
    List *newvar = malloc(sizeof(List));
    
    if(!newvar) {
      yyerror("insufficient space");
      exit(0);
    }
    newvar->listtype = VARIABLE;
    newvar->location = location;
    newvar->value.var = var;
    newvar->next = next;

    return newvar;
}

List *addNodePair (YYLTYPE location, GPNodePair *nodepair, List *next)
{ 
    List *newpair = malloc(sizeof(List));
    
    if(!newpair) {
      yyerror("insufficient space");
      exit(0);
    }
    newpair->listtype = INTERFACE;
    newpair->location = location;
    newpair->value.nodepair = nodepair;
    newpair->next = next;

    return newpair;
}

List *newLabel (YYLTYPE location, mark_t mark, List *next)
{
    List *newlabel = malloc(sizeof(List));
    
    if(!newlabel) {
      yyerror("insufficient space");
      exit(0);
    }
    newlabel->listtype = LABEL;
    newlabel->location = location;
    newlabel->value.mark = mark;
    newlabel->next = next;

    return newlabel;
}

List *addAtom (YYLTYPE location, GPAtomicExp *atom, List *next)
{
    List *newatom = malloc(sizeof(List));
   
    if(!newatom) {
      yyerror("insufficient space");
      exit(0);
    }
    newatom->listtype = LABEL;
    newatom->location = location;
    newatom->value.atom = atom;
    newatom->next = next;

    return newatom;
}

List *addNode (YYLTYPE location, GPNode *node, List *next)
{
     List *newnode = malloc(sizeof(List));
     
     if(!newnode) {
	yyerror("insufficient space");
        exit(0);
     }

     newnode->listtype = NODES;
     newnode->location = location;
     newnode->value.node = node;
     newnode->next = next;

     return newnode;
}
      
List *addEdge (YYLTYPE location, GPEdge *edge, List *next)
{
     List *newedge = malloc(sizeof(List));
     
     if(!newedge) {
	yyerror("insufficient space");
        exit(0);
     }

     newedge->listtype = EDGES;
     newedge->location = location;
     newedge->value.edge = edge;
     newedge->next = next;

     return newedge;
}

GPAtomicExp *newVariable (YYLTYPE location, symbol *name)
{
     GPAtomicExp *variable = malloc(sizeof(GPAtomicExp));
 
     if(!variable) {
       yyerror("insufficient space");
       exit(0);
     }
     variable->nodetype = VARIABLE;
     variable->location = location;
     variable->value.var = name;

     return variable;
}

GPAtomicExp *newNumber (YYLTYPE location, int num)
{
     GPAtomicExp *number = malloc(sizeof(GPAtomicExp));
 
     if(!number) {
       yyerror("insufficient space");
       exit(0);
     }
     number->nodetype = INT_CONSTANT;
     number->location = location;
     number->value.num = num;

     return number;
}


GPAtomicExp *newString (YYLTYPE location, char *str)
{
     GPAtomicExp *string = malloc(sizeof(GPAtomicExp));
 
     if(!string) {
       yyerror("insufficient space");
       exit(0);
     }
     string->nodetype = STRING_CONSTANT;
     string->location = location;
     string->value.str = str;

     return string;
}

GPAtomicExp *newDegreeOp (atomexp_t nodetype, YYLTYPE location, symbol *node_id)
{
     GPAtomicExp *degree = malloc(sizeof(GPAtomicExp));
 
     if(!degree) {
       yyerror("insufficient space");
       exit(0);
     }
     degree->nodetype = nodetype; /*INDEGREE or OUTDEGREE */
     degree->location = location;
     degree->value.degree.node_id = node_id;

     return degree;
}

GPAtomicExp *newListLength (YYLTYPE location, AST *list)
{
     GPAtomicExp *llength = malloc(sizeof(GPAtomicExp));
 
     if(!llength) {
       yyerror("insufficient space");
       exit(0);
     }
     llength->nodetype = LIST_LENGTH 
     llength->location = location;
     llength->value.llength.list = list;

     return llength;
}

GPAtomicExp *newStringLength (YYLTYPE location, GPAtomicExp *slength)
{
     GPAtomicExp *slength = malloc(sizeof(GPAtomicExp));
 
     if(!slength) {
       yyerror("insufficient space");
       exit(0);
     }
     slength->nodetype = STRING_LENGTH 
     slength->location = location;
     slength->value.slength = slength;

     return slength;
}

GPAtomicExp *newBinaryOp (atomexp_t nodetype, YYLTYPE location, GPAtomicExp *leftexp, GPAtomicExp *rightexp)
{
     GPAtomicExp *newbinop = malloc(sizeof(GPAtomicExp));
 
     if(!newbinop) {
       yyerror("insufficient space");
       exit(0);
     }
     newbinop->nodetype = nodetype /* ADD, SUBTRACT, MULTIPLE, DIVIDE, CONCAT */ 
     newbinop->location = location;
     newbinop->value.binOp.left = leftexp;
     newbinop->value.binOp.right = rightexp;

     return newbinop;
}

GPCondExp *newSubtypePred (condexp_t nodetype, YYLTYPE location, symbol *var)
{
     GPCondExp *newsubtypepred = malloc(sizeof(GPCondExp));
 
     if(!newsubtypepred) {
       yyerror("insufficient space");
       exit(0);
     }
     newsubtypepred->nodetype = SUBTYPE
     newsubtypepred->location = location;
     newsubtypepred->var = var;

     return newsubtypepred;
}

GPCondExp *newEdgePred (YYLTYPE location, symbol *source, symbol *target, List *label)
{
     GPCondExp *edgepred = malloc(sizeof(GPCondExp));
 
     if(!c) {
       yyerror("insufficient space");
       exit(0);
     }
     edgepred->nodetype = EDGE_PRED
     edgepred->location = location;
     edgepred->value.edgePred.source = source;
     edgepred->value.edgePred.target = target;
     edgepred->value.edgePred.label = label;

     return c;
}


GPCondExp *newNotExp (YYLTYPE location, GPCondExp *exp)
{
     GPCondExp *notexp = malloc(sizeof(GPCondExp));
 
     if(!notexp) {
       yyerror("insufficient space");
       exit(0);
     }
     notexp->nodetype = NOT
     notexp->location = location;
     notexp->value.notExp = exp;

     return notexp;
}

GPCondExp *newBinaryExp (condexp_t nodetype, YYLTYPE location, GPCondExp *leftexp, GPCondExp *rightexp)
{
     GPCondExp *binexp = malloc(sizeof(GPCondExp));
 
     if(!binexp) {
       yyerror("insufficient space");
       exit(0);
     }
     binexp->nodetype = nodetype /* AND, OR */ 
     binexp->location = location;
     binexp->value.binExp.left = leftexp;
     binexp->value.binExp.right = rightexp;

     return c;
}


GPCondExp *newRelationalExp (condexp_t nodetype, YYLTYPE location, List *leftexp, List *rightexp
{
     GPCondExp *relexp = malloc(sizeof(GPCondExp));
 
     if(!relexp) {
       yyerror("insufficient space");
       exit(0);
     }
     relexp->nodetype = nodetype /* EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL */ 
     relexp->location = location;
     relexp->value.relExp.left = leftexp;
     relexp->value.relExp.right = rightexp;

     return relexp;
}

   
	     

GPPos *newPosition (YYLTYPE location, int x, int y)
{
    GPPos *pos = malloc(sizeof(GPPos));
    
    if(!pos) {
      yyerror("insufficient space");
      exit(0);
    }
    pos->nodetype = POSITION;
    pos->location = location;
    pos->x = x;
    pos->y = y;

    return pos;
}

GPGraph *newGraph (YYLTYPE location, GPPos *position, List *nodes, List *edges);
{
    GPGraph *graph = malloc(sizeof(GPGraph));
    
    if(!graph) {
      yyerror("insufficient space");
      exit(0);
    }
    graph->nodetype = GRAPH;
    graph->location = location;
    graph->position = position; 
    graph->nodes = nodes;
    graph->edges = edges;

    return graph;
}


GPNode *newNode (YYLTYPE location, int root, symbol *name, List *label, GPPos *position)
{
    GPNode *node = malloc(sizeof(GPNode));
    
    if(!node) {
      yyerror("insufficient space");
      exit(0);
    }
    node->nodetype = NODE;
    node->location = location;
    node->root = root;
    node->name = name;
    node->label = label;
    node->position = position;

    return node;
}

GPEdge *newEdge (YYLTYPE location, symbol *name, symbol *source, symbol *target, List *label)
{
    GPEdge *edge = malloc(sizeof(GPEdge));
    
    if(!edge) {
      yyerror("insufficient space");
      exit(0);
    }
    edge->nodetype = EDGE;
    edge->location = location;
    edge->name = name;
    edge->source = source;
    edge->target = target;
    edge->label = label;

    return edge;
}

GPNodePair *newNodePair (YYLTYPE location, symbol *leftnode, symbol *rightnode)
{
    GPEdge *nodepair = malloc(sizeof(GPEdge));
    
    if(!nodepair) {
      yyerror("insufficient space");
      exit(0);
    }
    nodepair->nodetype = NODE_PAIR;
    nodepair->location = location;
    nodepair->leftnode = leftnode;
    nodepair->rightnode = rightnode;

    return nodepair;
}

/* ----------------------- */

AST *newCond (ast_node_t nodetype, YYLTYPE location, AST *condition, AST *then_branch, AST *else_branch)
{
    AST *cond = malloc(sizeof(GPCond));
    
    if(!cond) {
      yyerror("insufficient space");
      exit(0);
    }
    cond->nodetype = nodetype; /* if or try */
    cond->location = location;
    cond->condition = condition;
    cond->then_branch = then_branch;
    cond->else_branch = else_branch;

    return (struct ast *) cond;
}

AST *newMacroDecl (ast_node_t nodetype, YYLTYPE location, symbol *name, AST *localmacro, AST *localrule, AST* comseq)
{
    AST *macro = malloc(sizeof(GPMacroDecl));
    
    if(!macro) {
      yyerror("insufficient space");
      exit(0);
    }
    macro->nodetype = MacroDecl; /* macrodecl */
    macro->location = location;
    macro->name = name;
    macro->localmacro = localmacro;
    macro->localrule = localrule;

    return (struct ast *) macro;
}

AST *newAlap (ast_node_t nodetype, YYLTYPE location, AST *comseq)
{
    AST *alap = malloc(sizeof(GPAlap));
    
    if(!macro) {
      yyerror("insufficient space");
      exit(0);
    }
    alap->nodetype = Alap; /* alap */
    alap->location = location;
    alap->comseq = comseq; 

    return (struct ast *) alap;
}

AST *newChoice (ast_node_t nodetype, YYLTYPE location, AST *first_comseq, AST* second_comseq)
{
    AST *choice = malloc(sizeof(GPChoice));
    
    if(!choice) {
      yyerror("insufficient space");
      exit(0);
    }
    choice->nodetype = nodetype; /* choice */
    choice->location = location;
    choice->first_comseq = first_comseq;
    choice->second_comseq = second_comseq;

    return (struct ast *) choice;
}

AST *newRuleDecl (ast_node_t nodetype, YYLTYPE location, symbol *name, int injective, AST *vars, AST *graphs, AST *interface, AST *condition)
{
    AST *ruledecl = malloc(sizeof(GPRuleDecl));
    
    if(!ruledecl) {
      yyerror("insufficient space");
      exit(0);
    }
    ruledecl->nodetype = nodetype; /* ruledecl */
    ruledecl->location = location;
    ruledecl->name = name;
    ruledecl->injective = injective;
    ruledecl->vars = vars;
    ruledecl->graphs = graphs;
    ruledecl->interface = interface;
    ruledecl->condition = condition;

    return (struct ast *) ruledecl;
}




AST *newEdgePred (ast_node_t nodetype, YYLTYPE location, AST *source, AST *target, AST *label)
{
    AST *edgepred = malloc(sizeof(GPEdgePred));
    
    if(!edgepred) {
      yyerror("insufficient space");
      exit(0);
    }
    edgepred->nodetype = nodetype; /* if or try */
    edgepred->location = location;
    edgepred->source = source;
    edgepred->target = target;
    edgepred->label = label;

    return (struct ast *) edgepred;
}

AST *newDegree (ast_node_t nodetype, YYLTYPE location, symbol *name)
{
    AST *degree = malloc(sizeof(GPDegree));
    
    if(!degree) {
      yyerror("insufficient space");
      exit(0);
    }
    degree->nodetype = nodetype; /* indeg or outdeg */
    degree->location = location;
    degree->name = name;

    return (struct ast *) degree;
}

AST *newLength (ast_node_t nodetype, YYLTYPE location, AST *arg)
{
    AST *length = malloc(sizeof(GPLength));
    
    if(!typecheck) {
      yyerror("insufficient space");
      exit(0);
    }
    length->nodetype = nodetype; /* llen or slen */
    length->location = location;
    length->arg = arg;

    return (struct ast *) length;
}


AST *newTypeCheck (ast_node_t nodetype, YYLTYPE location, symbol *var)
{
    AST *typecheck = malloc(sizeof(GPTypeCheck));
    
    if(!typecheck) {
      yyerror("insufficient space");
      exit(0);
    }
    typecheck->nodetype = nodetype; /* int, string, atom */
    typecheck->location = location;
    typecheck->var = var;

    return (struct ast *) typecheck;
}

AST *newNumber (ast_node_t nodetype, YYLTYPE location, int val)
{
    AST *num = malloc(sizeof(GPNum));
    
    if(!num) {
      yyerror("insufficient space");
      exit(0);
    }
    num->nodetype = nodetype; /* num */
    num->location = location;
    num->val = val;

    return (struct ast *) num;
}

AST *newString (ast_node_t nodetype, YYLTYPE location, char *val)
{
    AST *string = malloc(sizeof(GPString));
    
    if(!string) {
      yyerror("insufficient space");
      exit(0);
    }
    string->nodetype = nodetype; /* string */
    string->location = location;
    string->val = val;

    return (struct ast *) string;
}

AST *newMark (ast_node_t nodetype, YYLTYPE location, mark_t val)
{
    AST *mark = malloc(sizeof(GPMark));
    
    if(!mark) {
      yyerror("insufficient space");
      exit(0);
    }
    mark->nodetype = nodetype; /* mark */
    mark->location = location;
    mark->val = val;

    return (struct ast *) mark;
}

 /* post processing AST functions: reverse lists, convert relational expressions with two or more comparsions into a bunch of ANDs */
