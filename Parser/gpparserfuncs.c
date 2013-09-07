/* ///////////////////////////////////////////////////////////////////////////////////////////////// */

/*                                       gpparserfuncs.c                                       
*
* Contains the functions for creating AST tree nodes, manipulating the AST and symbol table
* management. 
*
* Created on 22/7/2013 by Chris Bak 
* 
* ///////////////////////////////////////////////////////////////////////////////////////////////// */

#include <stdio.h>
#include <stdlib.h>
#include "gpparser.h"

ListNode *newLabelHead (YYLTYPE position, mark_t mark, ListNode *next)
{
    ListNode *l = malloc(sizeof(ListNode));
    
    if(!l) {
      yyerror("insufficient space");
      exit(0);
    }
    l->nodetype = LABEL;
    l->position = position;
    l->value.mark = mark;
    l->next = next;

    return l;
}

ListNode *newLabelHead (YYLTYPE position, GPAtomicExp *atom, ListNode *next)
{
    ListNode *l = malloc(sizeof(ListNode));
    
    if(!l) {
      yyerror("insufficient space");
      exit(0);
    }
    l->nodetype = LABEL;
    l->position = position;
    l->value.atom = atom;
    l->next = next;

    return l;
}

GPAtomicExp *newVariable (YYLTYPE position, symbol *name)
{
     GPAtomicExp *a = malloc(sizeof(GPAtomicExp));
 
     if(!a) {
       yyerror("insufficient space");
       exit(0);
     }
     a->nodetype = VARIABLE;
     a->position = position;
     a->value.var = name;

     return a;
}

GPAtomicExp *newNumber (YYLTYPE position, int num)
{
     GPAtomicExp *a = malloc(sizeof(GPAtomicExp));
 
     if(!a) {
       yyerror("insufficient space");
       exit(0);
     }
     a->nodetype = INT_CONSTANT;
     a->position = position;
     a->value.num = num;

     return a;
}


GPAtomicExp *newString (YYLTYPE position, char *str)
{
     GPAtomicExp *a = malloc(sizeof(GPAtomicExp));
 
     if(!a) {
       yyerror("insufficient space");
       exit(0);
     }
     a->nodetype = STRING_CONSTANT;
     a->position = position;
     a->value.str = str;

     return a;
}

GPAtomicExp *newDegreeOp (atomexp_t nodetype, YYLTYPE position, symbol *node_id)
{
     GPAtomicExp *a = malloc(sizeof(GPAtomicExp));
 
     if(!a) {
       yyerror("insufficient space");
       exit(0);
     }
     a->nodetype = nodetype; /*INDEGREE or OUTDEGREE */
     a->position = position;
     a->value.indeg.node_id = node_id;

     return a;
}

GPAtomicExp *newListLength (YYLTYPE position, AST *list)
{
     GPAtomicExp *a = malloc(sizeof(GPAtomicExp));
 
     if(!a) {
       yyerror("insufficient space");
       exit(0);
     }
     a->nodetype = LIST_LENGTH 
     a->position = position;
     a->value.llength.list = list;

     return a;
}

GPAtomicExp *newStringLength (YYLTYPE position, GPAtomicExp *slength)
{
     GPAtomicExp *a = malloc(sizeof(GPAtomicExp));
 
     if(!a) {
       yyerror("insufficient space");
       exit(0);
     }
     a->nodetype = STRING_LENGTH 
     a->position = position;
     a->value.slength = slength;

     return a;
}

GPAtomicExp *newBinaryOp (atomexp_t nodetype; YYLTYPE position, GPAtomicExp *left, GPAtomicExp *right)
{
     GPAtomicExp *a = malloc(sizeof(GPAtomicExp));
 
     if(!a) {
       yyerror("insufficient space");
       exit(0);
     }
     a->nodetype = nodetype /* ADD, SUBTRACT, MULTIPLE, DIVIDE, CONCAT */ 
     a->position = position;
     a->value.binOp.left = left;
     a->value.binOp.right = right;

     return a;
}

GPCondExp *newSubtypePred (YYLTYPE position, symbol *var)
{
     GPCondExp *c = malloc(sizeof(GPCondExp));
 
     if(!c) {
       yyerror("insufficient space");
       exit(0);
     }
     c->nodetype = SUBTYPE
     c->position = position;
     c->var = var;

     return c;
}

GPCondExp *newEdgePred (YYLTYPE position, symbol *source, symbol *target, AST *label)
{
     GPCondExp *c = malloc(sizeof(GPCondExp));
 
     if(!c) {
       yyerror("insufficient space");
       exit(0);
     }
     c->nodetype = EDGE_PRED
     c->position = position;
     c->value.edgePred.source = source;
     c->value.edgePred.target = target;
     c->value.edgePred.label = label;

     return c;
}


GPCondExp *newNotExp (YYLTYPE position, GPCondExp *notExp)
{
     GPCondExp *c = malloc(sizeof(GPCondExp));
 
     if(!c) {
       yyerror("insufficient space");
       exit(0);
     }
     c->nodetype = NOT
     c->position = position;
     c->value.notExp = notExp;

     return c;
}

GPCondExp *newBinaryExp (condexp_t nodetype; YYLTYPE position, GPCondExp *left, GPCondExp *right)
{
     GPCondExp *c = malloc(sizeof(GPCondExp));
 
     if(!c) {
       yyerror("insufficient space");
       exit(0);
     }
     c->nodetype = nodetype /* AND, OR */ 
     c->position = position;
     c->value.binExp.left = left;
     c->value.binExp.right = right;

     return c;
}


GPCondExp *newRelationalOp (condexp_t nodetype; YYLTYPE position, AST *left, AST *right)
{
     GPCondExp *c = malloc(sizeof(GPCondExp));
 
     if(!c) {
       yyerror("insufficient space");
       exit(0);
     }
     c->nodetype = nodetype /* EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL */ 
     c->position = position;
     c->value.relExp.left = left;
     c->value.rekExp.right = right;

     return c;
}



AST *newCond (ast_node_t nodetype, YYLTYPE position, AST *condition, AST *then_branch, AST *else_branch)
{
    AST *cond = malloc(sizeof(GPCond));
    
    if(!cond) {
      yyerror("insufficient space");
      exit(0);
    }
    cond->nodetype = nodetype; /* if or try */
    cond->position = position;
    cond->condition = condition;
    cond->then_branch = then_branch;
    cond->else_branch = else_branch;

    return (struct ast *) cond;
}

AST *newMacroDecl (ast_node_t nodetype, YYLTYPE position, symbol *name, AST *localmacro, AST *localrule, AST* comseq)
{
    AST *macro = malloc(sizeof(GPMacroDecl));
    
    if(!macro) {
      yyerror("insufficient space");
      exit(0);
    }
    macro->nodetype = MacroDecl; /* macrodecl */
    macro->position = position;
    macro->name = name;
    macro->localmacro = localmacro;
    macro->localrule = localrule;

    return (struct ast *) macro;
}

AST *newAlap (ast_node_t nodetype, YYLTYPE position, AST *comseq)
{
    AST *alap = malloc(sizeof(GPAlap));
    
    if(!macro) {
      yyerror("insufficient space");
      exit(0);
    }
    alap->nodetype = Alap; /* alap */
    alap->position = position;
    alap->comseq = comseq; 

    return (struct ast *) alap;
}

AST *newChoice (ast_node_t nodetype, YYLTYPE position, AST *first_comseq, AST* second_comseq)
{
    AST *choice = malloc(sizeof(GPChoice));
    
    if(!choice) {
      yyerror("insufficient space");
      exit(0);
    }
    choice->nodetype = nodetype; /* choice */
    choice->position = position;
    choice->first_comseq = first_comseq;
    choice->second_comseq = second_comseq;

    return (struct ast *) choice;
}

AST *newRuleDecl (ast_node_t nodetype, YYLTYPE position, symbol *name, int injective, AST *vars, AST *graphs, AST *interface, AST *condition)
{
    AST *ruledecl = malloc(sizeof(GPRuleDecl));
    
    if(!ruledecl) {
      yyerror("insufficient space");
      exit(0);
    }
    ruledecl->nodetype = nodetype; /* ruledecl */
    ruledecl->position = position;
    ruledecl->name = name;
    ruledecl->injective = injective;
    ruledecl->vars = vars;
    ruledecl->graphs = graphs;
    ruledecl->interface = interface;
    ruledecl->condition = condition;

    return (struct ast *) ruledecl;
}



AST *newGraph (ast_node_t nodetype, YYLTYPE position, AST *gpposition, AST *nodes, AST *edges)
{
    AST *graph = malloc(sizeof(GPGraph));
    
    if(!graph) {
      yyerror("insufficient space");
      exit(0);
    }
    graph->nodetype = nodetype; /* graph */
    graph->position = position;
    graph->gpposition = gpposition;
    graph->nodes = nodes;
    graph->edges = edges;

    return (struct ast *) graph;
}

AST *newPos (ast_node_t nodetype, YYLTYPE position, int x, int y)
{
    AST *pos = malloc(sizeof(GPPos));
    
    if(!pos) {
      yyerror("insufficient space");
      exit(0);
    }
    pos->nodetype = nodetype; /* position */
    pos->position = position;
    pos->x = x;
    pos->y = y;

    return (struct ast *) pos;
}

AST *newNode (ast_node_t nodetype, YYLTYPE position, symbol *name, int root, AST *label, AST *gpposition)
{
    AST *node = malloc(sizeof(GPNode));
    
    if(!node) {
      yyerror("insufficient space");
      exit(0);
    }
    node->nodetype = nodetype; /* node */
    node->position = position;
    node->name = name;
    node->root = root;
    node->y = y;

    return (struct ast *) node;
}

AST *newEdge (ast_node_t nodetype, YYLTYPE position, symbol *name, AST *source, AST *target, AST *label)
{
    AST *edge = malloc(sizeof(GPEdge));
    
    if(!edge) {
      yyerror("insufficient space");
      exit(0);
    }
    edge->nodetype = nodetype; /* edge */
    edge->position = position;
    edge->name = name;
    edge->source = source;
    edge->target = target;
    edge->label = label;

    return (struct ast *) edge;
}

AST *newEdgePred (ast_node_t nodetype, YYLTYPE position, AST *source, AST *target, AST *label)
{
    AST *edgepred = malloc(sizeof(GPEdgePred));
    
    if(!edgepred) {
      yyerror("insufficient space");
      exit(0);
    }
    edgepred->nodetype = nodetype; /* if or try */
    edgepred->position = position;
    edgepred->source = source;
    edgepred->target = target;
    edgepred->label = label;

    return (struct ast *) edgepred;
}

AST *newDegree (ast_node_t nodetype, YYLTYPE position, symbol *name)
{
    AST *degree = malloc(sizeof(GPDegree));
    
    if(!degree) {
      yyerror("insufficient space");
      exit(0);
    }
    degree->nodetype = nodetype; /* indeg or outdeg */
    degree->position = position;
    degree->name = name;

    return (struct ast *) degree;
}

AST *newLength (ast_node_t nodetype, YYLTYPE position, AST *arg)
{
    AST *length = malloc(sizeof(GPLength));
    
    if(!typecheck) {
      yyerror("insufficient space");
      exit(0);
    }
    length->nodetype = nodetype; /* llen or slen */
    length->position = position;
    length->arg = arg;

    return (struct ast *) length;
}


AST *newTypeCheck (ast_node_t nodetype, YYLTYPE position, symbol *var)
{
    AST *typecheck = malloc(sizeof(GPTypeCheck));
    
    if(!typecheck) {
      yyerror("insufficient space");
      exit(0);
    }
    typecheck->nodetype = nodetype; /* int, string, atom */
    typecheck->position = position;
    typecheck->var = var;

    return (struct ast *) typecheck;
}

AST *newNumber (ast_node_t nodetype, YYLTYPE position, int val)
{
    AST *num = malloc(sizeof(GPNum));
    
    if(!num) {
      yyerror("insufficient space");
      exit(0);
    }
    num->nodetype = nodetype; /* num */
    num->position = position;
    num->val = val;

    return (struct ast *) num;
}

AST *newString (ast_node_t nodetype, YYLTYPE position, char *val)
{
    AST *string = malloc(sizeof(GPString));
    
    if(!string) {
      yyerror("insufficient space");
      exit(0);
    }
    string->nodetype = nodetype; /* string */
    string->position = position;
    string->val = val;

    return (struct ast *) string;
}

AST *newMark (ast_node_t nodetype, YYLTYPE position, mark_t val)
{
    AST *mark = malloc(sizeof(GPMark));
    
    if(!mark) {
      yyerror("insufficient space");
      exit(0);
    }
    mark->nodetype = nodetype; /* mark */
    mark->position = position;
    mark->val = val;

    return (struct ast *) mark;
}


