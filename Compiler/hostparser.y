/* ////////////////////////////////////////////////////////////////////////////

  ==========
  GP2 Parser				
  ==========

  The Bison specification for GP2's parser. Defines GP2's abstract syntax
  and calls the appropriate AST constructor for each rule.
  
//////////////////////////////////////////////////////////////////////////// */

/* The names of the generated C files. */
%defines "runtime/hostParser.h"
%output "runtime/hostParser.c"

/* Code placed at the top of hostParser.h.  */
%code requires {
#include "../graph.h"
#include "../label.h"
}

/* Declarations of global variables placed at the bottom of parser.h. */ 
 %code provides {
extern struct Graph *host;
extern int *node_map;
extern string yytext;
extern FILE *yyin;
}

/* Code placed in hostParser.c. */
%{
#include "../graph.h"
#include "../globals.h"

void yyerror(const char *error_message);

/* Flags used in the Graph construction. */
bool is_root = false;

%}

%locations /* Generates code to process locations of symbols in the source file. */

%union {  
  int num;   /* value of NUM token. */
  char *str; /* value of STRING and CHAR tokens. */
  int id;  /* value of NodeID and EdgeID tokens. */
  int mark;  /* enum MarkTypes, value of MARK token. */
}

/* Single character tokens do not need to be explicitly declared. */
%token <mark> MARK
%token <num> NUM 
%token <str> STR      
%token <id> NODE_ID EDGE_ID
%token ROOT _EMPTY						

%union {  
   struct Label label;
   struct Atom *list; 
   struct Atom atom; 
} 

%type <label> HostLabel
%type <list> HostList
%type <atom> HostExp

%error-verbose

%start HostGraph

%%

HostGraph: '[' '|' ']'  		{ }
         | '[' Position '|' '|' ']'  	{ }
         | '[' HostNodeList '|' ']'  	{ }
         | '[' Position '|' HostNodeList '|' ']' { }
         | '[' HostNodeList '|' HostEdgeList ']' { }
         | '[' Position '|' HostNodeList '|' HostEdgeList ']' { }

HostNodeList: HostNode			{ }
            | HostNodeList HostNode	{ }

HostNode: '(' NODE_ID RootNode ',' HostLabel ')' { node_map[$2] = addNode(host, is_root, $5); 
 				   	          is_root = false; } 
HostNode: '(' NODE_ID RootNode ',' HostLabel Position ')'
    					{ node_map[$2] = addNode(host, is_root, $5); 
 					  is_root = false; } 

RootNode: /* empty */ 
	| ROOT 				{ is_root = true; }

 /* Layout information for the editor. This is ignored by the parser. */
Position: '(' NUM ',' NUM ')'           { } 

HostEdgeList: HostEdge			{ }
            | HostEdgeList HostEdge	{ } 

HostEdge: '(' EDGE_ID ',' NODE_ID ',' NODE_ID ',' HostLabel ')'
					{ addEdge(host, $8, node_map[$4], node_map[$6]); }

HostLabel: HostList			{ $$ = makeHostLabel(NONE, 0, $1); }
         | _EMPTY			{ $$ = makeEmptyLabel(NONE); }
         | HostList '#' MARK	  	{ $$ = makeHostLabel($3, 0, $1); }
         | _EMPTY '#' MARK	  	{ $$ = makeEmptyLabel($3);  }

HostList: HostExp 			{ $$ = prependAtom(NULL, $1); } 
        | HostList ':' HostExp 		{ $$ = prependAtom($1, $3); }


HostExp: NUM 				{ $$.type = INTEGER_CONSTANT; 
					  $$.number = $1;}
       | '-' NUM 	 	        { $$.type = INTEGER_CONSTANT; 
					  $$.number = -($2);}
       | STR 				{ $$.type = STRING_CONSTANT; 
					  $$.string = $1;
					  if($1) free($1); }
%%

/* Bison calls yyerror whenever it encounters an error. It prints error
 * messages to stderr and log_file. */
void yyerror(const char *error_message)
{
   fprintf(stderr, "Error at '%c': %s\n\n", yychar, error_message);
}

  
