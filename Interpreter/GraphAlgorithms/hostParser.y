/* ////////////////////////////////////////////////////////////////////////////

  ======================
  GP 2 Host Graph Parser				
  ======================

  The Bison grammar for GP2 host graphs. The generated parser builds the host
  graph from the textual description of a host graph.

//////////////////////////////////////////////////////////////////////////// */

/* The names of the generated C files. */
%defines "hostParser.h"
%output "hostParser.c"

/* Code placed at the top of hostParser.h.  */
%code requires {
#include "graph.h"
int yylex(void);
}

/* Declarations of global variables placed at the bottom of hostParser.h. */ 
 %code provides {
extern Graph *host;
extern int *node_map;
extern string yytext;
extern FILE *yyin;
}

/* Code placed in hostParser.c. */
%{
#include "graph.h"

void yyerror(const char *error_message);

/* Variables used in the Graph construction. */
bool graph_made = false;
int host_nodes = 0;

%}

%locations /* Generates code to process locations of symbols in the source file. */

%union {  
  int num;   /* value of NUM token. */
  int dnum;   /* value of DNUM token. */
  char *str; /* value of STRING and CHAR tokens. */
  int id;  /* value of NodeID and EdgeID tokens. */
  int mark;  /* enum MarkTypes, value of MARK token. */
}

/* Single character tokens do not need to be explicitly declared. */
%token <mark> MARK
%token <num> NUM 
%token <dnum> DNUM
%token <str> STR      
%token <id> NODE_ID EDGE_ID
%token ROOT _EMPTY						

%error-verbose

%start HostGraph

%%

HostGraph: '[' '|' ']'  		{ }
         | '[' Position '|' '|' ']'  	{ }
         | '[' HostNodeList '|' ']'  	{ }
         | '[' Position '|' HostNodeList '|' ']' { }
         | '[' HostNodeList '|' HostEdgeList ']' { }
         | '[' Position '|' HostNodeList '|' HostEdgeList ']' { }

HostNodeList: HostNode			{ host_nodes++; }
            | HostNodeList HostNode	{ host_nodes++; }

HostNode: '(' NODE_ID RootNode ',' HostLabel ')' 	  { }
HostNode: '(' NODE_ID RootNode ',' HostLabel Position ')' { }
    				
RootNode: /* empty */ 
	| ROOT 				{ } 

 /* Layout information for the editor. This is ignored by the parser. */
Position: '(' DNUM ',' DNUM ')'         { } 
        | '(' NUM ',' NUM ')'           { } 
        | '(' DNUM ',' NUM ')'          { } 
        | '(' NUM ',' DNUM ')'          { }

HostEdgeList: HostEdge			{ }
            | HostEdgeList HostEdge	{ } 

HostEdge: '(' EDGE_ID ',' NODE_ID ',' NODE_ID ',' HostLabel ')'
					{ if(!graph_made)
					  {
					     host = GRAPHinit(host_nodes);
					     graph_made = true;
					  }
					  GRAPHinsertE(host, $4, $6, directed); }

HostLabel: HostList			{ }
         | _EMPTY			{ }
         | HostList '#' MARK	  	{ }
         | _EMPTY '#' MARK	  	{ }

HostList: HostAtom 			{ } 
        | HostList ':' HostAtom		{ } 


HostAtom: NUM 				{ }
        | '-' NUM 	 	        { }
        | STR 				{ }
%%

/* Bison calls yyerror whenever it encounters an error. It prints error
 * messages to stderr and log_file. */
void yyerror(const char *error_message)
{
   fprintf(stderr, "Error at '%c': %s\n\n", yychar, error_message);
}

  
