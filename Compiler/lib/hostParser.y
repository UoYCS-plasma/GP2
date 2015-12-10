/* ////////////////////////////////////////////////////////////////////////////

  ======================
  GP 2 Host Graph Parser				
  ======================

  The Bison grammar for GP2 host graphs. The generated parser builds the host
  graph from the textual description of a host graph.
  
  =============
  Update Policy
  =============
  Changes to the GP 2 syntax as defined in this file must be mirrored in the
  host graph parser of the graphical editor, with possible updates to the
  action rules of the editor's parser.

//////////////////////////////////////////////////////////////////////////// */

/* The names of the generated C files. */
%defines "hostParser.h"
%output "hostParser.c"

/* Code placed at the top of hostParser.h.  */
%code requires {
#include "graph.h"
#include "label.h"
int yylex(void);
}

/* Declarations of global variables placed at the bottom of hostParser.h. */ 
 %code provides {
extern struct Graph *host;
extern int *node_map;
extern string yytext;
extern FILE *yyin;
}

/* Code placed in hostParser.c. */
%{
#include "graph.h"

void yyerror(const char *error_message);

/* Variables used in the Graph construction. */
bool is_root = false;
int length = 0;

/* Temporary automatic storage for host lists before they are added to the list hashtable. */
HostAtom array[64];
HostList *host_list = NULL;
%}

%locations /* Generates code to process locations of symbols in the source file. */

%union {  
  int num;   /* value of NUM token and NodeID/EdgeID. */
  int dnum;   /* value of DNUM token. */
  char *str; /* value of STRING and CHAR tokens. */
  int mark;  /* enum MarkTypes, value of MARK token. */
}

/* Single character tokens do not need to be explicitly declared. */
%token <mark> MARK
%token <num> NUM  
%token <dnum> DNUM
%token <str> STR      
%token ROOT _EMPTY						

%union {  
   struct HostLabel label;
   struct HostAtom atom; 
} 

%type <label> HostLabel
%type <atom> HostAtom
%type <num> NodeID EdgeID

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

HostNode: '(' NodeID RootNode ',' HostLabel ')' { node_map[$2] = addNode(host, is_root, $5); 
 				   	          is_root = false; } 
        | '(' NodeID RootNode ',' HostLabel Position ')'
    					{ node_map[$2] = addNode(host, is_root, $5); 
 					  is_root = false; } 

RootNode: /* empty */ 
	| ROOT 				{ is_root = true; }

 /* Layout information for the editor. This is ignored by the parser. */
Position: '<' DNUM ',' DNUM '>'         { } 
        | '<' NUM ',' NUM '>'           { } 
        | '<' NUM ',' '-' NUM '>'       { } 
        | '<' '-' NUM ','  NUM '>'      { } 
        | '<' '-' NUM ',' '-' NUM '>'   { } 
        | '<' DNUM ',' NUM '>'          { } 
        | '<' DNUM ',' '-' NUM '>'      { } 
        | '<' NUM ',' DNUM '>'          { }
        | '<' '-' NUM ',' DNUM '>'      { }

HostEdgeList: HostEdge			{ }
            | HostEdgeList HostEdge	{ } 

HostEdge: '(' EdgeID ',' NodeID ',' NodeID ',' HostLabel ')'
					{ addEdge(host, $8, node_map[$4], node_map[$6]); }

NodeID:  NUM				/* default $$ = $1 */
EdgeID:  NUM				/* default $$ = $1 */

HostLabel: HostList			{ if(length == 0) $$ = blank_label;	
				          else {
					     host_list = makeHostList(array, length, true);
					     $$ = makeHostLabel(NONE, length, host_list); 
					     length = 0;
					     host_list = NULL; 
					  }
	  				}
         | HostList '#' MARK	  	{ if(length == 0) $$ = makeEmptyLabel($3); 
	 			          else {
					     host_list = makeHostList(array, length, true); 
                                             $$ = makeHostLabel($3, length, host_list); 
					     length = 0;
					     host_list = NULL;
					  }
					}

HostList: HostAtom 			{ assert(length == 0);
					  array[length++] = $1; } 
        | _EMPTY			{ }
        | HostList ':' HostAtom		{ array[length++] = $3; } 
        | HostList ':' _EMPTY    	/* default $$ = $1 */


HostAtom: NUM 				{ $$.type = 'i'; 
					  $$.num = $1; }
        | '-' NUM 	 	        { $$.type  = 'i'; 
					  $$.num = -($2); }
        | STR 				{ $$.type = 's'; 
					  $$.str = $1; }

%%

/* Bison calls yyerror whenever it encounters an error. It prints error
 * messages to stderr and log_file. */
void yyerror(const char *error_message)
{
   fprintf(stderr, "Error at '%c': %s\n\n", yychar, error_message);
}

  
