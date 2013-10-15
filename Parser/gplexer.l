/*////////////////////////////////////////////////////////////////////////////

                             gplexer.lex                                          

  This is a Flex lexican analyser for the textual program format of GP2. 
  It scans the input file and sends a token to the Bison parser when required.

                     Created on 10/5/2013 by Chris Bak 

////////////////////////////////////////////////////////////////////////////*/ 


%option noyywrap nodefault yylineno

/* yywrap is an old flex library routine to manage multiple input files. This is done manually here.
   nodefault removes default action if the input rules don't cover all possible input.
   yylineno is a flex-maintained integer variable storing the current line number of input. */



%{

#include "ast.h"
#include "gpparser.tab.h" /* Token definitions */


int yycolumn = 1;

/* macro YY_USER_ACTION invoked for each token recognised by yylex before calling action
   code. Here it is defined to track line and column numbers for bison locations. */

#define YY_USER_ACTION  \
   yylloc.first_line = yylloc.last_line = yylineno; \
   yylloc.first_column = yycolumn; yylloc.last_column = yycolumn + yyleng-1; \
   yycolumn += yyleng;	

%}

/* exclusive start state for ignoring GP2 comments */
%x COMMENT	

/* exclusive start state for discarding double quotes in strings */
%x STRING    

%%

"/*"		    BEGIN(COMMENT);
<COMMENT>"*/"       BEGIN(INITIAL);
<COMMENT>([^*\n])+|.  /* ignore all characters except '*' */
<COMMENT>(\n)	   { yycolumn = 1; } /* reset yycolumn on newline */
<COMMENT><<EOF>>   { printf("%s:%d: Unterminated comment\n", file_name, yylineno); return 0; }

"\""	           BEGIN(STRING);
<STRING>"\""         BEGIN(INITIAL);
<STRING>[a-zA-Z0-9_-]*    { yylval.str = strdup(yytext); return STR; }
<STRING>[^\"a-zA-Z0-9_-]  { printf("%s:%d: Invalid character in string: %c\n",
                                    file_name, yylineno, yytext[0]); return 0; }
<STRING><<EOF>>    { printf("%s:%d: Unterminated string\n", file_name, yylineno); return 0; }     

[0-9]+             { yylval.num = atoi(yytext); return NUM; } 

 /* GP2 keywords */ 

Main		    return MAIN;
if	            return IF;
try		    return TRY;
then 	  	    return THEN;
else		    return ELSE;
skip                return SKIP;
fail	 	    return FAIL;
where		    return WHERE;
and                 return AND;
or                  return OR;
not                 return NOT;
edge                return EDGETEST;
true                return TRUE;
false               return FALSE;
indeg 		    return INDEG;
outdeg		    return OUTDEG;
interface	    return INTERFACE;
empty		    return EMPTY;
injective           return INJECTIVE;
llength		    return LLEN;
slength	            return SLEN;

 /* keywords for node and edge marks */

red		    { yylval.mark = RED; return MARK; } 
green		    { yylval.mark = GREEN; return MARK; } 
blue		    { yylval.mark = BLUE; return MARK; } 
grey		    { yylval.mark = GREY; return MARK; } 
dashed		    { yylval.mark = DASHED; return MARK; } 

 /* keywords for GP2 types */

int		    return INT;  
string		    return STRING;  
atom     	    return ATOM;  
list		    return LIST;  

 
"(" |		  
")" |		  
"{" |		  
"}" |
"[" |
"]" |		
"|" |		 
"," |               
";" | 		 
"!" |		
"." |		  
":" |		 
"+" |		  
"-" |		    
"*" |		  
"/" |	
">" |		   
"<" |         	  
"=" |            	    
"#"		 return yytext[0];

"=>"             return ARROW;
"(R)"	 	 return ROOT;
"!="		 return NEQ; 
">="	         return GTEQ; 
"<="	         return LTEQ; 

 /* Procedure identifiers must start with a capital letter. 
    All other identifiers start with a lowercase letter. */  
[A-Z][a-zA-Z0-9_-]*   { yylval.id = strdup(yytext); return PROCID; } /* other characters may be allowed. */
[a-z][a-zA-Z0-9_-]*   { yylval.id = strdup(yytext); return ID; }
[ \t\r]+              /* ignore white space */
\n		      { yycolumn = 1; }  /* reset yycolumn on newline */
.                     printf("%s:%d: Invalid symbol '%s'\n", file_name, yylineno, yytext);

%%



