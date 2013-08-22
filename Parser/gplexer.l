/* ///////////////////////////////////////////////////////////////////////////////////////////////// */

/*                                       gplexer.lex                                          
*
* This is a Flex scanner for the textual program format of GP2. 
* The scanner tokenises its input files and passes them to the Bison parser. 
*
* Created on 10/5/2013 by Chris Bak 
*
* 
* ///////////////////////////////////////////////////////////////////////////////////////////////// */


%option noyywrap nodefault yylineno

/* yywrap is an old flex library routine to manage multiple input files. This is done manually here */
/* nodefault removes default action if the input rules don't cover all possible input. */
/* yylineno is a flex-maintained integer variable storing the current line number of input */



%{

#include "gpparser.h"
#include "gpparser.tab.c"

int yycolumn = 1;

/* macro YY_USER_ACTION invoked for each token recognised by yylex before calling action
   code. Here it is defined to track line and column numbers for bison locations. */

#define YY_USER_ACTION yylloc.filename = filename; \
   yylloc.first_line = yylloc.last_line = yylineno; \
   yylloc.first_column = yycolumn; yylloc.last_column = yycolumn + yyleng-1; \
   yycolumn += yyleng;	

%}

 /* exclusive start state for ignoring GP2 comments */
%x COMMENT	    

%%

"/*"		    BEGIN(COMMENT);
<COMMENT>"*/"       BEGIN(INITIAL);
<COMMENT>([^*\n])+|.  /* ignore all characters except '*' */
<COMMENT>(\n)	   { yycolumn = 1; } /* reset yycolumn on newline */
<COMMENT><<EOF>>   { printf("%s:%d: Unterminated comment\n", filename, yylineno); return 0; }

[0-9]+              { yylval.num = atoi(yytext); return NUM; } 

 /* keywords */
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
edge                return EDGE;
true                return TRUE;
false               return FALSE;
indeg 		    return INDEG;
outdeg		    return OUTDEG;
int		    return INT;
string	            return STRING;
atom		    return ATOM;
list		    return LIST;
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

 /* keywords for GP types */
int		    { yylval.type = GP_INT; return INT; } 
string		    { yylval.type = GP_STRING; return STRING; } 
atom     	    { yylval.type = GP_ATOM; return ATOM; } 
list		    { yylval.type = GP_LIST; return LIST; } 

 /* relational operators */
">"		    { yylval.type = GT; return CMP; } 
"<"          	    { yylval.type = LT; return CMP; } 
"="                 { yylval.rel = EQ; return CMP; }
"!="		    { yylval.rel = NEQ; return CMP; }
">="	            { yylval.rel = GTE; return CMP; }
"<="	            { yylval.rel = LTE; return CMP; }

 /* single character tokens */
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
"#"		 return yytext[0];
"=>"             return ARROW;
"(R)"	 	 return ROOT;

 /* macro identifiers must start with a capital letter 
  * other identifiers start with a lowercase letter    */
[A-Z][a-zA-Z0-9_-]*   { yylval.str = yytext; return MACID; } /* other characters may be allowed */
[a-z][a-zA-Z0-9_-]*   { yylval.str = yytext; return ID; }
\"[a-z_-]*\"          { yylval.str = yytext; return STR; }
[ \t\r]+              /* ignore white space */
\n		      { yycolumn = 1;}  /* reset yycolumn on newline */
.                     printf("%s:%d: Unexpected symbol '%s'\n", filename, yylineno, yytext);

%%



