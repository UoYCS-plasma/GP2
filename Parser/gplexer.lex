/* ///////////////////////////////////////////////////////////////////////////////////////////////// */

/*                                       gplexer.lex                                          
*
* This is a Flex scanner for the textual program format of GP2. 
* The scanner tokenises its input files and passes them to the Bison parser. 
*
* Created on 10/5/2013 by Chris Bak 
*
* Potential issue: 'or' is a program keyword and a condition keyword. Not sure how to deal with this yet.
*
* ///////////////////////////////////////////////////////////////////////////////////////////////// */


%option noyywrap nodefault yylineno

/* yywrap is an old flex library routine to manage multiple input files. This is done manually here */
/* nodefault removes default action if the input rules don't cover all possible input. */
/* yylineno is a flex-maintained integer variable storing the current line number of input */

/* exclusive start state for ignoring GP2 comments */
%x COMMENT	    

%{

# include "gpparser.h"
# include "gpparser.tab.c"

char *curfilename;   /* name of current input file; used for error messages */

%}

%%

"/*"		    BEGIN(COMMENT);
<COMMENT>"*/"       BEGIN(INITIAL);
<COMMENT>([^*]|\n)+|.  /* ignore all characters except '*' */
<COMMENT><<EOF>>   { printf("%s:%d: Unterminated comment\n", curfilename, yylineno); return 0; }

[0-9]+              { yylval.num = atoi(yytext); return NUM; } 

 /* keywords */
main		    return MAIN;
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
red		    return RED;
green		    return GREEN;
blue		    return BLUE;
grey		    return GREY;
dashed		    return DASHED;
interface	    return INTERFACE;
empty		    return EMPTY;
injective           return INJECTIVE;

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
"=" |                 
">" |		
"<" |	 	   
"#"	 return yytext[0];

 /* multiple character tokens */
"(R)" 		    return ROOT;
"!="		    return NE;
"=>"                return ARROW;
">="	            return GTE;
"<="	            return LTE;

 /* macro identifiers must start with a capital letter 
  * other identifiers start with a lowercase letter    */
[A-Z][a-zA-Z0-9_-]*   { yylval.str = yytext; return MACID; }
[a-z][a-zA-Z0-9_-]*   { yylval.str = yytext; return ID; }
\"[a-z]*\"          { yylval.str = yytext; return STR; }
[ \t\n\r]+          /* ignore white space */
.                   printf("%s:%d: Mystery character '%s'\n", curfilename, yylineno, yytext);

%%



