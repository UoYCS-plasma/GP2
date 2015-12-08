/*////////////////////////////////////////////////////////////////////////////

  ====================
  GP2 Lexical Analyser
  ====================                            

  The Flex specification for the GP2 lexical analyser.
  Defines the tokens of GP2 and their associated regular expressions.
  
  =============
  Update Policy
  =============
  Changes to the GP 2 grammar as defined in this file must be mirrored in the
  file GP2-editor/src/developer/translate/gplexer.lex in order to maintain
  consistency with the graphical editor.

/////////////////////////////////////////////////////////////////////////// */ 

/* yywrap is an old flex library routine to manage multiple input files. 
 * This is done manually here.
 * nodefault removes default action if the input rules don't cover all 
 * possible input.
 * yylineno is a flex-maintained integer variable storing the current line 
 * number of input. 
 */

%option noyywrap nodefault yylineno noinput nounput
%option outfile="lexer.c"

%{
#include "globals.h"
#include "parser.h" 

int yycolumn = 1;

/* Defined in main.c according to which parser should be invoked. */
extern int parse_target;

/* The macro YY_USER_ACTION is invoked for each token recognised by yylex
 * before calling action code. Here it is defined to track line and column
 * numbers for bison locations. yylloc is a variable of type struct YYLTYPE,
 * a type used by Bison and defined in ast.h. 
 */

#define YY_USER_ACTION  \
   yylloc.first_line = yylloc.last_line = yylineno; \
   yylloc.first_column = yycolumn; yylloc.last_column = yycolumn + yyleng-1; \
   yycolumn += yyleng;	

%}

 /* Exclusive start states for comments, string literals, and character 
  * constants. */
%x IN_COMMENT	
%x IN_STRING    

%%

%{
  /* Return the appropriate token according to the grammar to be parsed with.
   * Bison token GP_PROGRAM triggers parsing with the program grammar.
   * Bison token GP_GRAPH triggers parsing with the host graph grammar.
   */  
  
  if(parse_target == 1) {
     parse_target = 0; 
     return GP_PROGRAM;
  }
  if(parse_target == 2) {
     parse_target = 0; 
     return GP_GRAPH;  
  }
  if(parse_target == 3) {
     parse_target = 0; 
     return GP_RULE;  
  }
%}


"//"		  		 BEGIN(IN_COMMENT);
<IN_COMMENT>(\n)	  	 { yycolumn = 1; 
                                   BEGIN(INITIAL); } 
<IN_COMMENT>([^\n])+|.  	 /* Ignore characters except newline. */
<IN_COMMENT><<EOF>>  		 { print_to_console("Warning: Unterminated "
                                                    "comment.\n");
			           print_to_log("Line %d: Unterminated comment", 
                                                yylineno); 
				   return 0; }

 /* The empty string is valid GP2 syntax. */
"\"\""				 { yylval.str = strdup(""); return STR; } 
"\""	            		 BEGIN(IN_STRING);
<IN_STRING>"\""        		 BEGIN(INITIAL);
 /* ASCII characters 040-176 (octal) */
<IN_STRING>[\040,\041,\043-\172]{0,63}  { yylval.str = strdup(yytext); return STR; }
<IN_STRING>(\n)                  { print_to_log("%d.%d-%d.%d: String "
          				        "continues on new line.\n", 
                                        yylloc.first_line, yylloc.first_column, 
                                        yylloc.last_line, yylloc.last_column); 	
                                   return 0; }
<IN_STRING><<EOF>>   		 { print_to_log("Line %d: Unterminated "
          				        "string.\n", yylineno);                   
                                   return 0; }  



 /* GP2 keywords */ 
Main		    return MAIN;
if	            return IF;
try		    return TRY;
then 	  	    return THEN;
else		    return ELSE;
skip                return SKIP;
fail	 	    return FAIL;
break	 	    return BREAK;
where		    return WHERE;
and                 return AND;
or                  return OR;
not                 return NOT;
edge                return EDGETEST;
indeg 		    return INDEG;
outdeg		    return OUTDEG;
interface	    return INTERFACE;
empty		    return _EMPTY;
length		    return _LENGTH;

 /* Keywords for node and edge marks */
red		    { yylval.mark = RED; return MARK; } 
green		    { yylval.mark = GREEN; return MARK; } 
blue		    { yylval.mark = BLUE; return MARK; } 
grey		    { yylval.mark = GREY; return MARK; } 
dashed		    { yylval.mark = DASHED; return MARK; }
 /* Any has a distinct token since it cannot appear in the host graph and
    therefore must be distinguished from the other marks. */
any		    { yylval.mark = ANY; return ANY_MARK; } 

 /* Keywords for GP2 types */
int		    return INT;  
char		    return CHARACTER;
string		    return STRING;  
atom     	    return ATOM;  
list		    return LIST;  

[-+]?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)? { yylval.dnum = atof(yytext); return DNUM; } 
 
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
"(B)"            return BIDIRECTIONAL;
"!="		 return NEQ; 
">="	         return GTEQ; 
"<="	         return LTEQ; 

[0-9]+              { yylval.num = atoi(yytext); return NUM; } 

 /* Procedure identifiers must start with a capital letter.
  * All other identifiers start with a lowercase letter.
  * Identifier names are retained with strdup which itself calls malloc,
  * so these strings need to be explicitly freed. */  
[A-Z][a-zA-Z0-9_]{0,63}  { yylval.id = strdup(yytext); return PROCID; } /* other characters may be allowed. */
[a-z][a-zA-Z0-9_]{0,63}  { yylval.id = strdup(yytext); return ID; }

[ \t\r]+              /* ignore white space */
\n		      { yycolumn = 1; }  /* reset yycolumn on newline */
<<EOF>>		      { return 0; }
.                     { print_to_console("Error: Invalid symbol '%c'\n", yytext[0]);
			return 0; }


%%

