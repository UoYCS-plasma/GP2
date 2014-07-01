/*////////////////////////////////////////////////////////////////////////////

  ====================================
  gplexer.lex - Chris Bak (10/05/2013)
  ====================================                                  

  The Flex lexical analyser for GP2. It scans the input files, sending tokens 
  to the Bison parser when required.

/////////////////////////////////////////////////////////////////////////// */ 

/* yywrap is an old flex library routine to manage multiple input files. 
 * This is done manually here.
 * nodefault removes default action if the input rules don't cover all 
 * possible input.
 * yylineno is a flex-maintained integer variable storing the current line 
 * number of input. 
 */

%option noyywrap nodefault yylineno

%{

#include "ast.h" /* Printing macros and enum MarkType */
#include "gpparser.tab.h" /* Token definitions */
#include <stdbool.h>
#include <string.h> 

int yycolumn = 1;

extern int abort_scan; /* Defined in main.c */
extern FILE *log_file; /* Defined in main.c */

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

 /* Exclusive start state for ignoring GP2 comments. */
%x IN_COMMENT	

 /* Exclusive start state for discarding double quotes in string constants. */
%x IN_STRING    

 /* Exclusive start state for discarding single quotes in character constants. */
%x IN_CHAR 

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
%}


"/*"		  		 BEGIN(IN_COMMENT);
<IN_COMMENT>"*/"      		 BEGIN(INITIAL);
<IN_COMMENT>([^*\n])+|.  	 /* ignore all characters except '*' */
<IN_COMMENT>(\n)	  	 { yycolumn = 1; } /* reset yycolumn on newline */
<IN_COMMENT><<EOF>>  		 { print_to_console("Warning: Unterminated "
                                                    "comment.\n");
			           print_to_log("Line %d: Unterminated comment", 
                                                yylineno); 
				   abort_scan = true;
				   yyterminate(); }

 /* The empty string is valid GP2 syntax. */
"\"\""				 { yylval.str = NULL; return STR; } 

"\""	            		 BEGIN(IN_STRING);
<IN_STRING>"\""        		 BEGIN(INITIAL);
<IN_STRING>[a-zA-Z0-9_]{0,63} 	 { yylval.str = strdup(yytext); return STR; }
<IN_STRING>(\n)                  { print_to_log("%d.%d-%d.%d: String "
          				         "continues on new line.\n", 
                                        yylloc.first_line, yylloc.first_column, 
                                        yylloc.last_line, yylloc.last_column); 	
				   abort_scan = true;
                                   yyterminate(); }
<IN_STRING>[^\"a-zA-Z0-9_]       { print_to_console("Warning: Invalid character "
                                                "in string: '%c'.\n", yytext[0]); 
			           print_to_log("%d.%d-%d.%d: Invalid character: "
          				        "'%c'.\n", 
                                           yylloc.first_line, yylloc.first_column, 
                                           yylloc.last_line, yylloc.last_column,
                                           yytext[0]);	
				   yylval.str = strdup(yytext); return STR; }
<IN_STRING><<EOF>>   		 { print_to_log("Line %d: Unterminated "
          				        "string.\n", yylineno);                   
				   abort_scan = true; 
                                   yyterminate(); }  

 /* The empty character is not valid GP2 syntax. */
"''"				 { print_to_console("Error: Empty character "
					            "expression.\n"); 
				   print_to_log("%d.%d-%d.%d: Empty character "
          				  "expression.\n", 
					  yylloc.first_line, yylloc.first_column, 
                                          yylloc.last_line, yylloc.last_column); 
		                    abort_scan = true;
				    yylval.str = NULL; return CHAR; }

'				BEGIN(IN_CHAR);
<IN_CHAR>'			BEGIN(INITIAL);
<IN_CHAR>[a-zA-Z0-9_]		{ yylval.str = strdup(yytext); return CHAR; }
<IN_CHAR>[a-zA-Z0-0_]{2,}       { print_to_console("Error: Invalid character "
					       "expression: '%s'.\n", yytext); 
				  print_to_log("%d.%d-%d.%d: Invalid character "
          				  "expression: '%s'.\n", 
					  yylloc.first_line, yylloc.first_column, 
                                          yylloc.last_line, yylloc.last_column,
                                          yytext); 
		                  abort_scan = true;
				  yylval.str = strdup(yytext); return CHAR; }
<IN_CHAR>(\n)                   { print_to_log("%d.%d-%d.%d: Character "
          				"expression continues on new line.\n", 
                                        yylloc.first_line, yylloc.first_column, 
                                        yylloc.last_line, yylloc.last_column);	
				   abort_scan = true;
                                   yyterminate(); }
<IN_CHAR>[^'a-zA-Z0-9_]         { print_to_console("Error: Invalid character: "
			 		           "'%c'.\n", yytext[0]); 
				  print_to_log("%d.%d-%d.%d: Invalid character: "
          				       "%c'.\n", 
					  yylloc.first_line, yylloc.first_column, 
                                          yylloc.last_line, yylloc.last_column,
                                          yytext[0]); 
		                  abort_scan = true;
				  yylval.str = strdup(yytext); return CHAR; }
<IN_CHAR><<EOF>>   	        { print_to_log("Line %d: Unterminated "
          		                       "character.\n", yylineno); 
				  abort_scan = true;
				  yyterminate(); }   

[0-9]+              { yylval.num = atoi(yytext); return NUM; } 

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

 /* Keywords for node and edge marks */

red		    { yylval.mark = RED; return MARK; } 
green		    { yylval.mark = GREEN; return MARK; } 
blue		    { yylval.mark = BLUE; return MARK; } 
grey		    { yylval.mark = GREY; return MARK; } 
dashed		    { yylval.mark = DASHED; return MARK; }
 /* Cyan has a distinct token since it cannot appear in the host graph and
    therefore must be distinguished from the other marks. */
cyan		    { yylval.mark = CYAN; return CYAN_MARK; } 

 /* keywords for GP2 types */

int		    return INT;  
char		    return CHAR;
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
"(B)"            return BIDIRECTIONAL;
"!="		 return NEQ; 
">="	         return GTEQ; 
"<="	         return LTEQ; 


 /* Procedure identifiers must start with a capital letter.
  * All other identifiers start with a lowercase letter.
  * Identifier names are retained with strdup which itself calls malloc,
  * so these strings need to be explicitly freed. 
  */  

[A-Z][a-zA-Z0-9_]{0,63}  { yylval.id = strdup(yytext); return PROCID; } /* other characters may be allowed. */
[a-z][a-zA-Z0-9_]{0,63}  { yylval.id = strdup(yytext); return ID; }

 /* This rule catches an invalid identifier: a sequence of digits followed
  * by one valid non-numeric identifier character followed by any valid 
  * identifier character. In this case, token ID is returned to continue
  * the parse and potentially catch more invalid identifiers. abort_scan is 
  * also set to prevent semantic checking from starting. 
  */

[0-9]+[a-zA-Z_][a-zA-Z0-9_]*  { print_to_console("Error (%s): Identifiers must "
     			              	"start with a letter.\n", yytext); 
		                print_to_log("%d.%d-%d.%d: Invalid identifier: "
				             "%s.\n",
			                yylloc.first_line, yylloc.first_column,
			                yylloc.last_line, yylloc.last_column,
					yytext);
			        abort_scan = true;
			        yylval.id = strdup(yytext);
			        return ID; }

[ \t\r]+              /* ignore white space */
\n		      { yycolumn = 1; }  /* reset yycolumn on newline */
<<EOF>>		      { yyterminate(); }
.                     { printf("Error: Invalid symbol '%c'\n", yytext[0]);
			abort_scan = true; }


%%

