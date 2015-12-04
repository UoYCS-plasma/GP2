/*////////////////////////////////////////////////////////////////////////////

  ================================
  GP 2 Host Graph Lexical Analyser
  ================================                           

  The Flex specification for GP 2 host graph tokens.
  
  =============
  Update Policy
  =============
  Changes to the GP 2 syntax as defined in this file must be mirrored in the
  host graph lexer of the graphical editor.

/////////////////////////////////////////////////////////////////////////// */ 

/* yywrap is an old flex library routine to manage multiple input files. 
 * This is done manually here.
 * nodefault removes default action if the input rules don't cover all 
 * possible input.
 */

%option noyywrap nodefault noinput nounput
%option outfile="hostLexer.c"

%{
#include "hostParser.h" 

int yycolumn = 1;
%}

/* Exclusive start states for comments, string literals, and character 
 * constants. */
%x IN_COMMENT	
%x IN_STRING    

%%

"//"		  		 BEGIN(IN_COMMENT);
<IN_COMMENT>(\n)	  	 { yycolumn = 1; 
                                   BEGIN(INITIAL); } 
<IN_COMMENT>([^\n])+|.  	 /* Ignore characters except newline. */
<IN_COMMENT><<EOF>>  		 { fprintf(stderr, "Warning: Unterminated comment.\n");
			           print_to_log("Line %d: Unterminated comment", 
                                                yylineno); 
				   return 0; }

 /* The empty string is valid GP2 syntax. */
"\"\""				 { yylval.str = strdup(""); return STR; } 
"\""	            		 BEGIN(IN_STRING);
<IN_STRING>"\""        		 BEGIN(INITIAL);
<IN_STRING>[a-zA-Z0-9_ ]{0,63} 	 { yylval.str = strdup(yytext); return STR; }
<IN_STRING>(\n)                  { print_to_log("%d.%d-%d.%d: String "
          				        "continues on new line.\n", 
                                        yylloc.first_line, yylloc.first_column, 
                                        yylloc.last_line, yylloc.last_column); 	
                                   return 0; }
<IN_STRING>[^\"a-zA-Z0-9_ ]       { fprintf(stderr, "Warning: Invalid character "
                                            "in string: '%c'.\n", yytext[0]); 
			           print_to_log("%d.%d-%d.%d: Invalid character: "
          				        "'%c'.\n", 
                                           yylloc.first_line, yylloc.first_column, 
                                           yylloc.last_line, yylloc.last_column,
                                           yytext[0]);	
				   return 0; }
<IN_STRING><<EOF>>   		 { print_to_log("Line %d: Unterminated "
          				        "string.\n", yylineno);                   
                                   return 0; }  

[-+]?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)? { yylval.dnum = atof(yytext); return DNUM; } 
[0-9]+              { yylval.num = (int)strtol(yytext, NULL, 10); return NUM; } 

 /* Host graph keywords. */ 
empty		    return _EMPTY;
red		    { yylval.mark = RED; return MARK; } 
green		    { yylval.mark = GREEN; return MARK; } 
blue		    { yylval.mark = BLUE; return MARK; } 
grey		    { yylval.mark = GREY; return MARK; } 
dashed		    { yylval.mark = DASHED; return MARK; }

"(" |		  
")" |		  
"[" |
"]" |		
"|" |		 
"," |               
":" |     
"-" |
"<" |
">" |
"#"		    return yytext[0];

"(R)"	 	    return ROOT;

n[0-9]+  	   { yylval.id = (int)strtol(yytext+1, NULL, 10); return NODE_ID; }
e[0-9]+  	   { yylval.id = (int)strtol(yytext+1, NULL, 10); return EDGE_ID; }
[ \t\r\n]          /* Ignore white space. */
<<EOF>>		   { return 0; }
.                  { fprintf(stderr, "Error: Invalid symbol '%c'\n", yytext[0]);
		     return 0; }

%%

