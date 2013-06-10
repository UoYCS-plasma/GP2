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
* Potential issue: Positions can be represented with decimal numbers. The compiler may be able to ignore this, depending on what the editor does. If not, then unclear whether to represent decimal numbers as a separate token from integers. Probably. 
*
* Edit action code to return yytext[0] for single characters.
*
* 29/5/13: Added tokens INTER (interface keyword), EMPTY (empty keyword) and NE (!=).
* 10/6/13: Added token INJECTIVE (injective matching flag), changed INTER to INTERFACE, modified
* rules and tokens for single characters
* 
* ///////////////////////////////////////////////////////////////////////////////////////////////// */


%option noyywrap nodefault case-insensitive yylineno

/* yywrap is an old flex library routine to manage multiple input files. This is done manually here */
/* nodefault removes default action if the input rules don't cover all possible input. */
/* case-insensitive tells flex to treat upper- and lowercase the same */
/* yylineno is a flex-maintained integer variable storing the current line number of input */

/* exclusive start state for ignoring GP2 comments */
%x COMMENT	    

%{

# include "gpparser.h"
# include "gpparser-tab.h"

typedef enum {
   END = 258, 						     /* End of file. Value 258 avoids clashes with character literals */		
   MAIN, IF, TRY, THEN, ELSE, SKIP, FAIL,                    /* Program text keywords */
   WHERE, AND, OR, NOT, EDGE, TRUE, FALSE, INDEG, OUTDEG,    /* Schema condition keywords */
   INT, STRING, ATOM, LIST,                                  /* Type keywords */
   INTERFACE, EMPTY, INJECTIVE,				     /* Other keywords */
   NUM, STR, ID, ROOT, ARROW,                                /* Numbers, strings, identifiers, root node, arrow */
   NE, GTE, LTE, EQ = '=', GT = '>', LT = '<', 		     /* Boolean operators */
   BAR = '|', COMMA = ',',      			     /* Delimiters */
   SEQ = ';', ALAP = '!',                                    /* Program operators */
   DOT = '.', COLON = ':',				     /* Label operators */
   ADD = '+', SUB = '-', MUL = '*', DIV = '/',  	     /* Arithmetic operators */
   LPAR = '(', RPAR = ')', LBRACE = '{', RBRACE = '}'        /* Left and right brackets */ 	
} Token;

/* The single character tokens are declared after the other tokens to make the enumeration
work when compiling this Flex scanner. */

void printToken(Token t)
{
   if (t == MAIN)	printf("MAIN");
   if (t == IF)		printf("IF");
   if (t == TRY)	printf("TRY");
   if (t == THEN)	printf("THEN");
   if (t == ELSE)	printf("ELSE");
   if (t == SKIP)	printf("SKIP");
   if (t == FAIL)	printf("FAIL");
   if (t == INTERFACE)	printf("INTERFACE");
   if (t == EMPTY)	printf("EMPTY");
   if (t == WHERE)	printf("WHERE");
   if (t == INJECTIVE)	printf("INJECTIVE");
   if (t == AND)	printf("AND");
   if (t == OR)	        printf("OR");
   if (t == NOT)	printf("NOT");
   if (t == EDGE)	printf("EDGE");
   if (t == TRUE)	printf("TRUE");
   if (t == FALSE)	printf("FALSE");
   if (t == INDEG)	printf("INDEG");
   if (t == OUTDEG)	printf("OUTDEG");
   if (t == INT)	printf("INT");
   if (t == STRING)	printf("STRING");
   if (t == ATOM)	printf("ATOM");
   if (t == LIST)	printf("LIST");
   if (t == LPAR)	printf("(");
   if (t == RPAR)	printf(")");
   if (t == LBRACE)	printf("{");
   if (t == RBRACE)	printf("}");
   if (t == BAR)	printf("|");
   if (t == COMMA)	printf(",");
   if (t == ARROW)	printf("ARROW");
   if (t == SEQ)	printf(";");
   if (t == ALAP)	printf("!");
   if (t == DOT)	printf(".");
   if (t == COLON)	printf(":");
   if (t == ADD)	printf("+");
   if (t == SUB)	printf("-");
   if (t == MUL)	printf("*");
   if (t == DIV)	printf("/");
   if (t == EQ)	        printf("=");
   if (t == NE)	        printf("NE");
   if (t == GT)		printf(">");
   if (t == GTE)	printf("GTE");
   if (t == LT)		printf("<");
   if (t == LTE)	printf("LTE");
   if (t == NUM)	printf("NUM");
   if (t == STR)	printf("STR");
   if (t == ID)		printf("ID");
   if (t == ROOT)	printf("ROOT");
   if (t == END)	printf("END");
}

typedef union {
  int num;    /* A NUM token contains as number */
  char *str;  /* STRING and ID tokens contain a string */
} TokenValue;

TokenValue yylval;   /* stores semantic value of tokens */

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
interface	    return INTERFACE;
empty		    return EMPTY;
injective           return INJECTIVE;

 /* single character tokens */
"(" |		  
")" |		  
"{" |		  
"}" |		
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
"<"		    return yytext[0];

 /* multiple character tokens */
"(R)" 		    return ROOT;
"!="		    return NE;
"=>"                return ARROW;
">="	            return GTE;
"<="	            return LTE;

[_a-z][a-z0-9_-]*   { yylval.str = yytext; return ID; }
\"[a-z]*\"          { yylval.str = yytext; return STR; }
[ \t\n\r]+          /* ignore white space */
<<EOF>>             return END;      
.                   printf("%s:%d: Mystery character '%s'\n", curfilename, yylineno, yytext);

%%



