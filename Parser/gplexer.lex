/* ///////////////////////////////////////////////////////////////////////////////////////////////// */

/*                                       gplexer.lex  
*                                        Version 1.1

* This is a Flex scanner for the textual program format of GP2. 
* The scanner tokenises its input files and passes them to the Bison parser. 

* Created on 10/5/2013 by Chris Bak 

* Potential issue: 'or' is a program keyword and a condition keyword. Not sure how to deal with this yet.

* Potential issue: Positions can be represented with decimal numbers. The compiler may be able to ignore this, depending on what the editor does. If not, then unclear whether to represent decimal numbers as a separate token from integers. Probably. */

/* ///////////////////////////////////////////////////////////////////////////////////////////////// */


%option noyywrap nodefault case-insensitive yylineno

/* yywrap is an old flex library routine to manage multiple input files. This is done manually here */
/* nodefault removes default action if the input rules don't cover all possible input. */
/* case-insensitive tells flex to treat upper- and lowercase the same */
/* yylineno is a flex-maintained integer variable storing the current line number of input */

%x COMMENT	/* exclusive start state for ignoring GP2 comments */    

%{

typedef enum {
   MAIN, IF, TRY, THEN, ELSE, SKIP, FAIL,                    /* Program text keywords */
   WHERE, AND, OR, NOT, EDGE, TRUE, FALSE, INDEG, OUTDEG,    /* Schema condition keywords */
   INT, STRING, ATOM, LIST,                                  /* Types keywords */
   LPAR, RPAR, LBRACE, RBRACE, 				     /* Left and right brackets */
   BAR, COMMA, ARROW,					     /* Delimiters */
   SEQ, ALAP,                                                /* Program operators */
   DOT, COLON,						     /* Label operators */
   ADD, SUB, MUL, DIV,  				     /* Arithmetic operators */
   EQ, GT, GTE, LT, LTE,				     /* Boolean operators */
   NUM, STR, ID, ROOT,                                       /* Numbers, strings, identifiers, root node */
   END 							     /* End of file */
} Token;

void printToken(Token t)
{
   if (t == MAIN)	printf("MAIN");
   if (t == IF)		printf("IF");
   if (t == TRY)	printf("TRY");
   if (t == THEN)	printf("THEN");
   if (t == ELSE)	printf("ELSE");
   if (t == SKIP)	printf("SKIP");
   if (t == FAIL)	printf("FAIL");
   if (t == WHERE)	printf("WHERE");
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
   if (t == LPAR)	printf("LPAR");
   if (t == RPAR)	printf("RPAR");
   if (t == LBRACE)	printf("LBRACE");
   if (t == RBRACE)	printf("RBRACE");
   if (t == BAR)	printf("BAR");
   if (t == COMMA)	printf("COMMA");
   if (t == ARROW)	printf("ARROW");
   if (t == SEQ)	printf("SEQ");
   if (t == ALAP)	printf("ALAP");
   if (t == DOT)	printf("DOT");
   if (t == COLON)	printf("COLON");
   if (t == ADD)	printf("ADD");
   if (t == SUB)	printf("SUB");
   if (t == MUL)	printf("MUL");
   if (t == DIV)	printf("DIV");
   if (t == EQ)	        printf("EQ");
   if (t == GT)		printf("GT");
   if (t == GTE)	printf("GTE");
   if (t == LT)		printf("LT");
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
"("		    return LPAR;
")"		    return RPAR;
"(R)" 		    return ROOT;
"{"		    return LBRACE;
"}"		    return RBRACE;
"|"		    return BAR;
","                 return COMMA;
";"		    return SEQ;
"!"		    return ALAP;
"." 		    return DOT;
":"		    return COLON;
"+"		    return ADD;
"-"		    return SUB;
"*"		    return MUL;
"/"		    return DIV;
"="                 return EQ;
"=>"                return ARROW;
">="	            return GTE;
">" 		    return GT; 
"<="	            return LTE;
"<"		    return LT;
[_a-z][a-z0-9_-]*   { yylval.str = yytext; return ID; }
\"[a-z]*\"          { yylval.str = yytext; return STR; }
[ \t\n\r]+          /* ignore white space */
<<EOF>>             return END;      
.                   printf("%s:%d: Mystery character '%s'\n", curfilename, yylineno, yytext);

%%

int main(int argc, char** argv) {
  Token t; 	/* stores current token */
  int i;        
  if(argc < 2) {
    fprintf(stderr, "ERROR: filename required\n");
    return 1;
  }
  for(i=1; i<argc; i++) {	/* Iterate over all files from the command line */

     if(!(yyin = fopen(argv[i], "r"))) {	/* Flex scans from yyin which is assigned to input file. */
       perror(argv[1]);
       yylineno = 1;	/* reset yylineno */
       return 1;
       }
  
  curfilename = argv[i];
  printf("Processing %s\n\n", curfilename);

     do {
       t = yylex();	/* flex function to read tokens from input */
       printToken(t);       
       if (t == NUM) printf("(%d)", yylval.num);
       if (t == STRING) printf("(%s)", yylval.str);
       if (t == ID) printf("(%s)", yylval.str);
       printf(" ");
     } while(t!=END);  /* t=END when end of file has been reached */
     
  printf("\n\n");
  } 

return 0;
} 


